{-# LANGUAGE CPP #-}

-- | Implementation module. Public API is documented in "Data.Text.Template".
module Data.Text.Template.Internal
    (
     -- * The @Template@ type
     Template,

     -- * The @Context@ type
     Context,
     ContextA,

     -- * Basic interface
     template,
     templateSafe,
     render,
     substitute,
     showTemplate,

     -- * Applicative interface
     renderA,
     substituteA,
    ) where

import Control.Applicative (Applicative(pure), (<$>))
import Control.Monad (liftM, liftM2, replicateM_)
import Control.Monad.State.Strict (State, evalState, get, put)
import Data.Char (isAlphaNum, isLower)
import Data.Function (on)
import Data.Maybe (fromJust, isJust)
import Data.Monoid (Monoid(mempty, mappend))
import Data.Traversable (traverse)
import Prelude hiding (takeWhile)

#ifdef HAVE_SEMIGROUP
import Data.Semigroup (Semigroup)
import qualified Data.Semigroup as Semigroup
#endif

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

-- -----------------------------------------------------------------------------

-- | A representation of a 'Data.Text' template, supporting efficient
-- rendering.
newtype Template = Template [Frag]

instance Eq Template where
    (==) = (==) `on` showTemplate

append :: Template -> Template -> Template
Template frags `append` Template frags' = Template $ frags ++ frags'

#ifdef HAVE_SEMIGROUP
-- | Property that holds:
--
-- @
-- template x <> template y = template $ x \`T.append\` y
-- @
instance Semigroup Template where
    (<>) = append
#endif

-- | Properties that hold:
--
-- 1. @template \"\" = mempty@
--
-- 2. @template x \`mappend\` template y = template $ x \`T.append\` y@
instance Monoid Template where
    mempty = Template []

    mappend =
#ifdef HAVE_SEMIGROUP
        (Semigroup.<>)
#else
        append
#endif

instance Show Template where
    show = T.unpack . showTemplate

-- | Show the template string.
showTemplate :: Template -> T.Text
showTemplate (Template fs) = T.concat $ map showFrag fs

-- | A template fragment.
data Frag = Lit {-# UNPACK #-} !T.Text | Var {-# UNPACK #-} !T.Text !Bool

instance Show Frag where
    show = T.unpack . showFrag

showFrag :: Frag -> T.Text
showFrag (Var s b)
    | b          = T.concat [T.pack "${", s, T.pack "}"]
    | otherwise  = T.concat [T.pack "$", s]
showFrag (Lit s) = T.concatMap escape s
    where escape '$' = T.pack "$$"
          escape c   = T.singleton c

-- | A mapping from placeholders in the template to values.
type Context = T.Text -> T.Text

-- | Like 'Context', but with an applicative lookup function.
type ContextA f = T.Text -> f T.Text

-- -----------------------------------------------------------------------------
-- Basic interface

-- | Create a template from a template string.  A malformed template
-- string will raise an 'error'.
template :: T.Text -> Template
template = templateFromFrags . runParser pFrags

-- | Create a template from a template string.  A malformed template
-- string will cause 'templateSafe' to return @Left (row, col)@, where
-- @row@ starts at 1 and @col@ at 0.
templateSafe :: T.Text -> Either (Int, Int) Template
templateSafe =
    either Left (Right . templateFromFrags) . runParser pFragsSafe

templateFromFrags :: [Frag] -> Template
templateFromFrags = Template . combineLits

combineLits :: [Frag] -> [Frag]
combineLits [] = []
combineLits xs =
    let (lits,xs') = span isLit xs
    in case lits of
         []    -> gatherVars xs'
         [lit] -> lit : gatherVars xs'
         _     -> Lit (T.concat (map fromLit lits)) : gatherVars xs'
  where
    gatherVars [] = []
    gatherVars ys =
      let (vars,ys') = span isVar ys
      in vars ++ combineLits ys'

    isLit (Lit _) = True
    isLit _       = False

    isVar = not . isLit

    fromLit (Lit v) = v
    fromLit _       = undefined

-- | Perform the template substitution, returning a new 'LT.Text'.
render :: Template -> Context -> LT.Text
render (Template frags) ctxFunc = LT.fromChunks $ map renderFrag frags
  where
    renderFrag (Lit s)   = s
    renderFrag (Var x _) = ctxFunc x

-- | Like 'render', but allows the lookup to have side effects.  The
-- lookups are performed in order that they are needed to generate the
-- resulting text.
--
-- You can use this e.g. to report errors when a lookup cannot be made
-- successfully.  For example, given a list @ctx@ of key-value pairs
-- and a 'Template' @tmpl@:
--
-- > renderA tmpl (flip lookup ctx)
--
-- will return 'Nothing' if any of the placeholders in the template
-- don't appear in @ctx@ and @Just text@ otherwise.
renderA :: Applicative f => Template -> ContextA f -> f LT.Text
renderA (Template frags) ctxFunc = LT.fromChunks <$> traverse renderFrag frags
  where
    renderFrag (Lit s)   = pure s
    renderFrag (Var x _) = ctxFunc x

-- | Perform the template substitution, returning a new 'LT.Text'.  A
-- malformed template string will raise an 'error'.  Note that
--
-- > substitute tmpl ctx == render (template tmpl) ctx
substitute :: T.Text -> Context -> LT.Text
substitute = render . template

-- | Perform the template substitution in the given 'Applicative',
-- returning a new 'LT.Text'. Note that
--
-- > substituteA tmpl ctx == renderA (template tmpl) ctx
substituteA :: Applicative f => T.Text -> ContextA f -> f LT.Text
substituteA = renderA . template

-- -----------------------------------------------------------------------------
-- Template parser

pFrags :: Parser [Frag]
pFrags = do
    c <- peek
    case c of
      Nothing  -> return []
      Just '$' -> do c' <- peekSnd
                     case c' of
                       Just '$' -> do discard 2
                                      continue (return $ Lit $ T.pack "$")
                       _        -> continue pVar
      _        -> continue pLit
  where
    continue x = liftM2 (:) x pFrags

pFragsSafe :: Parser (Either (Int, Int) [Frag])
pFragsSafe = pFragsSafe' []
  where
    pFragsSafe' frags = do
        c <- peek
        case c of
          Nothing  -> return . Right . reverse $ frags
          Just '$' -> do c' <- peekSnd
                         case c' of
                           Just '$' -> do discard 2
                                          continue (Lit $ T.pack "$")
                           _        -> do e <- pVarSafe
                                          either abort continue e
          _        -> do l <- pLit
                         continue l
      where
        continue x = pFragsSafe' (x : frags)
        abort      = return . Left

pVar :: Parser Frag
pVar = do
    discard 1
    c <- peek
    case c of
      Just '{' -> do discard 1
                     v <- pIdentifier
                     c' <- peek
                     case c' of
                       Just '}' -> do discard 1
                                      return $ Var v True
                       _        -> liftM parseError pos
      _        -> do v <- pIdentifier
                     return $ Var v False

pVarSafe :: Parser (Either (Int, Int) Frag)
pVarSafe = do
    discard 1
    c <- peek
    case c of
      Just '{' -> do discard 1
                     e <- pIdentifierSafe
                     case e of
                       Right v -> do c' <- peek
                                     case c' of
                                       Just '}' -> do discard 1
                                                      return $ Right (Var v True)
                                       _        -> liftM parseErrorSafe pos
                       Left m  -> return $ Left m
      _        -> do e <- pIdentifierSafe
                     return $ either Left (\v -> Right $ Var v False) e

pIdentifier :: Parser T.Text
pIdentifier = do
    m <- peek
    if isJust m && isIdentifier0 (fromJust m)
      then takeWhile isIdentifier1
      else liftM parseError pos

pIdentifierSafe :: Parser (Either (Int, Int) T.Text)
pIdentifierSafe = do
    m <- peek
    if isJust m && isIdentifier0 (fromJust m)
      then liftM Right (takeWhile isIdentifier1)
      else liftM parseErrorSafe pos

pLit :: Parser Frag
pLit = do
    s <- takeWhile (/= '$')
    return $ Lit s

isIdentifier0 :: Char -> Bool
isIdentifier0 c = or [isLower c, c == '_']

isIdentifier1 :: Char -> Bool
isIdentifier1 c = or [isAlphaNum c, c `elem` "_'"]

parseError :: (Int, Int) -> a
parseError = error . makeParseErrorMessage

parseErrorSafe :: (Int, Int) -> Either (Int, Int) a
parseErrorSafe = Left

makeParseErrorMessage :: (Int, Int) -> String
makeParseErrorMessage (row, col) =
    "Invalid placeholder at " ++
    "row " ++ show row ++ ", col " ++ show col

-- -----------------------------------------------------------------------------
-- Text parser

-- | The parser state.
data S = S {-# UNPACK #-} !T.Text  -- Remaining input
           {-# UNPACK #-} !Int     -- Row
           {-# UNPACK #-} !Int     -- Col

type Parser = State S

char :: Parser (Maybe Char)
char = do
    S s row col <- get
    if T.null s
      then return Nothing
      else do c <- return $! T.head s
              case c of
                '\n' -> put $! S (T.tail s) (row + 1) 1
                _    -> put $! S (T.tail s) row (col + 1)
              return $ Just c

peek :: Parser (Maybe Char)
peek = do
    s <- get
    c <- char
    put s
    return c

peekSnd :: Parser (Maybe Char)
peekSnd = do
    s <- get
    _ <- char
    c <- char
    put s
    return c

takeWhile :: (Char -> Bool) -> Parser T.Text
takeWhile p = do
    S s row col <- get
#if MIN_VERSION_text(0,11,0)
    case T.span p s of
#else
    case T.spanBy p s of
#endif
      (x, s') -> do
                  let xlines = T.lines x
                      row' = row + fromIntegral (length xlines - 1)
                      col' = case xlines of
                               [] -> col -- Empty selection
                               [sameLine] -> T.length sameLine
                                             -- Taken from this line
                               _  -> T.length (last xlines)
                                     -- Selection extends
                                     -- to next line at least
                  put $! S s' row' col'
                  return x

discard :: Int -> Parser ()
discard n = replicateM_ n char

pos :: Parser (Int, Int)
pos = do
    S _ row col <- get
    return (row, col)

runParser :: Parser a -> T.Text -> a
runParser p s = evalState p $ S s 1 0
