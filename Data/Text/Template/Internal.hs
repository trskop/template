{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}

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

     ParseOptions(..),
     ParseError(..),
     showTemplateError,
     showParseError,
     mkParseError,
     parseError,
     parse,
    ) where

import Control.Applicative (Applicative(pure), (<$>))
import Control.Exception (Exception, throw)
import Control.Monad (liftM, liftM2, replicateM_)
import Control.Monad.State.Strict (State, evalState, get, put)
import Data.Bifunctor (bimap)
import Data.Char (isAlphaNum, isLower)
import Data.Function (on)
import Data.Maybe (fromJust, isJust, listToMaybe)
import Data.Monoid (Monoid(mempty, mappend))
import Data.Traversable (traverse)
import Prelude hiding (takeWhile)

#ifdef HAVE_SEMIGROUP
import Data.Semigroup (Semigroup)
import qualified Data.Semigroup as Semigroup
#endif

import Data.CallStack
    ( HasCallStack
    , SrcLoc(srcLocFile, srcLocStartLine)
    , callStack
    )
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
-- string will raise a 'ParseError'.
template :: HasCallStack => T.Text -> Template
template = templateFromFrags . runParser (pFrags defaultParseOptions)

-- | Create a template from a template string.  A malformed template
-- string will cause 'templateSafe' to return @Left (row, col)@, where
-- @row@ starts at 1 and @col@ at 0.
templateSafe :: T.Text -> Either (Int, Int) Template
templateSafe =
    fmap templateFromFrags . runParser (pFragsSafe defaultParseOptions)

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

parse :: HasCallStack => ParseOptions -> T.Text -> Either ParseError Template
parse opts = bimap mkParseError templateFromFrags . runParser (pFragsSafe opts)

pFrags :: HasCallStack => ParseOptions -> Parser [Frag]
pFrags opts = do
    c <- peek
    case c of
      Nothing  -> return []
      Just '$' -> do c' <- peekSnd
                     case c' of
                       Just '$' -> do discard 2
                                      continue (return $ Lit $ T.pack "$")
                       _        -> continue (pVar opts)
      _        -> continue pLit
  where
    continue x = liftM2 (:) x (pFrags opts)

pFragsSafe :: ParseOptions -> Parser (Either (Int, Int) [Frag])
pFragsSafe opts = pFragsSafe' []
  where
    pFragsSafe' frags = do
        c <- peek
        case c of
          Nothing  -> return . Right . reverse $ frags
          Just '$' -> do c' <- peekSnd
                         case c' of
                           Just '$' -> do discard 2
                                          continue (Lit $ T.pack "$")
                           _        -> do e <- pVarSafe opts
                                          either abort continue e
          _        -> do l <- pLit
                         continue l
      where
        continue x = pFragsSafe' (x : frags)
        abort      = return . Left

pVar :: HasCallStack => ParseOptions -> Parser Frag
pVar ParseOptions{bracketsRequired} = do
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

      _ | bracketsRequired -> parseError <$> pos
        | otherwise        -> do v <- pIdentifier
                                 return $ Var v False

pVarSafe :: ParseOptions -> Parser (Either (Int, Int) Frag)
pVarSafe ParseOptions{bracketsRequired} = do
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

      _ | bracketsRequired -> parseErrorSafe <$> pos
        | otherwise        -> do e <- pIdentifierSafe
                                 return $ (\v -> Var v False) <$> e

pIdentifier :: HasCallStack => Parser T.Text
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

-- -----------------------------------------------------------------------------
-- Parse Options

newtype ParseOptions = ParseOptions
    { bracketsRequired :: Bool
    }
  deriving Show

-- |
-- @
-- 'bracketsRequired' = False
-- @
defaultParseOptions :: ParseOptions
defaultParseOptions = ParseOptions
    { bracketsRequired = False
    }

-- -----------------------------------------------------------------------------
-- Parse Error

data ParseError = ParseError
    { sourceLocation :: Maybe SrcLoc
    , templateLocation :: (Int, Int)
    }

instance Show ParseError where
    showsPrec _ = showParseError

instance Exception ParseError

mkParseError :: HasCallStack => (Int, Int) -> ParseError
mkParseError templateLocation = ParseError {sourceLocation, templateLocation}
  where
    sourceLocation :: Maybe SrcLoc
    sourceLocation = snd <$> listToMaybe (reverse callStack)

showParseError :: ParseError -> ShowS
showParseError ParseError{sourceLocation, templateLocation} =
    showLocation sourceLocation . showTemplateError templateLocation
  where
    showLocation :: Maybe SrcLoc -> ShowS
    showLocation = maybe id $ \loc ->
        showString (srcLocFile loc) . showChar ':'
        . shows (srcLocStartLine loc) . showString ":\n"

-- | Render template error position as an error message.
showTemplateError :: (Int, Int) -> ShowS
showTemplateError (row, col) =
    showString "Invalid placeholder at row " . shows row
    . showString ", col " . shows col

parseError :: HasCallStack => (Int, Int) -> a
parseError = throw . mkParseError

parseErrorSafe :: (Int, Int) -> Either (Int, Int) a
parseErrorSafe = Left

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
