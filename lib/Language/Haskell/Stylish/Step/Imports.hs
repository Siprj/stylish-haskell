{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
module Language.Haskell.Stylish.Step.Imports
    ( Options (..)
    , defaultOptions
    , ImportAlign (..)
    , ListAlign (..)
    , LongListAlign (..)
    , EmptyListAlign (..)
    , ListPadding (..)
    , step
    ) where


--------------------------------------------------------------------------------
import           Control.Arrow                   ((&&&))
import           Control.Monad                   (void)
import           Data.Char                       (toLower)
import           Data.List                       (intercalate, sortBy)
import           Data.Maybe                      (isJust, maybeToList)
import           Data.Ord                        (comparing)
import qualified Language.Haskell.Exts           as H
import qualified Data.Aeson                      as A
import qualified Data.Aeson.Types                as A
import           Data.Monoid                     ((<>), mconcat)


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Block
import           Language.Haskell.Stylish.Editor
import           Language.Haskell.Stylish.Step
import           Language.Haskell.Stylish.Util

--------------------------------------------------------------------------------
data Options = Options
--    { importAlign    :: ImportAlign
--    , listAlign      :: ListAlign
--    , longListAlign  :: LongListAlign
--    , emptyListAlign :: EmptyListAlign
--    , listPadding    :: ListPadding
--    , separateLists  :: Bool
--    } deriving (Eq, Show)

defaultOptions :: Options
defaultOptions = Options
--    { importAlign    = Global
--    , listAlign      = AfterAlias
--    , longListAlign  = Inline
--    , emptyListAlign = Inherit
--    , listPadding    = LPConstant 4
--    , separateLists  = True
--    }

-- data ListPadding
--     = LPConstant Int
--     | LPModuleName
--     deriving (Eq, Show)
--
-- data ImportAlign
--     = Global
--     | File
--     | Group
--     | None
--     deriving (Eq, Show)
--
-- data ListAlign
--     = NewLine
--     | WithAlias
--     | AfterAlias
--     deriving (Eq, Show)
--
-- data EmptyListAlign
--     = Inherit
--     | RightAfter
--     deriving (Eq, Show)
--
-- data LongListAlign
--     = Inline
--     | InlineWithBreak
--     | InlineToMultiline
--     | Multiline
--     deriving (Eq, Show)

--------------------------------------------------------------------------------
imports :: H.Module l -> [H.ImportDecl l]
imports (H.Module _ _ _ is _) = is
imports _                     = []


--------------------------------------------------------------------------------
-- importName :: H.ImportDecl l -> String
-- importName i = let (H.ModuleName _ n) = H.importModule i in n
--
--
-- --------------------------------------------------------------------------------
longestImport :: [H.ImportDecl l] -> Int
longestImport = maximum . map (length . importName)


--------------------------------------------------------------------------------
-- | Compare imports for ordering
compareImports :: H.ImportDecl l -> H.ImportDecl l -> Ordering
compareImports = comparing (map toLower . importName &&& H.importQualified)


--------------------------------------------------------------------------------
-- | The implementation is a bit hacky to get proper sorting for input specs:
-- constructors first, followed by functions, and then operators.
compareImportSpecs :: H.ImportSpec l -> H.ImportSpec l -> Ordering
compareImportSpecs = comparing key
  where
    key :: H.ImportSpec l -> (Int, Bool, String)
    key (H.IVar _ x)         = (1, isOperator x, nameToString x)
    key (H.IAbs _ _ x)       = (0, False, nameToString x)
    key (H.IThingAll _ x)    = (0, False, nameToString x)
    key (H.IThingWith _ x _) = (0, False, nameToString x)


--------------------------------------------------------------------------------
-- | Sort the input spec list inside an 'H.ImportDecl'
sortImportSpecs :: H.ImportDecl l -> H.ImportDecl l
sortImportSpecs imp = imp {H.importSpecs = sort' <$> H.importSpecs imp}
  where
    sort' (H.ImportSpecList l h specs) = H.ImportSpecList l h $
        sortBy compareImportSpecs specs


--------------------------------------------------------------------------------
-- | Order of imports in sublist is:
-- Constructors, accessors/methods, operators.
compareImportSubSpecs :: H.CName l -> H.CName l -> Ordering
compareImportSubSpecs = comparing key
  where
    key :: H.CName l -> (Int, Bool, String)
    key (H.ConName _ x) = (0, False,        nameToString x)
    key (H.VarName _ x) = (1, isOperator x, nameToString x)


--------------------------------------------------------------------------------
-- | By default, haskell-src-exts pretty-prints
--
-- > import Foo (Bar(..))
--
-- but we want
--
-- > import Foo (Bar (..))
--
-- instead.
--prettyImportSpec :: (Ord l) => Bool -> H.ImportSpec l -> String
--prettyImportSpec separate = prettyImportSpec'
--  where
--    prettyImportSpec' (H.IThingAll  _ n)     = H.prettyPrint n ++ sep "(..)"
--    prettyImportSpec' (H.IThingWith _ n cns) = H.prettyPrint n
--        ++ sep "("
--        ++ intercalate ", "
--          (map H.prettyPrint $ sortBy compareImportSubSpecs cns)
--        ++ ")"
--    prettyImportSpec' x                      = H.prettyPrint x
--
--    sep = if separate then (' ' :) else id
--
--
----------------------------------------------------------------------------------
--prettyImport :: (Ord l, Show l) =>
--    Int -> Options -> Bool -> Bool -> Int -> H.ImportDecl l -> [String]
--prettyImport columns Options{..} padQualified padName longest imp
--    | (void `fmap` H.importSpecs imp) == emptyImportSpec = emptyWrap
--    | otherwise = case longListAlign of
--        Inline            -> inlineWrap
--        InlineWithBreak   -> longListWrapper inlineWrap inlineWithBreakWrap
--        InlineToMultiline -> longListWrapper inlineWrap inlineToMultilineWrap
--        Multiline         -> longListWrapper inlineWrap multilineWrap
--  where
--    emptyImportSpec = Just (H.ImportSpecList () False [])
--    -- "import" + space + qualifiedLength has space in it.
--    listPadding' = listPaddingValue (6 + 1 + qualifiedLength) listPadding
--      where
--        qualifiedLength =
--            if null qualified then 0 else 1 + sum (map length qualified)
--
--    longListWrapper shortWrap longWrap
--        | listAlign == NewLine
--        || length shortWrap > 1
--        || length (head shortWrap) > columns
--            = longWrap
--        | otherwise = shortWrap
--
--    emptyWrap = case emptyListAlign of
--        Inherit -> inlineWrap
--        RightAfter -> [paddedNoSpecBase ++ " ()"]
--
--    inlineWrap = inlineWrapper
--        $ mapSpecs
--        $ withInit (++ ",")
--        . withHead ("(" ++)
--        . withLast (++ ")")
--
--    inlineWrapper = case listAlign of
--        NewLine    -> (paddedNoSpecBase :) . wrapRest columns listPadding'
--        WithAlias  -> wrap columns paddedBase (inlineBaseLength + 1)
--        -- Add 1 extra space to ensure same padding as in original code.
--        AfterAlias -> withTail (' ' :)
--            . wrap columns paddedBase (afterAliasBaseLength + 1)
--
--    inlineWithBreakWrap = paddedNoSpecBase : wrapRest columns listPadding'
--        ( mapSpecs
--        $ withInit (++ ",")
--        . withHead ("(" ++)
--        . withLast (++ ")"))
--
--    inlineToMultilineWrap
--        | length inlineWithBreakWrap > 2
--        || any ((> columns) . length) (tail inlineWithBreakWrap)
--            = multilineWrap
--        | otherwise = inlineWithBreakWrap
--
--    -- 'wrapRest 0' ensures that every item of spec list is on new line.
--    multilineWrap = paddedNoSpecBase : wrapRest 0 listPadding'
--        ( mapSpecs
--          ( withHead ("( " ++)
--          . withTail (", " ++))
--        ++ [")"])
--
--    paddedBase = base $ padImport $ importName imp
--
--    paddedNoSpecBase = base $ padImportNoSpec $ importName imp
--
--    padImport = if hasExtras && padName
--        then padRight longest
--        else id
--
--    padImportNoSpec = if (isJust (H.importAs imp) || hasHiding) && padName
--        then padRight longest
--        else id
--
--    base' baseName importAs hasHiding' = unwords $ concat $ filter (not . null)
--        [ ["import"]
--        , qualified
--        , show <$> maybeToList (H.importPkg imp)
--        , [baseName]
--        , importAs
--        , hasHiding'
--        ]
--
--    base baseName = base' baseName
--        ["as " ++ as | H.ModuleName _ as <- maybeToList $ H.importAs imp]
--        ["hiding" | hasHiding]
--
--    inlineBaseLength = length $ base' (padImport $ importName imp) [] []
--
--    afterAliasBaseLength = length $ base' (padImport $ importName imp)
--        ["as " ++ as | H.ModuleName _ as <- maybeToList $ H.importAs imp] []
--
--    (hasHiding, importSpecs) = case H.importSpecs imp of
--        Just (H.ImportSpecList _ h l) -> (h, Just l)
--        _                             -> (False, Nothing)
--
--    hasExtras = isJust (H.importAs imp) || isJust (H.importSpecs imp)
--
--    qualified
--        | H.importQualified imp = ["qualified"]
--        | padQualified          = ["         "]
--        | otherwise             = []
--
--    mapSpecs f = case importSpecs of
--        Nothing -> []     -- Import everything
--        Just [] -> ["()"] -- Instance only imports
--        Just is -> f $ map (prettyImportSpec separateLists) is

--import qualified Module.Name (spec, Spec(SubSpec, subSpec))

data Import = Import
    { _padQualified :: Qualified
    , _formatIfSpecsEmpty :: [Piece]
    , _shortSpec :: Spec
    , _longSpec :: Spec
    }

data Spec = Spec
    { specsBefore :: [Piece]
    , specsAfter :: [Piece]
    , specsAfterBreak :: [Piece]
    , specsOther :: [Piece]
    , subSpecs :: SubSpec
    }

data SubSpec = SubSpec
    { subSpecsBefore :: [Piece]
    , subSpecsAfter :: [Piece]
    , subSpecsAfterBreak :: [Piece]
    , subSpecsOther :: [Piece]
    , subSpecsAll :: [Piece]
    }

data NewLine' = NewLine' | NewLineAsFarAsPossible

data Piece = Lit String | NewLine NewLine'

type PieceResult =
    ( String
    -- ^ String
    , Int
    -- ^ length of string on current line
    , Bool
    -- ^ Is new line in this spec
    )

data ShouldBreak = SBBreak | SBTest | SBDontBreak

data Qualified = GlobalPad | FilePad | GroupPad | NoPad

-- | Structure holding information about length of individual parts of
-- pretty printed import.
-- > import qualified Module.Name as Alias hiding (Foo (Bar, Baz))
--                              |        |      |
-- afterModuleName -------------+        |      |
-- afterAlias ---------------------------+      |
-- afterBase  ----------------------------------+
--
-- If the hiding part is missing afterBase is equal to afterAlias.
-- If the as alias is missing afterAlias is equal to afterModuleName.
data Stats = Stats
    { afterModuleName :: Int
    , afterAlias :: Int
    , afterBase :: Int
    }

data GlobalStats = GlobalStats
    { statsPadQualified :: Bool
    , stats :: [Stats]
    }

data GroupStats = GroupStats
    { gropuStats :: [Stats]
    , globalStats :: GlobalStats
    }

data Options' = Options'
    { importStyle :: Import
    , columns :: Int
    }

addString :: String -> PieceResult -> PieceResult
addString str2 (str, len, break) =
    (str <> str2, len + length str2, break)

specToEither :: [H.ImportSpec a ] -> Either (String, [H.CName a]) String
specToEither (H.IThingAll  _ x) = Left (H.prettyPrint x, [])
specToEither (H.IThingWith _ x sscs) = Left (H.prettyPrint x, sscs)
specToEither x = Right H.prettyPrint x

modNewLineFlag :: (Bool -> Bool) -> PieceResult -> PieceResult
modNewLineFlag f (str, len, flag) =  (str, len, f flag)

nullNewLineFlag :: PieceResult -> PieceResult
nullNewLineFlag (str, len, flag) =  (str, len, False)

getNewLineFlag :: PieceResult -> Bool
getNewLineFlag (_, _, flag) = flag

getLength :: PieceResult -> Int
getLength (_, _, flag) = flag

prettyMy :: GroupStats -> Options' -> [H.ImportDecl LineBlock] -> Lines
prettyMy GroupStats{..} Options'{..} imps =
    mconcat $ fmap (prettyPrintWhole importStyle) imps
  where
    prettyPrintWhole :: Import -> H.ImportDecl LineBlock -> [String]
    prettyPrintWhole Import{..} imp@H.ImportDecl{..} [] = case H.importSpecs of
        Nothing -> unwords prettyPrintBase
        Just (H.ImportSpecList _ _ []) -> let (str, len, _) = printEmpty False
              in if len > columns
                  then lines str
                  else lines fst $ printEmpty True
        Just (H.ImportSpecList _ _ xs) -> printShort
            -- TODO: Long
      where
        printEmpty pred imp = printPieces _formatIfSpecsEmpty base pred

        prettyPrintBase =
            prettyOnlyBase (statsPadQualified globalStats) importStyle imp

        base = (prettyPrintBase, length prettyPrintBase, False)

-- {{{ Printing short ---------------------------------------------------------

        printShort (x : xs) spec@Spec{..} =
            let res = printPieces specsBefore False base
            in printShort' xs spec $ case specToEither x of
                Right specStr -> addString specStr res
                Left (specStr, sscs) ->
                    printSubSpec sscs subSpecs False $ addString specStr res

        printShort' [] Spec{..} r = printPieces specsAfter False r
        printShort' (x : xs) Spec{..} r =
            printShort' xs subSpec . speaddString . H.prettyPrint x
                $ if getNewLineFlag r
                    then nullNewLineFlag $ printPieces subSpecsAfterBreak r
                    else printPieces subSpecsAfter r

-- }}} Printing short ---------------------------------------------------------
-- {{{ Printing sub spec ------------------------------------------------------

        printSubSpec :: [H.CName a] -> SubSpec -> Bool -> PieceResult
        printSubSpec [] subSpec@SubSpec{..} pred =
            printPieces subSpecsAll pred
        printSubSpec (x : xs) subSpec@SubSpec{..} pred r =
            printSubSpec' xs subSpec pred . addString . H.prettyPrint x
                $ printPieces subSpecsBefore pred r

        printSubSpec' :: [H.CName a] -> SubSpec -> Bool -> PieceResult
        printSubSpec' [] SubSpec{..} pred r =
            printPieces subSpecsAfter pred r
        printSubSpec' (x : xs) subSpec@SubSpec{..} pred r =
            printSubSpec' xs subSpec pred . speaddString . H.prettyPrint x
                $ if getNewLineFlag r
                    then nullNewLineFlag $ printPieces subSpecsAfterBreak pred r
                    else printPieces subSpecsAfter pred r

        printPieces :: [Piece] -> Bool -> PieceResult -> PieceResult
        printPieces ps shouldBreak base = foldl' printPiece base ps

-- }}} Printing sub spec ------------------------------------------------------
-- {{{ Printing pieces --------------------------------------------------------

        printPiece
            :: PieceResult
            -> Bool
            -- ^ Should break?
            -> Piece
            -- ^ Previous pretty printed string
            -> PieceResult
        printPiece (xs, len, pred) _ (Lit str) =
            (xs <> str, len + length str, False || pred)
        printPiece (xs, len, pred) shouldBreak (NewLine nl) =
            case nl of
                NewLine' -> (xs <> newLine, 0, True)
                NewLineAsFarAsPossible -> if shouldBreak
                    then (xs <> newLine, 0, True)
                    else (xs, len, False || pred)
          where
            newLine = "\n"

    toList a = [a]

-- }}} Printing pieces --------------------------------------------------------

prettyOnlyBase
    :: Bool
    -> Import
    -> H.ImportDecl LineBlock
    -> (String, Stats)
prettyOnlyBase padQualified (Import qualified _ _ _) imp =
    let afterNameLength = length $ unwords moduleName
        afterAliasLenght = length $ unwords alias
        afterBaseLength = length $ unwords base
    in ( unwords base
       , Stats
           { afterModuleName = afterNameLength
           , afterAlias = afterAliasLenght
           , afterBase = afterBaseLength
           }
       )
  where
    isImporQualified = H.importQualified imp
    qualified True = "qualified"
    qualified False = if padQualified
        then "         "
        else ""

    moduleNameRec (H.ModuleName _ n) = n

    moduleName =
        [ "import"
        , qualified isImporQualified
        , moduleNameRec $ H.importModule imp
        ]
    alias = moduleName
        <> ["as " <> moduleNameRec x | x <- maybeToList $ H.importAs imp]
    base = alias
        <> if hasHiding
            then ["hiding"]
            else []

    hasHiding :: Bool
    hasHiding = maybe False hasHiding' $ H.importSpecs imp
    hasHiding' (H.ImportSpecList _ x _) = x


--------------------------------------------------------------------------------
prettyImportGroup :: Int -> GroupStats -> Options' -> Int
                  -> [H.ImportDecl LineBlock]
                  -> Lines
prettyImportGroup columns globalStats options longest imps =
    prettyMy globalStats options $ sortBy compareImports imps


--------------------------------------------------------------------------------
step :: Int -> Options -> Step
step columns = makeStep "Imports" . step' columns

i = Import
    { _padQualified = FilePad
    , _formatIfSpecsEmpty = [Lit " ()"]
    , _shortSpec = Spec [Lit " "] [Lit " "] [Lit " "] [Lit " "]
        (SubSpec [Lit " "] [Lit " "] [Lit " "] [Lit " "] [Lit " "])
    , _longSpec =  Spec [Lit " "] [Lit " "] [Lit " "] [Lit " "]
        (SubSpec [Lit " "] [Lit " "] [Lit " "] [Lit " "] [Lit " "])
    }
o = Options' i 80

--------------------------------------------------------------------------------
step' :: Int -> Options -> Lines -> Module -> Lines
step' columns align ls (module', _) = applyChanges
    [ change block . const $
        prettyImportGroup columns (groupStats importGroup) o longest importGroup
    | (block, importGroup) <- groups
    ]
    ls
  where
    imps    = map sortImportSpecs $ imports $ fmap linesFromSrcSpan module'
    longest = longestImport imps
    groups  = groupAdjacent [(H.ann i, i) | i <- imps]

    groupStats :: [H.ImportDecl LineBlock] -> GroupStats
    groupStats group = GroupStats
        { gropuStats = fmap (snd . prettyOnlyBase (shouldPadQualified group) i) group
        , globalStats = globalStats' group
        }

    globalStats' :: [H.ImportDecl LineBlock] -> GlobalStats
    globalStats' group = GlobalStats
        { stats = map (snd . prettyOnlyBase (shouldPadQualified group) i) imps
        , statsPadQualified = shouldPadQualified group
        }

    shouldPadQualified group = case qualOpt of
        GlobalPad -> True
        FilePad -> any H.importQualified imps
        GroupPad -> any H.importQualified group
        NoPad -> False

    qualOpt = FilePad

--------------------------------------------------------------------------------
--listPaddingValue :: Int -> ListPadding -> Int
--listPaddingValue _ (LPConstant n) = n
--listPaddingValue n LPModuleName   = n

--------------------------------------------------------------------------------

--instance A.FromJSON ListPadding where
--    parseJSON (A.String "module_name") = return LPModuleName
--    parseJSON (A.Number n) | n' >= 1   = return $ LPConstant n'
--      where
--        n' = truncate n
--    parseJSON v                        = A.typeMismatch "'module_name' or >=1 number" v
