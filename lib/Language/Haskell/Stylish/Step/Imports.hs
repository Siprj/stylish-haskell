{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
--------------------------------------------------------------------------------
module Language.Haskell.Stylish.Step.Imports
    ( Options (..)
    , defaultOptions
    --, ImportAlign (..)
    --, ListAlign (..)
    --, LongListAlign (..)
    --, EmptyListAlign (..)
    --, ListPadding (..)
    , step
    ) where


--------------------------------------------------------------------------------
import           Control.Arrow                   ((&&&))
import           Control.Monad                   (void)
import           Data.Char                       (toLower)
import           Data.List
import qualified Data.List.NonEmpty              as NEL
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
importName :: H.ImportDecl l -> String
importName i = let (H.ModuleName _ n) = H.importModule i in n


--------------------------------------------------------------------------------
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
    , specsOther :: [Piece]
    , subSpecs :: SubSpec
    }

data SubSpec = SubSpec
    { subSpecsBefore :: [Piece]
    , subSpecsAfter :: [Piece]
    , subSpecsOther :: [Piece]
    , subSpecsAll :: [Piece]
    }

data Piece
    = NewLine' NewLine
    | Other' Other

data Other
    = Lit String
    | SpecAlias

data NewLine
    = NewLine [Other]
    | NewLineAsFarAsPossible [Other]

data Break
    = Break
    | BreakAFAP
    | Nill

-- | (lines delimited by '\n', length of current line)
type PieceResult = (String, Break, String)

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
addString str2 (str, br, str') = (str <> str2, br, str')

addStringAfterBreak :: String -> PieceResult -> PieceResult
addStringAfterBreak str2 (str, br, str') = (str, br, str' <> str2)

specToEither :: H.ImportSpec a -> Either (String, [H.CName a]) String
specToEither (H.IThingAll  _ x) = Left (H.prettyPrint x, [])
specToEither (H.IThingWith _ x sscs) = Left (H.prettyPrint x, sscs)
specToEither x = Right $ H.prettyPrint x

getString :: PieceResult -> String
getString (str, _, _) = str

getStringAfterBreak :: PieceResult -> String
getStringAfterBreak (_, _, str) = str

setBreak :: Break -> PieceResult -> PieceResult
setBreak br (str, _, str') = (str, br, str')

getBreak :: PieceResult -> Break
getBreak (_, br, _) = br

onHead
    :: (a -> a)
    -> NEL.NonEmpty a
    -> NEL.NonEmpty a
onHead f (a NEL.:| as)  = (f a NEL.:| as)

prettyMy :: GroupStats -> Options' -> [H.ImportDecl LineBlock] -> Lines
prettyMy GroupStats{..} Options'{..} imps =
    mconcat $ fmap (prettyPrintWhole importStyle) imps
  where
    prettyPrintWhole :: Import -> H.ImportDecl LineBlock -> [String]
    prettyPrintWhole Import{..} imp@H.ImportDecl{..} =
        lines . expandBreaks $ case importSpecs of
            Nothing -> strToPieceResult prettyPrintBase
            Just (H.ImportSpecList _ _ []) -> prettyEmpty
            Just (H.ImportSpecList _ _ xs) ->
                if isOverflowing $ short xs
                    then long xs
                    else short xs
      where
        prettyEmpty = prettyPieces "" _formatIfSpecsEmpty base

        strToPieceResult a = (a, Nill, "") NEL.:| []

        short xs = prettySpec xs _shortSpec base
        long xs = prettySpec xs _longSpec base

        isOverflowing = any ((> columns) . length) . lines . expandBreaks

        prettyPrintBase :: String
        prettyPrintBase =
            let (b, _) = prettyOnlyBase (statsPadQualified globalStats)
                    importStyle imp
            in b

        base :: NEL.NonEmpty PieceResult
        base = (prettyPrintBase, Nill, "") NEL.:| []

-- {{{ Printing short ---------------------------------------------------------

        prettySpec
            :: [H.ImportSpec a]
            -> Spec
            -> NEL.NonEmpty PieceResult
            -> NEL.NonEmpty PieceResult
        prettySpec (x : xs) spec@Spec{..} r = NEL.reverse
            . prettySpec' xs spec $ case specToEither x of
                Right specStr ->
                    prettyPieces specStr specsOther r
                Left (specStr, sscs) ->
                    prettySubSpec sscs subSpecs
                    $ prettyPieces specStr specsOther r

        prettySpec'
            :: [H.ImportSpec a]
            -> Spec
            -> NEL.NonEmpty PieceResult
            -> NEL.NonEmpty PieceResult
        prettySpec' [] Spec{..} r = prettyPieces "" specsAfter r
        prettySpec' (x : xs) spec@Spec{..} r = prettySpec' xs spec $
            case specToEither x of
                Right specStr ->
                    prettyPieces specStr specsOther r
                Left (specStr, sscs) ->
                    prettySubSpec sscs subSpecs
                    $ prettyPieces specStr specsOther r

-- }}} prettying short ---------------------------------------------------------
-- {{{ prettying sub spec ------------------------------------------------------

        prettySubSpec
            :: [H.CName a]
            -> SubSpec
            -> NEL.NonEmpty PieceResult
            -> NEL.NonEmpty PieceResult
        prettySubSpec [] SubSpec{..} r =
            prettyPieces "" subSpecsAll r
        prettySubSpec (x : xs) subSpec@SubSpec{..} r =
            prettySubSpec' xs subSpec $ prettyPieces specStr subSpecsBefore r
          where
            specStr = H.prettyPrint x

        prettySubSpec'
            :: [H.CName a]
            -> SubSpec
            -> NEL.NonEmpty PieceResult
            -> NEL.NonEmpty PieceResult
        prettySubSpec' [] SubSpec{..} r =
            prettyPieces "" subSpecsAfter r
        prettySubSpec' (x : xs) subSpec@SubSpec{..} r =
            prettySubSpec' xs subSpec $ prettyPieces specStr subSpecsOther r
          where
            specStr = H.prettyPrint x

        prettyPieces
            :: String
            -- ^ String which will replace Spec
            -> [Piece]
            -- ^ Pieces which should be prettyed
            -> NEL.NonEmpty PieceResult
            -- ^ Previsou steps
            -> NEL.NonEmpty PieceResult
        prettyPieces spec ps r = foldl' (flip $ prettyPiece spec) r ps

-- }}} prettying sub spec ------------------------------------------------------
-- {{{ prettying pieces --------------------------------------------------------

        prettyPiece
            :: String
            -- ^ String which will replace Spec
            -> Piece
            -- ^ Previous pretty prettyed string
            -> NEL.NonEmpty PieceResult
            -> NEL.NonEmpty PieceResult
        prettyPiece spec (Other' o) r =
            prettyOther spec addString o r
        prettyPiece spec (NewLine' nl) r = case nl of
            NewLine os -> emptyRes NEL.<| (onHead (setBreak Break)
                $ prettyOthers spec addStringAfterBreak os r)
            NewLineAsFarAsPossible os -> emptyRes NEL.<|
                (onHead (setBreak BreakAFAP)
                $ prettyOthers spec addStringAfterBreak os r)

-- }}} Printing pieces --------------------------------------------------------
-- {{{ Printing pieces --------------------------------------------------------

        prettyOthers
            :: String
            -> (String -> PieceResult -> PieceResult)
            -> [Other]
            -- ^ Previous pretty prettyed string
            -> NEL.NonEmpty PieceResult
            -> NEL.NonEmpty PieceResult
        prettyOthers spec f os r = foldl' (flip $ prettyOther spec f) r os

        prettyOther
            :: String
            -> (String -> PieceResult -> PieceResult)
            -> Other
            -- ^ Previous pretty prettyed string
            -> NEL.NonEmpty PieceResult
            -> NEL.NonEmpty PieceResult
        prettyOther spec fun (Lit str) r = onHead (fun str) r
        prettyOther spec fun (SpecAlias) r = onHead (fun spec) r

        emptyRes = ("", Nill, "")

        expandBreaks :: NEL.NonEmpty PieceResult -> String
        expandBreaks ((str, br, strAfterBr) NEL.:| xs) = fst $
            case br of
                Break -> expandBreaks' xs $ break p
                BreakAFAP -> if willNextOverflow p xs
                    then expandBreaks' xs $ break  p
                    else expandBreaks' xs p
                Nill -> expandBreaks' xs p
          where
            len = length str
            p = (str, len)
            break = addStr strAfterBr . addNewLine

        expandBreaks' :: [PieceResult] -> (String, Int) -> (String, Int)
        expandBreaks' [] r = r
        expandBreaks' ((str, br, strAfterBr) : xs) r =
            case br of
                Break -> expandBreaks' xs $ break r
                BreakAFAP -> if willNextOverflow r xs
                    then expandBreaks' xs $ break r
                    else expandBreaks' xs r
                Nill -> expandBreaks' xs r
          where
            break = addStr strAfterBr . addNewLine

        addNewLine :: (String, Int) -> (String, Int)
        addNewLine (str, _) = (str <> newLine, 0)

        addStr str (str', len) = (str' <> str, len + length str)
        getLen (_, len) = len

        willNextOverflow r [] = False
        willNextOverflow r ((str, _, _) : _) =
            if getLen (addStr str r) > columns
                then True
                else False

        newLine = "\n"

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

iM = Import
    { _padQualified = FilePad
    , _formatIfSpecsEmpty = [Other' $ Lit " ()"]
    , _shortSpec = Spec [Other' $ Lit " "] [Other' $ Lit " "] [Other' $ Lit " "]
        (SubSpec [Other' $ Lit " "] [Other' $ Lit " "] [Other' $ Lit " "] [Other' $ Lit " "])
    , _longSpec =  Spec [Other' $ Lit " "] [Other' $ Lit " "] [Other' $ Lit " "]
        (SubSpec [Other' $ Lit " "] [Other' $ Lit " "] [Other' $ Lit " "] [Other' $ Lit " "])
    }
oP :: Options'
oP = Options' iM 80

--------------------------------------------------------------------------------
step' :: Int -> Options -> Lines -> Module -> Lines
step' columns align ls (module', _) = applyChanges
    [ change block . const $
        prettyImportGroup columns (groupStats importGroup) oP longest importGroup
    | (block, importGroup) <- groups
    ]
    ls
  where
    imps    = map sortImportSpecs $ imports $ fmap linesFromSrcSpan module'
    longest = longestImport imps
    groups  = groupAdjacent [(H.ann i, i) | i <- imps]

    groupStats :: [H.ImportDecl LineBlock] -> GroupStats
    groupStats group' = GroupStats
        { gropuStats =
            fmap (snd . prettyOnlyBase (shouldPadQualified group') iM) group'
        , globalStats = globalStats' group'
        }

    globalStats' :: [H.ImportDecl LineBlock] -> GlobalStats
    globalStats' group' = GlobalStats
        { stats = map (snd . prettyOnlyBase (shouldPadQualified group') iM) imps
        , statsPadQualified = shouldPadQualified group'
        }

    shouldPadQualified group' = case qualOpt of
        GlobalPad -> True
        FilePad -> any H.importQualified imps
        GroupPad -> any H.importQualified group'
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
