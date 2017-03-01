{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
--------------------------------------------------------------------------------
module Language.Haskell.Stylish.Step.Imports
    ( Options (..)
    , Pad (..)
    , Other (..)
    , Piece (..)
    , Spec (..)
    , SubSpec (..)
    , NewLine (..)
    , defaultOptions
    , step

    -- * Exported fot tests
    , prettyPieces
    , prettyPiece
    , prettyOthers
    , prettyOther
    ) where


--------------------------------------------------------------------------------
import           Prelude                         hiding (break)
import           Control.Arrow                   ((&&&))
import           Data.Char                       (toLower)
import qualified Data.List.NonEmpty              as NEL
import           Data.List                       (sortBy, foldl', dropWhileEnd)
import           Data.Maybe                      (maybeToList)
import           Data.Ord                        (comparing)
import qualified Language.Haskell.Exts           as H
import           Data.Monoid                     ((<>), mconcat)


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Block
import           Language.Haskell.Stylish.Editor
import           Language.Haskell.Stylish.Step
import           Language.Haskell.Stylish.Util

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
    sort' (H.ImportSpecList l h specs) = H.ImportSpecList l h
        . fmap sortImportSubSpecs $ sortBy compareImportSpecs specs


--------------------------------------------------------------------------------
-- | Order of imports in sublist is:
-- Constructors, accessors/methods, operators.
compareImportSubSpecs :: H.CName l -> H.CName l -> Ordering
compareImportSubSpecs = comparing key
  where
    key :: H.CName l -> (Int, Bool, String)
    key (H.ConName _ x) = (0, False,        nameToString x)
    key (H.VarName _ x) = (1, isOperator x, nameToString x)

sortImportSubSpecs :: H.ImportSpec l -> H.ImportSpec l
sortImportSubSpecs (H.IThingWith l n x) =
    H.IThingWith l n $ sortBy compareImportSubSpecs x
sortImportSubSpecs x = x

data Options = Options
    { _padQualified :: Pad
    , _padModifier :: Pad
    , _formatIfSpecsEmpty :: [Piece]
    , _shortSpec :: Spec
    , _longSpec :: Spec
    }
  deriving (Show)

data Spec = Spec
    { specsBefore :: [Piece]
    , specsAfter :: [Piece]
    , specsOther :: [Piece]
    , subSpecs :: SubSpec
    }
  deriving (Show)

data SubSpec = SubSpec
    { subSpecsBefore :: [Piece]
    , subSpecsAfter :: [Piece]
    , subSpecsOther :: [Piece]
    , subSpecsAll :: [Piece]
    }
  deriving (Show)

data Piece
    = NewLine' NewLine
    | Other' Other
  deriving (Show)

data Other
    = Lit String
    | PadToModulName
    | PadToAlias
    | PadToModulePad
    | PadToBase
    | SpecAlias
  deriving (Show)

data NewLine
    = NewLine [Other]
    | NewLineAsFarAsPossible [Other]
  deriving (Show)

data Break
    = Break
    | BreakAFAP
    | Nill
  deriving (Show)

-- | (lines delimited by '\n', length of current line)
type PieceResult = (String, Break, String)

data Pad = GlobalPad | FilePad | GroupPad | NoPad
  deriving (Show, Eq)

-- | Structure holding information about length of individual parts of
-- pretty printed import.
-- > import qualified Module.Name   as Alias hiding (Foo (Bar, Baz))
--                              |  |       |      |
-- afterModuleName -------------+  |       |      |
-- afterModulePad -----------------+       |      |
-- afterAlias -----------------------------+      |
-- afterBase  ------------------------------------+
--
-- If the hiding part is missing afterBase is equal to afterAlias.
-- If the as alias is missing afterAlias is equal to afterModuleName.
data Stats = Stats
    { afterModuleName :: Int
    , afterModulePad :: Int
    , afterAlias :: Int
    , afterBase :: Int
    }
  deriving (Show)

data GlobalStats = GlobalStats
    { statsPadQualified :: Bool
    , stats :: [Stats]
    }
  deriving (Show)

data GroupStats = GroupStats
    { gropuStats :: [Stats]
    , globalStats :: GlobalStats
    }
  deriving (Show)

addString :: String -> PieceResult -> PieceResult
addString str2 (str, br, str') = (str <> str2, br, str')

addStringAfterBreak :: String -> PieceResult -> PieceResult
addStringAfterBreak str2 (str, br, str') = (str, br, str' <> str2)

specToEither :: H.ImportSpec a -> Either (String, [H.CName a]) String
specToEither (H.IThingAll  _ x) = Left (H.prettyPrint x, [])
specToEither (H.IThingWith _ x sscs) = Left (H.prettyPrint x, sscs)
specToEither x = Right $ H.prettyPrint x

setBreak :: Break -> PieceResult -> PieceResult
setBreak br (str, _, str') = (str, br, str')

onHead
    :: (a -> a)
    -> NEL.NonEmpty a
    -> NEL.NonEmpty a
onHead f (a NEL.:| as)  = (f a NEL.:| as)

prettyMy :: GroupStats -> Int -> Options -> [H.ImportDecl LineBlock] -> Lines
prettyMy GroupStats{..} columns options@Options{..} imps =
    mconcat $ fmap (prettyPrintWhole options) imps
  where
    prettyPrintWhole :: Options -> H.ImportDecl LineBlock -> [String]
    prettyPrintWhole Options{..} imp@H.ImportDecl{..} =
        removeSpaces. lines . expandBreaks columns $ case importSpecs of
            Nothing -> strToPieceResult prettyPrintBase
            Just (H.ImportSpecList _ _ xs) ->
                if isOverflowing $ short xs
                    then long xs
                    else short  xs
      where
        strToPieceResult a = (a, Nill, "") NEL.:| []

        removeSpaces xs = fmap (dropWhileEnd (== ' ')) xs

        magic = case _padModifier of
            GlobalPad -> fileModuleNamePadSize
            FilePad -> fileModuleNamePadSize
            GroupPad -> groupModuleNamePadSize
            NoPad -> 0

        fileModuleNamePadSize =
            foldl' max 0 . fmap afterModuleName $ stats globalStats

        groupModuleNamePadSize =
            foldl' max 0 $ fmap afterModuleName gropuStats

        computeImportStats =
            snd $ prettyOnlyBase (statsPadQualified globalStats)
                options magic imp

        short xs = prettySpec xs _shortSpec base
        long xs = prettySpec xs _longSpec base

        isOverflowing =
            any ((> columns) . length) . lines . expandBreaks columns

        prettyPrintBase :: String
        prettyPrintBase =
            let (b, _) = prettyOnlyBase (statsPadQualified globalStats)
                    options magic imp
            in b

        base :: NEL.NonEmpty PieceResult
        base = (prettyPrintBase, Nill, "") NEL.:| []

-- {{{ Printing short ---------------------------------------------------------

        prettySpec
            :: [H.ImportSpec a]
            -> Spec
            -> NEL.NonEmpty PieceResult
            -> NEL.NonEmpty PieceResult
        prettySpec [] Spec{..} r =
            NEL.reverse $ prettyPieces computeImportStats options "" _formatIfSpecsEmpty r
        prettySpec (x : xs) spec@Spec{..} r = NEL.reverse
            . prettySpec' xs spec $ case specToEither x of
                Right specStr ->
                    prettyPieces computeImportStats options specStr specsBefore r
                Left (specStr, sscs) ->
                    prettySubSpec sscs subSpecs
                    $ prettyPieces computeImportStats options specStr specsBefore r

        prettySpec'
            :: [H.ImportSpec a]
            -> Spec
            -> NEL.NonEmpty PieceResult
            -> NEL.NonEmpty PieceResult
        prettySpec' [] Spec{..} r =
            prettyPieces computeImportStats options "" specsAfter r
        prettySpec' (x : xs) spec@Spec{..} r = prettySpec' xs spec $
            case specToEither x of
                Right specStr ->
                    prettyPieces computeImportStats options specStr specsOther r
                Left (specStr, sscs) ->
                    prettySubSpec sscs subSpecs
                    $ prettyPieces computeImportStats options specStr specsOther r

-- }}} prettying short ---------------------------------------------------------
-- {{{ prettying sub spec ------------------------------------------------------

        prettySubSpec
            :: [H.CName a]
            -> SubSpec
            -> NEL.NonEmpty PieceResult
            -> NEL.NonEmpty PieceResult
        prettySubSpec [] SubSpec{..} r =
            prettyPieces computeImportStats options "" subSpecsAll r
        prettySubSpec (x : xs) subSpec@SubSpec{..} r =
            prettySubSpec' xs subSpec
                $ prettyPieces computeImportStats options specStr subSpecsBefore r
          where
            specStr = H.prettyPrint x

        prettySubSpec'
            :: [H.CName a]
            -> SubSpec
            -> NEL.NonEmpty PieceResult
            -> NEL.NonEmpty PieceResult
        prettySubSpec' [] SubSpec{..} r =
            prettyPieces computeImportStats options "" subSpecsAfter r
        prettySubSpec' (x : xs) subSpec@SubSpec{..} r =
            prettySubSpec' xs subSpec
                $ prettyPieces computeImportStats options specStr subSpecsOther
                    r
          where
            specStr = H.prettyPrint x

-- }}} prettying sub spec -----------------------------------------------------
-- {{{ prettying pieces -------------------------------------------------------

prettyPieces
    :: Stats
    -> Options
    -> String
    -- ^ String which will replace Spec
    -> [Piece]
    -- ^ Pieces which should be pretty-printed
    -> NEL.NonEmpty PieceResult
    -- ^ Previsou steps
    -> NEL.NonEmpty PieceResult
prettyPieces stats options spec ps r =
    foldl' (flip $ prettyPiece stats options spec) r ps


prettyPiece
    :: Stats
    -> Options
    -> String
    -- ^ String which will replace Spec
    -> Piece
    -- ^ Previous pretty prettyed string
    -> NEL.NonEmpty PieceResult
    -> NEL.NonEmpty PieceResult
prettyPiece stats options spec (Other' o) r =
    prettyOther stats options spec addString o r
prettyPiece stats options spec (NewLine' nl) r = case nl of
    NewLine os -> emptyRes NEL.<|
        (onHead (setBreak Break) $ prettyOthers
            stats options spec addStringAfterBreak os r)
    NewLineAsFarAsPossible os -> emptyRes NEL.<|
        (onHead (setBreak BreakAFAP) $ prettyOthers
            stats options spec addStringAfterBreak os r)
  where
    emptyRes = ("", Nill, "")

-- }}} Printing pieces --------------------------------------------------------
-- {{{ Printing Others --------------------------------------------------------

prettyOthers
    :: Stats
    -> Options
    -> String
    -> (String -> PieceResult -> PieceResult)
    -> [Other]
    -- ^ Previous pretty prettyed string
    -> NEL.NonEmpty PieceResult
    -> NEL.NonEmpty PieceResult
prettyOthers stats options spec f os r =
    foldl' (flip $ prettyOther stats options spec f) r os

prettyOther
    :: Stats
    -> Options
    -> String
    -> (String -> PieceResult -> PieceResult)
    -> Other
    -- ^ Previous pretty prettyed string
    -> NEL.NonEmpty PieceResult
    -> NEL.NonEmpty PieceResult
prettyOther importStats Options{..} spec fun others r = prettyOther' others
  where
    importModuleNamePadSize = afterModuleName importStats
    importModulePadModulePadSize = afterModulePad importStats
    importAliasPadSize = afterAlias importStats
    importBasePadSize = afterBase importStats

    prettyOther'
        :: Other
        -- ^ Previous pretty prettyed string
        -> NEL.NonEmpty PieceResult
    prettyOther' (Lit str) = onHead (fun str) r
    prettyOther' (SpecAlias) = onHead (fun spec) r
    prettyOther' (PadToModulName) =
        onHead (fun (replicate importModuleNamePadSize ' ')) r
    prettyOther' (PadToModulePad) =
        onHead (fun (replicate importModulePadModulePadSize ' ')) r
    prettyOther' (PadToAlias) =
        onHead (fun (replicate importAliasPadSize ' ')) r
    prettyOther' (PadToBase) =
        onHead (fun (replicate importBasePadSize ' ')) r

-- }}} Printing Others --------------------------------------------------------
-- {{{ Expand breaks ----------------------------------------------------------


expandBreaks :: Int -> NEL.NonEmpty PieceResult -> String
expandBreaks columns ((str, br, strAfterBr) NEL.:| xs) =
    fst $ m xs p br strAfterBr
  where
    len = length str
    p = (str, len)

    expandBreaks' :: [PieceResult] -> (String, Int) -> (String, Int)
    expandBreaks' [] r = r
    expandBreaks' ((str', br', strAfterBr') : xs') r =
        m xs' current br' strAfterBr'
       where
         current = addStr str' r

    m bs a brk strAB = case brk of
        Break -> expandBreaks' bs $ break a
        BreakAFAP -> if willNextOverflow columns a bs
            then expandBreaks' bs $ break  a
            else expandBreaks' bs a
        Nill -> expandBreaks' bs a
      where
        break = addStr strAB . addNewLine



willNextOverflow :: Int -> (String, Int) -> [PieceResult] -> Bool
willNextOverflow _ _ [] = False
willNextOverflow columns r ((str', _, _) : _) =
    if getLen (addStr str' r) > columns + 1
        then True
        else False
  where
    getLen (_, len) = len

addNewLine :: (String, Int) -> (String, Int)
addNewLine (str, _) = (str <> "\n", 0)

addStr :: String -> (String, Int) -> (String, Int)
addStr str (str', len) = (str' <> str, len + length str)

-- }}} Expand breaks ----------------------------------------------------------

prettyOnlyBase
    :: Bool
    -> Options
    -> Int
    -- ^ Pad inmpor modifier to some colum
    -> H.ImportDecl LineBlock
    -> (String, Stats)
prettyOnlyBase padQualified (Options _ _ _ _ _) padModifierColum imp =
    let afterNameLength = length $ unwords moduleName
        afterModulePad = (length $ unwords modulePad) - 1
        afterAliasLenght = length $ unwords alias
        afterBaseLength = length $ unwords base
    in ( unwords base
       , Stats
           { afterModuleName = afterNameLength
           , afterAlias = afterAliasLenght
           , afterModulePad = afterModulePad
           , afterBase = afterBaseLength
           }
       )
  where
    isImporQualified = H.importQualified imp

    qualified' True = ["qualified"]
    qualified' False = if padQualified
        then ["         "]
        else []

    moduleNameRec (H.ModuleName _ n) = n

    padModifier = if len < padModifierColum
        then [replicate (padModifierColum - len - 1) ' ']
        else []
      where
        len = length (unwords moduleName)

    moduleName :: [String]
    moduleName =
        ["import"]
        <> qualified' isImporQualified
        <> [moduleNameRec $ H.importModule imp]
    modulePad = moduleName <> padModifier
    alias = modulePad
        <> ["as " <> moduleNameRec x | x <- maybeToList $ H.importAs imp]
    base = alias
        <> if hasHiding
            then ["hiding"]
            else []

    hasHiding :: Bool
    hasHiding = maybe False hasHiding' $ H.importSpecs imp
    hasHiding' (H.ImportSpecList _ x _) = x


--------------------------------------------------------------------------------
prettyImportGroup :: Int -> GroupStats -> Options -> Int
                  -> [H.ImportDecl LineBlock]
                  -> Lines
prettyImportGroup columns globalStats options _longest imps =
    prettyMy globalStats columns options $ sortBy compareImports imps


--------------------------------------------------------------------------------
step :: Int -> Options -> Step
step columns = makeStep "Imports" . step' columns

defaultOptions :: Options
defaultOptions = Options
    { _padQualified = GlobalPad
    , _padModifier = GlobalPad
    , _formatIfSpecsEmpty = [Other' $ Lit " ()"]
    , _shortSpec = Spec
        [Other' $ Lit " (", Other' SpecAlias]
        [Other' $ Lit ")"]
        [Other' $ Lit ", ", Other' SpecAlias]
        ( SubSpec
            [Other' $ Lit " (", Other' SpecAlias]
            [Other' $ Lit ")"]
            [Other' $ Lit ", ", Other' SpecAlias]
            [Other' $ Lit " (..)"]
        )
    , _longSpec = Spec
        [Other' $ Lit " (", Other' SpecAlias]
        [Other' $ Lit ")"]
        [ Other' $ Lit ", "
        , NewLine' (NewLineAsFarAsPossible [PadToAlias, Lit "  "])
        , Other' SpecAlias
        ]
        ( SubSpec
            [Other' $ Lit " (", Other' SpecAlias]
            [Other' $ Lit ")"]
            [Other' $ Lit ", "
            , NewLine' (NewLineAsFarAsPossible [PadToAlias, Lit "  "])
            , Other' SpecAlias
            ]
            [Other' $ Lit " (..)"]
        )
    }



--------------------------------------------------------------------------------
step' :: Int -> Options -> Lines -> Module -> Lines
step' columns options@Options{..} ls (module', _) = applyChanges
    [ change block . const $
        prettyImportGroup columns (groupStats importGroup)
            options longest importGroup
    | (block, importGroup) <- groups
    ]
    ls
  where
    imps    = map sortImportSpecs $ imports $ fmap linesFromSrcSpan module'
    longest = longestImport imps
    groups  = groupAdjacent [(H.ann i', i') | i' <- imps]

    groupStats :: [H.ImportDecl LineBlock] -> GroupStats
    groupStats group' = GroupStats
        { gropuStats =
            fmap (snd . prettyOnlyBase (shouldPadQualified group') options 0)
                group'
        , globalStats = globalStats' group'
        }

    globalStats' :: [H.ImportDecl LineBlock] -> GlobalStats
    globalStats' group' = GlobalStats
        { stats =
            map (snd . prettyOnlyBase (shouldPadQualified group') options 0) imps
        , statsPadQualified = shouldPadQualified group'
        }

    shouldPadQualified group' = case _padQualified of
        GlobalPad -> True
        FilePad -> any H.importQualified imps
        GroupPad -> any H.importQualified group'
        NoPad -> False
