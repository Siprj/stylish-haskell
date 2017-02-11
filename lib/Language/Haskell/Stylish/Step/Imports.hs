{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
--------------------------------------------------------------------------------
module Language.Haskell.Stylish.Step.Imports
    ( Options (..)
    , Import (..)
    , Pad (..)
    , Other (..)
    , Piece (..)
    , Spec (..)
    , SubSpec (..)
    , NewLine (..)
    , defaultOptions
    --, ImportAlign (..)
    --, ListAlign (..)
    --, LongListAlign (..)
    --, EmptyListAlign (..)
    --, ListPadding (..)
    , step
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
-- TODO: sort subspecs
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

data Import = Import
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

data Options = Options
    { columns :: Int
    , importStyle :: Import
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

prettyMy :: GroupStats -> Options -> [H.ImportDecl LineBlock] -> Lines
prettyMy GroupStats{..} Options{..} imps =
    mconcat $ fmap (prettyPrintWhole importStyle) imps
  where
    prettyPrintWhole :: Import -> H.ImportDecl LineBlock -> [String]
    prettyPrintWhole Import{..} imp@H.ImportDecl{..} =
        removeSpaces. lines . expandBreaks $ case importSpecs of
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

        importModuleNamePadSize = afterModuleName computeImportStats
        importModulePadModulePadSize = afterModulePad computeImportStats
        importAliasPadSize = afterAlias computeImportStats
        importBasePadSize = afterBase computeImportStats

        computeImportStats =
            snd $ prettyOnlyBase (statsPadQualified globalStats)
                importStyle magic imp

        short xs = prettySpec xs _shortSpec base
        long xs = prettySpec xs _longSpec base

        isOverflowing = any ((> columns) . length) . lines . expandBreaks

        prettyPrintBase :: String
        prettyPrintBase =
            let (b, _) = prettyOnlyBase (statsPadQualified globalStats)
                    importStyle magic imp
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
            NEL.reverse $ prettyPieces "" _formatIfSpecsEmpty r
        prettySpec (x : xs) spec@Spec{..} r = NEL.reverse
            . prettySpec' xs spec $ case specToEither x of
                Right specStr ->
                    prettyPieces specStr specsBefore r
                Left (specStr, sscs) ->
                    prettySubSpec sscs subSpecs
                    $ prettyPieces specStr specsBefore r

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
        prettyOther _ fun (Lit str) r = onHead (fun str) r
        prettyOther spec fun (SpecAlias) r = onHead (fun spec) r
        prettyOther _ fun (PadToModulName) r =
            onHead (fun (replicate importModuleNamePadSize ' ')) r
        prettyOther _ fun (PadToModulePad) r =
            onHead (fun (replicate importModulePadModulePadSize ' ')) r
        prettyOther _ fun (PadToAlias) r =
            onHead (fun (replicate importAliasPadSize ' ')) r
        prettyOther _ fun (PadToBase) r =
            onHead (fun (replicate importBasePadSize ' ')) r

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
                Break -> expandBreaks' xs . break $ current
                BreakAFAP -> if willNextOverflow current xs
                    then expandBreaks' xs $ break current
                    else expandBreaks' xs current
                Nill -> expandBreaks' xs current
          where
            break = addStr strAfterBr . addNewLine
            current = addStr str r

        addNewLine :: (String, Int) -> (String, Int)
        addNewLine (str, _) = (str <> newLine, 0)

        addStr str (str', len) = (str' <> str, len + length str)
        getLen (_, len) = len

        willNextOverflow _ [] = False
        willNextOverflow r ((str, _, _) : _) =
            if getLen (addStr str r) > columns + 1
                then True
                else False

        newLine = "\n"

-- }}} Printing pieces --------------------------------------------------------

prettyOnlyBase
    :: Bool
    -> Import
    -> Int
    -- ^ Pad inmpor modifier to some colum
    -> H.ImportDecl LineBlock
    -> (String, Stats)
prettyOnlyBase padQualified (Import _ _ _ _ _) padModifierColum imp =
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
prettyImportGroup _columns globalStats options _longest imps =
    prettyMy globalStats options $ sortBy compareImports imps


--------------------------------------------------------------------------------
step :: Int -> Options -> Step
step columns = makeStep "Imports" . step' columns

defaultOptions :: Options
defaultOptions = Options 80 Import
    { _padQualified = GlobalPad
    , _padModifier = GlobalPad
    , _formatIfSpecsEmpty = [Other' $ Lit " ()"]
    , _shortSpec = Spec
        [ Other' $ Lit " (", Other' SpecAlias]
        [ Other' $ Lit ")"]
        [ Other' $ Lit ", ", Other' SpecAlias]
        ( SubSpec
            [Other' $ Lit "(", Other' SpecAlias]
            [Other' $ Lit ")"]
            [Other' $ Lit ", ", Other' SpecAlias]
            [Other' $ Lit " (..)"]
        )
    , _longSpec = Spec
        [ Other' $ Lit " (", Other' SpecAlias]
        [ Other' $ Lit ")"]
        [ Other' $ Lit ", ", NewLine' (NewLineAsFarAsPossible [PadToAlias, Lit "  "]), Other' SpecAlias]
        ( SubSpec
            [ Other' $ Lit "(", Other' SpecAlias]
            [ Other' $ Lit ")"]
            [ Other' $ Lit ", ", NewLine' (NewLineAsFarAsPossible [PadToAlias, Lit "  "]), Other' SpecAlias]
            [ Other' $ Lit " (..)"]
        )
    }



--------------------------------------------------------------------------------
step' :: Int -> Options -> Lines -> Module -> Lines
step' _columns o@(Options c i) ls (module', _) = applyChanges
    [ change block . const $
        prettyImportGroup c (groupStats importGroup) o longest importGroup
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
            fmap (snd . prettyOnlyBase (shouldPadQualified group') i 0) group'
        , globalStats = globalStats' group'
        }

    globalStats' :: [H.ImportDecl LineBlock] -> GlobalStats
    globalStats' group' = GlobalStats
        { stats =
            map (snd . prettyOnlyBase (shouldPadQualified group') i 0) imps
        , statsPadQualified = shouldPadQualified group'
        }

    shouldPadQualified group' = case _padQualified i of
        GlobalPad -> True
        FilePad -> any H.importQualified imps
        GroupPad -> any H.importQualified group'
        NoPad -> False
