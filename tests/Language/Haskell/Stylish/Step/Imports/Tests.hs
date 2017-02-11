--------------------------------------------------------------------------------
module Language.Haskell.Stylish.Step.Imports.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     (Assertion, (@=?))


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Step.Imports
import           Language.Haskell.Stylish.Tests.Util



----------------------------------------------------------------------------------
--fromImportAlign :: ImportAlign -> Options
--fromImportAlign align = defaultOptions { importAlign = align }


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Language.Haskell.Stylish.Step.Imports.Tests"
    [ testCase "case 01" case01
    , testCase "case 02" case02
    , testCase "case 03" case03
    , testCase "case 04" case04
    , testCase "case 05" case05
    , testCase "case 06" case06
    , testCase "case 07" case07
    , testCase "case 08" case08
    , testCase "case 09" case09
    , testCase "case 10" case10
    , testCase "case 11" case11
    , testCase "case 12" case12
    , testCase "case 13" case13
    , testCase "case 14" case14
    , testCase "case 15" case15
    , testCase "case 16" case16
    , testCase "case 17" case17
    , testCase "case 18" case18
    , testCase "case 19" case19
    , testCase "case 19b" case19b
    , testCase "case 19c" case19c
    , testCase "case 19d" case19d
    ]


--------------------------------------------------------------------------------
input :: String
input = unlines
    [ "module Herp where"
    , ""
    , "import qualified Data.Map  as M"
    , "import Control.Monad"
    , "import  Only.Instances()"
    , "import       Data.Map     (lookup, (!), insert, Map)"
    , "import Data.List as List (concat, foldl, foldr, head, init, last,\
     \ length, map, null, reverse, tail, (++))"
    , ""
    , "import Herp.Derp.Internals hiding (foo)"
    , "import  Foo (Bar (..))"
    , ""
    , "herp = putStrLn \"import Hello world\""
    ]


--------------------------------------------------------------------------------
case01 :: Assertion
case01 = expected @=? testStep (step 80 $ defaultOptions) input
  where
    expected = unlines
        [ "module Herp where"
        , ""
        , "import           Control.Monad"
        , "import           Data.List           as List (concat, foldl, foldr, head, init,"
        , "                                              last, length, map, null, reverse,"
        , "                                              tail, (++))"
        , "import           Data.Map            (Map, insert, lookup, (!))"
        , "import qualified Data.Map            as M"
        , "import           Only.Instances      ()"
        , ""
        , "import           Foo                 (Bar (..))"
        , "import           Herp.Derp.Internals hiding (foo)"
        , ""
        , "herp = putStrLn \"import Hello world\""
        ]


--------------------------------------------------------------------------------
case02Options :: Options
case02Options =
    let (Options column style) = defaultOptions
    in Options column style
    { _padQualified = GroupPad
    , _padModifier = GroupPad
    }

case02 :: Assertion
case02 = expected @=? testStep (step 80 case02Options) input
  where
    expected = unlines
        [ "module Herp where"
        , ""
        , "import           Control.Monad"
        , "import           Data.List      as List (concat, foldl, foldr, head, init, last,"
        , "                                         length, map, null, reverse, tail, (++))"
        , "import           Data.Map       (Map, insert, lookup, (!))"
        , "import qualified Data.Map       as M"
        , "import           Only.Instances ()"
        , ""
        , "import Foo                 (Bar (..))"
        , "import Herp.Derp.Internals hiding (foo)"
        , ""
        , "herp = putStrLn \"import Hello world\""
        ]


--------------------------------------------------------------------------------
case03Options :: Options
case03Options =
    let (Options column style) = defaultOptions
    in Options column style
    { _padQualified = NoPad
    , _padModifier = NoPad
    }

case03 :: Assertion
case03 = expected @=? testStep (step 80 case03Options) input
  where
    expected = unlines
        [ "module Herp where"
        , ""
        , "import Control.Monad"
        , "import Data.List as List (concat, foldl, foldr, head, init, last, length, map,"
        , "                          null, reverse, tail, (++))"
        , "import Data.Map (Map, insert, lookup, (!))"
        , "import qualified Data.Map as M"
        , "import Only.Instances ()"
        , ""
        , "import Foo (Bar (..))"
        , "import Herp.Derp.Internals hiding (foo)"
        , ""
        , "herp = putStrLn \"import Hello world\""
        ]


--------------------------------------------------------------------------------
case04Options :: Options
case04Options =
    let (Options column style) = defaultOptions
    in Options column style
    { _padQualified = GlobalPad
    , _padModifier = GlobalPad
    }

case04 :: Assertion
case04 = expected @=? testStep (step 80 case04Options) input'
  where
    input' =
        "import Data.Aeson.Types (object, typeMismatch, FromJSON(..)," ++
        "ToJSON(..), Value(..), parseEither, (.!=), (.:), (.:?), (.=))"

    expected = unlines
        [ "import           Data.Aeson.Types (FromJSON (..), ToJSON (..), Value (..),"
        , "                                   object, parseEither, typeMismatch, (.!=),"
        , "                                   (.:), (.:?), (.=))"
        ]


--------------------------------------------------------------------------------
case05Options :: Options
case05Options =
    let (Options column style) = defaultOptions
    in Options column style
    { _padQualified = GroupPad
    , _padModifier = GroupPad
    }

case05 :: Assertion
case05 = input' @=? testStep (step 80 case05Options) input'
  where
    input' = "import Distribution.PackageDescription.Configuration " ++
        "(finalizePackageDescription)\n"


--------------------------------------------------------------------------------
case06Options :: Options
case06Options =
    let (Options column style) = defaultOptions
    in Options column style
    { _padQualified = FilePad
    , _padModifier = FilePad
    }

case06 :: Assertion
case06 = input' @=? testStep (step 80 case06Options) input'
  where
    input' = unlines
        [ "import Bar.Qux"
        , "import Foo.Bar"
        ]


--------------------------------------------------------------------------------
case07Options :: Options
case07Options =
    let (Options column style) = defaultOptions
    in Options column style
    { _padQualified = FilePad
    , _padModifier = FilePad
    }

case07 :: Assertion
case07 = expected @=? testStep (step 80 case07Options) input'
  where
    input' = unlines
        [ "import Bar.Qux"
        , ""
        , "import qualified Foo.Bar"
        ]

    expected = unlines
        [ "import           Bar.Qux"
        , ""
        , "import qualified Foo.Bar"
        ]


--------------------------------------------------------------------------------
case08Options :: Options
case08Options =
    let (Options column style) = defaultOptions
    in Options column style
    { _padQualified = FilePad
    , _padModifier = FilePad
    , _longSpec = Spec
        [ Other' $ Lit " (", Other' SpecAlias]
        [ Other' SpecAlias, Other' $ Lit ")"]
        [ Other' $ Lit ", ", NewLine' (NewLineAsFarAsPossible [PadToModulePad , Lit "  "]), Other' SpecAlias]
        ( SubSpec
            [Other' $ Lit "(", Other' SpecAlias]
            [Other' $ Lit ")"]
            [Other' $ Lit ", ", Other' SpecAlias]
            [Other' $ Lit " (..)"]
        )
    }

case08 :: Assertion
case08 = expected @=? testStep (step 80 case08Options) input
  where
    expected = unlines
        [ "module Herp where"
        , ""
        , "import           Control.Monad"
        , "import           Data.List           as List (concat, foldl, foldr, head, init,"
        , "                                     last, length, map, null, reverse, tail,"
        , "                                     (++))"
        , "import           Data.Map            (Map, insert, lookup, (!))"
        , "import qualified Data.Map            as M"
        , "import           Only.Instances      ()"
        , ""
        , "import           Foo                 (Bar (..))"
        , "import           Herp.Derp.Internals hiding (foo)"
        , ""
        , "herp = putStrLn \"import Hello world\""
        ]


--------------------------------------------------------------------------------
case09Options :: Options
case09Options =
    let (Options column style) = defaultOptions
    in Options column style
    { _padQualified = GlobalPad
    , _padModifier = GlobalPad
    , _longSpec = Spec
        [ NewLine' $ NewLine [], Other' $ Lit "    ( ", Other' SpecAlias]
        [ NewLine' $ NewLine [], Other' $ Lit "    ", Other' $ Lit ")"]
        [ NewLine' $ NewLine [Lit "    , "], Other' SpecAlias]
        ( SubSpec
            [Other' $ Lit "(", Other' SpecAlias]
            [Other' $ Lit ")"]
            [Other' $ Lit ", ", Other' SpecAlias]
            [Other' $ Lit " (..)"]
        )
    }

case09 :: Assertion
case09 = expected @=? testStep (step 80 case09Options) input
  where
    expected = unlines
        [ "module Herp where"
        , ""
        , "import           Control.Monad"
        , "import           Data.List           as List"
        , "    ( concat"
        , "    , foldl"
        , "    , foldr"
        , "    , head"
        , "    , init"
        , "    , last"
        , "    , length"
        , "    , map"
        , "    , null"
        , "    , reverse"
        , "    , tail"
        , "    , (++)"
        , "    )"
        , "import           Data.Map            (Map, insert, lookup, (!))"
        , "import qualified Data.Map            as M"
        , "import           Only.Instances      ()"
        , ""
        , "import           Foo                 (Bar (..))"
        , "import           Herp.Derp.Internals hiding (foo)"
        , ""
        , "herp = putStrLn \"import Hello world\""
        ]


--------------------------------------------------------------------------------
case10Options :: Options
case10Options =
    let (Options _ style) = defaultOptions
    in Options 40 style
    { _padQualified = GroupPad
    , _padModifier = GroupPad
    , _longSpec = Spec
        [ NewLine' $ NewLine [], Other' $ Lit "    ( ", Other' SpecAlias]
        [ NewLine' $ NewLine [], Other' $ Lit "    ", Other' $ Lit ")"]
        [ NewLine' $ NewLine [Lit "    , "], Other' SpecAlias]
        ( SubSpec
            [Other' $ Lit "(", Other' SpecAlias]
            [Other' $ Lit ")"]
            [Other' $ Lit ", ", Other' SpecAlias]
            [Other' $ Lit " (..)"]
        )
    }

case10 :: Assertion
case10 = expected @=? testStep (step 40 case10Options) input
  where
    expected = unlines
        [ "module Herp where"
        , ""
        , "import           Control.Monad"
        , "import           Data.List      as List"
        , "    ( concat"
        , "    , foldl"
        , "    , foldr"
        , "    , head"
        , "    , init"
        , "    , last"
        , "    , length"
        , "    , map"
        , "    , null"
        , "    , reverse"
        , "    , tail"
        , "    , (++)"
        , "    )"
        , "import           Data.Map"
        , "    ( Map"
        , "    , insert"
        , "    , lookup"
        , "    , (!)"
        , "    )"
        , "import qualified Data.Map       as M"
        , "import           Only.Instances ()"
        , ""
        , "import Foo                 (Bar (..))"
        , "import Herp.Derp.Internals hiding (foo)"
        , ""
        , "herp = putStrLn \"import Hello world\""
        ]


--------------------------------------------------------------------------------
case11Options :: Options
case11Options =
    let (Options _ style) = defaultOptions
    in Options 80 style
    { _padQualified = GroupPad
    , _padModifier = GroupPad
    , _formatIfSpecsEmpty = [ NewLine' $ NewLine [], Other' $ Lit "    ()"]
    , _shortSpec = Spec
        [ NewLine' $ NewLine [], Other' $ Lit "    (", Other' SpecAlias]
        [ Other' $ Lit ")"]
        [ Other' $ Lit ", ", NewLine' $ NewLineAsFarAsPossible [Lit "    "], Other' SpecAlias]
        ( SubSpec
            [Other' $ Lit "(", Other' SpecAlias]
            [Other' $ Lit ")"]
            [Other' $ Lit ", ", NewLine' $ NewLineAsFarAsPossible [Lit "    "], Other' SpecAlias]
            [Other' $ Lit " (..)"]
        )
    }

case11 :: Assertion
case11 = expected @=? testStep (step 80 case11Options) input
  where
    expected = unlines
        [ "module Herp where"
        , ""
        , "import           Control.Monad"
        , "import           Data.List      as List"
        , "    (concat, foldl, foldr, head, init, last, length, map, null, reverse, tail,"
        , "    (++))"
        , "import           Data.Map"
        , "    (Map, insert, lookup, (!))"
        , "import qualified Data.Map       as M"
        , "import           Only.Instances"
        , "    ()"
        , ""
        , "import Foo"
        , "    (Bar (..))"
        , "import Herp.Derp.Internals hiding"
        , "    (foo)"

        , ""
        , "herp = putStrLn \"import Hello world\""
        ]


------------------------------------------------------------------------------
case12Options :: Options
case12Options =
    let (Options _ style) = defaultOptions
    in Options 80 style
    { _padQualified = GroupPad
    , _padModifier = GroupPad
    , _formatIfSpecsEmpty = [ NewLine' $ NewLine [], Other' $ Lit "    ()"]
    , _shortSpec = Spec
        [ NewLine' $ NewLine [], Other' $ Lit "  (", Other' SpecAlias]
        [ Other' $ Lit ")"]
        [ Other' $ Lit ", ", NewLine' $ NewLineAsFarAsPossible [Lit "    "], Other' SpecAlias]
        ( SubSpec
            [Other' $ Lit "(", Other' SpecAlias]
            [Other' $ Lit ")"]
            [Other' $ Lit ", ", NewLine' $ NewLineAsFarAsPossible [Lit "    "], Other' SpecAlias]
            [Other' $ Lit " (..)"]
        )
    }

case12 :: Assertion
case12 = expected @=? testStep (step 80 case12Options) input'
  where
    input' = unlines
        [ "import Data.List (map)"
        ]

    expected = unlines
        [ "import Data.List"
        , "  (map)"
        ]


--------------------------------------------------------------------------------
case13Options :: Options
case13Options =
    let (Options _ style) = defaultOptions
    in Options 80 style
    { _padQualified = NoPad
    , _padModifier = NoPad
    , _formatIfSpecsEmpty = [ NewLine' $ NewLine [], Other' $ Lit "    ()"]
    , _shortSpec = Spec
        [ NewLine' $ NewLine [], Other' $ Lit "    (", Other' SpecAlias]
        [ Other' $ Lit ")"]
        [ Other' $ Lit ", ", NewLine' $ NewLineAsFarAsPossible [Lit "    "], Other' SpecAlias]
        ( SubSpec
            [Other' $ Lit "(", Other' SpecAlias]
            [Other' $ Lit ")"]
            [Other' $ Lit ", ", NewLine' $ NewLineAsFarAsPossible [Lit "    "], Other' SpecAlias]
            [Other' $ Lit " (..)"]
        )
    }

case13 :: Assertion
case13 = expected @=? testStep (step 80 case13Options) input'
  where
    input' = unlines
        [ "import qualified Data.List as List (concat, foldl, foldr, head, init,"
        , "    last, length, map, null, reverse, tail, (++))"
        ]

    expected = unlines
        [ "import qualified Data.List as List"
        , "    (concat, foldl, foldr, head, init, last, length, map, null, reverse, tail,"
        , "    (++))"
        ]


--------------------------------------------------------------------------------
case14Options :: Options
case14Options =
    let (Options _ style) = defaultOptions
    in Options 80 style
    { _padQualified = NoPad
    , _padModifier = NoPad
    }

case14 :: Assertion
case14 = expected @=? testStep (step 80 case14Options) expected
  where
    expected = unlines
        [ "import qualified Data.List as List (concat, map, null, reverse, tail, (++))"
        ]


--------------------------------------------------------------------------------
case15Options :: Options
case15Options =
    let (Options _ style) = defaultOptions
    in Options 80 style
    { _padQualified = NoPad
    , _padModifier = NoPad
    , _shortSpec = Spec
        [ Other' $ Lit " (", Other' SpecAlias]
        [ Other' $ Lit ")"]
        [ Other' $ Lit ", ", Other' SpecAlias]
        ( SubSpec
            [Other' $ Lit " (", Other' SpecAlias]
            [Other' $ Lit ")"]
            [Other' $ Lit ", ", Other' SpecAlias]
            [Other' $ Lit " (..)"]
        )
    , _longSpec = Spec
        [ NewLine' $ NewLine [Lit "    "], Other' $ Lit "( ", Other' SpecAlias]
        [ NewLine' $ NewLine [Lit "    "], Other' $ Lit ")"]
        [ NewLine' $ NewLine [Lit "    "], Other' $ Lit ", ", Other' SpecAlias]
        ( SubSpec
            [ Other' $ Lit " (", Other' SpecAlias]
            [ Other' $ Lit ")"]
            [ NewLine' $ NewLine [Lit "    "], Other' $ Lit ", ", Other' SpecAlias]
            [ Other' $ Lit " (..)"]
        )
    }

case15 :: Assertion
case15 = expected @=? testStep (step 80 case15Options) input'
  where
    expected = unlines
        [ "import Data.Acid (AcidState)"
        , "import qualified Data.Acid as Acid"
        , "    ( closeAcidState"
        , "    , createCheckpoint"
        , "    , openLocalStateFrom"
        , "    )"
        , "import Data.Default.Class (Default (def))"
        , ""
        , "import qualified Herp.Derp.Internal.Types.Foobar as Internal (bar, foo)"
        ]

    input' = unlines
        [ "import Data.Acid (AcidState)"
        , "import qualified Data.Acid as Acid (closeAcidState, createCheckpoint, openLocalStateFrom)"
        , "import Data.Default.Class (Default (def))"
        , ""
        , "import qualified Herp.Derp.Internal.Types.Foobar as Internal (foo, bar)"
        ]


--------------------------------------------------------------------------------
case16Options :: Options
case16Options =
    let (Options _ style) = defaultOptions
    in Options 80 style
    { _padQualified = NoPad
    , _padModifier = NoPad
    }

case16 :: Assertion
case16 = expected @=? testStep (step 80 case16Options) input'
  where
    expected = unlines
        [ "import Data.Acid (AcidState)"
        , "import Data.Default.Class (Default(def))"
        , ""
        , "import Data.Maybe (Maybe(Just, Nothing))"
        , ""
        , "import Data.Foo (Foo(Bar, Foo), Goo(Goo))"
        ]

    input' = unlines
        [ "import Data.Acid (AcidState)"
        , "import Data.Default.Class (Default(def))"
        , ""
        , "import Data.Maybe (Maybe   (Just, Nothing))"
        , ""
        , "import Data.Foo (Foo (Foo,Bar), Goo(Goo))"
        ]


--------------------------------------------------------------------------------
case17Options :: Options
case17Options =
    let (Options _ style) = defaultOptions
    in Options 80 style
    { _padQualified = NoPad
    , _padModifier = NoPad
    , _shortSpec = Spec
        [ Other' $ Lit " (", Other' SpecAlias]
        [ Other' $ Lit ")"]
        [ Other' $ Lit ", ", Other' SpecAlias]
        ( SubSpec
            [Other' $ Lit " (", Other' SpecAlias]
            [Other' $ Lit ")"]
            [Other' $ Lit ", ", Other' SpecAlias]
            [Other' $ Lit " (..)"]
        )
    }

case17 :: Assertion
case17 = expected @=? testStep (step 80 case17Options) input'
  where
    expected = unlines
        [ "import Control.Applicative (Applicative (pure, (<*>)))"
        , ""
        , "import Data.Identity (Identity (Identity, runIdentity))"
        ]

    input' = unlines
        [ "import Control.Applicative (Applicative ((<*>),pure))"
        , ""
        , "import Data.Identity (Identity (runIdentity,Identity))"
        ]


--------------------------------------------------------------------------------
case18Options :: Options
case18Options =
    let (Options _ style) = defaultOptions
    in Options 40 style
    { _padQualified = NoPad
    , _padModifier = NoPad
    , _shortSpec = Spec
        [ NewLine' $ NewLineAsFarAsPossible [Lit "   "], Other' $ Lit " (", Other' SpecAlias]
        [ Other' $ Lit ")"]
        [ Other' $ Lit ", ", Other' SpecAlias]
        ( SubSpec
            [Other' $ Lit " (", Other' SpecAlias]
            [Other' $ Lit ")"]
            [Other' $ Lit ", ", Other' SpecAlias]
            [Other' $ Lit " (..)"]
        )
    , _longSpec = Spec
        [ NewLine' $ NewLine [Lit "    "], Other' $ Lit "( ", Other' SpecAlias]
        [ NewLine' $ NewLine [Lit "    "], Other' $ Lit ")"]
        [ NewLine' $ NewLine [Lit "    "], Other' $ Lit ", ", Other' SpecAlias]
        ( SubSpec
            [ Other' $ Lit "(", Other' SpecAlias]
            [ Other' $ Lit ")"]
            [ Other' $ Lit ", ", NewLine' (NewLineAsFarAsPossible [PadToAlias, Lit "  "]), Other' SpecAlias]
            [ Other' $ Lit " (..)"]
        )
    }

case18 :: Assertion
case18 = expected @=? testStep (step 40 case18Options) input'
  where
    expected = unlines
           ----------------------------------------
        [ "import Data.Foo as Foo (Bar, Baz, Foo)"
        , ""
        , "import Data.Identity"
        , "    (Identity (Identity, runIdentity))"
        , ""
        , "import Data.Acid as Acid"
        , "    ( closeAcidState"
        , "    , createCheckpoint"
        , "    , openLocalStateFrom"
        , "    )"
        ]

    input' = unlines
        [ "import Data.Foo as Foo (Bar, Baz, Foo)"
        , ""
        , "import Data.Identity (Identity (Identity, runIdentity))"
        , ""
        , "import Data.Acid as Acid (closeAcidState, createCheckpoint, openLocalStateFrom)"
        ]

--------------------------------------------------------------------------------
case19Options :: Options
case19Options =
    let (Options _ style) = defaultOptions
    in Options 39 style
    { _padQualified = GlobalPad
    , _padModifier = NoPad
    , _formatIfSpecsEmpty = [Other' $ Lit " ()"]
    , _shortSpec = Spec
        [ NewLine' $ NewLine [Lit "                 "]
        , Other' $ Lit "(", Other' SpecAlias
        ]
        [ Other' $ Lit ")"]
        [ Other' $ Lit ", ", NewLine' $ NewLineAsFarAsPossible [Lit "                 "]
        , Other' SpecAlias
        ]
        ( SubSpec
            [Other' $ Lit " (", Other' SpecAlias]
            [Other' $ Lit ")"]
            [Other' $ Lit ", ", Other' SpecAlias]
            [Other' $ Lit " (..)"]
        )
    }

case19 :: Assertion
case19 = expected @=? testStep (step 39 case19Options) case19input
  where
    expected = unlines
           ----------------------------------------
        [ "import           Prelude ()"
        , "import           Prelude.Compat hiding"
        , "                 (foldMap)"
        , ""
        , "import           Data.List"
        , "                 (foldl', intercalate,"
        , "                 intersperse)"
        ]

case19BOptions :: Options
case19BOptions =
    let (Options _ style) = case19Options
    in Options 39 style
    { _padQualified = FilePad
    , _padModifier = NoPad
    }

case19b :: Assertion
case19b = expected @=? testStep (step 39 case19BOptions) case19input
  where
    expected = unlines
           ----------------------------------------
        [ "import Prelude ()"
        , "import Prelude.Compat hiding"
        , "                 (foldMap)"
        , ""
        , "import Data.List"
        , "                 (foldl', intercalate,"
        , "                 intersperse)"
        ]

case19COptions :: Options
case19COptions =
    let (Options _ style) = defaultOptions
    in Options 39 style
    { _padQualified = NoPad
    , _padModifier = NoPad
    , _formatIfSpecsEmpty = [Other' $ Lit " ()"]
    , _shortSpec = Spec
        [ NewLine' $ NewLine [Lit "       "]
        , Other' $ Lit "(", Other' SpecAlias
        ]
        [ Other' $ Lit ")"]
        [ Other' $ Lit ", ", NewLine' $ NewLineAsFarAsPossible [Lit "       "]
        , Other' SpecAlias
        ]
        ( SubSpec
            [Other' $ Lit " (", Other' SpecAlias]
            [Other' $ Lit ")"]
            [Other' $ Lit ", ", Other' SpecAlias]
            [Other' $ Lit " (..)"]
        )
    , _longSpec = Spec
        [ NewLine' $ NewLine [Lit "       "]
        , Other' $ Lit "(", Other' SpecAlias
        ]
        [ Other' $ Lit ")"]
        [ Other' $ Lit ", ", NewLine' $ NewLineAsFarAsPossible [Lit "       "]
        , Other' SpecAlias
        ]
        ( SubSpec
            [Other' $ Lit " (", Other' SpecAlias]
            [Other' $ Lit ")"]
            [Other' $ Lit ", ", Other' SpecAlias]
            [Other' $ Lit " (..)"]
        )
    }

case19c :: Assertion
case19c = expected @=? testStep (step 39 case19COptions) case19input
  where
    expected = unlines
           ----------------------------------------
        [ "import Prelude ()"
        , "import Prelude.Compat hiding"
        , "       (foldMap)"
        , ""
        , "import Data.List"
        , "       (foldl', intercalate,"
        , "       intersperse)"
        ]

case19d :: Assertion
case19d = expected @=? testStep
    (step 40 $ case19Options) case19input
  where
    expected = unlines
           ----------------------------------------
        [ "import           Prelude ()"
        , "import           Prelude.Compat hiding"
        , "                 (foldMap)"
        , ""
        , "import           Data.List"
        , "                 (foldl', intercalate,"
        , "                 intersperse)"
        ]

case19input :: String
case19input = unlines
        [ "import Prelude.Compat hiding (foldMap)"
        , "import Prelude ()"
        , ""
        , "import Data.List (foldl', intercalate, intersperse)"
        ]
