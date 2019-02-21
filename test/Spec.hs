import Test.Hspec

import Parser
import CoreParser
import Language

main :: IO ()
main = hspec $ do
    describe "CoreParser" $ do
        it "can parse Def" $ do
            parse parseDef "a_b0 = 42;" `shouldBe` [(("a_b0", ENum 42), ";")]

        it "can parse Alter" $ do
            parse parseAlter "<42> x y z -> 24;" `shouldBe` [((42, ["x", "y", "z"], ENum 24), ";")]

        it "can parse Constr" $ do
            parse parseConstr "Pack {42, 3};" `shouldBe` [(EConstr 42 3, ";")]

        it "can parse Let" $ do
            parse parseLet "let x = 42 in 100;" `shouldBe` [(ELet NonRecursive [("x", ENum 42)] (ENum 100), ";")]
            parse parseLet "let x = 42; y = 24 in 100;" `shouldBe` [(ELet NonRecursive [("x", ENum 42), ("y", ENum 24)] (ENum 100), ";")]
            parse parseLet "letrec x = 42; y = 24 in 100;" `shouldBe` [(ELet Recursive [("x", ENum 42), ("y", ENum 24)] (ENum 100), ";")]
