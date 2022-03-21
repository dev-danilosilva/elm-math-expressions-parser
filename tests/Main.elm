module Main exposing (..)

import Expect
import Fuzz
import Helpers
import Max.Parser as Parser
import Max.Parser.Ast as Ast
import Test



-- The terms is bounded to the Integer Set
-- The operations supported are going to be + , - , * , /


ast_tests : Test.Test
ast_tests =
    Test.describe "ast"
        [ Test.describe "((fromString . toString) x) == x"
            [ Test.fuzz Fuzz.int "integers" <|
                \num ->
                    Helpers.identityTest Ast.fromString Ast.toString (String.fromInt num)
            , Test.test "sum operator" <|
                \_ ->
                    Helpers.identityTest Ast.fromString Ast.toString "+"
            , Test.test "subt operator" <|
                \_ ->
                    Helpers.identityTest Ast.fromString Ast.toString "-"
            , Test.test "mult operator" <|
                \_ ->
                    Helpers.identityTest Ast.fromString Ast.toString "-"
            , Test.test "div operator" <|
                \_ ->
                    Helpers.identityTest Ast.fromString Ast.toString "/"
            ]
        ]


parsing_tests : Test.Test
parsing_tests =
    Test.describe "parsing a string"
        [ Test.fuzz (Fuzz.intRange 0 500) "positive integers" <|
            \num ->
                Parser.fromString (String.fromInt num)
                    |> Expect.equal (Ok (Ast.Int_ num))
        , Test.test "sum operator" <|
            \_ ->
                Parser.fromString "+"
                    |> Expect.equal (Ok (Ast.SingleOperationSign_ Ast.Sum))
        , Test.test "subtraction operator" <|
            \_ ->
                Parser.fromString "-"
                    |> Expect.equal (Ok (Ast.SingleOperationSign_ Ast.Subt))
        , Test.test "product operator" <|
            \_ ->
                Parser.fromString "*"
                    |> Expect.equal (Ok (Ast.SingleOperationSign_ Ast.Mult))
        , Test.test "division operator" <|
            \_ ->
                Parser.fromString "/"
                    |> Expect.equal (Ok (Ast.SingleOperationSign_ Ast.Div))
        , Test.fuzz2 (Fuzz.intRange 0 500) (Fuzz.intRange 0 500) "simple sum" <|
            \x y ->
                Parser.fromString ((String.fromInt x) ++ " + " ++ (String.fromInt y)) 
                    |> Expect.equal (Ok (Ast.BinaryOperation_ Ast.Sum (Ast.Int_ x) (Ast.Int_ y)))
        , Test.fuzz3 (Fuzz.intRange 0 500) (randomOperation) (Fuzz.intRange 0 500) "operations without spaces" <|
            \x op y ->
                Parser.fromString ((String.fromInt x) ++ (Ast.toString (Ast.SingleOperationSign_ op)) ++ (String.fromInt y))
                    |> Expect.equal (Ok (Ast.BinaryOperation_ op (Ast.Int_ x) (Ast.Int_ y)))
        , Test.fuzz2 (Fuzz.intRange 0 500) (Fuzz.intRange 0 500) "simple subtraction" <|
            \x y ->
                Parser.fromString ((String.fromInt x) ++ " - " ++ (String.fromInt y))
                    |> Expect.equal (Ok (Ast.BinaryOperation_ Ast.Subt (Ast.Int_ x) (Ast.Int_ y)))
        , Test.fuzz2 (Fuzz.intRange 0 500) (Fuzz.intRange 0 500) "simple mult" <|
            \x y ->
                Parser.fromString ((String.fromInt x) ++ " * " ++ (String.fromInt y))
                    |> Expect.equal (Ok (Ast.BinaryOperation_ Ast.Mult (Ast.Int_ x) (Ast.Int_ y)))
        , Test.fuzz2 (Fuzz.intRange 0 500) (Fuzz.intRange 0 500) "simple division" <|
            \x y ->
                Parser.fromString ((String.fromInt x) ++ " / " ++ (String.fromInt y))
                    |> Expect.equal (Ok (Ast.BinaryOperation_ Ast.Div (Ast.Int_ x) (Ast.Int_ y)))
        , Test.fuzz3 (Fuzz.intRange 0 500) (Fuzz.intRange 0 500) (Fuzz.intRange 0 500) "3 terms sum" <|
            \x y z ->
                Parser.fromString ((String.fromInt x) ++ " + " ++ (String.fromInt y) ++ "-" ++ (String.fromInt z))
                    |> Expect.equal (Ok (Ast.BinaryOperation_ Ast.Subt (Ast.BinaryOperation_ Ast.Sum (Ast.Int_ x) (Ast.Int_ y)) (Ast.Int_ z)))
        , Test.fuzz3 (Fuzz.intRange 0 500) (Fuzz.intRange 0 500) (Fuzz.intRange 0 500) "4 terms sum" <|
            \x y z ->
                Parser.fromString ((String.fromInt x) ++ " + " ++ (String.fromInt y) ++ "-" ++ (String.fromInt z) ++ " + 5")
                    |> Expect.equal (Ok (Ast.BinaryOperation_ Ast.Sum (Ast.BinaryOperation_ Ast.Subt (Ast.BinaryOperation_ Ast.Sum (Ast.Int_ x) (Ast.Int_ y)) (Ast.Int_ z)) (Ast.Int_ 5)))
        , Test.test "5 terms sum" <|
            \_ ->
                Parser.fromString "10 + 3 * 4 + 5 + 45"
                    |> Expect.equal (Ok (Ast.BinaryOperation_ Ast.Sum (Ast.BinaryOperation_ Ast.Sum (Ast.BinaryOperation_ Ast.Mult (Ast.BinaryOperation_ Ast.Sum (Ast.Int_ 10) (Ast.Int_ 3)) (Ast.Int_ 4)) (Ast.Int_ 5)) (Ast.Int_ 45)))
        ]

randomOperation : Fuzz.Fuzzer Ast.Operation
randomOperation =
    Fuzz.oneOf
        [ Fuzz.constant Ast.Sum
        , Fuzz.constant Ast.Subt
        , Fuzz.constant Ast.Mult
        , Fuzz.constant Ast.Div
        ]

randomPositiveInteger : Fuzz.Fuzzer Int
randomPositiveInteger =
    Fuzz.intRange 1 200
