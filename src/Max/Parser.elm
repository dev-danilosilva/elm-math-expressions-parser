module Max.Parser exposing (..)

import Max.Parser.Ast as Ast
import Parser exposing ((|.), (|=))


fromString : String -> Result String Ast.Value
fromString =
    Parser.run parser >> Result.mapError Parser.deadEndsToString

parser : Parser.Parser Ast.Value
parser =
    Parser.oneOf
        [ Parser.backtrackable singleInteger
        , singleOperationSign
        , binaryOperation
        ]

binaryOperation : Parser.Parser Ast.Value
binaryOperation =
    Parser.loop Nothing binaryOperationHelper

binaryOperationHelper : Maybe Ast.Value -> Parser.Parser (Parser.Step (Maybe Ast.Value) Ast.Value)
binaryOperationHelper state =
    case state of
        Nothing ->
            Parser.succeed (\x op y -> {x = x, y = y, op = op})
            |= integerFactor
            |. Parser.spaces
            |= operations
            |. Parser.spaces
            |= integerFactor
            |> Parser.andThen
                (\result ->
                    Ast.BinaryOperation_ result.op result.x result.y
                    |> Just
                    |> Parser.Loop
                    |> Parser.succeed)
        Just value ->
            Parser.oneOf
                [ Parser.succeed (\op factor -> {factor = factor, op = op})
                    |. Parser.spaces
                    |= operations
                    |. Parser.spaces
                    |= integerFactor
                    |> Parser.andThen
                        (\result ->
                            Just (Ast.BinaryOperation_ result.op value result.factor)
                            |> Parser.Loop
                            |> Parser.succeed)
                , Parser.succeed (Parser.Done value) 
                ]
    

integerFactor : Parser.Parser Ast.Value
integerFactor =
    Parser.succeed Ast.Int_
        |= Parser.int

operation : String -> Ast.Operation -> Parser.Parser Ast.Operation
operation str op =
    Parser.succeed (\_ -> op)
        |= Parser.symbol str

operations : Parser.Parser Ast.Operation
operations =
    Parser.oneOf
        [ operation "+" Ast.Sum
        , operation "-" Ast.Subt
        , operation "*" Ast.Mult
        , operation "/" Ast.Div
        ]

singleInteger : Parser.Parser Ast.Value
singleInteger =
    Parser.succeed identity
        |. Parser.spaces
        |= integerFactor
        |. Parser.spaces
        |. Parser.end

singleOperationSign : Parser.Parser Ast.Value
singleOperationSign =
       operations |> Parser.andThen
                        (\op ->
                            Parser.succeed (Ast.SingleOperationSign_ op))
