module Max.Parser.Ast exposing (Operation(..), Value(..), fromString, toString)


type Value
    = Int_ Int
    | SingleOperationSign_ Operation
    | BinaryOperation_ Operation Value Value
    | Null_


type Operation
    = Sum
    | Subt
    | Mult
    | Div


fromString : String -> Value
fromString str =
    case String.toInt str of
        Just n ->
            Int_ n

        Nothing ->
            case str of
                "+" ->
                    SingleOperationSign_ Sum

                "-" ->
                    SingleOperationSign_ Subt

                "*" ->
                    SingleOperationSign_ Mult

                "/" ->
                    SingleOperationSign_ Div

                _ ->
                    Null_


toString : Value -> String
toString v =
    case v of
        Int_ n ->
            String.fromInt n
        
        BinaryOperation_  op value1 value2->
           (toString value1) ++ " " ++ ((SingleOperationSign_ >> toString) op) ++ " " ++ (toString value2)

        SingleOperationSign_ op ->
            case op of
                Sum ->
                    "+"

                Subt ->
                    "-"

                Mult ->
                    "*"

                Div ->
                    "/"

        Null_ ->
            ""
