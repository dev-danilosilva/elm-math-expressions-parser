module Helpers exposing (..)

import Expect


identityTest : (a -> b) -> (b -> a) -> a -> Expect.Expectation
identityTest f g x =
    g (f x)
        |> Expect.equal x
