module Tests (..) where

import ElmTest exposing (..)
import Generate as G exposing (..)


all : Test
all =
  suite
    "Generate test suite"
    [ test "there and back again"
        <| assertEqual (fromList [ 1, 2, 3 ] |> toList) [ 1, 2, 3 ]
    , test "simple mapping"
        <| assertEqual (fromList [ 1, 2, 3 ] |> map ((+) 1) |> toList) [ 2, 3, 4 ]
    , test "simple filtering"
        <| assertEqual (fromList [ 1, 2, 3 ] |> filter (flip (>) 1) |> toList) [ 2, 3 ]
    , test "simple removing"
        <| assertEqual (fromList [ 1, 2, 3 ] |> remove (flip (>) 1) |> toList) [ 1 ]
    , test "stacking"
        <| assertEqual
            (fromList [ 1, 2, 3 ]
              |> map ((+) 1)
              |> filter (flip (>) 1)
              |> map toString
              |> filter ((/=) "4")
              |> toList
            )
            [ "2", "3" ]
    , test "reversing"
        <| assertEqual
            (fromList [ 1, 2, 3 ]
              |> map ((+) 1)
              |> filter (flip (>) 1)
              |> map toString
              |> reverse
              |> toList
            )
            [ "4", "3", "2" ]
    , test "foldl"
        <| assertEqual (fromList [ 1, 2, 3 ] |> foldl (::) []) [ 3, 2, 1 ]
    , test "foldr"
        <| assertEqual (fromList [ 1, 2, 3 ] |> foldr (::) []) [ 1, 2, 3 ]
    , test "empty all"
        <| assertEqual (fromList [] |> G.all (flip (>) 1)) True
    , test "false all"
        <| assertEqual (fromList [1, 2, 3] |> G.all (flip (>) 1)) False
    , test "true all"
        <| assertEqual (fromList [2, 3, 4] |> G.all (flip (>) 1)) True
    , test "empty any"
        <| assertEqual (fromList [] |> any (flip (>) 1)) False
    , test "false any"
        <| assertEqual (fromList [1] |> any (flip (>) 1)) False
    , test "true any"
        <| assertEqual (fromList [1, 2, 3] |> any (flip (>) 1)) True
    , test "empty sum"
        <| assertEqual (fromList [] |> sum) 0
    , test "sum"
        <| assertEqual (fromList [ 1, 2, 3 ] |> product) 6
    , test "empty produt"
        <| assertEqual (fromList [] |> product) 1
    , test "product"
        <| assertEqual (fromList [ 1, 2, 3 ] |> product) 6
    , test "empty length"
        <| assertEqual (fromList [] |> length) 0
    , test "length"
        <| assertEqual (fromList [1, 2, 3] |> length) 3
    ]
