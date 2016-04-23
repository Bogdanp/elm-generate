module Tests where

import ElmTest exposing (..)

import Generate exposing (..)

all : Test
all =
  suite
    "Generate test suite"
    [ test "there and back again"
        <| assertEqual (fromList [1, 2, 3] |> toList) [1, 2, 3]

    , test "simple mapping"
        <| assertEqual (fromList [1, 2, 3] |> map ((+) 1) |> toList) [2, 3, 4]

    , test "simple filtering"
        <| assertEqual (fromList [1, 2, 3] |> filter (flip (>) 1) |> toList) [2, 3]

    , test "stacking"
        <| assertEqual (fromList [1, 2, 3] |> map ((+) 1) |> filter (flip (>) 1) |> map toString |> filter ((/=) "4") |> toList) ["2", "3"]

    , test "reversing"
        <| assertEqual (fromList [1, 2, 3] |> map ((+) 1) |> filter (flip (>) 1) |> map toString |> reverse |> toList) ["4", "3", "2"]
    ]
