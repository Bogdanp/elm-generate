# elm-generate [![Build Status](https://travis-ci.org/Bogdanp/elm-generate.svg?branch=master)](https://travis-ci.org/Bogdanp/elm-generate)

``` shell
elm package install Bogdanp/elm-generate
```

A lazy list manipulation library for [Elm][elm].  See the
documentation of the `Generate` module for more information.

```elm
import Generate as G

G.fromList [1, 2, 3]
  |> G.map ((+) 1)
  |> G.filter (\x -> x % 2 == 0)
  |> G.map toString
  |> G.toList
```


[elm]: http://elm-lang.org
