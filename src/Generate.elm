module Generate ( Generator
                , singleton, fromList, toList, next
                , map, filter, remove, reverse, take, drop
                , foldl, foldr, any, all, sum, product, length
                ) where

{-| Generate is a library for lazy list manipulation.

# Types
@docs Generator

## Converting to and from Generators
@docs singleton, fromList, toList

## Extracting values
@docs next

## Transforming Generators
@docs map, filter, remove, reverse, take, drop

## Producing values
@docs foldl, foldr, any, all, sum, product, length
-}


{-| Generators are lists whose transformations are applied lazily as
items are requested.  This makes is so that expressions like:

    import Generate as G

    G.fromList [1,2,3]
      |> G.map ((+) 1)
      |> G.filter (\x -> x % 2 == 0)
      |> G.map toString
      |> G.toList

Only end up iterating through the list once while still providing the
same results as their List module counterparts.

-}
type Generator a b
  = Generator { items : List a
              , transform : a -> Mutator b
              }


type Mutator a
  = Keep a
  | Skip


unwrap gen =
  case gen of
    Generator gen ->
      gen


mapMutator : (a -> b) -> Mutator a -> Mutator b
mapMutator f m =
  case m of
    Skip ->
      Skip

    Keep x ->
      Keep (f x)


filterMutator : (a -> Bool) -> Mutator a -> Mutator a
filterMutator f m =
  case m of
    Skip ->
      Skip

    Keep x ->
      if f x then
        Keep x
      else
        Skip


{-| Construct a Generator from a single value.

    > singleton 1
    Generator { items = [1], transform = <function> }
      : Generate.Generator number number
-}
singleton : a -> Generator a a
singleton a =
  Generator { items = [a], transform = Keep }


{-| Construct a Generator from a List.

    > fromList [1, 2, 3]
    Generator { items = [1,2,3], transform = <function> }
      : Generate.Generator number number

-}
fromList : List a -> Generator a a
fromList xs =
  Generator { items = xs, transform = Keep }


{-| Construct a List from a Generator.

    > fromList [1, 2, 3] |> toList
    [1, 2, 3] : List number

-}
toList : Generator a b -> List b
toList =
  List.reverse << foldl (::) []


{-| Get the next element in the generator after transforming it.

    > fromList [1, 2, 3] |> filter ((/=) 1) |> next
    (Just 2, Generator { items = [3], transform = <function> })
      : ( Maybe.Maybe number, Generate.Generator number number )

    > fromList [1, 2, 3] |> filter ((==) 5) |> next
    (Nothing, Generator { items = [], transform = <function> })
      : ( Maybe.Maybe number, Generate.Generator number number )

-}
next : Generator a b -> (Maybe b, Generator a b)
next gen =
  let gen' = unwrap gen in
  case gen'.items of
    [] ->
      (Nothing, gen)

    (x::xs) ->
      case gen'.transform x of
        Skip ->
          next <| Generator { gen' | items = xs }

        Keep a ->
          (Just a, Generator { items = xs, transform = gen'.transform })


{-| Transform the elements of a Generator.


    > fromList [1, 2, 3] |> map ((+) 1) |> toList
    [2, 3, 4] : List number

-}
map : (b -> c) -> Generator a b -> Generator a c
map f gen =
  let gen' = unwrap gen in
  Generator { gen' | transform = gen'.transform >> mapMutator f }


{-| Filter the elements of a Generator.

    > fromList [1, 2, 3] |> filter (flip (>) 1) |> toList
    [2, 3] : List number

-}
filter : (b -> Bool) -> Generator a b -> Generator a b
filter f gen =
  let gen' = unwrap gen in
  Generator { gen' | transform = gen'.transform >> filterMutator f }


{-| Remove elements that match the given predicate from the Generator.

    > fromList [1, 2, 3] |> remove (\x -> x % 2 == 0) |> toList
    [1, 3] : List number

-}
remove : (b -> Bool) -> Generator a b -> Generator a b
remove f = filter (not << f)


{-| Reverse a Generator's elements. -}
reverse : Generator a b -> Generator a b
reverse gen =
  let gen' = unwrap gen in
  Generator { gen' | items = List.reverse gen'.items }


{-| Take the first `n` elements from a Generator.

    > fromList [1, 2, 3] |> take 1 |> toList
    [1] : List number

*Note*: This function is applied to the *original* list:

    > fromList [1, 2, 3] |> filter ((/=) 1) |> take 1 |> toList
    [] : List number

-}
take : Int -> Generator a b -> Generator a b
take n gen =
  let gen' = unwrap gen in
  Generator { gen' | items = List.take n gen'.items }


{-| Drop the first `n` elements from a Generator.

    > fromList [1, 2, 3] |> drop 1 |> toList
    [2, 3] : List number

*Note*: This function is applied to the *original* list:

    > fromList [1, 2, 3] |> filter ((/=) 1) |> drop 1 |> toList
    [2, 3] : List number

-}
drop : Int -> Generator a b -> Generator a b
drop n gen =
  let gen' = unwrap gen in
  Generator { gen' | items = List.drop n gen'.items }


{-| Fold a Generator from the left.

    > fromList [1, 2, 3] |> foldl (::) []
    [3, 2, 1] : List number

-}
foldl : (b -> c -> c) -> c -> Generator a b -> c
foldl f e gen =
  let
    go gen acc =
      case next gen of
        (Nothing, _) ->
          acc

        (Just x, gen) ->
          go gen (f x acc)
  in
    go gen e


{-| Fold a Generator from the right.

    > fromList [1, 2, 3] |> foldr (::) []
    [1, 2, 3] : List number

-}
foldr : (b -> c -> c) -> c -> Generator a b -> c
foldr f e gen =
  let
    go gen =
      case next gen of
        (Nothing, _) ->
          e

        (Just x, gen) ->
          f x (go gen)
  in
    go gen


{-| Determine if all of the Generator's elements match the predicate.

    > fromList [2, 4, 6] |> all (\x -> x % 2 == 0)
    True : Bool

    > fromList [1, 2, 3] |> all (\x -> x % 2 == 0)
    False : Bool

-}
all : (a -> Bool) -> Generator x a -> Bool
all f gen =
  case next gen of
    (Nothing, _) ->
      True

    (Just x, gen) ->
      if f x then -- The && version of this does not TCO
        all f gen

      else
        False


{-| Determine if any of the Generator's elements match the predicate.

    > fromList [1, 2, 3] |> any (\x -> x % 2 == 0)
    True : Bool

    > fromList [1, 3, 5] |> any (\x -> x % 2 == 0)
    False : Bool

-}
any : (a -> Bool) -> Generator x a -> Bool
any f gen =
  case next gen of
    (Nothing, _) ->
      False

    (Just x, gen) ->
      if f x then -- The || version of this does not TCO
        True

      else
        any f gen


{-| Compute the product of the Generator's elements.

    > fromList [1, 2, 3] |> sum
    6 : number

-}
sum : Generator a number -> number
sum gen =
  foldl (+) 0 gen


{-| Compute the product of the Generator's elements.

    > fromList [1, 2, 3] |> product
    6 : number

-}
product : Generator a number -> number
product gen =
  foldl (*) 1 gen


{-| Compute the length of a Generator.

    > fromList [1, 2, 3] |> length
    3 : Int

-}
length : Generator a b -> Int
length gen =
  foldl ((+) << (always 1)) 0 gen
