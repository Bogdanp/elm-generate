module Generate ( Generator
                , singleton, fromList, toList, next
                , map, filter, remove, reverse, take, drop
                ) where

{-| Generate is a library for lazy list transformation.

# Types
@docs Generator

## Converting to and from Generators
@docs singleton, fromList, toList

## Extracting values
@docs next

## Transforming Generators
@docs map, filter, remove, reverse, take, drop

-}


{-| Generators are lists whose map and filter operations are applied
lazily as items are requested.  This means that operations like:

    fromList [1,2,3]
      |> map ((+) 1)
      |> filter (\x -> x % 2 == 0)
      |> map toString
      |> toList

Only iterate through the list once.

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
singleton a = Generator { items = [a], transform = Keep }


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
toList gen =
  let
    gen' = unwrap gen

    toList' gen acc =
      case next gen of
        (Nothing, _) ->
          List.reverse acc

        (Just x, gen) ->
          toList' gen (x :: acc)
  in
    toList' gen []


{-| Get the next element in the generator after transforming it.

    > fromList [1, 2, 3] |> filter ((/=) 1) |> next
    (Just 2,Generator { items = [3], transform = <function> })
      : ( Maybe.Maybe number, Generate.Generator number number )

    > fromList [1, 2, 3] |> filter ((==) 5) |> next
    (Nothing,Generator { items = [], transform = <function> })
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

-}
take : Int -> Generator a b -> Generator a b
take n gen =
  let gen' = unwrap gen in
  Generator { gen' | items = List.take n gen'.items }


{-| Drop the first `n` elements from a Generator.

    > fromList [1, 2, 3] |> drop 1 |> toList
    [2, 3] : List number

-}
drop : Int -> Generator a b -> Generator a b
drop n gen =
  let gen' = unwrap gen in
  Generator { gen' | items = List.drop n gen'.items }
