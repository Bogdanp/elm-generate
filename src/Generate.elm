module Generate ( Generator
                , fromList, toList, next
                , map, filter
                ) where

{-| Generate is a library for lazy list transformation.

# Types
@docs Generator

## Converting to and from lists
@docs fromList, toList

## Extracting values
@docs next

## Transforming values
@docs map, filter

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
    [1,2,3] : List number

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


{-| Transform the elements of a Generator.


    > fromList [1, 2, 3] |> map ((+) 1) |> toList
    [2,3,4] : List number

-}
map : (a -> b) -> Generator x a -> Generator x b
map f gen =
  let gen' = unwrap gen in
  Generator <| { gen' | transform = gen'.transform >> mapMutator f }


{-| Filter the elements of a Generator.

    > fromList [1, 2, 3] |> filter (flip (>) 1) |> toList
    [2,3] : List number

-}
filter : (a -> Bool) -> Generator x a -> Generator x a
filter f gen =
  let gen' = unwrap gen in
  Generator <| { gen' | transform = gen'.transform >> filterMutator f }


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
          next <| Generator <| { gen' | items = xs }

        Keep a ->
          (Just a, Generator { items = xs, transform = gen'.transform })
