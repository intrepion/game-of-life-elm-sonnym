import Signal

import List

import Time
import Random

import Color exposing (rgb)
import Graphics.Element exposing (Element, flow, right, down, container, topLeft, spacer, color)

cellSize = 5
(columns, rows) = (35, 35)

main : Signal Element
main =
  Signal.sampleOn (Time.every Time.millisecond) (Signal.map seededGrid initialSeed)
    |> Signal.foldp (step) [[]]
    |> Signal.map renderGrid

step : List (List Bool) -> List (List Bool) -> List (List Bool)
step current past = if List.isEmpty (getPastHead past) then current else (evolve past)

getPastHead : List (List Bool) -> List Bool
getPastHead past =
  case List.head past of
    Just x -> x
    Nothing -> []

initialSeed : Signal Random.Seed
initialSeed = Signal.map (\(time, _) -> Random.initialSeed (round time)) <| Time.timestamp <| Signal.constant ()

seededGrid : Random.Seed -> List (List Bool)
seededGrid seed =
  let (lst, _) = Random.generate (Random.list (columns * rows) (Random.int 0 1)) seed
  in generateGrid lst

generateGrid : List Int -> List (List Bool)
generateGrid seeds = List.map generateRow (groupInto rows seeds)

generateRow : List Int -> List Bool
generateRow seeds = List.map (\n -> n == 1) seeds

evolve : List (List Bool) -> List (List Bool)
evolve generation =
  List.indexedMap (\y row ->
    List.indexedMap (\x _ ->
      descend generation x y) row) generation

descend : List (List Bool) -> Int -> Int -> Bool
descend grid x y =
  List.concatMap (\n -> List.map (\m -> (x + n, y + m))
                   [-1, 0, 1]) [-1, 0, 1]
    |> List.filter (\p -> (fst p) > -1 && (fst p) < columns &&
                          (snd p) > -1 && (snd p) < rows &&
                          (not ((fst p) == x && (snd p) == y)))
    |> List.filter (\p -> (itemAtBool (fst p)
                            (itemAtListBool (snd p) grid)) == True)
    |> List.length
    |> (\l -> ((itemAtBool x (itemAtListBool y grid))
                && l > 1 && l < 4) || l == 3)

renderGrid : List (List Bool) -> Element
renderGrid grid =
  grid
    |> List.map renderRow
    |> List.map (flow right)
    |> flow down
    |> container (cellSize * columns) (cellSize * rows) topLeft

renderRow : List Bool -> List Element
renderRow row = List.map renderCell row

renderCell : Bool -> Element
renderCell on =
  spacer cellSize cellSize
    |> color (if on then (rgb 0 0 0) else (rgb 255 255 255))

itemAtBool : Int -> List Bool -> Bool
itemAtBool i lst = case List.head (List.drop i lst) of
  Just x -> x
  Nothing -> False

itemAtListBool : Int -> List (List Bool) -> List Bool
itemAtListBool i lst = case List.head (List.drop i lst) of
  Just x -> x
  Nothing -> []

groupInto : Int -> List a -> List (List a)
groupInto groups initial =
  let
    len = List.length initial
    n = len // groups
  in
    List.repeat groups []
      |> List.indexedMap (\i _ ->
          List.take n (List.drop (n * i) initial))
