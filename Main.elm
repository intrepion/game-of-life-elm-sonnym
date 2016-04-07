import Signal exposing (map, Signal)
import Signal

import List exposing ((::))
import List

import Time
import Random

import Color exposing (rgb)
import Graphics.Element exposing (Element, flow, right, down, container, topLeft, spacer, color)

cellSize = 5
(columns, rows) = (35, 35)

main : Signal Element
main = map renderGrid <| map seededGrid initialSeed

initialSeed : Signal Random.Seed
initialSeed = map (\(time, _) -> Random.initialSeed <| round time) <| Time.timestamp <| Signal.constant ()

seededGrid : Random.Seed -> List (List Bool)
seededGrid seed =
  let (lst, _) = Random.generate (Random.list (columns * rows) (Random.int 0 1)) seed
  in generateGrid lst

generateGrid : List Int -> List (List Bool)
generateGrid seeds = List.map generateRow (groupInto rows seeds)

generateRow : List Int -> List Bool
generateRow seeds = List.map (\n -> n == 1) seeds

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

groupInto : Int -> List a -> List (List a)
groupInto groups initial =
  let
    len = List.length initial
    n = len // groups
  in
    List.repeat groups []
      |> List.indexedMap (\i _ ->
          List.take n (List.drop (n * i) initial))
