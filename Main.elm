import List

import Color exposing (rgb)
import Graphics.Element exposing (Element, flow, right, down, container, topLeft, spacer, color)

cellSize = 5
(columns, rows) = (35, 35)

main : Element
main = renderGrid generateGrid

generateGrid : List (List Bool)
generateGrid = List.repeat rows (List.repeat columns True)

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
