let isNumeric = input => RegExp.test(%re("/\d/"), input)

let isPartNumber = input => RegExp.test(%re("/[^\d\.]/"), input)

let get = (map, x, y) =>
  switch map->Array.at(y) {
  | None => None
  | Some(list) => list->Array.at(x)
  }

let solution = input => {
  let map = input->String.split("\n")
  let maxY = map->Array.length
  let map = map->Array.map(x => x->String.split(""))
  let maxX = map->Array.length
  let validMap = map->Array.mapWithIndex((lines, y) =>
    lines->Array.mapWithIndex((char, x) => {
      switch char->isNumeric {
      | false => false
      | true => {
          let list = [
            get(map, x + -1, y + -1),
            get(map, x + 0, y + -1),
            get(map, x + 1, y + -1),
            get(map, x + -1, y + 0),
            get(map, x + 1, y + 0),
            get(map, x + -1, y + 1),
            get(map, x + 0, y + 1),
            get(map, x + 1, y + 1),
          ]

          list->Array.some(
            x =>
              switch x {
              | None => false
              | Some(x) => x->isPartNumber
              },
          )
        }
      }
    })
  )

  let result = ref(0)

  for y in 0 to maxY - 1 {
    let buffer = ref("")
    let isPartNumber = ref(false)

    for x in 0 to maxX - 1 {
      let cursor = get(map, x, y)->Option.getOr(".")

      switch cursor->isNumeric {
      | true => {
          isPartNumber.contents = isPartNumber.contents || get(validMap, x, y)->Option.getOr(false)
          buffer.contents = buffer.contents ++ cursor
        }
      | false =>
        if isPartNumber.contents {
          result.contents =
            result.contents + buffer.contents->Int.fromString(~radix=10)->Option.getOr(0)
        }

        isPartNumber.contents = false
        buffer.contents = ""
      }
    }

    switch (buffer.contents, isPartNumber.contents) {
    | (number, true) if number != "" =>
      result.contents = result.contents + number->Int.fromString(~radix=10)->Option.getOr(0)
    | _ => ()
    }
  }

  result.contents
}

if Vitest.inSource {
  open Vitest

  test("solution", _ => {
    let input = `467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..`

    expect(input->solution)->Expect.toBe(4361)
  })
}
