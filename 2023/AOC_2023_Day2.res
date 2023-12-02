module Game = {
  type set = {red: int, green: int, blue: int}
  type t = {round: int, sets: array<set>}

  let parse = line => {
    let round = switch RegExp.exec(%re("/Game (?<round>\d+):/"), line) {
    | None => Exn.raiseError("Invalid input")
    | Some(result) =>
      switch result->Array.at(1) {
      | Some(round) => Int.fromString(round)->Option.getExn
      | _ => panic("Unreachable")
      }
    }

    let sets =
      line
      ->String.split(": ")
      ->Array.at(1)
      ->Option.getExn
      ->String.split(";")
      ->Array.map(set =>
        String.split(set, ", ")->Array.reduce({red: 0, green: 0, blue: 0}, (acc, set) => {
          switch set->String.trim->String.split(" ") {
          | [count, "red"] => {
              ...acc,
              red: acc.red + Int.fromString(count)->Option.getExn,
            }
          | [count, "blue"] => {
              ...acc,
              blue: acc.blue + Int.fromString(count)->Option.getExn,
            }
          | [count, "green"] => {
              ...acc,
              green: acc.green + Int.fromString(count)->Option.getExn,
            }
          | [""]
          | _ => acc
          }
        })
      )

    {round, sets}
  }
}

let part1 = (input, ~red, ~blue, ~green) => {
  input
  ->String.split("\n")
  ->Array.map(Game.parse)
  ->Array.filterMap(game => {
    let {round, sets} = game
    switch sets->Array.every(set => set.red <= red && set.blue <= blue && set.green <= green) {
    | true => Some(round)
    | false => None
    }
  })
  ->Array.reduce(0, (acc, round) => acc + round)
}

let part2 = input => {
  input
  ->String.split("\n")
  ->Array.map(Game.parse)
  ->Array.map(game => {
    let {red, green, blue} = game.sets->Array.reduce(
      {
        Game.red: 0,
        green: 0,
        blue: 0,
      },
      (acc, set) => {
        red: Math.max(acc.red->Int.toFloat, set.red->Int.toFloat)->Float.toInt,
        green: Math.max(acc.green->Int.toFloat, set.green->Int.toFloat)->Float.toInt,
        blue: Math.max(acc.blue->Int.toFloat, set.blue->Int.toFloat)->Float.toInt,
      },
    )

    red * green * blue
  })
  ->Array.reduce(0, (acc, round) => acc + round)
}

if Vitest.inSource {
  open Vitest

  test("solution", _ => {
    let input = `Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green;`

    expect(input->part1(~red=12, ~green=13, ~blue=14))->Expect.toBe(8)

    expect(input->part2)->Expect.toBe(2286)
  })
}
