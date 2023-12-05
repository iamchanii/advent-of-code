let parseInt = string => Int.fromString(string, ~radix=10)->Option.getExn

module Game = {
  type t = {round: int, winningNumbers: array<int>, ownedNumbers: array<int>}

  let fromString = string => {
    let round = switch string->String.match(%re("/Card\s+(\d+):/")) {
    | Some(result) => result->RegExp.Result.matches->Array.at(0)->Option.getExn->parseInt
    | _ => panic("Cannot parse round. Invalid Input: " ++ string)
    }

    let winningNumbers = switch string->String.match(%re("/:(.+)\|/")) {
    | Some(result) =>
      result
      ->RegExp.Result.matches
      ->Array.at(0)
      ->Option.getExn
      ->String.splitByRegExp(%re("/\s+/"))
      ->Array.filterMap(winningNumber => {
        switch winningNumber {
        | Some("")
        | None =>
          None
        | Some(winningNumber) => Some(winningNumber->parseInt)
        }
      })
    | _ => panic("Cannot parse winningNumbers. Invalid Input: " ++ string)
    }

    let ownedNumbers = switch string->String.match(%re("/\| (.+)$/")) {
    | Some(result) =>
      result
      ->RegExp.Result.matches
      ->Array.at(0)
      ->Option.getExn
      ->String.splitByRegExp(%re("/\s+/"))
      ->Array.filterMap(winningNumber => {
        switch winningNumber {
        | Some("")
        | None =>
          None
        | Some(winningNumber) => Some(winningNumber->parseInt)
        }
      })
    | _ => panic("Cannot parse winningNumbers. Invalid Input: " ++ string)
    }

    {round, winningNumbers, ownedNumbers}
  }

  let matchedNumbers = t => {
    t.winningNumbers->Array.filter(winningNumber => t.ownedNumbers->Array.includes(winningNumber))
  }

  let point = t => {
    t
    ->matchedNumbers
    ->Array.reduce(0, (point, _) => point === 0 ? 1 : point * 2)
  }
}

let part1 = input => {
  input
  ->String.split("\n")
  ->Array.map(Game.fromString)
  ->Array.map(Game.point)
  ->Array.reduce(0, (acc, point) => acc + point)
}

let part2 = input => {
  let cards = input->String.split("\n")->Array.map(Game.fromString)
  let countMap = Dict.fromArray(cards->Array.map(card => (card.round->Int.toString, 1)))

  for round in 1 to cards->Array.length {
    let count = countMap->Dict.get(round->Int.toString)->Option.getOr(0)

    switch cards->Array.find(x => x.round === round) {
    | None => ()
    | Some(card) => {
        let countOfmatchedNumbers = card->Game.matchedNumbers->Array.length

        for _ in 1 to count {
          for index in 1 to countOfmatchedNumbers {
            let targetRound = card.round + index
            let targetRoundCount = countMap->Dict.get(targetRound->Int.toString)->Option.getOr(0)
            countMap->Dict.set(targetRound->Int.toString, targetRoundCount + 1)
          }
        }
      }
    }
  }

  countMap->Dict.valuesToArray->Array.reduce(0, (acc, value) => acc + value)
}

if Vitest.inSource {
  open Vitest

  test("part1", _ => {
    let input = `Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
    Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
    Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
    Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
    Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
    Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11`

    expect(input->part1)->Expect.toBe(13)
  })

  test("part2", _ => {
    let input = `Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
    Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
    Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
    Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
    Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
    Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11`

    expect(input->part2)->Expect.toBe(30)
  })
}
