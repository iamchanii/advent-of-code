// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Vitest from "rescript-vitest/src/Vitest.bs.js";
import * as Vitest$1 from "vitest";
import * as Core__Int from "@rescript/core/src/Core__Int.bs.js";
import * as Core__Array from "@rescript/core/src/Core__Array.bs.js";
import * as Core__Option from "@rescript/core/src/Core__Option.bs.js";
import * as RescriptCore from "@rescript/core/src/RescriptCore.bs.js";

function $$parseInt(string) {
  return Core__Option.getExn(Core__Int.fromString(10, string));
}

function fromString(string) {
  var result = string.match(/Card\s+(\d+):/);
  var round = (result == null) ? RescriptCore.panic("Cannot parse round. Invalid Input: " + string) : $$parseInt(Core__Option.getExn(result.slice(1).at(0)));
  var result$1 = string.match(/:(.+)\|/);
  var winningNumbers = (result$1 == null) ? RescriptCore.panic("Cannot parse winningNumbers. Invalid Input: " + string) : Core__Array.filterMap(Core__Option.getExn(result$1.slice(1).at(0)).split(/\s+/), (function (winningNumber) {
            if (winningNumber !== undefined && winningNumber !== "") {
              return $$parseInt(winningNumber);
            }
            
          }));
  var result$2 = string.match(/\| (.+)$/);
  var ownedNumbers = (result$2 == null) ? RescriptCore.panic("Cannot parse winningNumbers. Invalid Input: " + string) : Core__Array.filterMap(Core__Option.getExn(result$2.slice(1).at(0)).split(/\s+/), (function (winningNumber) {
            if (winningNumber !== undefined && winningNumber !== "") {
              return $$parseInt(winningNumber);
            }
            
          }));
  return {
          round: round,
          winningNumbers: winningNumbers,
          ownedNumbers: ownedNumbers
        };
}

function matchedNumbers(t) {
  return t.winningNumbers.filter(function (winningNumber) {
              return t.ownedNumbers.includes(winningNumber);
            });
}

function point(t) {
  return Core__Array.reduce(matchedNumbers(t), 0, (function (point, param) {
                if (point === 0) {
                  return 1;
                } else {
                  return (point << 1);
                }
              }));
}

var Game = {
  fromString: fromString,
  matchedNumbers: matchedNumbers,
  point: point
};

function part1(input) {
  return Core__Array.reduce(input.split("\n").map(fromString).map(point), 0, (function (acc, point) {
                return acc + point | 0;
              }));
}

function part2(input) {
  var cards = input.split("\n").map(fromString);
  var countMap = Object.fromEntries(cards.map(function (card) {
            return [
                    card.round.toString(),
                    1
                  ];
          }));
  for(var round = 1 ,round_finish = cards.length; round <= round_finish; ++round){
    var count = Core__Option.getOr(countMap[round.toString()], 0);
    var card = cards.find((function(round){
        return function (x) {
          return x.round === round;
        }
        }(round)));
    if (card !== undefined) {
      var countOfmatchedNumbers = matchedNumbers(card).length;
      for(var _for = 1; _for <= count; ++_for){
        for(var index = 1; index <= countOfmatchedNumbers; ++index){
          var targetRound = card.round + index | 0;
          var targetRoundCount = Core__Option.getOr(countMap[targetRound.toString()], 0);
          countMap[targetRound.toString()] = targetRoundCount + 1 | 0;
        }
      }
    }
    
  }
  return Core__Array.reduce(Object.values(countMap), 0, (function (acc, value) {
                return acc + value | 0;
              }));
}

if (import.meta.vitest) {
  Vitest.test("part1", undefined, (function (param) {
          Vitest$1.expect(part1("Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53\n    Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19\n    Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1\n    Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83\n    Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36\n    Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")).toBe(13);
        }));
  Vitest.test("part2", undefined, (function (param) {
          Vitest$1.expect(part2("Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53\n    Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19\n    Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1\n    Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83\n    Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36\n    Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")).toBe(30);
        }));
}

export {
  $$parseInt ,
  Game ,
  part1 ,
  part2 ,
}
/*  Not a pure module */
