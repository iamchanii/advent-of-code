// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Js_exn from "rescript/lib/es6/js_exn.js";
import * as Vitest from "rescript-vitest/src/Vitest.bs.js";
import * as Vitest$1 from "vitest";
import * as Core__Int from "@rescript/core/src/Core__Int.bs.js";
import * as Core__Array from "@rescript/core/src/Core__Array.bs.js";
import * as Core__Option from "@rescript/core/src/Core__Option.bs.js";
import * as RescriptCore from "@rescript/core/src/RescriptCore.bs.js";

function parse(line) {
  var result = /Game (?<round>\d+):/.exec(line);
  var round;
  if (result == null) {
    round = Js_exn.raiseError("Invalid input");
  } else {
    var round$1 = result.at(1);
    round = round$1 !== undefined ? Core__Option.getExn(Core__Int.fromString(undefined, round$1)) : RescriptCore.panic("Unreachable");
  }
  var sets = Core__Option.getExn(line.split(": ").at(1)).split(";").map(function (set) {
        return Core__Array.reduce(set.split(", "), {
                    red: 0,
                    green: 0,
                    blue: 0
                  }, (function (acc, set) {
                      var match = set.trim().split(" ");
                      var len = match.length;
                      if (len >= 3) {
                        return acc;
                      }
                      switch (len) {
                        case 0 :
                            return acc;
                        case 1 :
                            return acc;
                        case 2 :
                            var count = match[0];
                            var match$1 = match[1];
                            switch (match$1) {
                              case "blue" :
                                  return {
                                          red: acc.red,
                                          green: acc.green,
                                          blue: acc.blue + Core__Option.getExn(Core__Int.fromString(undefined, count)) | 0
                                        };
                              case "green" :
                                  return {
                                          red: acc.red,
                                          green: acc.green + Core__Option.getExn(Core__Int.fromString(undefined, count)) | 0,
                                          blue: acc.blue
                                        };
                              case "red" :
                                  return {
                                          red: acc.red + Core__Option.getExn(Core__Int.fromString(undefined, count)) | 0,
                                          green: acc.green,
                                          blue: acc.blue
                                        };
                              default:
                                return acc;
                            }
                        
                      }
                    }));
      });
  return {
          round: round,
          sets: sets
        };
}

var Game = {
  parse: parse
};

function part1(input, red, blue, green) {
  return Core__Array.reduce(Core__Array.filterMap(input.split("\n").map(parse), (function (game) {
                    if (game.sets.every(function (set) {
                            return set.red <= red && set.blue <= blue ? set.green <= green : false;
                          })) {
                      return game.round;
                    }
                    
                  })), 0, (function (acc, round) {
                return acc + round | 0;
              }));
}

function part2(input) {
  return Core__Array.reduce(input.split("\n").map(parse).map(function (game) {
                  var match = Core__Array.reduce(game.sets, {
                        red: 0,
                        green: 0,
                        blue: 0
                      }, (function (acc, set) {
                          return {
                                  red: Math.max(acc.red, set.red) | 0,
                                  green: Math.max(acc.green, set.green) | 0,
                                  blue: Math.max(acc.blue, set.blue) | 0
                                };
                        }));
                  return Math.imul(Math.imul(match.red, match.green), match.blue);
                }), 0, (function (acc, round) {
                return acc + round | 0;
              }));
}

if (import.meta.vitest) {
  Vitest.test("solution", undefined, (function (param) {
          var input = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\nGame 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\nGame 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\nGame 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\nGame 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green;";
          Vitest$1.expect(part1(input, 12, 14, 13)).toBe(8);
          Vitest$1.expect(part2(input)).toBe(2286);
        }));
}

export {
  Game ,
  part1 ,
  part2 ,
}
/*  Not a pure module */