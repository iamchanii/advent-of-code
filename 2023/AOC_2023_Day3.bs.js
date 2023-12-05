// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Vitest from "rescript-vitest/src/Vitest.bs.js";
import * as Vitest$1 from "vitest";
import * as Core__Int from "@rescript/core/src/Core__Int.bs.js";
import * as Core__Option from "@rescript/core/src/Core__Option.bs.js";

function isNumeric(input) {
  return /\d/.test(input);
}

function isPartNumber(input) {
  return /[^\d\.]/.test(input);
}

function get(map, x, y) {
  var list = map.at(y);
  if (list !== undefined) {
    return list.at(x);
  }
  
}

function solution(input) {
  var map = input.split("\n");
  var maxY = map.length;
  var map$1 = map.map(function (x) {
        return x.split("");
      });
  var maxX = map$1.length;
  var validMap = map$1.map(function (lines, y) {
        return lines.map(function ($$char, x) {
                    if (!isNumeric($$char)) {
                      return false;
                    }
                    var list = [
                      get(map$1, x - 1 | 0, y - 1 | 0),
                      get(map$1, x + 0 | 0, y - 1 | 0),
                      get(map$1, x + 1 | 0, y - 1 | 0),
                      get(map$1, x - 1 | 0, y + 0 | 0),
                      get(map$1, x + 1 | 0, y + 0 | 0),
                      get(map$1, x - 1 | 0, y + 1 | 0),
                      get(map$1, x + 0 | 0, y + 1 | 0),
                      get(map$1, x + 1 | 0, y + 1 | 0)
                    ];
                    return list.some(function (x) {
                                if (x !== undefined) {
                                  return isPartNumber(x);
                                } else {
                                  return false;
                                }
                              });
                  });
      });
  var result = 0;
  for(var y = 0; y < maxY; ++y){
    var buffer = "";
    var isPartNumber$1 = false;
    for(var x = 0; x < maxX; ++x){
      var cursor = Core__Option.getOr(get(map$1, x, y), ".");
      if (isNumeric(cursor)) {
        isPartNumber$1 = isPartNumber$1 || Core__Option.getOr(get(validMap, x, y), false);
        buffer = buffer + cursor;
      } else {
        if (isPartNumber$1) {
          result = result + Core__Option.getOr(Core__Int.fromString(10, buffer), 0) | 0;
        }
        isPartNumber$1 = false;
        buffer = "";
      }
    }
    var match = buffer;
    var match$1 = isPartNumber$1;
    if (match$1 && match !== "") {
      result = result + Core__Option.getOr(Core__Int.fromString(10, match), 0) | 0;
    }
    
  }
  return result;
}

if (import.meta.vitest) {
  Vitest.test("solution", undefined, (function (param) {
          Vitest$1.expect(solution("467..114..\n...*......\n..35..633.\n......#...\n617*......\n.....+.58.\n..592.....\n......755.\n...$.*....\n.664.598..")).toBe(4361);
        }));
}

export {
  isNumeric ,
  isPartNumber ,
  get ,
  solution ,
}
/*  Not a pure module */
