// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Js_exn from "rescript/lib/es6/js_exn.js";
import * as Vitest from "rescript-vitest/src/Vitest.bs.js";
import * as Vitest$1 from "vitest";
import * as Core__Int from "@rescript/core/src/Core__Int.bs.js";
import * as Core__Array from "@rescript/core/src/Core__Array.bs.js";

function calibrate(input) {
  var matches = input.match(/(?=(\d|one|two|three|four|five|six|seven|eight|nine)).*(\d|one|two|three|four|five|six|seven|eight|nine)/);
  var arr;
  if (matches == null) {
    arr = Js_exn.raiseError("Invalid input");
  } else {
    var matches$1 = matches.slice(1);
    var match = matches$1.at(0);
    var match$1 = matches$1.at(-1);
    arr = match !== undefined && match$1 !== undefined ? [
        match,
        match$1
      ] : Js_exn.raiseError("Invalid input");
  }
  return arr.map(function (x) {
                switch (x) {
                  case "eight" :
                      return "8";
                  case "five" :
                      return "5";
                  case "four" :
                      return "4";
                  case "nine" :
                      return "9";
                  case "one" :
                      return "1";
                  case "seven" :
                      return "7";
                  case "six" :
                      return "6";
                  case "three" :
                      return "3";
                  case "two" :
                      return "2";
                  default:
                    return x;
                }
              }).join("");
}

function solution(input) {
  return Core__Array.reduce(input.trim().split("\n"), 0, (function (acc, line) {
                var number = Core__Int.fromString(10, calibrate(line));
                if (number !== undefined) {
                  return acc + number | 0;
                } else {
                  return acc;
                }
              }));
}

if (import.meta.vitest) {
  Vitest.describe("calibrate", undefined, (function () {
          Vitest.it("should calibrate", undefined, (function (param) {
                  Vitest$1.expect(calibrate("1abc2")).toBe("12");
                  Vitest$1.expect(calibrate("pqr3stu8vwx")).toBe("38");
                  Vitest$1.expect(calibrate("a1b2c3d4e5f")).toBe("15");
                  Vitest$1.expect(calibrate("treb7uchet")).toBe("77");
                  Vitest$1.expect(calibrate("two1nine")).toBe("29");
                  Vitest$1.expect(calibrate("eightwothree")).toBe("83");
                  Vitest$1.expect(calibrate("abcone2threexyz")).toBe("13");
                  Vitest$1.expect(calibrate("xtwone3four")).toBe("24");
                  Vitest$1.expect(calibrate("4nineeightseven2")).toBe("42");
                  Vitest$1.expect(calibrate("zoneight234")).toBe("14");
                  Vitest$1.expect(calibrate("7pqrstsixteen")).toBe("76");
                }));
        }));
  Vitest.testAsync("solution", undefined, (async function (param) {
          Vitest$1.expect(solution("1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet")).toBe(142);
          return Vitest$1.expect(solution("two1nine\neightwothree\nabcone2threexyz\nxtwone3four\n4nineeightseven2\nzoneight234\n7pqrstsixteen")).toBe(281);
        }));
}

export {
  calibrate ,
  solution ,
}
/*  Not a pure module */