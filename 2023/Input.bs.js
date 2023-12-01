// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Process from "process";
import * as Caml_option from "rescript/lib/es6/caml_option.js";
import * as Core__Option from "@rescript/core/src/Core__Option.bs.js";

async function get(year, day) {
  var session = Core__Option.getExn(Process.env["AOC_SESSION"]);
  var url = "https://adventofcode.com/" + year.toString() + "/day/" + day.toString() + "/input";
  var response = await fetch(url, {
        method: "GET",
        headers: Caml_option.some(new Headers({
                  Cookie: "session=" + session
                }))
      });
  return await response.text();
}

export {
  get ,
}
/* process Not a pure module */