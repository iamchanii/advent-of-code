let get = async (~year, ~day) => {
  let session = {
    open NodeJs
    Process.process->Process.env->Dict.get("AOC_SESSION")->Option.getExn
  }

  open Fetch

  let url = `https://adventofcode.com/${year->Int.toString}/day/${day->Int.toString}/input`

  let response = await fetch(
    url,
    {
      method: #GET,
      headers: Headers.fromObject({
        "Cookie": `session=${session}`,
      }),
    },
  )

  await response->Response.text
}
