let calibrate = input => {
  let arr = switch input->String.match(
    %re(
      "/(?=(\d|one|two|three|four|five|six|seven|eight|nine)).*(\d|one|two|three|four|five|six|seven|eight|nine)/"
    ),
  ) {
  | Some(matches) =>
    let matches = matches->RegExp.Result.matches
    switch (matches->Array.at(0), matches->Array.at(-1)) {
    | (Some(head), Some(tail)) => [head, tail]
    | _ => Exn.raiseError("Invalid input")
    }
  | None => Exn.raiseError("Invalid input")
  }

  arr
  ->Array.map(x =>
    switch x {
    | "one" => "1"
    | "two" => "2"
    | "three" => "3"
    | "four" => "4"
    | "five" => "5"
    | "six" => "6"
    | "seven" => "7"
    | "eight" => "8"
    | "nine" => "9"
    | x => x
    }
  )
  ->Array.joinWith("")
}

let solution = input => {
  input
  ->String.trim
  ->String.split("\n")
  ->Array.reduce(0, (acc, line) => {
    switch line->calibrate->Int.fromString(~radix=10) {
    | Some(number) => acc + number
    | None => acc
    }
  })
}

if Vitest.inSource {
  open Vitest

  describe("calibrate", _ => {
    it("should calibrate", _ => {
      expect(calibrate("1abc2"))->Expect.toBe("12")
      expect(calibrate("pqr3stu8vwx"))->Expect.toBe("38")
      expect(calibrate("a1b2c3d4e5f"))->Expect.toBe("15")
      expect(calibrate("treb7uchet"))->Expect.toBe("77")
      expect(calibrate("two1nine"))->Expect.toBe("29")
      expect(calibrate("eightwothree"))->Expect.toBe("83")
      expect(calibrate("abcone2threexyz"))->Expect.toBe("13")
      expect(calibrate("xtwone3four"))->Expect.toBe("24")
      expect(calibrate("4nineeightseven2"))->Expect.toBe("42")
      expect(calibrate("zoneight234"))->Expect.toBe("14")
      expect(calibrate("7pqrstsixteen"))->Expect.toBe("76")
    })
  })

  Vitest.testAsync("solution", async _ => {
    let input = `1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet`

    expect(input->solution)->Expect.toBe(142)

    let input = `two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen`

    expect(input->solution)->Expect.toBe(281)
  })
}
