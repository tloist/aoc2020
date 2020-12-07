import PasswordListParser.InputLine

class PasswordPolicySpecDay1 extends munit.FunSuite {
  import Part1.given_PasswordPolicy

  test("Example #1") {
    assert(InputLine(ConstrainedChar('a', 1, 3), "abcde").isValid)
  }

  test("Example #2") {
    assert(!InputLine(ConstrainedChar('b', 1, 3), "cdefg").isValid)
  }

  test("Example #3") {
    assert(InputLine(ConstrainedChar('c', 2, 9), "ccccccccc").isValid)
  }
}
