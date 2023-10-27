object Main:
  def main(args: Array[String]): Unit =
    val x = Nat(42)
    val y = Nat(24)
    val xy = Plus(x, y).value
    val yx = Plus(y, x).value
    assert(xy.flip == yx)
    println(prettyPrint(xy))
    println(prettyPrint(yx))

  private def prettyPrint(p: Plus[?, ?, ?]): String =
    s"${p.lArg.toInt} + ${p.rArg.toInt} = ${p.result.toInt}"
