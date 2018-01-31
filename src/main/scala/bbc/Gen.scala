package bbc

case class Gen[+A](sample: State[RNG,A])

object Prop {
  type FailedCase = String
  type Gen[A] = State[RNG,A]
}

object Gen {
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = Gen(State(rng => {
    val (n, rng2) = rng.nextInt
    ((n % 2 == 0), rng2)
  }))

  def choose(start: Int, stopExclusive: Int): Gen[Int] = Gen({
    val state = State(RNG.nonNegativeInt)
    state.map(n => start + n % (stopExclusive - start))
  })

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen({
    val stateList = List.fill(n)(g.sample)
    State.sequence(stateList)
  })

  def string(length: Int): Gen[String] = Gen({
    // ascii including numbers and letters, range 48 to 122
    val asciiCodeGen = listOfN(length, choose(48,122))
    asciiCodeGen.sample.map(codes => codes.foldLeft("")((acc,x) => acc + x.toChar))
  })


}