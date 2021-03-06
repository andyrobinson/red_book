package bbc

case class Gen[+A](sample: State[RNG,A]) {
  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    Gen(sample.flatMap(f(_).sample))
  }

  def listOfGenSize(size: Gen[Int]): Gen[List[A]] = {
    size.flatMap(n => Gen.listOfN(n,this))
  }
}

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

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean.flatMap{
    x => if(x) g1 else g2
  }

  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A], Double)): Gen[A] = {
      val threshold = g1._2 / (g1._2 + g2._2)
      Gen(State(RNG.double).flatMap(n => {if (n <= threshold) g1._1.sample else g2._1.sample}))
  }

}