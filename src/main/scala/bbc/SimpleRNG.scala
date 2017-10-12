package bbc

case class SimpleRNG(see: Long) extends RNG {
  override def nextInt: (Int, RNG) =  {
    val newSeed = (see * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>>16).toInt
    (n, nextRNG)
  }
}
