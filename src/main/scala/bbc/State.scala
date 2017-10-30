package bbc

case class State[S, +A](run: S => (A, S)) {

  def map[B](f: A => B): State[S,B] =
    flatMap(x => State.unit(f(x)))

  def map2[B,C](sb: State[S,B])(f: (A, B) => C): State[S,C] =
    flatMap(a => (sb.map(b => f(a,b))))

  def flatMap[B](f: (A => State[S,B])) : State[S,B] =
    State(s => {
      val (a, s2) = run(s)
      f(a).run(s2)
    })

}

object State {

  def unit[S, A](a: A): State[S, A] = State(s => (a,s))

  def sequence[S,A](states: List[State[S,A]]): State[S,List[A]] =
    states.foldRight(unit(List.empty[A]):State[S,List[A]])((sa, acc) => sa.map2(acc)(_ :: _))

}