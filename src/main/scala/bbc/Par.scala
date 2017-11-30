package bbc

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

// 7.2 Possible implementation
// Nothing like the level of sophistication required :-(
//object Possible {
//  def unit[A](a: A): Par[A] = new Par(a)
//  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = ???
//    // the usual
//
//  def fork[A](a: => Par[A]): Par[A] = a.flatMap(x => new Par(x,forkable))
//  or it could be in some kind of additional structure, e.g. Forkable(a)
//
//  def run[A](a: Par[A])(implicit ec: ExecutionContext): A = ??? // something with an execution context
//
//}

object Par {

  type Par[A] = ExecutorService => Future[A]

  private case class UnitFuture[A](a: A) extends Future[A] {
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false
    override def isCancelled: Boolean = false
    override def isDone: Boolean = true
    override def get(timeout: Long, unit: TimeUnit): A = a
    override def get(): A = a
  }

  // how on earth do we test this ????
  private case class FutureMap2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C)(es: ExecutorService)
    extends Future[C] {
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false
    override def isCancelled: Boolean = false
    override def isDone: Boolean = true
    override def get(): C = f(a(es).get,b(es).get)

    override def get(timeout: Long, unit: TimeUnit): C = {
      val startTime = System.nanoTime()
      val x = a(es).get(timeout, unit)
      val elapsed = System.nanoTime() - startTime
      unit.convert(elapsed,TimeUnit.NANOSECONDS)
      val y = b(es).get(timeout-unit.convert(elapsed,TimeUnit.NANOSECONDS), unit)
      // assuming f is fast
      f(x,y)
    }
  }

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
    (es: ExecutorService) => {
      FutureMap2(a, b)(f)(es)
    }

  def map[A,B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a,_) => f(a))

  def flatMap[A,B](pa: Par[A])(f: A => Par[B]): Par[B] =
    (es: ExecutorService) => {
      val innerValue = pa(es).get
      f(innerValue)(es)
    }

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get()
    })

  def run[A](es: ExecutorService)(a: Par[A]): Future[A] = a(es)

  // derived functions
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A,B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  // probably needs a fork, otherwise this just runs sequentially
  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    ps.foldRight(unit(List.empty[A]))((x,acc) => {map2(x, acc)(_ :: _)})
  }

  // yup, see the recursive and balanced versions of sequence

  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  // Sample answer
  //  def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
  //    val pars: List[Par[List[A]]] =
  //      l map (asyncF((a: A) => if (f(a)) List(a) else List()))
  //    map(sequence(pars))(_.flatten) // convenience method on `List` for concatenating a list of lists
  //  }


  // Actually Option followed by flatten seems a more natural approach
  def parFilter[A](as: List[A])(predicate: A => Boolean): Par[List[A]] = {
    var elements: List[Par[Option[A]]] = as.map(asyncF((a:A) => if (predicate(a)) Some(a) else None))
    map(sequence(elements))(_.flatten)
  }

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    flatMap(n)(choices(_))

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN(map(cond)(x => if(x) 0 else 1))(List(t,f))

  def choiceFlatMap[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    flatMap(cond)(if(_) t else f)

  def join[A](a: Par[Par[A]]): Par[A] =
    es => {
      val innerResult = a(es).get
      innerResult(es)
    }

  def flatMapUsingJoin[A,B](pa: Par[A])(f: A => Par[B]): Par[B] =
    join(map(pa)(f))

  def joinUsingFlatmap[A](a: Par[Par[A]]): Par[A] =
    flatMap(a)(x => x)

}

