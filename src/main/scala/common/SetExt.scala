package common


object SetExt {
  class SetExt[A](set: Set[A]) {
    def cartesianProduct[B](that: Set[B]): Set[(A, B)] = {
      for(e1 <- set; e2 <- that) yield (e1, e2)
    }

    //noinspection RangeToIndices
    def combinations(): Set[Set[A]] = {
      def append(e: A, src: Set[Set[A]]): Set[Set[A]] =
        src.map { se: Set[A] =>
          se ++ Set(e)
        }

      set.foldLeft(Set[Set[A]]())((acc, e) => {
        (acc + Set(e)) ++ append(e, acc) ++ acc
      })
    }
  }

  implicit def set2SetExt[A](s: Set[A]): SetExt[A] = new SetExt[A](s)
}
