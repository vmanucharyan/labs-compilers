package common


object SetExt {
  class SetExt[A](set: Set[A]) {
    def cartesianProduct[B](that: Set[B]): Set[(A, B)] = {
      for(e1 <- set; e2 <- that) yield (e1, e2)
    }
  }

  implicit def set2SetExt[A](s: Set[A]): SetExt[A] = new SetExt[A](s)
}
