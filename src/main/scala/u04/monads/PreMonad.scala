package u04.monads

object PreMonad:

  // F[_] is called a "type constructor", or 1-kinded type
  // Monad is generic in a type constructor named F
  trait Monad[F[_]]:
    // two constructs for monads, namely, its true "definition"
    def unit[A](a: => A): F[A]
    def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]

    // monadic laws:
    // Left identity: flatMap(unit(a),f) === f(a)
    // Right identity: flatMap(m,unit(_)) === m
    // Associativity: flatMap(flatMap(m,f),g) ===
    //                flatMap(m,x=>flatMap(f(x),g))

    // map as a key derived op
    def map[A, B](ma: F[A])(f: A => B): F[B] =
      flatMap(ma)(a => unit(f(a)))

  object MonadOps:
    // defining flatMap, and map, as OO-based ops
    extension [F[_]: Monad, A](ma: F[A])

      def flatMap[B](f: A => F[B]): F[B] =
        summon[Monad[F]].flatMap[A, B](ma)(f)

      def map[B](f: A => B): F[B] =
        ma.flatMap(a => summon[Monad[F]].unit(f(a)))

