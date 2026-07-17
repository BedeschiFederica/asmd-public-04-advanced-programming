package lab04

object BinaryTrees:

  enum BinaryTree[E]:
    case Node(value: E, left: BinaryTree[E], right: BinaryTree[E])
    case Nil()

  object BinaryTree:

    def merge[A](left: BinaryTree[A], right: BinaryTree[A]): BinaryTree[A] = (left, right) match
      case (Nil(), _) => right
      case (_, Nil()) => left
      case (Node(v, l, r), _) => Node(v, l, merge(r, right))

    extension [A](t: BinaryTree[A])

      def contains(element: A): Boolean = t match
        case Node(value, left, right) => value == element || left.contains(element) || right.contains(element)
        case _ => false

      def size(): Int = t match
        case Node(_, left, right) => left.size() + right.size() + 1
        case _ => 0

      def insert(element: A): BinaryTree[A] = t match
        case Node(value, left, right) if left.size() <= right.size() => Node(value, left.insert(element), right)
        case Node(value, left, right) => Node(value, left, right.insert(element))
        case _ => Node(element, Nil(), Nil())

      def remove(element: A): BinaryTree[A] = t match
        case Node(value, left, right) if value == element => merge(left, right)
        case Node(value, left, right) => Node(value, left.remove(element), right.remove(element))
        case _ => Nil()
