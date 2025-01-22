package kse.unit1.challenge

import scala.annotation.tailrec

object arithmetic:

  type Number = Long

  val increment: Number => Number =
    value => value + 1

  val decrement: Number => Number =
    value => value - 1

  val isZero: Number => Boolean =
    value => value == 0

  val isNonNegative: Number => Boolean =
    value => value >= 0

  val abs: Number => Number =
    value =>
      if isNonNegative(value) then value
      else -value

  @tailrec
  def addition(left: Number, right: Number): Number =
    if isZero(left) then right
    else if isNonNegative(left) then addition(decrement(left), increment(right))
    else addition(increment(left), decrement(right))

  def multiplication(left: Number, right: Number): Number =
    @tailrec
    def helper(count: Number, acc: Number, absLeft: Number): Number =
      if isZero(count) then acc
      else helper(decrement(count), addition(acc, absLeft), absLeft)

    def negate(value: Number): Number =
      @tailrec
      def loop(current: Number, acc: Number): Number =
        if isZero(current) then acc
        else loop(decrement(current), decrement(acc))

      loop(abs(value), 0)

    if isZero(left) || isZero(right) then 0
    else
      val absLeft  = abs(left)
      val absRight = abs(right)
      val result   = helper(absRight, 0, absLeft)
      if (isNonNegative(left) && isNonNegative(right)) || (!isNonNegative(left) && !isNonNegative(right)) then result
      else negate(result)

  def power(base: Number, p: Number): Number =

    @tailrec
    def helper(exp: Number, acc: Number): Number =
      if isZero(exp) then acc
      else helper(decrement(exp), multiplication(acc, base))

    if isZero(p) then 1
    else helper(p, 1)

end arithmetic
