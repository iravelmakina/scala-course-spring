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
    def multiplyAccumulate(count: Number, value: Number, acc: Number): Number =
      if isZero(count) then acc
      else multiplyAccumulate(decrement(count), value, addition(acc, value))

    if isZero(left) || isZero(right) then 0
    else if isNonNegative(left) then multiplyAccumulate(decrement(left), right, right)
    else if isNonNegative(right) then multiplyAccumulate(decrement(right), left, left)
    else multiplyAccumulate(decrement(abs(right)), abs(left), abs(left))

  def power(base: Number, p: Number): Number =

    @tailrec
    def powerAccumulate(exp: Number, acc: Number): Number =
      if isZero(exp) then acc
      else powerAccumulate(decrement(exp), multiplication(acc, base))

    if isZero(p) then 1
    else powerAccumulate(decrement(p), base)

end arithmetic
