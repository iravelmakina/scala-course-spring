package kse.unit4.challenge

import scala.annotation.targetName

object numerals:

  trait Numeral:

    def isZero: Boolean

    def predecessor: Numeral

    def successor: Numeral = Successor(this)

    @targetName("greater than")
    infix def >(that: Numeral): Boolean

    @targetName("greater or equal to")
    infix def >=(that: Numeral): Boolean = !(that > this)

    @targetName("less than")
    infix def <(that: Numeral): Boolean = that > this

    @targetName("less or equal to")
    infix def <=(that: Numeral): Boolean = !(this > that)

    @targetName("addition")
    infix def +(that: Numeral): Numeral

    // Optional
    @targetName("subtraction")
    infix def -(that: Numeral): Numeral

    def toInt: Int

    override def toString: String = s"Nat($predecessor)"

  type Zero = Zero.type

  object Zero extends Numeral:

    def isZero: Boolean = true

    def predecessor: Numeral = this

    @targetName("greater than")
    infix def >(that: Numeral): Boolean = false

    @targetName("addition")
    infix def +(that: Numeral): Numeral = that

    // Optional
    @targetName("subtraction")
    infix def -(that: Numeral): Numeral = this

    def toInt: Int = 0

    override def toString: String = "Zero"

    override def equals(that: Any): Boolean =
      that match
        case that: Zero.type => true
        case _               => false

  object Successor:
    def unapply(successor: Successor): Option[Numeral] = Option(successor.predecessor)

  class Successor(n: Numeral) extends Numeral:

    def isZero: Boolean = false

    def predecessor: Numeral = n

    @targetName("greater than")
    infix def >(that: Numeral): Boolean =
      that match
        case Zero         => true
        case Successor(m) => predecessor > m

    @targetName("addition")
    infix def +(that: Numeral): Numeral = Successor(predecessor + that)

    // Optional
    @targetName("subtraction")
    infix def -(that: Numeral): Numeral =
      that match
        case Zero         => this
        case Successor(m) => predecessor - m

    def toInt: Int = 1 + n.toInt

    override def toString: String = s"Suc($predecessor)"

    override def equals(that: Any): Boolean =
      that match
        case Successor(m) => predecessor == m
        case _            => false
