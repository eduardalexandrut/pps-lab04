package it.unibo.pps.tasks.adts

/*  Exercise 1: 
 *  Complete the implementation of ComplexADT trait below, so that it passes
 *  the test in ComplexTest.
 */

object Ex1ComplexNumbers:

  trait ComplexADT:
    type Complex
    def complex(re: Double, im: Double): Complex
    extension (complex: Complex)
      def re(): Double
      def im(): Double
      def sum(other: Complex): Complex
      def subtract(other: Complex): Complex
      def asString(): String

  object BasicComplexADT extends ComplexADT:
    // Change assignment below: should probably define a case class and use it?
    case class ComplexNumber(rePart: Double, imPart: Double)

    type Complex = ComplexNumber
    def complex(re: Double, im: Double): Complex = ComplexNumber(re, im)
    extension (complex: Complex)
      def re(): Double = complex.rePart
      def im(): Double = complex.imPart
      def sum(other: Complex): Complex = ComplexNumber(complex.rePart + other.re(), complex.imPart + other.imPart)
      def subtract(other: Complex): Complex = ComplexNumber(complex.rePart - other.re(), complex.imPart - other.imPart)
      def asString(): String =
        val r = complex.rePart
        val i = complex.imPart

        (r, i) match
          case (0, 0) => "0.0"
          case (0, _) => s"${complex.imPart}i"
          case (_, 0) => s"${complex.rePart}"
          case (_, pos) if pos > 0 => s"${complex.rePart} + ${pos}i"
          case (_, neg) if neg < 0 => s"${complex.rePart} - ${neg.abs}i"
