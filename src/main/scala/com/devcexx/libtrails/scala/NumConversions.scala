/*
 *  This file is part of libtrails.
 *  libtrails is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  libtrails is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with libtrails.  If not, see <http://www.gnu.org/licenses/>.
 */
package com.devcexx.libtrails.scala

trait NumberConverter[A,B] {
  def apply(o: A): B
  def reverse()(implicit conv: NumberConverter[B, A]) = conv

}

object NumberConverters {
  //From Byte
  implicit object ByteToByteConverter 
    extends NumberConverter[Byte, Byte] {

    override def apply(o: Byte): Byte = o
  }
  implicit object ByteToShortConverter 
    extends NumberConverter[Byte, Short] {

    override def apply(o: Byte): Short = o.toShort
  }
  implicit object ByteToCharConverter 
    extends NumberConverter[Byte, Char] {

    override def apply(o: Byte): Char = o.toChar
  }
  implicit object ByteToIntConverter 
    extends NumberConverter[Byte, Int] {

    override def apply(o: Byte): Int = o.toInt
  }
  implicit object ByteToLongConverter 
    extends NumberConverter[Byte, Long] {

    override def apply(o: Byte): Long = o.toLong
  }
  implicit object ByteToFloatConverter 
    extends NumberConverter[Byte, Float] {

    override def apply(o: Byte): Float = o.toFloat
  }
  implicit object ByteToDoubleConverter 
    extends NumberConverter[Byte, Double] {

    override def apply(o: Byte): Double = o.toDouble
  }
  implicit object ByteToBigIntConverter 
    extends NumberConverter[Byte, BigInt] {

    override def apply(o: Byte): BigInt = BigInt(o)
  }
  implicit object ByteToBigDecimalConverter 
    extends NumberConverter[Byte, BigDecimal] {

    override def apply(o: Byte): BigDecimal = BigDecimal(o)
  }
  //From Short
  implicit object ShortToByteConverter 
    extends NumberConverter[Short, Byte] {

    override def apply(o: Short): Byte = o.toByte
  }
  implicit object ShortToShortConverter 
    extends NumberConverter[Short, Short] {

    override def apply(o: Short): Short = o
  }
  implicit object ShortToCharConverter 
    extends NumberConverter[Short, Char] {

    override def apply(o: Short): Char = o.toChar
  }
  implicit object ShortToIntConverter 
    extends NumberConverter[Short, Int] {

    override def apply(o: Short): Int = o.toInt
  }
  implicit object ShortToLongConverter 
    extends NumberConverter[Short, Long] {

    override def apply(o: Short): Long = o.toLong
  }
  implicit object ShortToFloatConverter 
    extends NumberConverter[Short, Float] {

    override def apply(o: Short): Float = o.toFloat
  }
  implicit object ShortToDoubleConverter 
    extends NumberConverter[Short, Double] {

    override def apply(o: Short): Double = o.toDouble
  }
  implicit object ShortToBigIntConverter 
    extends NumberConverter[Short, BigInt] {

    override def apply(o: Short): BigInt = BigInt(o)
  }
  implicit object ShortToBigDecimalConverter 
    extends NumberConverter[Short, BigDecimal] {

    override def apply(o: Short): BigDecimal = BigDecimal(o)
  }
  //From Char
  implicit object CharToByteConverter 
    extends NumberConverter[Char, Byte] {

    override def apply(o: Char): Byte = o.toByte
  }
  implicit object CharToShortConverter 
    extends NumberConverter[Char, Short] {

    override def apply(o: Char): Short = o.toShort
  }
  implicit object CharToCharConverter 
    extends NumberConverter[Char, Char] {

    override def apply(o: Char): Char = o
  }
  implicit object CharToIntConverter 
    extends NumberConverter[Char, Int] {

    override def apply(o: Char): Int = o.toInt
  }
  implicit object CharToLongConverter 
    extends NumberConverter[Char, Long] {

    override def apply(o: Char): Long = o.toLong
  }
  implicit object CharToFloatConverter 
    extends NumberConverter[Char, Float] {

    override def apply(o: Char): Float = o.toFloat
  }
  implicit object CharToDoubleConverter 
    extends NumberConverter[Char, Double] {

    override def apply(o: Char): Double = o.toDouble
  }
  implicit object CharToBigIntConverter 
    extends NumberConverter[Char, BigInt] {

    override def apply(o: Char): BigInt = BigInt(o)
  }
  implicit object CharToBigDecimalConverter 
    extends NumberConverter[Char, BigDecimal] {

    override def apply(o: Char): BigDecimal = BigDecimal(o)
  }
  //From Int
  implicit object IntToByteConverter 
    extends NumberConverter[Int, Byte] {

    override def apply(o: Int): Byte = o.toByte
  }
  implicit object IntToShortConverter 
    extends NumberConverter[Int, Short] {

    override def apply(o: Int): Short = o.toShort
  }
  implicit object IntToCharConverter 
    extends NumberConverter[Int, Char] {

    override def apply(o: Int): Char = o.toChar
  }
  implicit object IntToIntConverter 
    extends NumberConverter[Int, Int] {

    override def apply(o: Int): Int = o
  }
  implicit object IntToLongConverter 
    extends NumberConverter[Int, Long] {

    override def apply(o: Int): Long = o.toLong
  }
  implicit object IntToFloatConverter 
    extends NumberConverter[Int, Float] {

    override def apply(o: Int): Float = o.toFloat
  }
  implicit object IntToDoubleConverter 
    extends NumberConverter[Int, Double] {

    override def apply(o: Int): Double = o.toDouble
  }
  implicit object IntToBigIntConverter 
    extends NumberConverter[Int, BigInt] {

    override def apply(o: Int): BigInt = BigInt(o)
  }
  implicit object IntToBigDecimalConverter 
    extends NumberConverter[Int, BigDecimal] {

    override def apply(o: Int): BigDecimal = BigDecimal(o)
  }
  //From Long
  implicit object LongToByteConverter 
    extends NumberConverter[Long, Byte] {

    override def apply(o: Long): Byte = o.toByte
  }
  implicit object LongToShortConverter 
    extends NumberConverter[Long, Short] {

    override def apply(o: Long): Short = o.toShort
  }
  implicit object LongToCharConverter 
    extends NumberConverter[Long, Char] {

    override def apply(o: Long): Char = o.toChar
  }
  implicit object LongToIntConverter 
    extends NumberConverter[Long, Int] {

    override def apply(o: Long): Int = o.toInt
  }
  implicit object LongToLongConverter 
    extends NumberConverter[Long, Long] {

    override def apply(o: Long): Long = o
  }
  implicit object LongToFloatConverter 
    extends NumberConverter[Long, Float] {

    override def apply(o: Long): Float = o.toFloat
  }
  implicit object LongToDoubleConverter 
    extends NumberConverter[Long, Double] {

    override def apply(o: Long): Double = o.toDouble
  }
  implicit object LongToBigIntConverter 
    extends NumberConverter[Long, BigInt] {

    override def apply(o: Long): BigInt = BigInt(o)
  }
  implicit object LongToBigDecimalConverter 
    extends NumberConverter[Long, BigDecimal] {

    override def apply(o: Long): BigDecimal = BigDecimal(o)
  }
  //From Float
  implicit object FloatToByteConverter 
    extends NumberConverter[Float, Byte] {

    override def apply(o: Float): Byte = o.toByte
  }
  implicit object FloatToShortConverter 
    extends NumberConverter[Float, Short] {

    override def apply(o: Float): Short = o.toShort
  }
  implicit object FloatToCharConverter 
    extends NumberConverter[Float, Char] {

    override def apply(o: Float): Char = o.toChar
  }
  implicit object FloatToIntConverter 
    extends NumberConverter[Float, Int] {

    override def apply(o: Float): Int = o.toInt
  }
  implicit object FloatToLongConverter 
    extends NumberConverter[Float, Long] {

    override def apply(o: Float): Long = o.toLong
  }
  implicit object FloatToFloatConverter 
    extends NumberConverter[Float, Float] {

    override def apply(o: Float): Float = o
  }
  implicit object FloatToDoubleConverter 
    extends NumberConverter[Float, Double] {

    override def apply(o: Float): Double = o.toDouble
  }
  implicit object FloatToBigIntConverter 
    extends NumberConverter[Float, BigInt] {

    override def apply(o: Float): BigInt = BigInt(o.toLong)
  }
  implicit object FloatToBigDecimalConverter 
    extends NumberConverter[Float, BigDecimal] {

    override def apply(o: Float): BigDecimal = BigDecimal(o)
  }
  //From Double
  implicit object DoubleToByteConverter 
    extends NumberConverter[Double, Byte] {

    override def apply(o: Double): Byte = o.toByte
  }
  implicit object DoubleToShortConverter 
    extends NumberConverter[Double, Short] {

    override def apply(o: Double): Short = o.toShort
  }
  implicit object DoubleToCharConverter 
    extends NumberConverter[Double, Char] {

    override def apply(o: Double): Char = o.toChar
  }
  implicit object DoubleToIntConverter 
    extends NumberConverter[Double, Int] {

    override def apply(o: Double): Int = o.toInt
  }
  implicit object DoubleToLongConverter 
    extends NumberConverter[Double, Long] {

    override def apply(o: Double): Long = o.toLong
  }
  implicit object DoubleToFloatConverter 
    extends NumberConverter[Double, Float] {

    override def apply(o: Double): Float = o.toFloat
  }
  implicit object DoubleToDoubleConverter 
    extends NumberConverter[Double, Double] {

    override def apply(o: Double): Double = o
  }
  implicit object DoubleToBigIntConverter 
    extends NumberConverter[Double, BigInt] {

    override def apply(o: Double): BigInt = BigInt(o.toLong)
  }
  implicit object DoubleToBigDecimalConverter 
    extends NumberConverter[Double, BigDecimal] {

    override def apply(o: Double): BigDecimal = BigDecimal(o)
  }
  //From BigInt
  implicit object BigIntToByteConverter 
    extends NumberConverter[BigInt, Byte] {

    override def apply(o: BigInt): Byte = o.toByte
  }
  implicit object BigIntToShortConverter 
    extends NumberConverter[BigInt, Short] {

    override def apply(o: BigInt): Short = o.toShort
  }
  implicit object BigIntToCharConverter 
    extends NumberConverter[BigInt, Char] {

    override def apply(o: BigInt): Char = o.toChar
  }
  implicit object BigIntToIntConverter 
    extends NumberConverter[BigInt, Int] {

    override def apply(o: BigInt): Int = o.toInt
  }
  implicit object BigIntToLongConverter 
    extends NumberConverter[BigInt, Long] {

    override def apply(o: BigInt): Long = o.toLong
  }
  implicit object BigIntToFloatConverter 
    extends NumberConverter[BigInt, Float] {

    override def apply(o: BigInt): Float = o.toFloat
  }
  implicit object BigIntToDoubleConverter 
    extends NumberConverter[BigInt, Double] {

    override def apply(o: BigInt): Double = o.toDouble
  }
  implicit object BigIntToBigIntConverter 
    extends NumberConverter[BigInt, BigInt] {

    override def apply(o: BigInt): BigInt = o
  }
  implicit object BigIntToBigDecimalConverter 
    extends NumberConverter[BigInt, BigDecimal] {

    override def apply(o: BigInt): BigDecimal = BigDecimal(o)
  }
  //From BigDecimal
  implicit object BigDecimalToByteConverter 
    extends NumberConverter[BigDecimal, Byte] {

    override def apply(o: BigDecimal): Byte = o.toByte
  }
  implicit object BigDecimalToShortConverter 
    extends NumberConverter[BigDecimal, Short] {

    override def apply(o: BigDecimal): Short = o.toShort
  }
  implicit object BigDecimalToCharConverter 
    extends NumberConverter[BigDecimal, Char] {

    override def apply(o: BigDecimal): Char = o.toChar
  }
  implicit object BigDecimalToIntConverter 
    extends NumberConverter[BigDecimal, Int] {

    override def apply(o: BigDecimal): Int = o.toInt
  }
  implicit object BigDecimalToLongConverter 
    extends NumberConverter[BigDecimal, Long] {

    override def apply(o: BigDecimal): Long = o.toLong
  }
  implicit object BigDecimalToFloatConverter 
    extends NumberConverter[BigDecimal, Float] {

    override def apply(o: BigDecimal): Float = o.toFloat
  }
  implicit object BigDecimalToDoubleConverter 
    extends NumberConverter[BigDecimal, Double] {

    override def apply(o: BigDecimal): Double = o.toDouble
  }
  implicit object BigDecimalToBigIntConverter 
    extends NumberConverter[BigDecimal, BigInt] {

    override def apply(o: BigDecimal): BigInt = o.toBigInt()
  }
  implicit object BigDecimalToBigDecimalConverter 
    extends NumberConverter[BigDecimal, BigDecimal] {

    override def apply(o: BigDecimal): BigDecimal = o
  }
}

