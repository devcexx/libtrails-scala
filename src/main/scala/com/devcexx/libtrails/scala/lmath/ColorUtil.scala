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
package com.devcexx.libtrails.scala.lmath

/**
  * Object that contains utils to work with color and conversions between color spaces
  */
object ColorUtil {

  /**
    * Represents a color space
    */
  trait ColorSpace {
    type CompT
  }

  /**
    * Identifies the HSV color space
    */
  trait HSV extends ColorSpace {
    override type CompT = HSVRawData
  }

  /**
    * Identifies the RGB color space
    */
  trait RGB extends ColorSpace {
    override type CompT = RGBRawData
  }

  /**
    * Represents a HSV color data container
    * @param h the hue of the color (from 0 to 360 exclusive)
    * @param s the saturation of the color (from 0
    * @param v the value (or brightness) of the color
    */
  case class HSVRawData(h: Float, s: Float, v: Float)

  /**
    * Represents a RGB color data container
    * @param r the red component (from 0 to 1)
    * @param g the green component (from 0 to 1)
    * @param b the blue component (from 0 to 1)
    */
  case class RGBRawData(r: Float, g: Float, b: Float)

  /**
    * Represents an structure that allows to convert a color from a color space to another
    * @tparam ISpace the color space input type
    * @tparam OSpace the color space output type
    */
  trait ConvertibleColorSpace[ISpace <: ColorSpace, OSpace <: ColorSpace] {

    /**
      * Converts a color from the space ISpace to the space OSpace
      * @param in the input color
      * @return the same color with the desired color space.
      */
    def convert(in: Color[ISpace]): Color[OSpace]

    /**
      * Returns an object that allows the inverse conversion of the current one.
      */
    val reversed: ConvertibleColorSpace[OSpace, ISpace]
  }

  /**
    * Object that allows the conversion from RGB color to HSV color
    */
  implicit object RGBIsConvertibleToHSV extends ConvertibleColorSpace[RGB, HSV] {
    override def convert(in: Color[RGB]): Color[HSV] = {
      val cmax = math.max(in.data.r, math.max(in.data.g, in.data.b))
      val cmin = math.min(in.data.r, math.min(in.data.g, in.data.b))
      val delta = cmax - cmin
      
      val data = HSVRawData(
        if (cmax == cmin) 0f
        else
          60f * (cmax match {
            case in.data.r => ((in.data.g - in.data.b) / delta) % 6
            case in.data.g => ((in.data.b - in.data.r) / delta) + 2
            case _ => ((in.data.r - in.data.g) / delta) + 4
          }),

        if (cmax == 0) 0f
        else delta / cmax,

        cmax)

      Color[HSV](data)
    }
    override lazy val reversed = HSVIsConvertibleToRGB

  }

  /**
    * Object that allows the conversion from RGB color space to RGB color space (identity)
    */
  implicit object RGBIsConvertibleToRGB extends ConvertibleColorSpace[RGB, RGB] {
    override def convert(in: Color[RGB]): Color[RGB] = in
    override lazy val reversed = RGBIsConvertibleToRGB
  }

  /**
    * Object that allows the conversion from HSV color space to HSV color space (identity)
    */
  implicit object HSVIsConvertibleToHSV extends ConvertibleColorSpace[HSV, HSV] {
    override def convert(in: Color[HSV]): Color[HSV] = in
    override lazy val reversed = HSVIsConvertibleToHSV
  }

  /**
    * Object that allows the conversion from HSV color space to RGB color space
    */
  implicit object HSVIsConvertibleToRGB extends ConvertibleColorSpace[HSV, RGB] {
    override def convert(in: Color[HSV]): Color[RGB] = {
      val c = in.data.v * in.data.s
      val x = c * (1f - math.abs((in.data.h / 60f) % 2 - 1))
      val m = in.data.v - c

      val h = in.data.h

      val rgb =
      if (h > 0 && h < 60)
        RGBRawData(c, x, 0f)
      else if (h >= 60 && h < 120)
        RGBRawData(x, c, 0f)
      else if (h >= 120 && h < 180)
        RGBRawData(0f, c, x)
      else if (h >= 180 && h < 240)
        RGBRawData(0f, x, c)
      else if (h >= 240 && h < 300)
        RGBRawData(x, 0f, c)
      else RGBRawData(c, 0f, x)

      Color[RGB](rgb.copy(r = rgb.r + m, g = rgb.g + m, b = rgb.b + m))
    }
    override lazy val reversed = RGBIsConvertibleToHSV
  }

  /**
    * Represents a color in a color space.
    * @param data the underlying data of the color
    * @tparam S The color space used to represent this color.
    */
  class Color[S <: ColorSpace](val data: S#CompT) {

    /**
      * Converts the current color to another color space.
      * @param conv The implicit converter parameter that allows the conversion between the current
      *             color space and the target color space.
      * @tparam To The final color space.
      * @return This color converted to the desired color space.
      */
    def convertColorSpace[To <: ColorSpace](implicit conv: ConvertibleColorSpace[S, To]) =
      conv.convert(this)

    override def toString: String = data.toString
  }

  /**
    * Represents a color in a RGB space
    */
  type ColorRGB = Color[RGB]

  /**
    * Represents a color in a HSV space
    */
  type ColorHSV = Color[HSV]


  /**
    * Color class companion
    */
  object Color {
    def apply[S <: ColorSpace](data: S#CompT) =
      new Color[S](data)
  }

  /**
    * Color class companion for RGB color space
    */
  object ColorRGB {
    def apply(data: RGBRawData): Color[RGB] = Color[RGB](data)
    def apply(r: Float, g: Float, b: Float): Color[RGB] = apply(RGBRawData(r,g,b))
  }

  /**
    * Color class companion for HSV color space
    */
  object ColorHSV {
    def apply(data: HSVRawData): Color[HSV] = Color[HSV](data)
    def apply(h: Float, s: Float, v: Float): Color[HSV] = apply(HSVRawData(h,s,v))
  }

}
