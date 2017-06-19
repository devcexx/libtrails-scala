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
package com.devcexx.libtrails.scala.suppliers

import java.awt.image.BufferedImage

import com.devcexx.libtrails.scala.lmath.ColorUtil.{Color, ColorHSV, ColorRGB, ColorSpace, ConvertibleColorSpace, HSV, RGB}
import com.devcexx.libtrails.scala.lmath.Linalg.Vectors.Vector3f
import com.devcexx.libtrails.scala.{EffectColorable, Particle, SuppliedParticle}

/**
  * Object that contains methods to create and customize a Particle supplier that is able to render an image using
  * particles.
  */
object BitmapSupplier {

  /**
    * Builds a new particle source that maps every pixel of the input image with a particle with its same color.
    * @param effect the coloreable effect that will be used to render each pixel.
    * @param visibilityRadius the visibility radius of each pixel.
    * @param spaceConverter the space converter that allows to convert the input color space to RGB.
    * @tparam CS the color space that the supplier is working with.
    * @return a new particle source with the given properties.
    */
  def mkParticleSourceDefault[CS <: ColorSpace](effect: EffectColorable, visibilityRadius: Int)
                                               (implicit spaceConverter: ConvertibleColorSpace[CS, RGB]): Color[CS] => Particle =
     color => Particle.mkColorParticleWithColor[CS](effect, visibilityRadius)(_ => color)


  /**
    * Builds a new particle source that maps every pixel of the input image with a particle with its appropiate
    * grayscale color
    * @param effect the coloreable effect that will be used to render each pixel.
    * @param visibilityRadius the visibility radius of each pixel.
    * @param spaceConverter the space converter that allows to convert the input color space to RGB.
    * @tparam CS the color space that the supplier is working with.
    * @return a new particle source with the given properties.
    */
  def mkParticleSourceGrayScale[CS <: ColorSpace](effect: EffectColorable, visibilityRadius: Int)
                                                 (implicit spaceConverter: ConvertibleColorSpace[CS, RGB]): Color[CS] => Particle =
    color => {
      val cdata = spaceConverter.convert(color).data
      val m = cdata.r + cdata.g + cdata.b / 3.0f
      val c = Particle.colorOffsetRGB(m, m, m)
      Particle.mkColorParticleWithOff(effect, visibilityRadius)(_ => c)
    }

  /**
    * Builds a new partilce source that maps every pixel of the input image with a HSV particle, which saturation and
    * brightness values are the same as the input, but its hue is time-dependent.
    * @param effect the coloreable effect that will be used to render each pixel.
    * @param visibilityRadius the visibility radius of each pixel.
    * @param loopTicks the time will take the hue to get from 0 to 360, in ticks.
    * @param spaceConverter the space converter that allows to convert the input color space to HSV.
    * @tparam CS the color space that the supplier is working with.
    * @return a new particle source with the given properties.
    */
  def mkParticleSourceRainbowColor[CS <: ColorSpace](effect: EffectColorable, visibilityRadius: Int, loopTicks: Int)
                                                    (implicit spaceConverter: ConvertibleColorSpace[CS, HSV]): Color[CS] => Particle =
    color => {
      Particle.mkColorRainbowParticle(effect, visibilityRadius, spaceConverter.convert(color), loopTicks)
    }

  /**
    * Builds a pixel source that gets each pixel from a buffered image.
    * @param source the source image.
    * @param alphaThreshold the threshold that determines whether a pixel should be considered that is present or not.
    *                       If a pixel has an alpha < alphaThreshold is considered as gone. The range of values of the
    *                       threshold is from 0 to 1
    * @param spaceConverter  the space converter that allows to convert the input color space from RGB to the CS color
    *                        space.
    * @tparam CS the color space that the supplier is working with.
    * @return a new pixel source with the given properties.
    */
  def mkPixelSourceBitmap[CS <: ColorSpace](source: BufferedImage, alphaThreshold: Float = 0.5f)
                                           (implicit spaceConverter: ConvertibleColorSpace[RGB, CS]): (Int, Int) => Option[Color[CS]] =
    (x, y) => {
      val argb = source.getRGB(x, y)
      val a = ((argb >> 24) & 0xFF).toFloat / 255.0f

      if (a < alphaThreshold) {
        None
      } else {
        val r = ((argb >> 16) & 0xFF).toFloat / 255.0f
        val g = ((argb >> 8) & 0xFF).toFloat / 255.0f
        val b = (argb & 0xFF).toFloat / 255.0f
        Some(ColorRGB(r, g, b).convertColorSpace[CS])
      }
    }

  /**
    * Builds a new Fixed Lazy Supplier from a pixel matrix, usually taken from a bitmap. The pixel values are got
    * from a pixel source, and mapped as particles with a particle source. Also, the size of the output may be
    * adjusted using the k-Nearest Neightbor algorithm to scale the input pixels.
    *
    * @param pixelSource the source of the pixels. Must be a function of arity 2, where the first parameter is the
    *                    x coordinate of the pixel. The source must allow values from 0 to sourceWidth - 1
    * @param particleSource the particle source, that will map every pixel of the source with a particle of Minecraft.
    * @param sourceWidth the width of the pixel source.
    * @param sourceHeight the height of the pixel source.
    * @param scaledWidthBlocks the output width, in Minecraft blocks, of the rendered shape.
    * @param scaledHeightBlocks the output height, in Minecraft blocks, of the rendered shape.
    * @param hRes the horizontal density of the rendered shape, in pixels per block.
    * @param vRes the vertical density of the rendered shape, in pixels per block.
    * @param appearInterval the appear interval of the rendered shape.
    * @tparam CS the color space in which the pixel source will return the colors and the particle source will receive it.
    * @return a FixedLazySupplier with the final scaled, rendered image.
    */
  def apply[CS <: ColorSpace](pixelSource: (Int, Int) => Option[Color[CS]], particleSource: Color[CS] => Particle,
            sourceWidth: Int, sourceHeight: Int, scaledWidthBlocks: Float, scaledHeightBlocks: Float,
            hRes: Float, vRes: Float, appearInterval: Int) =
    FixedLazySupplier(() => {
      val finalw = math.ceil(scaledWidthBlocks * hRes).toInt
      val finalh = math.ceil(scaledHeightBlocks * vRes).toInt

      val xdist = 1.0f / hRes
      val zdist = 1.0f / vRes

      val midw = scaledWidthBlocks / 2.0f
      val midh = scaledHeightBlocks / 2.0f

      val xScale = sourceWidth.toDouble / finalw.toDouble
      val yScale = sourceHeight.toDouble / finalh.toDouble

     (for (i <- 0 until finalh; j <- 0 until finalw) yield {
        val px = math.floor(j * xScale).toInt
        val py = math.floor(i * yScale).toInt

        pixelSource(px, py) match {
          case some: Some[Color[CS]] => Some(SuppliedParticle(particleSource(some.get),
            Vector3f(j * xdist - midw, 0, midh - (i * zdist))))
          case _ => None
        }
      }).flatten

    }, appearInterval)
}
