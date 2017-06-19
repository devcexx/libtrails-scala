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

trait Effect { val bukkitEffect: org.bukkit.Effect }
trait EffectColorable extends Effect

/**
  * Object that contains objects that reprents each effect that may be used to build suppliers and trails.
  */
object Effects {
  object FIREWORKS_SPARK extends Effect { override val bukkitEffect = org.bukkit.Effect.FIREWORKS_SPARK }
  object MAGIC_CRIT extends Effect { override val bukkitEffect = org.bukkit.Effect.MAGIC_CRIT }
  object POTION_SWIRL extends Effect { override val bukkitEffect = org.bukkit.Effect.POTION_SWIRL }
  object POTION_SWIRL_TRANSPARENT extends Effect { override val bukkitEffect = org.bukkit.Effect.POTION_SWIRL_TRANSPARENT }
  object SPELL extends EffectColorable { override val bukkitEffect = org.bukkit.Effect.SPELL }
  object INSTANT_SPELL extends Effect { override val bukkitEffect = org.bukkit.Effect.INSTANT_SPELL }
  object WITCH_MAGIC extends Effect { override val bukkitEffect = org.bukkit.Effect.WITCH_MAGIC }
  object NOTE extends Effect { override val bukkitEffect = org.bukkit.Effect.NOTE }
  object PORTAL extends Effect { override val bukkitEffect = org.bukkit.Effect.PORTAL }
  object FLYING_GLYPH extends Effect { override val bukkitEffect = org.bukkit.Effect.FLYING_GLYPH }
  object FLAME extends Effect { override val bukkitEffect = org.bukkit.Effect.FLAME }
  object LAVA_POP extends Effect { override val bukkitEffect = org.bukkit.Effect.LAVA_POP }
  object FOOTSTEP extends Effect { override val bukkitEffect = org.bukkit.Effect.FOOTSTEP }
  object SPLASH extends Effect { override val bukkitEffect = org.bukkit.Effect.SPLASH }
  object PARTICLE_SMOKE extends Effect { override val bukkitEffect = org.bukkit.Effect.PARTICLE_SMOKE }
  object EXPLOSION_HUGE extends Effect { override val bukkitEffect = org.bukkit.Effect.EXPLOSION_HUGE }
  object EXPLOSION_LARGE extends Effect { override val bukkitEffect = org.bukkit.Effect.EXPLOSION_LARGE }
  object EXPLOSION extends Effect { override val bukkitEffect = org.bukkit.Effect.EXPLOSION }
  object VOID_FOG extends Effect { override val bukkitEffect = org.bukkit.Effect.VOID_FOG }
  object SMALL_SMOKE extends Effect { override val bukkitEffect = org.bukkit.Effect.SMALL_SMOKE }
  object CLOUD extends Effect { override val bukkitEffect = org.bukkit.Effect.CLOUD }
  object COLOURED_DUST extends EffectColorable { override val bukkitEffect = org.bukkit.Effect.COLOURED_DUST }
  object SNOWBALL_BREAK extends Effect { override val bukkitEffect = org.bukkit.Effect.SNOWBALL_BREAK }
  object WATERDRIP extends Effect { override val bukkitEffect = org.bukkit.Effect.WATERDRIP }
  object LAVADRIP extends Effect { override val bukkitEffect = org.bukkit.Effect.LAVADRIP }
  object SNOW_SHOVEL extends Effect { override val bukkitEffect = org.bukkit.Effect.SNOW_SHOVEL }
  object SLIME extends Effect { override val bukkitEffect = org.bukkit.Effect.SLIME }
  object HEART extends Effect { override val bukkitEffect = org.bukkit.Effect.HEART }
  object VILLAGER_THUNDERCLOUD extends Effect { override val bukkitEffect = org.bukkit.Effect.VILLAGER_THUNDERCLOUD }
  object HAPPY_VILLAGER extends Effect { override val bukkitEffect = org.bukkit.Effect.HAPPY_VILLAGER }
  object LARGE_SMOKE extends Effect { override val bukkitEffect = org.bukkit.Effect.LARGE_SMOKE }
  object ITEM_BREAK extends Effect { override val bukkitEffect = org.bukkit.Effect.ITEM_BREAK }
  object TILE_BREAK extends Effect { override val bukkitEffect = org.bukkit.Effect.TILE_BREAK }
  object TILE_DUST extends Effect { override val bukkitEffect = org.bukkit.Effect.TILE_DUST }
}