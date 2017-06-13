package com.devcexx.libtrails.scala

sealed trait Effect { val bukkitEffect: org.bukkit.Effect }
sealed trait EffectColorable extends Effect

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