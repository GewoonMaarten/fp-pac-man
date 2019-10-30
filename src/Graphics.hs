module Graphics where

import           Graphics.Gloss
import           Models.Ghost
data TextureSet = PacManTextureSet Picture Picture Picture Picture Picture
  | DeathTextureSet Picture Picture Picture
  | GhostTextureSet Picture Picture

data Textures = Textures {
  textureLiveCounter :: Picture,
  textureCherry :: Picture,
  texturesGhost :: [(GhostType, TextureSet)],
  texturesGhostAfraid :: TextureSet,
  texturesPacMan :: TextureSet,
  texturesPacManDeath :: TextureSet
}

loadTextures :: IO Textures
loadTextures = do
  pacManTextures <-
    PacManTextureSet
    <$> loadBMP "assets/pac_man_0.bmp"
    <*> loadBMP "assets/pac_man_1.bmp"
    <*> loadBMP "assets/pac_man_2.bmp"
    <*> loadBMP "assets/pac_man_3.bmp"
    <*> loadBMP "assets/pac_man_4.bmp"
  deathTextures <-
    DeathTextureSet
    <$> loadBMP "assets/spr_pacdeath_0.bmp"
    <*> loadBMP "assets/spr_pacdeath_1.bmp"
    <*> loadBMP "assets/spr_pacdeath_2.bmp"
  blinkyTextures <-
    GhostTextureSet <$> loadBMP "assets/spr_ghost_red_0.bmp" <*> loadBMP
      "assets/spr_ghost_red_1.bmp"
  inkyTextures <-
    GhostTextureSet <$> loadBMP "assets/spr_ghost_blue_0.bmp" <*> loadBMP
      "assets/spr_ghost_blue_1.bmp"
  pinkyTextures <-
    GhostTextureSet <$> loadBMP "assets/spr_ghost_pink_0.bmp" <*> loadBMP
      "assets/spr_ghost_pink_1.bmp"
  clydeTextures <-
    GhostTextureSet <$> loadBMP "assets/spr_ghost_orange_0.bmp" <*> loadBMP
      "assets/spr_ghost_orange_1.bmp"
  afraidTextures <-
    GhostTextureSet <$> loadBMP "assets/spr_afraid_0.bmp" <*> loadBMP
      "assets/spr_afraid_1.bmp"
  liveCounter   <- loadBMP "assets/spr_lifecounter_0.bmp"
  cherryTexture <- loadBMP "assets/spr_cherry_0.bmp"
  return Textures
    { textureLiveCounter  = liveCounter
    , textureCherry       = cherryTexture
    , texturesGhost       = [ (Blinky, blinkyTextures)
                            , (Inky  , inkyTextures)
                            , (Pinky , pinkyTextures)
                            , (Clyde , clydeTextures)
                            ]
    , texturesGhostAfraid = afraidTextures
    , texturesPacMan      = pacManTextures
    , texturesPacManDeath = deathTextures
    }
