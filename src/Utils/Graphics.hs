module Utils.Graphics where

import           Graphics.Gloss
import           Paths_PacMan

data TextureSet = PacManTextureSet Picture Picture Picture Picture
  | DeathTextureSet Picture Picture Picture
  | GhostTextureSet Picture Picture

data Textures = Textures {
  textureLifeCounter :: Picture,
  textureCherry :: Picture,
  texturesGhost :: [(Int, TextureSet)],
  texturesGhostAfraid :: TextureSet,
  texturesPacMan :: TextureSet,
  texturesPacManDeath :: TextureSet,
  textureBanner :: Picture
}
loadBMP' :: String -> IO Picture
loadBMP' fileName = do
  filePath <- getDataFileName fileName
  loadBMP filePath

loadTextures :: IO Textures
loadTextures = do
  pacManTextures <-
    PacManTextureSet
    <$> loadBMP' "assets/bmp/pac_man_0.bmp"
    <*> loadBMP' "assets/bmp/pac_man_1.bmp"
    <*> loadBMP' "assets/bmp/pac_man_2.bmp"
    <*> loadBMP' "assets/bmp/pac_man_3.bmp"
  deathTextures <-
    DeathTextureSet
    <$> loadBMP' "assets/bmp/spr_pacdeath_0.bmp"
    <*> loadBMP' "assets/bmp/spr_pacdeath_1.bmp"
    <*> loadBMP' "assets/bmp/spr_pacdeath_2.bmp"
  blinkyTextures <-
    GhostTextureSet <$> loadBMP' "assets/bmp/spr_ghost_red_0.bmp" <*> loadBMP'
      "assets/bmp/spr_ghost_red_1.bmp"
  inkyTextures <-
    GhostTextureSet <$> loadBMP' "assets/bmp/spr_ghost_blue_0.bmp" <*> loadBMP'
      "assets/bmp/spr_ghost_blue_1.bmp"
  pinkyTextures <-
    GhostTextureSet <$> loadBMP' "assets/bmp/spr_ghost_pink_0.bmp" <*> loadBMP'
      "assets/bmp/spr_ghost_pink_1.bmp"
  clydeTextures <-
    GhostTextureSet
    <$> loadBMP' "assets/bmp/spr_ghost_orange_0.bmp"
    <*> loadBMP' "assets/bmp/spr_ghost_orange_1.bmp"
  afraidTextures <-
    GhostTextureSet <$> loadBMP' "assets/bmp/spr_afraid_0.bmp" <*> loadBMP'
      "assets/bmp/spr_afraid_1.bmp"
  lifeCounter   <- loadBMP' "assets/bmp/spr_lifecounter_0.bmp"
  cherryTexture <- loadBMP' "assets/bmp/spr_cherry_0.bmp"
  bannerTexture <- loadBMP' "assets/bmp/spr_banner_0.bmp"
  return Textures
    { textureLifeCounter  = lifeCounter
    , textureCherry       = cherryTexture
    , texturesGhost       = [ (0, blinkyTextures)
                            , (1, inkyTextures)
                            , (2, pinkyTextures)
                            , (3, clydeTextures)
                            ]
    , texturesGhostAfraid = afraidTextures
    , texturesPacMan      = pacManTextures
    , texturesPacManDeath = deathTextures
    , textureBanner       = bannerTexture
    }
