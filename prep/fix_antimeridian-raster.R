library(raster)
library(leaflet)

# define raster that exceeds [-180,180]
r <- raster(xmn = 165, xmx = 185, ymn = -50, ymx = -31, crs = "+init=epsg:4326", nrows = 50, ncols = 50)
r[] <- rnorm(ncell(r))
r # extent: 165, 180, -50, -31  (xmin, xmax, ymin, ymax)

# map with > 180 chopped off
leaflet() %>% addTiles() %>% addRasterImage(r) %>% addGraticule()

# option A: rotate raster
a = rotate(r)

# map rotated raster
leaflet() %>% addTiles() %>% addRasterImage(a) %>% addGraticule()

# option B: shift and bisect into raster pieces left and right of dateline to minimize size
b  <- shift(r, -360)
b1 <- crop(b, extent(-180,180,-90,90), snap="in") %>% trim()
b2 <- crop(b, extent(-360,-180,-90,90), snap="in") %>% shift(360) %>% trim()

# map with bisected chunks, worldCopyJump = TRUE
leaflet(options=leafletOptions(worldCopyJump = TRUE)) %>% 
  addTiles() %>%
  addRasterImage(b1) %>%
  addRasterImage(b2) %>%
  addGraticule()

# inspect:    (xmin,  xmax, ymin, ymax);           (nrow, ncol, ncell) 
r  # extent:   165  ,  180  ,  -50, -31 ; dimensions : 50,  50,  2500
a  # extent : -179.8,  180.2,  -50, -31 ; dimensions : 50, 900, 45000
b  # extent : -195  , -180  ,  -50, -31 ; dimensions : 50,  50,  2500    
b1 # extent : -179.8, -175  ,  -50, -31 ; dimensions : 50,  12,   600
b2 # extent :  165  ,  179.8,  -50, -31 ; dimensions : 50,  37,  1850

# get bounding box on bigger raster
bb = extent(b2) %>% as.matrix()

# map with bisected chunks, worldCopyJump = TRUE, fitBounds()
leaflet(options=leafletOptions(worldCopyJump = TRUE)) %>% 
  addTiles() %>%
  addRasterImage(b1, opacity=.5) %>%
  addRasterImage(b2, opacity=.5) %>%
  addGraticule() %>%
  fitBounds(bb["x","min"], bb["y","min"], bb["x","max"], bb["y","max"])

