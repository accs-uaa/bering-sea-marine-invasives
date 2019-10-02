## This file is intended to set the working directory and load all required packages for all scripts.

baseDir <- 'C:/Users/adroghini/Documents/bering-invaders/PMEL-Models'
setwd(file.path(baseDir))

# Load packages

require(doSNOW) ## For parallel processing of the run length encoding function.

require(raster)
require(rgdal)
require(rgeos)

require(sp)

require(viridis) ## Thanks to Jordan Watson at NOAA for a color ramp that is perceived as uniform across its range.
				## https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html