## This file is intended to set the working directory and load all required packages for all scripts.

baseDir <- 'C:/Users/adroghini/Documents/bering-invaders/PMEL-Models'
setwd(baseDir)

# Load packages

require(plyr)

require(doSNOW) ## For parallel processing of the run length encoding function.
require(dplyr)

require(ncdf4)

require(marmap)

require(raster)
require(rgdal)
require(rgeos)

require(sp)

require(tidyverse)

require(viridis) ## color ramp that is perceived as uniform across its range.
				## https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html
