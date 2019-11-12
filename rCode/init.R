## This file is intended to set the working directory and load all required packages for all scripts.

baseDir <- 'C:/Users/adroghini/Documents/bering-invaders/PMEL-Models'
setwd(baseDir)

# Load packages

require(plyr)

require(circlize)

require(doSNOW) ## For parallel processing of the run length encoding function.
require(dplyr)

require(ggplot2)

require(ncdf4)

require(marmap)

require(raster)
require(RColorBrewer)
require(rgdal)
require(rgeos)

require(sp)

require(tidyverse)
