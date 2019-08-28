# 03-weekly_survival-heat_map_by_latitude.R
# library(geosphere)
# library(sf)
require(sp)
require(raster)
require(rgdal)
require(dplyr)
prj <- "+proj=aeqd +lat_0=55 +lon_0=190 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"
prj.geo <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"


### Load data files 

# Read in latitudinal band file
# The file was derived from Natural Earth's 1 degree graticules 
#(https://www.naturalearthdata.com/downloads/10m-physical-vectors/10m-graticules/). We transformed the latitude lines into polygons. This required manual editing in ArcGIS because we originally obtained odd results when converting lines to polygons across the international date line.
latGrid <- spTransform( readOGR("D:/HelpingOthers/droghini/sea-invaders/GIS/zonestats/final_poly_zonal.shp"), 
				 prj)
# Read in our study area shapefile (Bering Sea shelf)	
# File is coded as 1 = shelf, 0 = non-shelf (deep water or land)		 
load('D:/HelpingOthers/droghini/sea-invaders/GIS/shelfRast.rData')				 
# ## st_sfc : creates simple features collection
## st_sf : creates simple feature object
plot(shelfRast)
plot(latGrid, add =T)

# Load taxa data
# Output from 02-analysis-habitat_suitability.R
baseDir <- "D:/HelpingOthers/droghini/sea-invaders"
dataDir <- file.path(baseDir, "rOut/SurvivalWeeks_which") 
ff <- list.files(dataDir, full.names = T )

### Determine weekly survival by latitude
maxWeeks <- 53
for(f in ff){ 
	#f <- ff[10]
	cat(f, fill = T)
	load(f)
	# remove deep water and land by 
	# multiplying the s_y_OK raster by the shelfRast raster that has a 0 for non-shelf pixels
	#ee <- extract(s_y_OK*shelfRast, latGrid, fun = mean, na.rm =T)
	ee <- raster::extract(s_y_OK*shelfRast, latGrid, fun = sum, na.rm =T)
	
	# Make determination of the presence of 'survival habitat' within this latitudinal band.
	# If ee >= 1 pixel, we determine that survival habitat exists in this band, for this modeled week
	ee[ee < 1] <- 0
	ee[ee >= 1] <- 1
	
	cat(nc <- ncol(ee), fill =T) ## should be 15 rows of 50 columns
	if(nc < maxWeeks){
		matrixExtra <- matrix(data = NA, nrow = 16, ncol = (maxWeeks - nc))
		ee <- cbind(ee, matrixExtra)
	}
	ee <- data.frame(ee)
	names(ee) <- sprintf("Week_%02d", 1:maxWeeks)
	if(f == ff[1]){
		cat('---First---', fill = T)
		df_ee <- ee
		df_ee$lat <- as.numeric(as.character(latGrid@data$lat))
		fsplit <- unlist(strsplit(basename(f), '_'))
		df_ee$taxa <- fsplit[1]
		df_ee$model <- fsplit[4]
		y <- as.numeric(substr(x = fsplit[5], start = 1, stop =4))
		df_ee$year <- y
		df_ee$studyPeriodOne <- (y < 2020) #Study periods are: 2003-2012 and 2030-2039
		
	} else{		
		dim(ee)
		df_t <- ee
		df_t$lat <- latGrid@data$lat
		fsplit <- unlist(strsplit(basename(f), '_'))
		df_t$taxa <- fsplit[1]
		df_t$model <- fsplit[4]
		y <- as.numeric(substr(x = fsplit[5], start = 1, stop =4))
		df_t$year <- y
		df_t$studyPeriodOne <- (y < 2020)
		dim(df_t)
		df_ee <- rbind(df_ee, df_t)
		rm(df_t)
	}
}
save(list = "df_ee", file = file.path(baseDir, '/rOut/week_survival_lat_taxa_model_studyPeriod.rData'))
# see 05-figure-weeklySurvival. for plotting

