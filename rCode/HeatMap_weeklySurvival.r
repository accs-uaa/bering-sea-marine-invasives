# D:\HelpingOthers\ABSIlcc\NFWF2016\Models\rCode\HeatMap_weeklySurvival.r
# library(geosphere)
# library(sf)
require(sp)
require(raster)
require(rgdal)
require(dplyr)
prj <- "+proj=aeqd +lat_0=55 +lon_0=190 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"
prj.geo <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
# # Build latitudinal bands in sp
# South <- 45
# North <- 66 
# gg <- data.frame(south_lat = seq(from = South, to = North-1, by = 1), north_lat = seq(from = South+1, to = North, by = 1))
# gg$geohash <- as.integer(rownames(gg))
# gg$east_lng <- -150
# gg$west_lng <- 160


## build function to create many small line segments from west to east for each row...
## Thanks to https://stackoverflow.com/questions/44335246/polygons-from-coordinates
# lst <- lapply(1:nrow(gg), function(x){
  # ## create a matrix of coordinates that also 'close' the polygon
  # res <- matrix(c(gg[x, 'west_lng'], gg[x, 'south_lat'], 
				# gg[x, 'east_lng'], gg[x, 'south_lat'],
				# gg[x, 'east_lng'], gg[x, 'north_lat'],
				# gg[x, 'west_lng'], gg[x, 'north_lat'],
				# gg[x, 'west_lng'], gg[x, 'south_lat'])  ## need to close the polygon
         # , ncol =2, byrow = T
  # )
  # ## create polygon objects
  # # st_polygon(list(res))
  # makePoly(res, interval = 100000)
  # }
# )
# #
latGrid <- spTransform( readOGR("D:/HelpingOthers/ABSIlcc/NFWF2016/Models/GIS/zonestats/final_poly_zonal.shp"), 
				 prj
				 )
# Read in the shelfRater (1 = 				 
load('D:/HelpingOthers/ABSIlcc/NFWF2016/Models/GIS/shelfRast.rData')				 
# ## st_sfc : creates simple features collection
## st_sf : creates simple feature object
plot(shelfRast)
plot(latGrid, add =T)
# plot(s_y_OK[[1]], add = T)
# plot(latGrid, add =T)

baseDir <- "D:/HelpingOthers/ABSIlcc/NFWF2016/Models"
dataDir <- file.path(baseDir, "rOut/SurvivalWeeks_which")
ff <- list.files(dataDir, full.names = T )
maxWeeks <- 53
for(f in ff){ ## debugging [10:13]
	#f <- ff[10]
	cat(f, fill = T)
	load(f)
	# remove deep water and land by 
	# multiplying the s_y_OK raster by the shelfRast raster that has a 0 for non-shelf pixels
	ee <- extract(s_y_OK*shelfRast, latGrid, fun = mean, na.rm =T)
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
		df_ee$studyPeriodOne <- (y < 2020)
		
	} else{		
		dim(ee)
		df_t <- ee
		df_t$lat <- latGrid@data$lat
		fsplit <- unlist(strsplit(basename(f), '_'))
		df_t$taxa <- fsplit[1]
		df_t$model <- fsplit[4]
		y <- as.numeric(substr(x = fsplit[5], start = 1, stop =4))
		df_t$studyPeriodOne <- (y < 2020)
		dim(df_t)
		df_ee <- rbind(df_ee, df_t)
		rm(df_t)
	}
}
save(list = "df_ee", file = file.path(baseDir, '/heatMap/week_survival_lat_taxa_model_studyPeriod.rData'))
## Use dplyr to take mean across years within model, study period and taxa.

## Use dplyr to take mean across study period and taxa.
	
heatmap(ee, Rowv = NA, Colv = NA)

