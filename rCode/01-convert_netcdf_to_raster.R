# Author: A.S. Fischbach (afischbach@usgs.gov)
# Intent:
#	1) Ingest the netCDF files provided by AXIOM.
#	2) Take the maximum value from the ocean layers provided after recoding NA values and dropping NA's from the maximum calculation.
#	3) Cast output as a SpatialGridDataFrame and save as an rData file.

# Dependency:
# 1) The netCDF ROMS data must first placed in the ..\netCDF directory.
# 2) The following directory structure must be in place.
# ..\Models\netCDF [contains netCDF files]
# ..\Models\rCode [contains scripts]
# ..\Models\rData [contains ROMS data cast as spatialGridDataFrames]

#### Set-up and load packages----
baseDir <- 'D:/HelpingOthers/ABSIlcc/NFWF2016/Models/'
setwd(file.path(baseDir, 'rCode'))
require(raster)
require(ncdf4)
require(sp)
require(rgdal)
require(doSNOW)

## Define projections
prj.StudyArea <- CRS("+proj=aeqd +lat_0=55 +lon_0=-170 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
prj.StudyAreaPositive <- CRS("+proj=aeqd +lat_0=55 +lon_0=190") # +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") #Lambert -170, 70
prj.geo<-CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0')

#### Convert netCDF to SGDF----

## Parallel processing set up
NumberOfCluster = 12     			## use 1/2 the CPU's. We have 24 on the walrus workstation
cl <- makeCluster(NumberOfCluster)

## List netCDF files
ff<-list.files(path = file.path(baseDir, 'netCDF'), pattern = '.nc')
cat(ff, fill = T)
vars<-c('sea_water_temperature', 'salinity')

## Process one at a time and convert to sGDF
for (f in ff){
	nc <- nc_open( filename = file.path(baseDir, 'netCDF', f), write=FALSE, readunlim=TRUE, verbose=T,
		auto_GMT=TRUE, suppress_dimvals=FALSE )
	
	for(Var in vars){ ## handle both variables.
	  st <- Sys.time()
	  TimeSteps <- data.frame(TIME=ncvar_get(nc, varid='TIME'))
	  TimeSteps$Date<-as.POSIXct(TimeSteps$TIME, tz='UTC',
	                             origin = as.POSIXct('01-Jan-1900 00:00:00', 
	                                                 format='%d-%b-%Y %H:%M:%S', tz='UTC'))
	  TimeSteps$Week<-format(TimeSteps$Date, '%Y_%V_%d')
	  
	  registerDoSNOW(cl)					## DoSNOW housekeeping
	  
	  sgdf.Max <- foreach(i = 1:nrow(TimeSteps), .combine = 'cbind', .packages = 'raster') %dopar% {
	    b<-brick(file.path(baseDir, 'netCDF', f), 	varname =  Var, lvar = 4, level = i) ##, levels...DEPTH
	    b[values(b)== -1e+34]<-NA  	## remove the NA values
	    bMax <- max(b, na.rm=T)  	## summarize each point in the raster, take the maximum temperature and cast result as a raster.
	    bMax.prj <- rastser::projectRaster(from=bMax, res = 10000, method='ngb', crs= prj.StudyAreaPositive, over=T, na.rm=T)
	    sgdf.Max<- as(bMax.prj, 'SpatialGridDataFrame')
	    names(sgdf.Max)<-TimeSteps[i,]$Week
	    return(sgdf.Max)
	  } ## time step
	  stopCluster(cl) ## release the cluster
	  
	  fileName<-file.path(file.path(baseDir, 'rData', paste0(unlist(strsplit(f, ".nc")), '_', Var,  '.rData')))
	  cat('..... --> Saving', fileName, fill=T)
	  save(sgdf.Max, file=fileName)
	  td <- Sys.time() - st
	  units(td) <- 'mins'
	  cat('... this took', round(td, 1), 'mins', fill = T)
	}
}
#
