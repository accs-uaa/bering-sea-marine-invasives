# Author: A.S. Fischbach (afischbach@usgs.gov)
# A. Droghini (adroghini@alaska.edu)
# With insights on base r's rle function and parallel processing from Vijay Patil (vpatil@usgs.gov), 16 Febuary 2017

# Intent:
# 1) In a forLoop read in each of the ROMS model extracts as spatialGridDataFrames for both temperature and salinity.
# 2) Step through each marine invasive taxa to evaluate habitat suitability for
# 2a) overall survival conditions throughout the year;
# 2b) the number of survival condition weeks during each model year;
# 2c) the reproductive conditions (for taxa with documented reproductive parameters).
# 3) For each evaluation, generate a raster output and a cartographic ouput.
#
#See ReadMe for file dependencies

##start up requirements
require(sp)
require(raster)
require(viridis)## Thanks to Jordan Watson at NOAA for a color ramp that is perceived as uniform across its range.
				## https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html
require(doSNOW) ## For parallel processing of the run length encoding function.
#CPUs <- 10
#
.libPaths()  
si <- sessionInfo()
if(si$R.version$os != "linux-gnu"){
	baseDir <- 'D:/HelpingOthers/ABSIlcc/NFWF2016/Models'
	}else{
	baseDir <- '/home/afischbach/NFWF2016/Models'
	}
#
setwd(file.path(baseDir, 'rCode'))

## Build functions to evaluate length of consecutive weeks of suitable habitat.
newRLEfunction <- function(x){
					# vv <- values(x)
					# vv[is.na(vv)] <- 0
					# values(x) <- vv
					RLE <- rle(x) 	## summarize runs lengths using the run length encoding function
					if(1 %in% RLE$values ){
						nConsecutiveWeeksGrowth<-max(RLE$lengths[which(RLE$values == 1)]) ## extract the runs of weeks of habitat
					}else{
						nConsecutiveWeeksGrowth <- 0
					}
					#as.numeric(max(nConsecutiveWeeksGrowth) >= nWeeks) ## does the maximum run of habitat weeks exceed the threshold?
					return(nConsecutiveWeeksGrowth)
				}
				#
ConsecutiveWeeksofHabitatCalc <- function(rb){
									calc(stack(rb), fun=newRLEfunction)
									}
									#

## system.time(r_y_OK <- ConsecutiveWeeksofHabitatCalc(s_y_OK) ) 	##for development. Examine computation time 
																	## using the s_y_OK rasterStack built by hand processing
																	
################################################################################
## USED for PLOTTING ###########################################################
### Base coastal map
prj.geo<-CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0')
prj.StudyArea <- CRS("+proj=aeqd +lat_0=55 +lon_0=-170 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") 
ContinentsDataFile<-file.path(baseDir, 'rCode/Continents.rData') 
if(!file.exists(ContinentsDataFile)){
	require(rgdal)
	dsn<-"D:/Walrus/BaseMaps/ESRI_DATA/Continents.shp" ## replace with an OpenSource version for this. NaturalEarth probably has some??
	ogrInfo(dsn, layer='Continents')
	landAll <- readOGR(dsn, layer='Continents')
	landAll <- landAll[c(1:3),]                                 # Keep northern continents only for viewing purposes
	# Define projections
	land <- spTransform(landAll, prj.StudyArea)                           # Transform from WGS84 to polar stereographic projection
	land <- as(land, "SpatialPolygons")                         # Strip out the dataframe
	iceColors <- colorRampPalette(colors=c('aliceblue', 'darkblue'), space='rgb')(12)
	rm(list=c('landAll'))
	save(land, file = ContinentsDataFile)
}else{
	load(ContinentsDataFile)
}
ports <- read.csv(file = file.path(baseDir, 'rCode/roms_habitat/ports.csv'), na.strings='', stringsAsFactors = F)
ports <- ports[!(ports$IATA %in% c('KQA', 'UDA')), ]
coordinates(ports) <- c('x','y')
proj4string(ports) <- prj.geo
ports.prj <- spTransform(ports, prj.StudyArea)
landColor <- 'seashell2' ##'antiquewhite4' ## 'yellow4' ## '#574835'#'burlywood4'
portColor <- 'black' ##'yellow2'
temperature.colors <- plasma(10) ## magma(10)
## END USED for PLOTTING #######################################################
################################################################################


#read files
SpToler<-read.csv(file.path(baseDir, 'rCode/roms_habitat/Species_Tolerances.csv'))  ## Read species tolerance file
SpToler$Sci_Name <- make.names(SpToler$Sci_Name) ## Ensure taxa names are r-friendly

## Restrict consideration to taxa with full information on T-S habitat space
SpToler <- SpToler[!is.na(SpToler$Min_TempC) & !is.na(SpToler$Max_TempC) & !is.na(SpToler$Min_Salinity) & !is.na(SpToler$Max_Salinity), ]

## Gather a list of the model data files
ff<-list.files(path = file.path(baseDir, 'rData'), pattern='.rData') 
ff<-sort(ff[ff != "portData.rData" ]) ## drop port data
#
##iterate through the list of model datasets (these datasets come in pairs, so iterate by 2's) 
for(i in seq(from = 1, to = length(ff), by = 2)){ 
	f = ff[i]
	Model <- unlist(strsplit(unlist(strsplit(f, ".rData")), '_'))[2] ##create character string for model name
		if(Model=="CCCma"){
			ModelName<-"CGCM3-t47"
		}else if(Model=="MIROC"){
			ModelName<-"MIROC3.2"
		}else{
			ModelName<-"ECHO-G"
		}
	modelPeriod <- paste(Model,unlist(strsplit(unlist(strsplit(f, ".rData")), '_'))[3]) ##create character string for model + time period
	model_Period <- modelPeriod
	#modelPeriod <- paste(unlist(strsplit(unlist(strsplit(f, ".rData")), '_'))[2:3], collapse='_') 
	#model_Period <- paste(unlist(strsplit(unlist(strsplit(f, ".rData")), '_'))[2:3], collapse=' ')
	cat(i, '=== Handling ===', f, fill =T) 
	
	load(file.path(baseDir, 'rData', f))
	sgdf.S<-sgdf.Max 	## cast this as the salinity spatialGridDataFrame
	rm('sgdf.Max')
	f = ff[i+1] ## grab the next file name
	load(file.path(baseDir, 'rData', f))
	sgdf.T <- sgdf.Max 	## cast this as the temperature spatialGridDataFrame
	rm('sgdf.Max')		
	yy <- as.numeric(sort(unique(substring(names(sgdf.T@data), 1,4))))  ## Extract the years from the spatial grid dataframe columns
	if(yy[1] == 2029){
		yy <- yy[2:11] ##for forecast, restrict to years 2030-2039
	}else{
		yy <- yy[1:10] ##for hindcast, restrict to years 2003-2012
	}
	#
	for(T in 1:nrow(SpToler)){ ## iterate through taxa
		taxa <- SpToler[T, ]$Sci_Name
		## Survival aspect
		## retain cell for which no week was outside of the survival habitat bounds
		## set min. & max. T-S thresholds
			minSalinity.S <- SpToler[T, ]$Min_Salinity
			maxSalinity.S <- SpToler[T, ]$Max_Salinity
			minTemp.S <- SpToler[T, ]$Min_TempC
			maxTemp.S <- SpToler[T, ]$Max_TempC
		for(y.S in yy){ ## iterate through the years to evaluate survival conditions
			#y.S <- yy[1]
			ColumnsToSelect.T <- which(substring(names(sgdf.T@data), 1,4) == y.S)  	## Find columns in spatial grid dataframe from the specified year, y.S
			s_T_y <- raster::stack(sgdf.T[, , ColumnsToSelect.T]) ## Select temperature sgdf for this year and cast as a raster stack
			ColumnsToSelect.S <- which(substring(names(sgdf.S@data), 1,4) == y.S)  	## Find columns in spatial grid dataframe from the specified year, y.S
			##Need to consider separately from sgdf.T because of different column #s
			s_S_y <- raster::stack(sgdf.S[, , ColumnsToSelect.S]) ## Select salinity sgdf for this year and cast as a raster stack
			##For each week and pixel, evaluate temp. values against taxa tolerances
			s_T_y_MinTemp <- s_T_y >= minTemp.S  ## return 1 if temp >= taxon min. limit, else 0
			s_T_y_MaxTemp <- s_T_y <= maxTemp.S  ## return 1 if temp <= taxon max. limit, else 0
			s_T_y_OK <- s_T_y_MinTemp * s_T_y_MaxTemp ## Product of the above results: 1 both criteria met, else 0
			##Evaluate ocean salinity values against taxa tolerances
			s_S_y_MinSal <- s_S_y >= minSalinity.S ## return 1 if salinity >= taxon min. limit, else 0
			s_S_y_MaxSal <- s_S_y <= maxSalinity.S ## return 1 if salinity <= taxon max. limit, else 0
			s_S_y_OK <- s_S_y_MinSal * s_S_y_MaxSal ## Product of the above results: 1 both criteria met, else 0
			s_y_OK <- s_T_y_OK * s_S_y_OK  ## Overall T-S suitability as a stack of weekly values for the year
			rm(list = c('ColumnsToSelect.T', 'ColumnsToSelect.S', 's_T_y', 's_S_y', 's_T_y_MinTemp', 's_T_y_MaxTemp', 's_T_y_OK', 's_S_y_MinSal', 's_S_y_MaxSal', 's_S_y_OK'))
			r_y_OK.S <- min(s_y_OK) ## For each pixel, determine 'habitat suitability' across entire year by collapsing weekly values into a single RasterLayer. If 1 week within the year had a value of 0, pixel value = 0
			names(r_y_OK.S) <- paste0('year', y.S, '_fullYearHabitat')
			r_y_OK.N <- sum(s_y_OK) ## Sum the number of weeks with suitability value = 1
			names(r_y_OK.N) <- paste0('year', y.S, '_nweeksHabitat')
			## Save the data for this taxa /year / model
			(name<- paste(taxa, '_Weeks_Survival_', Model, '_', y.S, '.rData', sep=''))	
			fileName <- file.path(file.path(baseDir, 'rOut', 'SurvivalWeeks_which', name))# file name of raster stack of weeks of survival for this taxa/year/model
			save(s_y_OK, file = fileName)
			 cat('..... --> Saving', fileName, fill=T)
			rm(s_y_OK)
			
			if(y.S == yy[1]){ ##if first year, create stack
				s_OK.S <- stack(r_y_OK.S)
				s_nWeeks.S <- stack(r_y_OK.N)
			}else{ ##if 2+ year in loop, add to first year
				s_OK.S <- stack(s_OK.S, r_y_OK.S)
				s_nWeeks.S <- stack(s_nWeeks.S, r_y_OK.N)
			}
		}
		###################################
		####Create overall suitability maps (weekly and year-round) for taxa under consideration
		cat('\nN Weeks survival', fill = T)
			s_nWeeks.S
		## make a plot of the mean number of weeks with suitable survival.
		e <-  extent(s_nWeeks.S)
		## The following lines adjust the extent of the resultant plot to best show the Bering Sea.
		e[1] <- e[1] + 400*1000
		e[2] <- e[2] - 375*1000
		e[3] <- e[3] + 175*1000
		e[4] <- e[4] - 350*1000
		name<- paste(taxa, '_Survival_', modelPeriod, '.png', sep='')
		fileName<-file.path(file.path(baseDir, 'plots', 'taxa', 'SurvivalWeeks', name))
		plotResX = 2500 
		plotResY = 1.04*(plotResX * ((e[4] - e[3])/ (e[2] - e[1])))
		png(fileName, width = plotResX , height = plotResY)
			par(bg = 'white') #
			maxWeeks <- max(values(s_nWeeks.S), na.rm=T)
			if(maxWeeks < 49){
				maxWeeks <- 52
			}
			arg <- list(at=seq(from=0, to = maxWeeks, by = 10), labels=seq(from=0, to = maxWeeks, by = 10), cex = 4, 
			cex.axis = 3, col.axis = 'black', col.ticks = 'black', col = 'black', line = -0.5)
			plot(mean(s_nWeeks.S), col.main = 'black', main = paste0(taxa,'    N Weeks Survival    (model: ', model_Period, ')'), cex.main = 2.9, 
				breaks = seq(from=0, to = maxWeeks+1, by = 2),  
				xaxt='n', yaxt='n', bty='n', col = plasma(ceiling(maxWeeks +1 )/2), ext=e , cex = 4,
				axis.args=arg, box=FALSE)
			plot(land, col=landColor, border = landColor, add = T)
			text(y = rep(e[4] - 170*1000, 2), x = c(e[1] + 750*1000, e[2] - 300*1000), 
					labels = c('Russia', 'Alaska'), col = 'darkgray', cex = 4, font = 2, family = "sans")
			points(ports.prj, cex=1, pch=19, col=portColor)
			text(ports.prj, labels = ports@data$IATA, pos = 4, col=portColor, font = 2, cex = 3, family = 'sans',halo = TRUE, hw = 0.4, hc = "white") #add white halo to make black text stand out
			#
		dev.off()
		
		## Save the data for this taxa
		(name<- paste(taxa, '_N_Weeks_Survival_', modelPeriod, '.rData', sep=''))
		fileName<-file.path(file.path(baseDir, 'rOut', 'SurvivalWeeks', name))
		cat('..... --> Saving', fileName, fill=T)
		save(s_nWeeks.S, file=fileName)
		
		## make a plot of total number of years with year-round survival
		###################################
		cat('full year survival', fill = T)
			s_OK.S
		plotName <- (name<- paste(taxa, '_Survival_', modelPeriod, '.png', sep=''))
		fileName<-file.path(file.path(baseDir, 'plots', 'taxa', 'Survival', name))
		png(fileName, width = plotResX , height = plotResY)
			par(bg = 'white')
			arg <- list(at=seq(from = 0, to = length(yy), by = 2), labels=seq(from = 0, to = length(yy), by = 2), cex = 3, 
			cex.axis = 3, col.axis = 'black', col.ticks = 'black', col = 'black', line = -0.5)
			plot(sum(s_OK.S), main = paste0(taxa, '    Survival    (model: ', model_Period, ')'), col.main = 'black', cex.main = 2.9,
					breaks =0:length(yy), col=temperature.colors[1: length(yy)], 
					xaxt='n', yaxt='n', bty='n', ext=e , cex = 3, axis.args=arg, box=FALSE)
			plot(land, col=landColor, border = landColor, add = T)
			text(y = rep(e[4] - 170*1000, 2), x = c(e[1] + 750*1000, e[2] - 300*1000), labels = c('Russia', 'Alaska'), col = 'darkgray', cex = 4, font = 2, family = "sans")
			points(ports.prj, cex=1, pch=19, col=portColor)
			text(ports.prj, labels = ports@data$IATA, pos = 4, col=portColor, font = 2, cex = 3, family = 'sans',halo = TRUE, hw = 0.4, hc = "white")
			#
		dev.off()
		## Save the data for this taxa
		(name<- paste(taxa, '_Survival_', modelPeriod, '.rData', sep=''))
		fileName<-file.path(file.path(baseDir, 'rOut', 'Survival', name))
		cat('..... --> Saving', fileName, fill=T)
		save(s_OK.S, file=fileName)
		###################################

		#####################Reproduction Loop
		if(!is.na(SpToler[T,]$Min_ReproSal) & !is.na(SpToler[T,]$Max_ReproSal) & !is.na(SpToler[T,]$Min_ReproTempC) & !is.na(SpToler[T,]$Max_ReproTempC)){
			minSalinity.R <- SpToler[T, ]$Min_ReproSal
			maxSalinity.R <- SpToler[T, ]$Max_ReproSal
			minTemp.R <- SpToler[T, ]$Min_ReproTempC
			maxTemp.R <- SpToler[T, ]$Max_ReproTempC
			#nWeeks <- 6 	##Set arbitrary six-week value for 'growing season'. This is the number of consecutive weeks in a year that need to have suitable T-S reproductive values. No longer using this - having the code report back the number of weeks rather than set arbitrary threshold
			cat(T, '\n::: Processing', taxa, 
					'\nwhich needs a temperature range of', minTemp.R, 'to',maxTemp.R, 
					'\nand a salinity range of', minSalinity.R, 'to', maxSalinity.R, '\n')
			#Start parallel processing using DoSNOW
			NumberOfCluster = length(yy)		## set number of clusters equal to the number of years being processed
												## We have 24 on the walrus workstation
			# NumberOfCluster <- CPUs				## Set number of clusters equal to number of available CPUs
			cl <- makeCluster(NumberOfCluster) 	## Make clusters, Number of Clusters is defined as the number of years to be processed
			registerDoSNOW(cl)					## DoSNOW housekeeping
			## Use DoSNOW to parallel process the years of data.
			## This opperates on a numeric vector of years (yy), 
			## sends the function defined in the {} brackets after the %dopar% pipe to the clusters for processing,
			## enables the package 'raster' on each cluster,
			## and combines the results from the clusters using the raster::stack function
			s_OK.Repro <- foreach(y = yy, .combine = 'stack', .packages = 'raster') %dopar% { ## 
				ColumnsToSelect.T <- which(substring(names(sgdf.T@data), 1,4) == y)  	## Examine the spatial grid data frame column names, 
				## finding those from the specified year, y
				s_T_y <- raster::stack(sgdf.T[, , ColumnsToSelect.T]) ## Select temperature sgdf for this year and cast as a raster stack
				ColumnsToSelect.S <- which(substring(names(sgdf.S@data), 1,4) == y)  	## Examine the spatial grid data frame column names, 
				## finding those from the specified year, y
				s_S_y <- raster::stack(sgdf.S[, , ColumnsToSelect.S]) ## Select salinity sgdf for this year and cast as a raster stack
				## evaluate the weeks of temperature valuus against the taxa tolerances...
				s_T_y_MinTemp <- s_T_y >= minTemp.R  ## Evaluate the stack of weekly Temperature values against minimum temp, returning 1 or 0
				s_T_y_MaxTemp <- s_T_y <= maxTemp.R  ## Evaluate the stack of weekly Temperature values against maximum temp, returning 1 or 0
				s_T_y_OK <- s_T_y_MinTemp * s_T_y_MaxTemp ## Product of the above results: 1 both criteria met, 0 otherwise
				## evaluate the weeks of salinity values against the taxa tolerances...
				s_S_y_MinSal <- s_S_y >= minSalinity.R  ## Evaluate the stack of weekly Salinity values against minimum salinity, returning 1 or 0
				s_S_y_MaxSal <- s_S_y <= maxSalinity.R  ## Evaluate the stack of weekly Salinity values against maximum salinity, returning 1 or 0
				s_S_y_OK <- s_S_y_MinSal * s_S_y_MaxSal ## Product of the above results: 1 both criteria met, 0 otherwise
				s_y_OK <- s_T_y_OK * s_S_y_OK  ## Overall suitability for both Salinity and Temp as a stack of weekly values for the year.
				rm(list = c('ColumnsToSelect.T', 'ColumnsToSelect.S', 's_T_y', 's_S_y', 's_T_y_MinTemp', 's_T_y_MaxTemp', 's_T_y_OK', 's_S_y_MinSal', 's_S_y_MaxSal', 's_S_y_OK'))
				r_y_OK <- ConsecutiveWeeksofHabitatCalc(s_y_OK)  ## Determine if a minimum number of consecutive weeks of suitable habitat were present
				rm(s_y_OK)
				return(r_y_OK) ## Return this raster of habitat suitability for this year
			}## year doSNOW::foreach loop
			stopCluster(cl) ## release the cluster
			names(s_OK.Repro) <- paste0('ContiguousReproWeeks_year_', yy)
			s_OK.Repro ## print the summary of the annual stack of 
			
			## make a plot of the years of Reproductive invasion potential
			plotName <- (name<- paste(taxa, '_Reproduction_', modelPeriod, '.png', sep=''))
			fileName<-file.path(file.path(baseDir, 'plots', 'taxa', 'Reproduction', name))
			png(fileName, width = plotResX , height = plotResY)
				par(bg = 'white') ##'black'
				arg <- list(at=seq(from = 0, to = length(yy), by = 2), labels=seq(from = 0, to = length(yy), by = 2), cex = 3, 
				cex.axis = 3, col.axis = 'black', col.ticks = 'black', col = 'black', line = -0.5)
				plot(mean(s_OK.Repro), main = paste0(taxa, '    Reproduction    (model: ', model_Period, ')'), col.main = 'black', cex.main = 2.9,
					breaks =0:length(yy), col=temperature.colors[1: length(yy)], 
					xaxt='n', yaxt='n', bty='n', ext=e , cex = 3, axis.args=arg, box=FALSE)
				plot(land, col=landColor, border = landColor, add = T)
				text(y = rep(e[4] - 170*1000, 2), x = c(e[1] + 750*1000, e[2] - 300*1000), labels = c('Russia', 'Alaska'), col = 'darkgray', cex = 4, font = 2, family = "sans")
				points(ports.prj, cex=1, pch=19, col=portColor)
				text(ports.prj, labels = ports@data$IATA, pos = 4, col=portColor, font = 2, cex = 3, family = 'sans',halo = TRUE, hw = 0.4, hc = "white")
				#
			dev.off()
			## Save the data for this taxa
			(name<- paste(taxa, '_Reproduction_', modelPeriod, '.rData', sep=''))
			fileName<-file.path(file.path(baseDir, 'rOut', 'Reproduction', name))
			cat('..... --> Saving', fileName, fill=T)
			save(s_OK.Repro, file=fileName)
		} ## End Reproduction loop
	} ## taxa loop
} ## model loop
##

##Workspace clean-up
rm(T,taxa,sgdf.S,sgdf.T,s_OK.S,s_OK.Repro,s_nWeeks.S,r_y_OK.N,r_y_OK.S,nWeeks,NumberOfCluster,name,modelPeriod,ModelName,model_Period,Model,minTemp.S,minTemp.R,minSalinity.S,minSalinity.R,maxWeeks,maxTemp.S,maxTemp.R,maxSalinity.S,maxSalinity.R,i,fileName,ff,f,cl,SpToler,y.S,yy,ConsecutiveWeeksofHabitatCalc,newRLEfunction)
	
