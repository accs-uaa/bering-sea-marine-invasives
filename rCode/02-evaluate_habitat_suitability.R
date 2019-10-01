# Last updated: 30 Sep 2019

# Authors: A.S. Fischbach (afischbach@usgs.gov)
# 				 A. Droghini (adroghini@alaska.edu)
# With insights on base r's rle function and parallel processing from Vijay Patil (vpatil@usgs.gov), 16 Febuary 2017

# Intent:
# 1) In a forLoop read in each of the ROMS files. Files exist as spatialGridDataFrames. One for each model, study period (2003-2012, 2029-2039), and ocean variable (temperature, salinity).
# 2) Step through each marine invasive taxa to evaluate habitat suitability for:
# 2a) year-round survival conditions;
# 2b) the number of survival condition weeks during each model year;
# 2c) the reproductive conditions (for taxa with documented reproductive parameters).
# 3) For each evaluation, generate a raster output and a cartographic output.
#
# See ReadMe for file dependencies

#### Start up requirements----

# Load required packages
require(sp)
require(raster)
require(viridis)## Thanks to Jordan Watson at NOAA for a color ramp that is perceived as uniform across its range.
				## https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html
require(doSNOW) ## For parallel processing of the run length encoding function.

# Set working directory
baseDir <- getwd()

#### Prepare data files ----

## Source functions for calculating consecutive number of weeks
source('rCode/functionConsecutiveWeeks.R')

## Read in NIS physiological tolerances
SpToler <- read.csv('rData/Species_Tolerances.csv')
SpToler$Sci_Name <- make.names(SpToler$Sci_Name) ## Ensure taxa names are R-friendly

# Include only NIS that have full range of temperature and salinity thresholds
SpToler <- SpToler[!is.na(SpToler$Min_TempC) & !is.na(SpToler$Max_TempC) & !is.na(SpToler$Min_Salinity) & !is.na(SpToler$Max_Salinity), ]

## List ROMS data files
ff <- list.files(path = file.path(baseDir, 'PMEL-Models/rData'), pattern='.rData')
ff <- sort(ff[ff != "portData.rData" ]) ## drop port data

#### Determine habitat suitability----

# Iterate through list of model files
# Data come in pairs - one file for temperature, one for salinity, for each model-study period combination - so iterate by 2's

# For each model-study period combination, extract model information from file name:
# 1. Rename Model acquired from file name to proper Model name
# 2. Obtain study period from file name
# 3. Load in a) salinity and b) temperature rasters
# 4. Restrict to two, ten-year study periods: "current" (2003-2012) and "mid-century" (2030-2039)

# Iterate through each taxon

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
	cat(i, '=== Handling ===', f, fill =T)

	load(file.path(baseDir, 'PMEL-Models/rData', f))
	sgdf.S <- sgdf.Max 	## Cast this as the salinity spatialGridDataFrame
	rm('sgdf.Max')
	f <- ff[i+1] ## grab the next file name
	load(file.path(baseDir, 'PMEL-Models/rData', f))
	sgdf.T <- sgdf.Max 	## Cast this as the temperature spatialGridDataFrame
	rm('sgdf.Max')
	yy <- as.numeric(sort(unique(substring(names(sgdf.T@data), 1,4))))  ## Extract the years from the spatial grid dataframe columns

	if(yy[1] == 2029){
		yy <- yy[2:11] ## for Mid-century, restrict to years 2030-2039
	}else{
		yy <- yy[1:10] ## for Current, restrict to years 2003-2012
	}

	for(T in 1:nrow(SpToler)){ ## iterate through each taxon
		taxa <- SpToler[T, ]$Sci_Name
		# set min. & max. T-S thresholds
		minSalinity.S <- SpToler[T, ]$Min_Salinity
		maxSalinity.S <- SpToler[T, ]$Max_Salinity
		minTemp.S <- SpToler[T, ]$Min_TempC
		maxTemp.S <- SpToler[T, ]$Max_TempC

		cat(T, '\n::: Processing', taxa,
				'\nwhich needs a temperature range of', minTemp.S, 'to',maxTemp.S,
				'\nand a salinity range of', minSalinity.S, 'to', maxSalinity.S, '\n')

		#### Survival Loop
		for(y.S in yy){ ## iterate through the years to evaluate survival conditions
			ColumnsToSelect.T <- which(substring(names(sgdf.T@data), 1,4) == y.S)  	## Find columns in spatial grid dataframe from the specified year, y.S
			s_T_y <- raster::stack(sgdf.T[, , ColumnsToSelect.T]) ## Select temperature sgdf for this year and cast as a raster stack

			ColumnsToSelect.S <- which(substring(names(sgdf.S@data), 1,4) == y.S)  	## Need to consider separately from sgdf.T because column indices are different
			s_S_y <- raster::stack(sgdf.S[, , ColumnsToSelect.S]) ## Select salinity sgdf for this year and cast as a raster stack

			## Temperature: For each week and pixel, evaluate model temperature values against species' tolerances
			s_T_y_MinTemp <- s_T_y >= minTemp.S  ## return 1 if temp >= taxon min. limit, else 0
			s_T_y_MaxTemp <- s_T_y <= maxTemp.S  ## return 1 if temp <= taxon max. limit, else 0
			s_T_y_OK <- s_T_y_MinTemp * s_T_y_MaxTemp ## Product of the above results: 1 both criteria met, else 0

			## Salinity
			s_S_y_MinSal <- s_S_y >= minSalinity.S ## return 1 if salinity >= taxon min. limit, else 0
			s_S_y_MaxSal <- s_S_y <= maxSalinity.S ## return 1 if salinity <= taxon max. limit, else 0
			s_S_y_OK <- s_S_y_MinSal * s_S_y_MaxSal ## Product of the above results: 1 both criteria met, else 0

			## Overall T-S suitability as a stack of weekly values for the year
			# s_y_OK used in our Weekly Survival / Latitude analysis
			s_y_OK <- s_T_y_OK * s_S_y_OK

			## Year-round Survival
			# For each pixel, determine 'habitat suitability' across entire year by collapsing weekly values into a single RasterLayer
			# If >=1 week within the year had a value of 0 (no suitable habitat), pixel value = 0
			r_y_OK.S <- min(s_y_OK)
			names(r_y_OK.S) <- paste0('year', y.S, '_fullYearHabitat')

			## Total number of weeks with suitability value = 1
			# r_y_OK.N <- sum(s_y_OK)
			# names(r_y_OK.N) <- paste0('year', y.S, '_nweeksHabitat')

			if(y.S == yy[1]){ ## if first year, create stack
				s_OK.S <- stack(r_y_OK.S)
				# s_nWeeks.S <- stack(r_y_OK.N)
			}else{ ## if 2+ year in loop, add to first year
				s_OK.S <- stack(s_OK.S, r_y_OK.S)
				# s_nWeeks.S <- stack(s_nWeeks.S, r_y_OK.N)
			}
		}

		## Save the Survival data for this taxa
		(name<- paste(taxa, '_Weeks_Survival_', Model, '_', y.S, '.rData', sep=''))
		fileName <- file.path(file.path(baseDir, 'PMEL-Models', 'rOut', 'SurvivalWeeks_which', name))# file name of raster stack of weeks of survival for this taxa/year/model
		save(s_y_OK, file = fileName)
		cat('..... --> Saving', fileName, fill=T)

		(name<- paste(taxa, '_Survival_', modelPeriod, '.rData', sep=''))
		fileName<-file.path(file.path(baseDir, 'rOut', 'Survival', name))
		cat('..... --> Saving', fileName, fill=T)
		save(s_OK.S, file=fileName)

		rm(list = c('ColumnsToSelect.T', 'ColumnsToSelect.S', 's_T_y', 's_S_y', 's_T_y_MinTemp', 's_T_y_MaxTemp', 's_T_y_OK', 's_S_y_MinSal', 's_S_y_MaxSal', 's_S_y_OK','s_y_OK','s_OK.S','s_nWeeks.S','r_y_OK.S',
		'maxSalinity.S','maxTemp.S','minSalinity.S','minTemp.S'))

		##### Reproduction Loop
		if(!is.na(SpToler[T,]$Min_ReproSal) & !is.na(SpToler[T,]$Max_ReproSal) & !is.na(SpToler[T,]$Min_ReproTempC) & !is.na(SpToler[T,]$Max_ReproTempC)){
			minSalinity.R <- SpToler[T, ]$Min_ReproSal
			maxSalinity.R <- SpToler[T, ]$Max_ReproSal
			minTemp.R <- SpToler[T, ]$Min_ReproTempC
			maxTemp.R <- SpToler[T, ]$Max_ReproTempC

			## Start parallel processing using DoSNOW
			# This operates on a numeric vector of years (yy),
			# sends the function defined in the {} brackets after the %dopar% pipe to the clusters for processing,
			# enables the package 'raster' on each cluster,
			# and combines the results from the clusters using the raster::stack function

			NumberOfCluster = length(yy)		## set number of clusters equal to the number of years being processed
			cl <- makeCluster(NumberOfCluster)
			registerDoSNOW(cl)					## DoSNOW housekeeping

			s_OK.Repro <- foreach(y = yy, .combine = 'stack', .packages = 'raster') %dopar% { ##

				ColumnsToSelect.T <- which(substring(names(sgdf.T@data), 1,4) == y)  	## Examine the spatial grid data frame column names,
				## finding those from the specified year, y
				s_T_y <- raster::stack(sgdf.T[, , ColumnsToSelect.T]) ## Select temperature sgdf for this year and cast as a raster stack

				ColumnsToSelect.S <- which(substring(names(sgdf.S@data), 1,4) == y)
				s_S_y <- raster::stack(sgdf.S[, , ColumnsToSelect.S]) ## Select salinity sgdf for this year and cast as a raster stack

				## Evaluate ocean temperature values against the taxa tolerances...
				s_T_y_MinTemp <- s_T_y >= minTemp.R  ## Return 1 if ocean temperature is no lower than species' minimum threshold, else 0
				s_T_y_MaxTemp <- s_T_y <= maxTemp.R

				s_T_y_OK <- s_T_y_MinTemp * s_T_y_MaxTemp ## Product of the above results: 1 both criteria met, 0 otherwise

				## Evaluate ocean salinity values against the taxa tolerances...
				s_S_y_MinSal <- s_S_y >= minSalinity.R
				s_S_y_MaxSal <- s_S_y <= maxSalinity.R
				s_S_y_OK <- s_S_y_MinSal * s_S_y_MaxSal

				## Overall habitat suitability for reproduction as a stack of weekly values for the year
				s_y_OK <- s_T_y_OK * s_S_y_OK

				## Evaluate number of consecutive weeks of suitable habitat that were present
				r_y_OK <- ConsecutiveWeeksofHabitatCalc(s_y_OK)
				rm(s_y_OK)
				return(r_y_OK) ## Return this raster of habitat suitability for this year
			} ## year doSNOW::foreach loop
			stopCluster(cl) ## release the cluster

			names(s_OK.Repro) <- paste0('ContinuousReproWeeks_year_', yy)
			s_OK.Repro ## print summary

			## Save Reproduction data for this taxa
			(name<- paste(taxa, '_Reproduction_', modelPeriod, '.rData', sep=''))
			fileName<-file.path(file.path(baseDir, 'rOut', 'Reproduction', name))
			cat('..... --> Saving', fileName, fill=T)
			save(s_OK.Repro, file=fileName)

			rm(list = c('ColumnsToSelect.T', 'ColumnsToSelect.S', 's_T_y', 's_S_y', 's_T_y_MinTemp', 's_T_y_MaxTemp', 's_T_y_OK', 's_S_y_MinSal', 's_S_y_MaxSal', 's_S_y_OK',
			's_OK.Repro','minSalinity.R','maxSalinity.R','minTemp.R','maxTemp.R'))
		} ## End Reproduction loop
	} ## End Taxa loop
} ## End Model loop

## Workspace clean-up
rm(list=ls())
