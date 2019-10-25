#Last updated: 8 October 2017
#Author: Amanda Droghini (adroghini@alaska.edu)

##Intent: How many species can reproduce under a) current and b) future conditions? (Assume minimum 1 week required)
##What is the frequency distribution for the # of consecutive weeks under a) current and b) future conditions?
##Maximum/average/etc. # of consecutive weeks?

#### Start up requirements----

## Source initial run script
source('rCode/init.R')

## Load results of reproductive habitat suitability
ff <- list.files(file.path(baseDir, 'rOut', 'Reproduction'), pattern = 'rData')

#### Summary results for each taxon----
# For each model & study period combination
# Identify how many consecutive weeks of reproduction are available for each taxon
# Number of weeks chosen as maximum pixel value in our study area, for that year-model combination

## Create dataframe in which to place results
nn <- c('Taxa', 'Model', 'StudyPeriod', 'noRepro', eval(paste0('year', 1:10)))
resultsByTaxon <- data.frame(matrix(NA, ncol = length(nn), nrow = length(ff)))
names(resultsByTaxon) <- nn
rm(nn)

## Iterate through each taxon-model-study period combination

for(r in 1:nrow(resultsByTaxon)){
  f <- ff[r]

  # Parse out the taxon, model, and study period for the file
  resultsByTaxon$Taxa[r] <- unlist(strsplit(f, '_'))[1]
  resultsByTaxon$Model[r] <- substr(unlist(strsplit(f, '_'))[3], 1, 5)
  resultsByTaxon$StudyPeriod[r] <- substr(unlist(strsplit(f, '_'))[3], 7, 15)

  # Load file
  # Each .Rdata file contains a raster stack (s_OK.Repro) that has one layer for every year in the study period (10 years per period)
  # Pixel values represent the number of consecutive weeks of suitable reproductive habitat based on species' reproductive temperature and salinity requirements
  load(file.path(baseDir, 'rOut', 'Reproduction', f))
  cat(r, '=== Handling ===', f, fill =T)

  # Clip to study area
  reproClip <- s_OK.Repro * beringShelf

  # For each year in study period, extract pixel with the highest value
  # This represents the maximum number of weeks with suitable reproductive conditions found in at least one pixel in the Bering Sea
  # Place values into appropriate 'year' columns in resultsByTaxon dataframe
  # If max value = 0 for all years, column noRepro == TRUE

  cs <- cellStats(reproClip,max)
  resultsByTaxon[r, eval(paste0('year', 1:length(cs))) ] <- cs
  resultsByTaxon[r, 'noRepro'] <- all(cs == 0)

  # Calculate summary stats across all years in the study period
  resultsByTaxon$Average<-rowMeans(resultsByTaxon[,5:14])
  resultsByTaxon$stdev[r]=sd(resultsByTaxon[r,5:14])
}

#### Save and clear----

# Save output
fileName <- file.path(file.path(baseDir, 'rOut', "maxWeeksRepro.csv"))
write.csv(resultsByTaxon, file = fileName,row.names=FALSE)

# Clear workspace
rm(list=ls())
