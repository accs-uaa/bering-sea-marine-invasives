# Intent: Summarize results from reproductive habitat suitability analyses. Calculate the number of NIS w/ suitable habitat for each pixel-model-study period. Average across 10 year study period.

# Author: A. Droghini (adroghini@alaska.edu)
# Last updated: 25 October 2019

#### Start up requirements----

# Load study are extent (Bering Sea shelf)
# Sourcing this will also call init.R script
source('rCode/00-beringSeaRasterize.R')

# Load reproductive habitat files
# The folder contains one file for each taxon-model-study period combination
ff <- list.files(path=file.path(baseDir, 'rOut', 'Reproduction'), pattern = 'rData')

# Create dataframe----

# Make data.frame (output) with details of all .Rdata Repro files (file name, taxa, model, study period)
# To use as a selection criteria when iterating through files in for loop below
ff <- list.files(file.path(baseDir, 'rOut', 'Reproduction'), pattern = 'rData')
output <- data.frame(files = ff, stringsAsFactors=F)
for(r in 1:nrow(output)){
  f <- output[r, 1]
  output$Taxa[r] <- unlist(strsplit(f, '_'))[1]
  
  output$Model[r] <- substr(unlist(strsplit(f, '_'))[3], 1, 5)
  output$StudyPeriod[r] <- substr(unlist(strsplit(f, '_'))[3], 7, 15)
  output$aspect[r] <- "Reproduction"
}

spp <- unique(output$StudyPeriod)
taxa <- unique(output$Taxa)
rm(ff,f,r)

# Process results by study period & model---
# Export cumulative taxa results to rOut/OverallTaxa_Repro folder

# Extract unique study periods (current/future) and models (three ROMS)
sp1 <- spp[1]
sp2 <- spp[2]
mm <- unique(output$Model)

# Study period 1: Current (2003-2012)

for(model_i in 1:3){
  m = mm[model_i]
  cat('\nExploring survival modeled by', m, fill =T)
  for(i in 1:length(taxa)){ # read in the model-predicted habitat suitability file for each taxon.
    taxon <- taxa[i]
    cat('\n... processing', taxon, fill = T)
    criteria = (output$Taxa == taxon & output$StudyPeriod == sp1 & output$aspect == 'Reproduction' & output$Model == m)
    f <- file.path(baseDir, 'rOut', 'Reproduction', output[criteria, ]$files)
    cat("....", f, fill =T)
    load(f, verbose = T)
    s_OK.Repro <- s_OK.Repro * beringShelf # clip to Bering Sea shelf

    #replace all values >0 to 1 to calculate cumulative taxa per pixel
    s_OK.Repro[s_OK.Repro > 0] <- 1
    #print(max(values(s_OK.Repro),na.rm=T)) #code check
    
    if(i ==1){ ##if first species
      nRepro <- s_OK.Repro 
    }else{ 
      nRepro <- nRepro + s_OK.Repro ##add to previous results
    }
    
    # Take average across 10 years
    nRepro.avg <- mean(nRepro)
  }
# Export results
  name<- paste('Repro_',m,'_', spp[1], '.rData', sep='')	
  fileName <- file.path(file.path(baseDir, 'rOut', 'OverallTaxa_Repro', name))
  save(nRepro.avg, file = fileName)
  plot(nRepro.avg, main=fileName)
  rm(s_OK.Repro,taxon,f,nRepro,name,fileName,nRepro.avg,model_i,i,m,criteria)

}

# Study period 2: Mid-century (2030-2039)
for(model_i in 1:3){
  m = mm[model_i]
  cat('\nExploring survival modeled by', m, fill =T)
  for(i in 1:length(taxa)){ # read in the model-predicted habitat suitability file for each taxon.
    taxon <- taxa[i]
    cat('\n... processing', taxon, fill = T)
    criteria = (output$Taxa == taxon & output$StudyPeriod == sp2 & output$aspect == 'Reproduction' & output$Model == m)
    f <- file.path(baseDir, 'rOut', 'Reproduction', output[criteria, ]$files)
    cat("....", f, fill =T)
    load(f, verbose = T)
    s_OK.Repro <- s_OK.Repro * beringShelf # clip to Bering Sea shelf
    s_OK.Repro[s_OK.Repro > 0] <- 1 # calculate cumulative taxa per pixel
    #print(max(values(s_OK.Repro),na.rm=T)) #code check
    
    if(i ==1){ ##if first species
      nRepro <- s_OK.Repro 
    }else{ 
      nRepro <- nRepro + s_OK.Repro ##add to previous results
    }
    
    # Take average across 10 years
    nRepro.avg <- mean(nRepro)
  }
  # Export results
  name<- paste('Repro_',m,'_', spp[2], '.rData', sep='')	
  fileName <- file.path(file.path(baseDir, 'rOut', 'OverallTaxa_Repro', name))
  save(nRepro.avg, file = fileName)
  plot(nRepro.avg, main=fileName)
  rm(s_OK.Repro,taxon,f,nRepro,name,fileName,nRepro.avg,model_i,i,m,criteria)
  
}

rm(output)

# Produce summary results----
# Load OverallTaxa Repro files
toLoad <- list.files(file.path(baseDir, 'rOut', 'OverallTaxa_Repro'),full.names = TRUE)

# Create dataframe in which to place results
results <- data.frame(files = toLoad, stringsAsFactors=F)

for(r in 1:nrow(results)){
  f <- results[r, 1]
  results$Model[r] <- substr(unlist(strsplit(f, '_'))[3],1,5)
  results$StudyPeriod[r] <- substr(unlist(strsplit(f, '_'))[4], 1, 4)
  results$median[r] <- NA
  results$max[r] <- NA
}

# Iterate through each OverallTaxa_Repro file
# Calculate median and maximum number of taxa per pixel for each model & study period. 
# Median represents the median number of NIS with at least 1 week of reproductive habitat, across all pixels in the Bering Sea shelf.

for (i in 1:length(toLoad)){
  f <- toLoad[i]
  load(f)
  results$median[i] <- cellStats(nRepro.avg,median)
  results$max[i] <- cellStats(nRepro.avg,max)
}


