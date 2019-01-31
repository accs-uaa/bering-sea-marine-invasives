# Set working directory
#baseDir <- "...insert_base_directory/PMEL-Models"

# Load required packages
library(raster)
library(rgdal)

# Load reproduction files. Folder contains 1 file per taxa-model-study period combination
ff <- list.files(path=file.path(baseDir, 'rOut', 'Reproduction'), pattern = 'rData')


# Make data.frame (output) with details of all .Rdata Repro files (file name, taxa, model, study period)
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

# Process by study period

sp1 <- spp[1]
sp2 <- spp[2]
mm <- unique(output$Model)

####Study period 1: Current (2003-2012)
for(model_i in 1:3){
  m = mm[model_i]
  cat('\nExploring survival modeled by', m, fill =T)
  for(i in 1:length(taxa)){ ##read in the model level taxa suitability rData files for each taxa.
    taxon <- taxa[i]
    cat('\n... processing', taxon, fill = T)
    criteria = (output$Taxa == taxon & output$StudyPeriod == sp1 & output$aspect == 'Reproduction' & output$Model == m)
    f <- file.path(baseDir, 'rOut', 'Reproduction', output[criteria, ]$files)
    cat("....", f, fill =T)
    load(f, verbose = T)
    s_OK.Repro <- s_OK.Repro * Bering_200m #clip to Bering Sea
    # print(max(values(s_OK.Repro),na.rm=T)) #maximum number of consecutive weeks with year-round survival
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
  rm(s_OK.Repro,taxon,f,nRepro,name,fileName,nRepro.avg)

}

####Study period 2: Mid-century (2030-2039)
for(model_i in 1:3){
  m = mm[model_i]
  cat('\nExploring survival modeled by', m, fill =T)
  for(i in 1:length(taxa)){ ##read in the model level taxa suitability rData files for each taxa.
    taxon <- taxa[i]
    cat('\n... processing', taxon, fill = T)
    criteria = (output$Taxa == taxon & output$StudyPeriod == sp2 & output$aspect == 'Reproduction' & output$Model == m)
    f <- file.path(baseDir, 'rOut', 'Reproduction', output[criteria, ]$files)
    cat("....", f, fill =T)
    load(f, verbose = T)
    s_OK.Repro <- s_OK.Repro * Bering_200m #clip to Bering Sea
    # print(max(values(s_OK.Repro),na.rm=T)) #maximum number of consecutive weeks with year-round survival
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
  name<- paste('Repro_',m,'_', spp[2], '.rData', sep='')	
  fileName <- file.path(file.path(baseDir, 'rOut', 'OverallTaxa_Repro', name))
  save(nRepro.avg, file = fileName)
  plot(nRepro.avg, main=fileName)
  rm(s_OK.Repro,taxon,f,nRepro,name,fileName,nRepro.avg)
  
}

rm(output,i)

# Produce summary results
# Load OverallTaxa Repro files
file_to_load<-list.files(file.path(baseDir, 'rOut', 'OverallTaxa_Repro'),full.names = TRUE)

# Create dataframe (repro_results) in which to place results
repro_results <- data.frame(files = file_to_load, stringsAsFactors=F)

for(r in 1:nrow(repro_results)){
  f <- repro_results[r, 1]
  repro_results$Model[r] <- substr(unlist(strsplit(f, '_'))[3],1,5)
  repro_results$StudyPeriod[r] <- substr(unlist(strsplit(f, '_'))[4], 1, 4)
  repro_results$median[r] <- NA
  repro_results$max[r] <- NA
}

# Calculate median and maximum number of taxa per pixel, for each model & study period
for (i in 1:length(file_to_load)){
  f <- file_to_load[i]
  load(f)
  # print(cellStats(nRepro.avg,median))
  repro_results$median[i]<-cellStats(nRepro.avg,median)
  # print(cellStats(nRepro.avg,max))
  repro_results$max[i]<-cellStats(nRepro.avg,max)
}


