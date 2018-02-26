##October 10 2017
##Authors: A. Droghini (adroghini@alaska.edu)
#		   A.S. Fischbach (afischbach@usgs.gov)

##Intent: Evaluate the number of suitable weeks across taxa + plot. Standardize between 0 and 1.
##For each species, we calculated the average number of suitable weeks across each 10 year study period. 
##We then summarized our results across taxa by adding up this number for all species, which gives us the metric: average no. of weeks x species 

#baseDir <-  'C:/Users/adroghini/Documents/Marine-invasives/PMEL-Models'
setwd(file.path(baseDir))
ff <- list.files(file.path(baseDir, 'rOut', 'SurvivalWeeks'), pattern = 'rData') #list files

output <- data.frame(files = ff, stringsAsFactors=F)
  for(r in 1:nrow(output)){
    f <- output[r, 1]
    output$Taxa[r] <- unlist(strsplit(f, '_'))[1]
	output$Model[r] <- substr(unlist(strsplit(f, '_'))[5], 1, 5)
    output$StudyPeriod[r] <- substr(unlist(strsplit(f, '_'))[5], 7, 15)
  }
  
spp <- unique(output$StudyPeriod)
taxa <- unique(output$Taxa)

##################################
# Process by study period

sp1 <- spp[1]
sp2 <- spp[2]
mm <- unique(output$Model)

####Study period 1: Current (2003-2012)
for(model_i in 1:3){
	m = mm[model_i]
	cat('\nExploring survival modeled by', m, fill =T)
	for(i in 1:length(taxa)){ ##read in taxa suitability rData files for each taxa.
		taxon <- taxa[i]
		cat('\n... processing', taxon, fill = T)
		criteria = (output$Taxa == taxon & output$StudyPeriod == sp1 & output$Model == m)
		f <- file.path(baseDir, 'rOut', 'SurvivalWeeks', output[criteria, ]$files)
		cat("....", f, fill =T)
		load(f, verbose = T) #loads file s_nWeeks.S
		
		s_nWeeks.C<-s_nWeeks.S*Bering_200m #clip to study area extent. see BeringSea_Raster.R code
		
		print(mean(values(s_nWeeks.C),na.rm=T))

		if(i ==1){ ##if first species
			nWeekly <- mean(s_nWeeks.C) ##average
		}else{ 
			nWeekly <- nWeekly + (mean(s_nWeeks.C)) ##add to previous results
		}
		
		}
		
		#Save as .Rdata
		name<- paste('WeeklySurvival_',m,'_', spp[1], '.rData', sep='')	
		fileName <- file.path(file.path(baseDir, 'rOut', 'OverallTaxa_Weekly', name))
		save(nWeekly, file = fileName)
		plot(nWeekly, main=name)
		
		#Save as raster
		name<- paste('WeeklySurvival_',m,'_', spp[1], '.tif',sep='')
		fileName <- file.path(file.path(baseDir, 'rOut','OverallTaxa_Weekly', 'GIS', name))
		writeRaster(nWeekly, fileName, format = "GTiff",overwrite=TRUE)

		rm(s_nWeeks.S,taxon,f,nWeekly,name,fileName,s_nWeeks.C)
	}

####Study period 2: Forecast (2030-2039)
for(model_i in 1:3){
	m = mm[model_i]
	cat('\nExploring survival modeled by', m, fill =T)
	for(i in 1:length(taxa)){
		taxon <- taxa[i]
		cat('\n... processing', taxon, fill = T)
		criteria = (output$Taxa == taxon & output$StudyPeriod == sp2 & output$Model == m)
		f <- file.path(baseDir, 'rOut', 'SurvivalWeeks', output[criteria, ]$files)
		cat("....", f, fill =T)
		load(f, verbose = T)
		
		s_nWeeks.C<-s_nWeeks.S*Bering_200m
		
		print(mean(values(s_nWeeks.C),na.rm=T))
		
		if(i ==1){ ##if first species
			nWeekly <- mean(s_nWeeks.C) 
		}else{ 
			nWeekly <- nWeekly + (mean(s_nWeeks.C)) ##add to previous results
		}
		
		}
		
		#Save as .Rdata
		name<- paste('WeeklySurvival_',m,'_', spp[2], '.rData', sep='')	
		fileName <- file.path(file.path(baseDir, 'rOut', 'OverallTaxa_Weekly', name))
		save(nWeekly, file = fileName)
		plot(nWeekly, main=name)
		
		#Save as raster
		name<- paste('WeeklySurvival_',m,'_', spp[2], '.tif',sep='')
		fileName <- file.path(file.path(baseDir, 'rOut','OverallTaxa_Weekly', 'GIS', name))
		writeRaster(nWeekly, fileName, format = "GTiff",overwrite=TRUE)

		rm(s_nWeeks.S,taxon,f,nWeekly,name,fileName,s_nWeeks.C)
	}
	
##Workspace clean-up
rm(output,r,taxa,spp,sp1,sp2,m,model_i,mm,i,criteria)