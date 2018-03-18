#####################################################
## Project: Bering Sea marine invasives - Year-round Survival
## Intent: Summarize year-round survival suitability across taxa and across the 3 ROMS for a) current and b) mid-century projections.
## This script is dependent on outputs from the 02-analysis-habitat_suitability.R script
## Authors: A.S. Fischbach (afischbach@usgs.gov)
##			A. Droghini (adroghini@alaska.edu)
#####################################################

##load packages
require(raster)
require(sp)
require(rgdal)
require(rgeos)

##set working directory
#baseDir <-  'D:/HelpingOthers/ABSIlcc/NFWF2016/Models'
#setwd(file.path(baseDir, 'rCode'))
baseDir <-  'C:/Users/adroghini/Documents/Marine-invasives/PMEL-Models'
setwd(file.path(baseDir))

#make data.frame (output) with details of all .Rdata output files (file name, species, model, study period, and aspect)
ff <- list.files(file.path(baseDir, 'rOut', 'Survival'), pattern = 'rData')
output <- data.frame(files = ff, stringsAsFactors=F)
for(r in 1:nrow(output)){
  f <- output[r, 1]
  output$Taxa[r] <- unlist(strsplit(f, '_'))[1]
  output$Model[r] <- substr(unlist(strsplit(f, '_'))[3],1,5)
  output$StudyPeriod[r] <- substr(unlist(strsplit(f, '_'))[3], 7, 15)
}

spp <- unique(output$StudyPeriod)
taxa <- unique(output$Taxa)
mm <- unique(output$Model)

for (period_i in 1:2){ # Process by study period: a) current (2003-2012); b) mid-century (2030-2039)
	sp=spp[period_i]
	
	for(model_i in 1:3){ ##Process by model
		m = mm[model_i]
	cat('\nExploring survival for study period', sp, 'projected by model', m, fill =T)
	
	#Evaluate decadal year-round survival for each taxon
	#For each model, define habitat as suitable if taxon can survive year-round in a given pixel for >= 7 years (out of 10)
	for(i in 1:length(taxa)){
		taxon <- taxa[i]
		cat('\n... processing', taxon, fill = T)
		criteria = (output$Taxa == taxon & output$StudyPeriod == sp & output$Model == m)
		f <- file.path(baseDir, 'rOut', 'Survival', output[criteria, ]$files)
		load(f, verbose = T) #load RasterStack s_OK.S
		#s_OK.S contains 10 layers of habitat suitability, one for every year in the study period
		#binary raster where 1 = suitable year-round, 0 = unsuitable
		
		#print(mean(values((sum(s_OK.S) >= 7)), na.rm=T))
		#plot(sum(s_OK.S) >= 7)
		
		if(i ==1){ ##if first species
			nSurvival <- (sum(s_OK.S) >= 7) 
		}else{ 
			nSurvival <- nSurvival + (sum(s_OK.S) >= 7) ##add to previous results
		}
		rm(s_OK.S)
		} #end of across taxa loop
		
	##Summarize over all 3 models
	if(model_i == 1){
			CountSurvival <- stack(nSurvival)
		}else{
			CountSurvival <- addLayer(CountSurvival, nSurvival)
		}
	} #end of model loop
	
ensembleSurvival<- mean(CountSurvival)
ensemble.clip<-ensembleSurvival*Bering_200m #clip to study area (Bering Sea continental shelf)
plot(ensemble.clip,main=paste("Year-round survival, averaged across 3 ROMS for",sp,sep=" "))

fileName<-paste("ensemble_year_survival_",sp,".tif",sep="")
fileWrite <- file.path(file.path('C:/Users/adroghini/Documents/Marine-invasives/GIS/OverallTaxa/YearlySurvival/ensemble-mean', fileName))
writeRaster(ensemble.clip, fileWrite, format = "GTiff",overwrite=TRUE)

rm(ensembleSurvival,CountSurvival,ensemble.clip,fileName,fileWrite,taxon,sp,m,i,f,nSurvival)
}

#workspace clean-up
rm(spp,mm,taxa,output,ff,criteria,r,period_i,model_i)