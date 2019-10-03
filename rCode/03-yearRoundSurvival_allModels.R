## Year-round Survival
## Intent: Summarize year-round survival suitability across all taxa and across the three ROMS
## For a) current and b) mid-century projections.
## This script is dependent on outputs from the 02-evaluate_habitat_suitability.R script
## It also requires the beringSeaContinentalShelf.shp, which can be downloaded at https://accscatalog.uaa.alaska.edu/dataset/resource/ab3f41f1-7dc6-4905-b307-ec99b430ec12

## Authors: A.S. Fischbach (afischbach@usgs.gov)
##			    A. Droghini (adroghini@alaska.edu)

## Source initial run script
source('rCode/init.R')

## Create data frame to hold details of all .Rdata output files (file name, species, model, study period, and aspect)
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

#### Evaluate year-round survival for each taxon----

# Iterate through each study period-model-taxon combination
# First, process by study period: a) current (2003-2012); b) mid-century (2030-2039)
# Then, process by model
# Then by taxon
# Define habitat as 'suitable' if taxon can survive year-round in a given pixel for >= 7 years out of 10 year study period
# Sum habitat suitability across all taxa
# And average across the three ROMS

for (period_i in 1:2){
	sp=spp[period_i]

	for(model_i in 1:3){
		m = mm[model_i]
	cat('\nExploring survival for study period', sp, 'projected by model', m, fill =T)

	for(i in 1:length(taxa)){
		taxon <- taxa[i]
		cat('\n... processing', taxon, fill = T)
		criteria = (output$Taxa == taxon & output$StudyPeriod == sp & output$Model == m)
		f <- file.path(baseDir, 'rOut', 'Survival', output[criteria, ]$files)
		load(f, verbose = T)
    #loads RasterStack s_OK.S
		#s_OK.S contains 10 layers of habitat suitability, one for every year in the study period
		#binary raster where 1 = suitable year-round, 0 = unsuitable for that year

    #Summarize survival across taxa
		if(i ==1){ #if first species
			nSurvival <- (sum(s_OK.S) >= 7)
		}else{
			nSurvival <- nSurvival + (sum(s_OK.S) >= 7) #add to previous results
		}
		rm(s_OK.S)
		} #end of across taxa loop

	#Summarize over all 3 models
	if(model_i == 1){
			CountSurvival <- stack(nSurvival)
		}else{
			CountSurvival <- addLayer(CountSurvival, nSurvival)
		}
	} #end of model loop

# Generate ensembleSurvival raster. This raster thenumber of NIS with year-round survival, averaged across the 3 models
ensembleSurvival<- mean(CountSurvival)

# Clip to study area - Bering Sea continental shelf
source('rCode/00-beringSeaRasterize.R')
ensembleClip <- ensembleSurvival * beringShelf

plot(ensembleClip,main=paste("Number of NIS with year-round survival",sp,sep=" "))

# Export as raster
# For generating Figure 2 in ESRI ArcMap
fileName<-paste("ensemble_year_survival_",sp,".tif",sep="")
fileWrite <- file.path(file.path('C:/Users/adroghini/Documents/bering-invaders/gis_products/allTaxa/yearlySurvival', fileName))
writeRaster(ensembleClip, fileWrite, format = "GTiff",overwrite=TRUE)

rm(ensembleSurvival,CountSurvival,ensembleClip,fileName,fileWrite,taxon,sp,m,i,f,nSurvival)
} # end of period loop

# Clean-up
rm(list=ls())
