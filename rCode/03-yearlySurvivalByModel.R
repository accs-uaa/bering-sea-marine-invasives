## Created: June 30th 2017
## Last updated: 2 October 2017
# Authors: A.S. Fischbach (afischbach@usgs.gov)
# 		   A. Droghini (adroghini@alaska.edu)
## Modified from ModelAveraged_TaxaSuitability code
## Intent: Summarize the number of taxa with year-round survival for each model under a) current (2003-2012) and b) future (2030-2039) conditions. Each ROMS models is assessed separately and results are expressed as change in # of species from current --> future time period.
# Suitability criteria: If a taxon is predicted to have suitable habitat in a given pixel for >= 7 years (out of 10), then count this taxon as having modeled survival (suitability = 1) in this pixel.

## load packages
require(raster)
require(sp)
require(rgdal)
require(rgeos)
require(viridis) ## replace color brewer with viridis colors, Thanks to Jordan Watson!
#baseDir <-  'D:/HelpingOthers/ABSIlcc/NFWF2016/Models'
setwd(file.path(baseDir, 'rCode'))

##################################PLOTTING REQUIREMENTS##################################
### Base coastal map
prj.geo<-CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0')
prj.StudyArea <- CRS("+proj=aeqd +lat_0=55 +lon_0=-170 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") 
ContinentsDataFile<-file.path(baseDir, 'rCode/Continents.rData') 
if(!file.exists(ContinentsDataFile)){
	require(rgdal)
	dsn<-"D:/Walrus/BaseMaps/ESRI_DATA/Continents.shp"
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
ports <- read.csv(file = file.path(baseDir, 'rCode/ports.csv'), na.strings='', stringsAsFactors = F)
ports <- ports[!(ports$IATA %in% c('KQA', 'UDA')), ]
coordinates(ports) <- c('x','y')
proj4string(ports) <- prj.geo
ports.prj <- spTransform(ports, prj.StudyArea)
landColor <- 'seashell2' ##'antiquewhite4' ## 'yellow4' ## '#574835'#'burlywood4'
portColor <- 'black' ##'yellow2'
backgroundColor <- 'white' #'snow1'
##################################END USED for PLOTTING##################################

# Make data.frame (output) with details of all .Rdata Survival files (file name, taxa, model, study period)
ff <- list.files(file.path(baseDir, 'rOut', 'Survival'), pattern = 'rData')
output <- data.frame(files = ff, stringsAsFactors=F)
  for(r in 1:nrow(output)){
    f <- output[r, 1]
    output$Taxa[r] <- unlist(strsplit(f, '_'))[1]
    
	output$Model[r] <- substr(unlist(strsplit(f, '_'))[3], 1, 5)
    output$StudyPeriod[r] <- substr(unlist(strsplit(f, '_'))[3], 7, 15)
    output$aspect[r] <- "Survival"
  }
  
spp <- unique(output$StudyPeriod)
taxa <- unique(output$Taxa)
rm(ff,f,r)

##################################
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
		criteria = (output$Taxa == taxon & output$StudyPeriod == sp1 & output$aspect == 'Survival' & output$Model == m)
		f <- file.path(baseDir, 'rOut', 'Survival', output[criteria, ]$files)
		cat("....", f, fill =T)
		load(f, verbose = T)
		print(mean(values((sum(s_OK.S) >= 7)), na.rm=T))
		#plot(sum(s_OK.S) >= 7) #faster if plotting not required

		if(i ==1){ ##if first species
			nSurvival <- (sum(s_OK.S) >= 7) ##Survival
		}else{ 
			nSurvival <- nSurvival + (sum(s_OK.S) >= 7) ##add to previous results
		}
		
		}
		
		#Save as...
		name<- paste('YearSurvival_',m,'_', spp[1], '.rData', sep='')	
		fileName <- file.path(file.path(baseDir, 'rOut', 'OverallTaxa_Year', name))
		save(nSurvival, file = fileName)
		plot(nSurvival, main=fileName)
		rm(s_OK.S,taxon,f,nSurvival,name,fileName)
	}


####Study period 2: Forecast (2030-2039)
for(model_i in 1:3){
	m = mm[model_i]
	cat('\nExploring survival modeled by', m, fill =T)
	for(i in 1:length(taxa)){
		taxon <- taxa[i]
		cat('\n... processing', taxon, fill = T)
		criteria = (output$Taxa == taxon & output$StudyPeriod == sp2 & output$aspect == 'Survival' & output$Model == m)
		f <- file.path(baseDir, 'rOut', 'Survival', output[criteria, ]$files)
		cat("....", f, fill =T)
		load(f, verbose = T)
		
		print(mean(values((sum(s_OK.S) >= 7)), na.rm=T))
		if(i ==1){
			nSurvival <- (sum(s_OK.S) >= 7)
		}else{
			nSurvival <- nSurvival + (sum(s_OK.S) >= 7)
		}
		
		}
		
		#Save as...
		name<- paste('YearSurvival_',m,'_', spp[2], '.rData', sep='')	
		fileName <- file.path(file.path(baseDir, 'rOut', 'OverallTaxa_Year', name))
		save(nSurvival, file = fileName)
		plot(nSurvival, main=fileName)
		rm(s_OK.S,taxon,f,nSurvival,name,fileName)
	}

rm(taxa,sp1,sp2,criteria,spp,i,m,mm,model_i,output)