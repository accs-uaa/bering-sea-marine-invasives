## D:\HelpingOthers\ABSIlcc\NFWF2016\Models\rCode\EnsembleTaxaSuitability.r
## A.S. Fischbach 16 Feb 2017
## Intent: read in the model level taxa suitability rData files for each taxa.
### Generalize these as an ensemble mean for each taxa / study period. Produce a plot of these ensemble means for each taxa / study period. Produce a single index of marine invasive habitat suitability for each study period following these rules. 
### We classified each pixel as being available for colonization by a taxa if the ensemble mean indicated that the majority of model year runs were scored as suitable habitat (ensemble mean value of 0.5 or greater). Apply ensemble mean to within-model/across years for forecast data (keep 3 models separate). To identify regions with the greatest susceptibility to marine invasive colonization, we summed the number of taxa found to have suitable habitat in the pixel during each study period.  
## load packages
require(raster)
require(sp)
require(rgdal)
require(rgeos)
require(viridis) ## replace color brewer with viridis colors, Thanks to Jordan Watson!
baseDir <-  'D:/HelpingOthers/ABSIlcc/NFWF2016/Models'
setwd(file.path(baseDir, 'rCode'))
## USED for PLOTTING ###########################################################
PrintEverthing = F
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
## END USED for PLOTTING #######################################################
# Make data.frame (output) with details on all .Rdata output files (file name, species, model, study period, and aspect)
aspects <- c('Reproduction', 'Survival', 'SurvivalWeeks')
for(a in aspects){
	cat(a, fill = T)
	ff <- list.files(file.path(baseDir, 'rOut', a), pattern = 'rData')
	out <- data.frame(files = ff, stringsAsFactors=F)
	for(r in 1:nrow(out)){
		f <- out[r, 1]
		out$Taxa[r] <- unlist(strsplit(f, '_'))[1]
		if(a == 'SurvivalWeeks'){
			add = 2
		}else{
			add = 0
		}
		out$Model[r] <- unlist(strsplit(f, '_'))[3+add]
		out$StudyPeriod[r] <- substr(unlist(strsplit(f, '_'))[4+add], 1, 9)
		out$aspect[r] <- a
	}
	if(a == aspects[1]){
		output <- out
	}else{
		output <- rbind(output, out)
	}
}
#
spp <- unique(output$StudyPeriod)
taxa <- unique(output$Taxa)
##################################
# Create a taxaAspect column combining Taxa + aspect 
output$taxaAspect <- paste(output$Taxa, output$aspect, sep='_')
ttaa <- sort(unique(output$taxaAspect))
for(ta in ttaa){
	cat(ta, fill = T)
	(taxon <- unique(output[output$taxaAspect == ta, ]$Taxa))
	(aspect <- unique(output[output$taxaAspect == ta, ]$aspect))
	cat('... processing', taxon, aspect, fill =T)
	mm <- unique(output[output$taxaAspect == ta, ]$Model)
	sp <- spp[1]
	ff <- output[output$taxaAspect == ta & output$StudyPeriod == sp, ]$files
		
		
	}

##################################
# Process by study period
# Summarize year-round survival.
# for study period 1 (hindcast):   summarize the number of taxa with year-round survival as an ensemble mean (across all 3 models).
# If a taxon is predicted to have suitable habitat in a given pixel for >= 7 years, then count this taxon as having modeled survival (suitability = 1) in this pixel.
sp1 <- spp[1]
# for study period 2 (forecast):	summarize the number of taxa with year-round survival for each model.
# If a taxon is predicted to have suitable habitat in a given pixel for >= 7 years, then count this taxon as having modeled survival (suitability = 1) in this pixel.
sp2 <- spp[2]

## process study period 1: for each taxon, take mean survival across all models
mm <- unique(output$Model)
for(model_i in 1:3){
	m = mm[model_i]
	cat('\nExploring survival modeled by', m, fill =T)
	for(i in 1:length(taxa)){
		taxon <- taxa[i]
		cat('\n... processing', taxon, fill = T)
		criteria = (output$Taxa == taxon & output$StudyPeriod == sp1 & output$aspect == 'Survival' & output$Model == m)
		f <- file.path(baseDir, 'rOut', 'Survival', output[criteria, ]$files)
		cat("....", f, fill =T)
		load(f, verbose = T)
		criteria = (output$Taxa == taxon & output$StudyPeriod == sp1 & output$aspect == 'SurvivalWeeks' & output$Model == m)
		f <- file.path(baseDir, 'rOut', 'SurvivalWeeks', output[criteria, ]$files)
		cat("....", f, fill =T)
		load(f, verbose = T)
		#rSurvival <- (sum(s_OK.S) >= 7)
		print(mean(values((sum(s_OK.S) >= 7)), na.rm=T))
		plot(sum(s_OK.S) >= 7)
		if(i ==1){ ##if first species
			nSurvival <- (sum(s_OK.S) >= 7) ##Survival
			nWeeksSurvival <- mean(s_nWeeks.S) ##SurvivalWeeks
			#nSurvival
		}else{ 
			nSurvival <- nSurvival + (sum(s_OK.S) >= 7) ##add to previous results
			nWeeksSurvival <- nWeeksSurvival + mean(s_nWeeks.S)
			#nSurvival
		}
		rm(s_OK.S)
		rm(s_nWeeks.S)
		}
		nSurvival
		plot(nSurvival)
		## Summarize over all 3 models
		if(model_i == 1){
			CountSurvival <- stack(nSurvival)
			nWeeksSurvival.S <- stack(nWeeksSurvival)
		}else{
			CountSurvival <- addLayer(CountSurvival, nSurvival)
			nWeeksSurvival.S <- addLayer(nWeeksSurvival.S, nWeeksSurvival)
		}
	}
ensembleMeanCountSurvival_sp1 <- mean(CountSurvival)
ensembleMeanWeeksSurvival_sp1 <- mean(nWeeksSurvival.S)
plot(ensembleMeanCountSurvival_sp1,main="Average year-round survival")
plot(ensembleMeanWeeksSurvival_sp1,main="Average weekly survival")
#

####for forecast data
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
		criteria = (output$Taxa == taxon & output$StudyPeriod == sp2 & output$aspect == 'SurvivalWeeks' & output$Model == m)
		f <- file.path(baseDir, 'rOut', 'SurvivalWeeks', output[criteria, ]$files)
		cat("....", f, fill =T)
		load(f, verbose = T)
		print(mean(values((sum(s_OK.S) >= 7)), na.rm=T))
		if(i ==1){
			nSurvival <- (sum(s_OK.S) >= 7)
			nWeeksSurvival <- mean(s_nWeeks.S)
		}else{
			nSurvival <- nSurvival + (sum(s_OK.S) >= 7)
			nWeeksSurvival <- nWeeksSurvival + mean(s_nWeeks.S)
		}
		rm(s_OK.S)
		rm(s_nWeeks.S)
		}
		nSurvival
		plot(nSurvival)
		##Summarize across the models
	if(model_i ==1){
		CountSurvival_sp2 <- nSurvival
		nWeeksSurvival_sp2 <- nWeeksSurvival
	}else{
		CountSurvival_sp2 <- addLayer(CountSurvival_sp2, nSurvival)
		nWeeksSurvival_sp2 <- addLayer(nWeeksSurvival_sp2, nWeeksSurvival)
	}
	}
	names(CountSurvival_sp2) <- mm
	names(nWeeksSurvival_sp2) <- mm
	
## Plot Year-Round Survival count
e <-  extent(CountSurvival_sp2)
## The following lines adjust the extent of the resultant plot to best show the Bering Sea.
		e[1] <- e[1] + 400*1000
		e[2] <- e[2] - 375*1000
		e[3] <- e[3] + 175*1000
		e[4] <- e[4] - 350*1000
plotName <- 'Summary_yearRound_Survival.png'
fileName<-file.path(file.path(baseDir, 'plots', plotName))
Expansion = 4
plotResX = 2500 *Expansion
plotResY = 1.04*(plotResX * ((e[4] - e[3])/ (e[2] - e[1])))
plotCarto <- function(){
	plot(land, col=landColor, border = landColor, add = T)
	text(y = rep(e[4] - 310*1000, 2), x = c(e[1] + 670*1000, e[2] - 500*1000), labels = c('Russia', 'Alaska'), 
	col = 'darkgray', cex = 3*Expansion, font = 2, family = "sans")
	points(ports.prj, cex=1*Expansion, pch=19, col=portColor)
	text(ports.prj, labels = ports@data$IATA, pos = 4, col=portColor, font = 2, cex = 2*Expansion, family = 'sans',halo = TRUE, hw = 0.65, hc = "white")
	}
bb.max <- max(c(max(values(ensembleMeanCountSurvival_sp1), na.rm=T), max(values(CountSurvival_sp2), na.rm=T)))
bb <- seq(from=0, to = bb.max, by = 2)
png(fileName, width = plotResX , height = plotResY)
	par(mfrow=c(2,2), bg = 'white')
	arg <- list(at=seq(from=0, to = bb.max, by = 5), labels=seq(from=0, to = bb.max, by = 5), cex = 4*Expansion, 
			cex.axis = 3, col.axis = 'white', col.ticks = 'white', col = 'white', line = -0.5)
	plot(ensembleMeanCountSurvival_sp1, breaks = bb, col = viridis::plasma(length(bb)), xaxt='n', yaxt='n', bty='n', ext=e,
	axis.args=arg, box=FALSE)
	plotCarto()
	text(x=e[1] + 200*1000, y=e[4] - 130*1000, labels = paste0(sp1, '\n Ensemble Mean'), ##'\n Ensemble\n Mean'
	col = portColor, cex = 4.5*Expansion, font = 2, family = 'sans', pos = 4)
	#
	for(model_i in 1:3){
	plot(CountSurvival_sp2[[model_i]], , breaks = bb, col = viridis::plasma(length(bb)), xaxt='n', yaxt='n', bty='n', ext=e,
	legend=FALSE, box=FALSE)
	plotCarto()
	text(x=e[1] + 200*1000, y=e[4] - 130*1000, labels = paste0(sp2, '\n Model ', mm[model_i]), 
	col = portColor, cex = 4.5*Expansion, font = 2, family = 'sans', pos = 4) #'\n Model\n '
	}
	#
dev.off()
#
## Plot normalized N Weeks Survival
plotName <- 'Summary_N_Weeks_Survival.png'
fileName<-file.path(file.path(baseDir, 'plots', plotName))
Expansion = 4
bb.max <- max(c(max(values(ensembleMeanWeeksSurvival_sp1), na.rm=T), max(values(nWeeksSurvival_sp2), na.rm=T)))
bb.min <- min(c(min(values(ensembleMeanWeeksSurvival_sp1), na.rm=T), min(values(nWeeksSurvival_sp2), na.rm=T)))
bb <- seq(from=300, to = 2000, by = 100)
png(fileName, width = plotResX , height = plotResY)
	par(mfrow=c(2,2), bg = 'white')
	arg <- list(at=seq(from=300, to = 2000, by = 100), labels=seq(from=300, to = 2000, by = 100), cex = 4*Expansion, 
			cex.axis = 3, col.axis = 'white', col.ticks = 'white', col = 'white', line = -0.5)
	plot(ensembleMeanWeeksSurvival_sp1, breaks = bb, col = viridis::plasma(length(bb)), xaxt='n', yaxt='n', bty='n', ext=e,
	axis.args=arg, box=FALSE)
	plotCarto()
	text(x=e[1] + 200*1000, y=e[4] - 130*1000, labels = paste0(sp1, '\n Ensemble Mean'), 
	col = portColor, cex = 4.5*Expansion, font = 2, family = 'sans', pos = 4)
	#
	for(model_i in 1:3){
	plot(nWeeksSurvival_sp2[[model_i]], breaks = bb, col = viridis::plasma(length(bb)), xaxt='n', yaxt='n', bty='n', ext=e,
	legend=FALSE, box=FALSE)
	plotCarto()
	text(x=e[1] + 200*1000, y=e[4] - 130*1000, labels = paste0(sp2, '\n Model ', mm[model_i]), 
	col = portColor, cex = 4.5*Expansion, font = 2, family = 'sans', pos = 4) 
	}
	#
dev.off()
#


