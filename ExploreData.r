baseDir <- 'D:/HelpingOthers/ABSIlcc/NFWF2016/PMEL Models/'
baseDir <- 'C:/ABSIlcc/NFWF2016/PMEL Models'
setwd(file.path(baseDir, 'rCode'))
require(raster)
require(ncdf4) 
require(sp)
require(rgdal)
require(maptools)## Used for some map tools and the base costline
require(dplyr)
require(RColorBrewer)
require(rasterVis)

## Define projections
prj.StudyArea <- CRS("+proj=aeqd +lat_0=55 +lon_0=-170 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") 
prj.StudyAreaPositive <- CRS("+proj=aeqd +lat_0=55 +lon_0=190") # +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") #Lambert -170, 70
prj.geo<-CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0')
## Build study area polygon in projected coordinate system
xLimits<-sort(c(177, -156.5))					## define the study area polygon using geographic coordinates (eastings WGS84)
yLimits<-sort(c(50, 65.6))					## (northings WGS84)
pts<-expand.grid(xLimits, yLimits)
names(pts)<-c('x','y')
coordinates(pts) <- ~x+y
proj4string(pts) <- prj.geo
pts.StudyArea<-spTransform(pts, prj.StudyArea)
ClipStudyArea_p = rgeos::gConvexHull(pts.StudyArea)  ## create the study area polygon

bb<-pts.StudyArea@bbox												## Build an extent box for the browse plot based on the pts.StudyArea@bbox
# bb[1,1]<-bb[1,1]*0.625  ## X east
# bb[1,2]<-bb[1,2]*0.68  ## X	west
# bb[2,1]<-bb[2,1]*0.9  	## Ymin
e<-extent(bb)
w = xmax(e) - xmin(e)
h = ymax(e) - ymin(e)
plotResX = 1000 
plotResY = plotResX * (h/w)


shelfColor<-'cadetblue4' ## #9190FF 
landColor<-'yellow4' ## '#574835'#'burlywood4'  ## burlywood4
salt.colors <- brewer.pal(10, 'Spectral')[10:1]
temperature.colors <- brewer.pal(10,  'RdBu')[10:1]
## 
## Bathymetry
## DUE to problem with antimeridian in current r version, I have run this portion of the script on an old r verions (v. 3.2.5)
## This requires attention. NOTE that 
## The solution to the previous post addressing projection of rasters across the antimeridian 
## (http://r-sig-geo.2731867.n2.nabble.com/coercing-marmap-bathy-object-to-raster-spanning-the-antimeridian-td7589828.html#a7589841)
## no-longer functions (as of 24 June 2016). SEE email correspondence with Micheal Sumner and Melanie Bacou  (mel@mbacou.com).
BathymetryDataFile<-file.path(baseDir, 'rCode/ETOPO1.rData') 
if(!file.exists(BathymetryDataFile)){
	require(marmap)
	bath <- getNOAA.bathy(xLimits[1]+15, xLimits[2]-10, yLimits[1]-5, yLimits[2]+10, resolution = 1, antimeridian = TRUE, keep =T)
	bath.r.geo<-marmap::as.raster(bath)
	bath.r.1<-projectRaster(from=bath.r.geo, res=2000, crs=prj.StudyAreaPositive, method="bilinear", over=TRUE)
	bath.r<-projectRaster(from=bath.r.1, res=2000, crs=prj.StudyArea, method="bilinear", over=TRUE)
	bath200<-rasterToContour(bath.r, levels=-200)
	save(bath.r, bath200, file=BathymetryDataFile)
	unloadNamespace('marmap')
	rm(bath, bath.r.geo, bath.r.1)
}else{
	load(BathymetryDataFile)
}
bath0_10_20_200<-rasterToContour(bath.r, levels=c(0, -10, -20, -200))
## bath.r[values(bath.r)>0]<-NA
##
### Base coastal map
ContinentsDataFile<-file.path(baseDir, 'rCode/Continents.rData') 
if(!file.exists(ContinentsDataFile)){
	dsn<-"D:/Walrus/BaseMaps/ESRI_DATA/Continents.shp"
	ogrInfo(dsn, layer='Continents')
	landAll <- readOGR(dsn, layer='Continents')
	landAll <- landAll[c(1:3),]                                 # Keep northern continents only for viewing purposes
	land <- spTransform(landAll, prj.StudyArea)                           # Transform from WGS84 to polar stereographic projection
	land <- as(land, "SpatialPolygons")                         # Strip out the dataframe
	iceColors <- colorRampPalette(colors=c('aliceblue', 'darkblue'), space='rgb')(12)
	rm(list=c('landAll'))
	save(land, file = ContinentsDataFile)
}else{
	load(ContinentsDataFile)
}
#
land.clip<-rgeos::gDifference(ClipStudyArea_p, land, byid=FALSE)

## Define area of the ports.
ports <- data.frame(port = c('Dutch', 'St. Paul', 'Nome'), lat = c( 53.859726,  57.124804,  64.501380), lon = c(-166.577000, -170.278313, -165.426569)) 
coordinates(ports) <- c('lon', 'lat') ## cast as spatial points
proj4string(ports) <- prj.geo
ports.prj<-spTransform(ports, prj.StudyArea) ## project the port to the study area projection
ports.buffer <- rgeos::gBuffer(ports.prj, width = 20*1000, byid = T)  ## buffer the port by 20 km

## Lattice set up
###################### The Good Plot #################
panel.mean <- function(x, y, ...) {
	cat('Mean', fill =T)
    tmp <- tapply(x, y, FUN = mean); print(tmp)
    panel.points(x=tmp, y=seq_along(tmp), ...)
}
cexFactor = 1

#
vars<-c('sea_water_temperature', 'salinity') 
MAX <- F
ff<-list.files(path = file.path(baseDir, 'rData'))
for(i in 1:length(ff)){ ## ff[c(2,4)]
	f = ff[i]
	cat('=== Handling ===', f, fill =T)
	load(file.path(baseDir, 'rData', f))

	## Consider mean and SE of salinity during peak growth weeks (week 20 - 40).
	ddPrimeWeeks<-tbl_df(sgdf.Max@data[, substring(names(sgdf.Max@data), 6,7) %in% as.character(20:40)])
	# (TheMax <- max(apply(ddPrimeWeeks, 1, max, na.rm=T), na.rm=T))
	# (TheMin <- min(apply(ddPrimeWeeks, 1, min, na.rm=T), na.rm=T))
	if(MAX){
		ddPW_max <- data.frame(meanPW = apply(ddPrimeWeeks, 1, max, na.rm=T)) ## Take the mean across the prime weeks.
		sgdf_PW@data <- ddPW_max
		plotName <- paste0('Maximum', unlist(strsplit(f, ".rData")),  '.png')
		theTitle <- paste0('Maximum', unlist(strsplit(f, ".rData")),  ' during the prime season')
	}else{
		ddPW_mean <- data.frame(meanPW = apply(ddPrimeWeeks, 1, mean, na.rm=T)) ## Take the mean across the prime weeks.
		sgdf_PW@data <- ddPW_mean
		plotName <- paste0(unlist(strsplit(f, ".rData")),  '.png')
		theTitle <- paste0(unlist(strsplit(f, ".rData")),  ' during the prime season')
	}
	sgdf_PW<-sgdf.Max ## Place the mean value for each cell back into a sgdf	
	PW.r<-raster(sgdf_PW)
	
	if(length(grep("sal", f))>0){
		pallet = salt.colors
		theBreaks = seq(from = 26, to = 44, length.out = 11)
	}else{
		if(MAX){
			pallet <- temperature.colors
			theBreaks <- seq(from = 0, to = 18, length.out = 11)
		}else{
			pallet <- temperature.colors
			theBreaks <- seq(from = -3, to = 10, length.out = 11)
		}
	}
	#
	cat('... plotting ...', fill =T)
	png(filename = file.path(file.path(baseDir, 'plots', plotName)), width = plotResX , height = plotResY)
		plot(bath.r, ext=e, col=c("black", shelfColor, landColor), 
					breaks=c(-10000, -200, 0, 6000),  	## Plot the ocean depth
					legend=FALSE, ann=FALSE, xaxt='n', yaxt='n', bty='n')
		plot(PW.r, ext=e, col=pallet, breaks = theBreaks, add = T, legend=T, lab.breaks=theBreaks) ##
		#levelplot(PW.r, ext=e, col=pallet, breaks = theBreaks, add = T) ##
		plot(bath.r, ext=e, col=landColor, breaks=c(0, 6000),  legend=FALSE, add = T)				## Plot the land
		##‰
		title(main = theTitle)
	dev.off()
	sgdf.Max.prj<-spTransform(sgdf.Max, prj.StudyArea)
	oo<-over(ports.buffer, sgdf.Max.prj)
	oo$port <- ports@data$port
	go<-gather(oo, key=date, value = value, -port)
	go$metric <- unlist(strsplit(unlist(strsplit(f, ".rData")), '_'))[length(unlist(strsplit(unlist(strsplit(f, ".rData")), '_')))]
	go$Model <- unlist(strsplit(unlist(strsplit(f, ".rData")), '_'))[2]
	go$studyPeriod <- unlist(strsplit(unlist(strsplit(f, ".rData")), '_'))[3]
	go$week <- as.integer(substring(go$date, 6,7))
	go$year <- as.integer(substring(go$date, 1,4))
	if(i == 1) {
		portData <- go
	}else{
		portData <- rbind(portData, go)
	}
}
#bwplot(temperature ~ week | portIdF, data = go)
save(portData, file= file.path(baseDir, 'rData', 'portData.rData'))
portData$model <- factor(portData$Model)
Metrics <- unique(portData$metric)
cexFactor <- 3
for(Metric in Metrics){
	plotName <- paste0(Metric, '_bwPlot.png')
	yLabel <- paste0("Sea ", Metric, " in upper layers")
	png(filename = file.path(file.path(baseDir, 'plots', plotName)), width = plotResX*2.5 , height = plotResY*2.5)
		d<-portData[portData$metric == Metric & portData$week>10, ]
		#par(fg='azure', bg='black')
		BWPlot <- bwplot(value ~ week | port,  
			data=d, 
			groups = model, drop.unused.levels =T,
			box.width = 1/5,
			auto.key = list(points = FALSE, rectangles = TRUE, space = "right", col=c('#333300', '#0000ff', '#330033', '#003333')),   ## T, ## 
			panel = panel.superpose, 
			panel.groups = function(x, y, ..., group.number) { 
			  panel.bwplot(x + (group.number-1.25)/5, y, ...) ##?? changed -1.5)/3 to -1.25)/5
			},
			xlab=list(label='Week of the year', cex=1*cexFactor),
			ylab=list(label=yLabel, cex=1*cexFactor),pch=9, col='#ff090905',
			par.settings = list(strip.background=list(col="lightgrey"), box.rectangle=list(col='#0066ff33', fill='#0066ff33')),
			scales=list(x=list(at=unique(d$week)[unique(d$week) %% 2 ==0] - 9, 
								labels=as.character(unique(d$week)[unique(d$week) %% 2 ==0]), 
								cex=0.6*cexFactor),  
						y=list(cex=0.6*cexFactor)), 
			layout = c(1,length(levels(go$port))), horizontal =F)
			 #
		print(BWPlot)
		#
	dev.off()
}
#
