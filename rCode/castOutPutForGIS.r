## D:\HelpingOthers\ABSIlcc\NFWF2016\Models\rCode\castOutPutForGIS.r
require(raster)
require(rgdal)
require(sp)
require(RColorBrewer)
require(rasterVis)

plotOnly <- T

baseDir <- 'C:/ABSIlcc/NFWF2016/PMEL Models'
## Prepare plot
ContinentsDataFile<-file.path(baseDir, 'rCode/Continents.rData') 
load(ContinentsDataFile)
## Define projections
prj.StudyArea <- CRS("+proj=aeqd +lat_0=58.375 +lon_0=-176 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") 
prj.StudyAreaPositive <- CRS("+proj=aeqd +lat_0=55 +lon_0=190") # +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") #Lambert -170, 70
prj.geo<-CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0')
## Build study area polygon in projected coordinate system

# ## Bring in the study area from the shape file of large marine area.
# Bering <- readOGR(dsn = 'C:/ABSIlcc/NFWF2016/PMEL Models/presentations/Cartography/AK Ecoregion/Data', layer = 'World_Ecoregions_AK')
# Bering <- Bering[Bering@data$ECO_CODE %in% c(20053, 25046, 25014) ]
# Bering <- spTransform(Bering, prj.StudyArea)
# bb <- bbox(Bering)

	xLimits<-sort(c(165, -157))					## define the study area polygon using geographic coordinates (eastings WGS84)
	yLimits<-sort(c(51.25, 65.5))					## (northings WGS84)
	pts<-expand.grid(xLimits, yLimits)
    names(pts)<-c('x','y')
	coordinates(pts) <- ~x+y
	proj4string(pts) <- prj.geo
	
	
	pts.StudyArea<-spTransform(pts, prj.StudyArea)
	# ClipStudyArea_p = rgeos::gConvexHull(pts.StudyArea)  ## create the study area polygon



bb<-pts.StudyArea@bbox												## Build an extent box for the browse plot based on the pts.StudyArea@bbox
# bb[1,1]<-bb[1,1]*0.625  ## X east
# bb[1,2]<-bb[1,2]*0.68  ## X	west
# bb[2,1]<-bb[2,1]*0.9  	## Ymin
e<-extent(bb)
w = xmax(e) - xmin(e)
h = ymax(e) - ymin(e)
plotResX = 1000 
plotResY = plotResX * (h/w)

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

shelfColor <- 'darkgray' #'cadetblue4' ## #9190FF 
deepWaterColor <- '#000599'
landColor <-  '#b5c782' ##'#98b95f' ##'darkolivegreen4' #'cornsilk4' #'burlywood' ## 'yellow4' ## '#574835'#'burlywood4'  ## burlywood4
#temperature.colors <- brewer.pal(9,  'PuRd')[11:2]
purples <- c("#F7F4F933", paste0(brewer.pal(9,  'PuRd'), as.character(round(seq(from = 85,  to=99, length = 9)))), "#67001FF0")
pallet <- brewer.pal(9,  'Oranges')
pallet <- c('azure', pallet)
theBreaks <- seq(from = 0, to = 1, length.out = 11)

## 


##

ff<-data.frame(fileName =list.files(path = file.path(baseDir, 'rOut'), pattern = '.rData'), stringsAsFactors = F)
ff$taxa <- NA
ff$model <- NA
ff$studyPeriod <- NA
for(i in 1:nrow(ff)){
	f <- ff$fileName[i]
	fs <- unlist(strsplit(unlist(strsplit(f, ".rData")), '_'))
	ff[i, ]$taxa <- fs[1]
	ff[i, ]$model <- fs[3]
	ff[i, ]$studyPeriod <- fs[4]
}
ff<-ff[order(ff$taxa, ff$studyPeriod),]
#
tt<-unique(ff$taxa)
spp <- unique(ff$studyPeriod)
for(Taxa in tt[1:length(tt)]){ ## loop within each taxa
	#debug Taxa = tt[1]
	cat('... working with', Taxa, fill = T)
	for(sp in spp){ ## loop within each study period
		cat('    ...', sp, fill = T)
		f_t_spp = ff[ff$taxa == Taxa & ff$studyPeriod == sp, ]$fileName ## select the file name of the .rData file)
		if(length(f_t_spp)>0){
			for(i in 1:length(f_t_spp)){
				f = f_t_spp[i]
				cat('       ', i, f, fill = T)
				load(file.path(baseDir, 'rOut', f))	## load the rData file
				nn <- names(sgdf.Max)
				
				r_Invadable <- mean(s_OK) ## summarize the annual 'invadable' classificaiton for each pixel as the mean, place into a raster
				if(i ==1){
					s_Ensemble_Invadable <- stack(r_Invadable)
				}else{
					s_Ensemble_Invadable <- stack(s_Ensemble_Invadable, r_Invadable)
				}
				rm(r_Invadable)
				(s_Ensemble_Invadable)
			}
		
			if(!plotOnly){
				fileName = file.path(baseDir, 'GIS', paste0(Taxa, '_', sp, '_Ensamble_mean_invadability.tif'))
				raster::writeRaster(mean(s_Ensemble_Invadable), filename=fileName, format='GTiff', overwrite=TRUE)
			}
			## Build a plot
			plotName <- paste0(Taxa, '_', sp, '_Ensamble_mean_invadability.png')
			cat('... plotting ...', plotName, fill =T)
			png(filename = file.path(file.path(baseDir, 'plots', 'ensemble', plotName)), width = plotResX , height = plotResY)
				plot(bath.r, ext=e, col=c(shelfColor, shelfColor, landColor), 
					breaks=c(-10000, -200, 0, 6000),  	## Plot the ocean depth
					legend=FALSE, ann=FALSE, xaxt='n', yaxt='n', bty='n', axes=FALSE, col.axes = 'white')
				plot(mean(s_Ensemble_Invadable), ext=e, col=pallet, breaks = theBreaks, add = T, legend=F)#, lab.breaks=theBreaks) ##
				plot(bath.r, ext=e, col=landColor, breaks=c(0, 6000),  legend=FALSE, add = T)				## Plot the land
				legend(legend = theBreaksTxt, "topright", inset=.125, title="Invadability\n",  fill=pallet[c(10,10:1)], border = pallet[c(10,10:1)], horiz=F, bty='n', cex =2.5,  y.intersp =0.5 ) ## theBreaks[11:1]
				titleTxt <- paste(paste(unlist(strsplit(Taxa, "[.]")), collapse = ' '), sp)
				title(main = titleTxt, cex = 2)
			dev.off()
			# par(fg='azure', bg='black')
				# plot(mean(s_Ensemble_Invadable), col=heat.colors(10), xaxt='n' , yaxt='n', bty='n', main = paste(Taxa, sp), col.main='yellow')
			# Sys.sleep(time=2)
			# dev.off()
			
			## 
			
			
			#  rm(s_Ensemble_Invadable)
		} ## End if found data for this taxa and study period
	} ## End study period loop
	#
} ## End taxa loop
#
