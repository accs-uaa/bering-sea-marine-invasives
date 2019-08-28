##Create maps describing average (model ensemble) for a) temperature and b) salinity conditions in the Bering Sea under i. current (2003-2012), and ii. future (2030-2039) projections
##Models used are three

#Last updated: 26 January 2018

#Authors: A. Droghini (adroghini@alaska.edu)
#		 A.S. Fischbach (afischbach@usgs.gov)

require(raster)
require(sp)
cexFactor = 1
#baseDir <- 'D:/HelpingOthers/ABSIlcc/NFWF2016/PMEL Models'
setwd(file.path(baseDir, 'rCode'))

ff<-data.frame(fileName = list.files(path = file.path(baseDir, 'rData'), pattern = '.rData'), 	
	stringsAsFactors =F) ## retain only temperature files
ff<-subset(ff,fileName!="portData.rData")
ff$model <- NA
ff$studyPeriod <- NA
ff$vari<-NA

for(i in 1:nrow(ff)){
	f <- ff$fileName[i]
	fs <- unlist(strsplit(unlist(strsplit(f, ".rData")), '_'))
	ff[i, ]$model <- fs[2]
	ff[i, ]$studyPeriod <- fs[3]
	ff[i, ]$vari<-fs[(length(fs))]
}

ff<-ff[order(ff$studyPeriod),]
spp <- unique(ff$studyPeriod)
vari <-unique(ff$vari)

for(z in 1:length(vari)){ ##loop through variables (temperature or salinity)
	varr<-vari[z]
	cat('[ Processing ', varr, ']', fill = T)
	ff.v <- ff[ff$vari == varr, ]$fileName
	
	for(j in 1:length(spp)){ ##loop through study periods 
		sp<-spp[j]
		cat('[ Processing ', sp, ']', fill = T)
		ff.sp <- ff[ff$studyPeriod == sp & ff$vari == varr, ]$fileName
	
	for(f in ff.sp){ ##Step through each model file for this study period
		cat('       ', f, fill = T)
		load(file.path(baseDir, 'rData', f))	## load rData file sgdf.Max
		nn <- names(sgdf.Max)
		yy <- unique(substring(nn, 1, 4))
		
		if (sp=="2003-2013"){
			yy <- yy[1:10] #limit to years 2003-2012
			}else{
			yy<-yy[2:11] #limit to years 2030-2039
			} 
		
		for(y in yy){ #calculate min. & max. temp. for every year
			cat('... processing', y, fill =T)
			ColumnsToSelect <- which(substring(nn, 1,4) == y)
			r.minT_y <- min(raster::stack(sgdf.Max[, , ColumnsToSelect]))
			r.maxT_y <- max(raster::stack(sgdf.Max[, , ColumnsToSelect]))
			#r.meanT_y <- mean(raster::stack(sgdf.Max[, , ColumnsToSelect]))
			
			if(y == yy[1]){ #cast as raster stack
				s.minT <- raster::stack(r.minT_y)
				s.maxT <- raster::stack(r.maxT_y)
				#s.meanT <- raster::stack(r.meanT_y)
			}else{
				s.minT <- raster::stack(s.minT, r.minT_y)
				s.maxT <- raster::stack(s.maxT, r.maxT_y)
				#s.meanT <- raster::stack(s.meanT, r.meanT_y)
			}
		}
		
		if(f == ff.sp[1]){ #average across years
			s.minT.sp <- raster::stack(mean(s.minT))
			s.maxT.sp <- raster::stack(mean(s.maxT))
			#s.meanT.sp <- raster::stack(mean(s.meanT))
		}else{
			s.minT.sp <- raster::stack(mean(s.minT), s.minT.sp)
			s.maxT.sp <- raster::stack(mean(s.maxT), s.maxT.sp)
			#s.meanT.sp <- raster::stack(mean(s.meanT), s.meanT.sp)
		}
		
	} ## end of model loop

	#average across 3 models
	avg.minT.sp <- mean(s.minT.sp)
	avg.maxT.sp <- mean(s.maxT.sp)
	#r.meanT.sp <- mean(s.meanT.sp)
	
	#clip to Bering Sea study area
	r.minT.sp<-avg.minT.sp*Bering_200m 
	r.maxT.sp<-avg.maxT.sp*Bering_200m
	
	#quick plot
	plot(r.minT.sp, col = heat.colors(10)[10:1])
	plot(r.maxT.sp, col = heat.colors(10)[10:1])
	
	#save as rasters
	
	#fileName <- paste0(sp, 'ensembleMeanMinTemp.rData')
	#save(r.minT.sp, file = file.path(baseDir, 'rOut', fileName)) ##Save raster file in native format
	saveDir <- 'C:/Users/adroghini/Documents/Marine-invasives'
	
	if(varr == "temperature"){ 
	fileName = file.path(saveDir, 'GIS', 'Climate-variables', paste0(sp, '_Ensemble_minTemp.tif'))
	raster::writeRaster(r.minT.sp, filename=fileName, format='GTiff', overwrite=TRUE)
	fileName = file.path(saveDir, 'GIS', 'Climate-variables', paste0(sp, '_Ensemble_maxTemp.tif'))
	raster::writeRaster(r.maxT.sp, filename=fileName, format='GTiff', overwrite=TRUE)
	}else{
	fileName = file.path(saveDir, 'GIS', 'Climate-variables', paste0(sp, '_Ensemble_minSal.tif'))
	raster::writeRaster(r.minT.sp, filename=fileName, format='GTiff', overwrite=TRUE)
	fileName = file.path(saveDir, 'GIS', 'Climate-variables', paste0(sp, '_Ensemble_maxSal.tif'))
	raster::writeRaster(r.maxT.sp, filename=fileName, format='GTiff', overwrite=TRUE)
	}
	
	#fileName = file.path(baseDir, 'GIS', 'Rasters', paste0(sp, '_Ensemble_mean_MaxTemp.tif'))
	#raster::writeRaster(r.meanT.sp, filename=fileName, format='GTiff', overwrite=TRUE)
	
} ## end sp loop
##
} ##end variable loop


##Clean up workspace
rm(ff,ColumnsToSelect,f,fileName,r.maxT_y,r.meanT_y,r.minT_y,s.maxT,s.maxT.sp,s.meanT,s.meanT.sp,s.minT,s.minT.sp,sgdf.Max,y,yy,nn,z,vari,varr,sp,spp,saveDir,ff.sp,ff.v,fs,avg.maxT.sp,avg.minT.sp,i,j,r.minT.sp,r.maxT.sp)