###Can use this code to create overall taxa suitability map
###For each species, combine temp. & salinity rasters to generate overall suitability
###Sum of all species' rasters to figure out how many species can establish in each pixel
###Might want to add seasonal component here? Double-check if min/max of salinity is seasonal like temp.

###for each taxa, create winter (min) and summer (max) suitability for a) temp and b) salinity

##lattice set up
require(lattice)
panel.mean <- function(x, y, ...) {
	cat('Mean', fill =T)
    tmp <- tapply(x, y, FUN = mean); print(tmp)
    panel.points(x=tmp, y=seq_along(tmp), ...)
}
cexFactor = 1

SpTolerNA<-read.csv(file.path(baseDir, 'taxaData/Species_Tolerances.csv'))
#subset NAs for temp. only
SpToler<-subset(SpTolerNA,(!is.na(Min_TempC)&!is.na(Max_TempC)))


for (i in 1:length(SpToler$Sci_Name)){ 
	cat('...processing', SpToler$Sci_Name[i], fill =T)
	##define tolerance
	lowT<-min(SpToler$Min_TempC[i], SpToler$Min_ReproTempC[i], na.rm = T)
	highT<-max(SpToler$Max_TempC[i], SpToler$Max_ReproTempC[i], na.rm = T)
	thres.temp<- c(-Inf, lowT, 0,  lowT, highT, 1,  highT, Inf, 0)
	Tmat <- matrix(thres.temp, ncol=3, byrow=TRUE)
	##reclassify temp raster based on threshold
	#rMinTemp <- reclassify(r_T_MeanOfAnnual_Min, Tmat)
	#rMaxTemp <- reclassify(r_T_MeanOfAnnual_Max, Tmat)
	rMinTemp <- reclassify(new_T_Min, Tmat)
	rMaxTemp <- reclassify(new_T_Max, Tmat)
	#plot - keep separate because we are assessing winter (min. temp) and summer (max. temp?)
	
	##winter - using min temps
	plotName <- paste0(paste0(unlist(strsplit(as.character(SpToler$Sci_Name[i])," ")),collapse = "_"),'_W.png')
	plotTitle <- paste0('Winter suitability for ', SpToler$Sci_Name[i])
	png(filename = file.path(file.path(baseDir, 'plots/TaxaSuitability/Temp/Survival', plotName)), width = 1600, height = 1600,res=200)
	plot(rMinTemp,main=plotTitle,breaks=c(0,0.5,1),col=c("lavender","cornflowerblue"),ext=e,xaxt='n', yaxt='n', bty='n')
	dev.off()
	##summer - using max temps
	plotName <- paste0(paste0(unlist(strsplit(as.character(SpToler$Sci_Name[i])," ")),collapse = "_"),'_S.png')
	plotTitle <- paste0('Summer suitability for ', SpToler$Sci_Name[i])
	png(filename = file.path(file.path(baseDir, 'plots/TaxaSuitability/Temp/Survival', plotName)), width = 1600, height = 1600,res=200)
	plot(rMaxTemp,main=plotTitle,breaks=c(0,0.5,1),col=c("lavender","goldenrod1"),ext=e,xaxt='n', yaxt='n', bty='n')
	dev.off()	
	
	}
	
#rm(rMaxTemp,rMinTemp,thres.temp,Tmat,i,lowT,highT,plotName,plotTitle) #do not delete if combining temp & salinity

##repeat for salinity
SpToler<-subset(SpTolerNA,(!is.na(Min_Salinity)&!is.na(Max_Salinity)))

for (i in 1:length(SpToler$Sci_Name)){ 
	cat('...processing', SpToler$Sci_Name[i], fill =T)
	##define tolerance
	lowSal<-min(SpToler$Min_Salinity[i], SpToler$Min_ReproSal[i], na.rm = T)
	highSal<-max(SpToler$Max_Salinity[i], SpToler$Max_ReproSal[i], na.rm = T)
	thres.sal<- c(-Inf, lowSal, 0,  lowSal, highSal, 1,  highSal, Inf, 0)
	Tmat <- matrix(thres.sal, ncol=3, byrow=TRUE)
	##reclassify temp raster based on threshold
	#rMinSal <- reclassify(r_S_MeanOfAnnual_Min, Tmat)
	#rMaxSal <- reclassify(r_S_MeanOfAnnual_Max, Tmat)
	rMinSal <- reclassify(new_S_Min, Tmat)
	rMaxSal <- reclassify(new_S_Max, Tmat)
	
	#plot - keep separate because we are assessing winter (min) and summer (max.)
	
	##winter - using min temps
	plotName <- paste0(paste0(unlist(strsplit(as.character(SpToler$Sci_Name[i])," ")),collapse = "_"),'_W.png')
	plotTitle <- paste0('Winter suitability for ', SpToler$Sci_Name[i])
	png(filename = file.path(file.path(baseDir, 'plots/TaxaSuitability/Salinity/Survival', plotName)), width = 1600, height = 1600,res=200)
	plot(rMinSal,main=plotTitle,breaks=c(0,0.5,1),col=c("lavender","cornflowerblue"),ext=e,xaxt='n', yaxt='n', bty='n')
	
	##summer - using max temps
	plotName <- paste0(paste0(unlist(strsplit(as.character(SpToler$Sci_Name[i])," ")),collapse = "_"),'_S.png')
	plotTitle <- paste0('Summer suitability for ', SpToler$Sci_Name[i])
	png(filename = file.path(file.path(baseDir, 'plots/TaxaSuitability/Salinity/Survival', plotName)), width = 1600, height = 1600,res=200)
	plot(rMaxSal,main=plotTitle,breaks=c(0,0.5,1),col=c("lavender","goldenrod1"),ext=e,xaxt='n', yaxt='n', bty='n')
	dev.off()	
	}

rm(rMaxSal,rMinSal,thres.sal,Tmat,i,lowSal,highSal,plotName,plotTitle)

##########################################################
##########################################################
##REPEAT FOR REPRODUCTION USING DIFFERENT SUBSET OF DATA##
##########################################################
##########################################################
SpTolerNA<-read.csv(file.path(baseDir, 'taxaData/Species_Tolerances.csv'))
#subset NAs for temp. only
SpToler<-subset(SpTolerNA,(!is.na(Min_ReproTempC)&!is.na(Max_ReproTempC)))


for (i in 1:length(SpToler$Sci_Name)){ 
	cat('...processing', SpToler$Sci_Name[i], fill =T)
	##define tolerance
	lowT<-min(SpToler$Min_ReproTempC[i], na.rm = T)
	highT<-max(SpToler$Max_ReproTempC[i], na.rm = T)
	thres.temp<- c(-Inf, lowT, 0,  lowT, highT, 1,  highT, Inf, 0)
	Tmat <- matrix(thres.temp, ncol=3, byrow=TRUE)
	##reclassify temp raster based on threshold
	#rMinTemp <- reclassify(r_T_MeanOfAnnual_Min, Tmat)
	#rMaxTemp <- reclassify(r_T_MeanOfAnnual_Max, Tmat)
	rMinTemp <- reclassify(new_T_Min, Tmat)
	rMaxTemp <- reclassify(new_T_Max, Tmat)
	#plot - keep separate because we are assessing winter (min. temp) and summer (max. temp?)
	
	##winter - using min temps
	plotName <- paste0(paste0(unlist(strsplit(as.character(SpToler$Sci_Name[i])," ")),collapse = "_"),'_WRepro.png')
	plotTitle <- paste0('Winter repro. suitability for ', SpToler$Sci_Name[i])
	png(filename = file.path(file.path(baseDir, 'plots/TaxaSuitability/Temp/Repro', plotName)), width = 1600, height = 1600,res=200)
	plot(rMinTemp,main=plotTitle,breaks=c(0,0.5,1),col=c("lavender","cornflowerblue"),ext=e,xaxt='n', yaxt='n', bty='n')
	dev.off()
	##summer - using max temps
	plotName <- paste0(paste0(unlist(strsplit(as.character(SpToler$Sci_Name[i])," ")),collapse = "_"),'_SRepro.png')
	plotTitle <- paste0('Summer repro. suitability for ', SpToler$Sci_Name[i])
	png(filename = file.path(file.path(baseDir, 'plots/TaxaSuitability/Temp/Repro', plotName)), width = 1600, height = 1600,res=200)
	plot(rMaxTemp,main=plotTitle,breaks=c(0,0.5,1),col=c("lavender","goldenrod1"),ext=e,xaxt='n', yaxt='n', bty='n')
	dev.off()	
	
	}

rm(rMaxTemp,rMinTemp,thres.temp,Tmat,i,lowT,highT,plotName,plotTitle)

##repeat for salinity
SpToler<-subset(SpTolerNA,(!is.na(Min_ReproSal)&!is.na(Max_ReproSal)))

for (i in 1:length(SpToler$Sci_Name)){ 
	cat('...processing', SpToler$Sci_Name[i], fill =T)
	##define tolerance
	lowSal<-min(SpToler$Min_ReproSal[i], na.rm = T)
	highSal<-max(SpToler$Max_ReproSal[i], na.rm = T)
	thres.sal<- c(-Inf, lowSal, 0,  lowSal, highSal, 1,  highSal, Inf, 0)
	Tmat <- matrix(thres.sal, ncol=3, byrow=TRUE)
	##reclassify temp raster based on threshold
	#rMinSal <- reclassify(r_S_MeanOfAnnual_Min, Tmat)
	#rMaxSal <- reclassify(r_S_MeanOfAnnual_Max, Tmat)
	rMinSal <- reclassify(new_S_Min, Tmat)
	rMaxSal <- reclassify(new_S_Max, Tmat)
	
	#plot - keep separate because we are assessing winter (min) and summer (max.)
	
	##winter - using min temps
	plotName <- paste0(paste0(unlist(strsplit(as.character(SpToler$Sci_Name[i])," ")),collapse = "_"),'_WRepro.png')
	plotTitle <- paste0('Winter repro. suitability for ', SpToler$Sci_Name[i])
	png(filename = file.path(file.path(baseDir, 'plots/TaxaSuitability/Salinity/Repro', plotName)), width = 1600, height = 1600,res=200)
	plot(rMinSal,main=plotTitle,breaks=c(0,0.5,1),col=c("lavender","cornflowerblue"),ext=e,xaxt='n', yaxt='n', bty='n')
	
	##summer - using max temps
	plotName <- paste0(paste0(unlist(strsplit(as.character(SpToler$Sci_Name[i])," ")),collapse = "_"),'_SRepro.png')
	plotTitle <- paste0('Summer repro. suitability for ', SpToler$Sci_Name[i])
	png(filename = file.path(file.path(baseDir, 'plots/TaxaSuitability/Salinity/Repro', plotName)), width = 1600, height = 1600,res=200)
	plot(rMaxSal,main=plotTitle,breaks=c(0,0.5,1),col=c("lavender","goldenrod1"),ext=e,xaxt='n', yaxt='n', bty='n')
dev.off()	
	}

rm(rMaxSal,rMinSal,thres.sal,Tmat,i,lowSal,highSal,plotName,plotTitle)
