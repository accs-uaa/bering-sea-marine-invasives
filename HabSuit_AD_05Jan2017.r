##Habitat suitability

#create binary suitability map for each species based on temp/sal tolerance

#read files
SpToler<-read.csv(file.path(baseDir, 'taxaData/Species_Tolerances.csv'))
##models
ff<-list.files(path = file.path(baseDir, 'rData')) 


###still need to loop all of this...
MAX<-F
f<-ff[4] ##hindcast temp data only - but use tony's loop in final code
load(file.path(baseDir, 'rData', f)) 

##from ExploreData script
ddPrimeWeeks<-tbl_df(sgdf.Max@data[, substring(names(sgdf.Max@data), 6,7) %in% as.character(20:40)]) ##sets weeks of interest 20-40
sgdf_PW<-sgdf.Max ##Recreate sgdf similar to sgdf.Max, but only considering (mean/max) across prime weeks
	if(MAX){
		ddPW_max <- data.frame(meanPW = apply(ddPrimeWeeks, 1, max, na.rm=TRUE)) ## Take the max across the prime weeks.
		sgdf_PW@data <- ddPW_max
		plotName <- paste0('Maximum', unlist(strsplit(f, ".rData")),  '.png')
		theTitle <- paste0('Maximum', unlist(strsplit(f, ".rData")),  ' during the prime season')
	}else{
		ddPW_mean <- data.frame(meanPW = apply(ddPrimeWeeks, 1, mean, na.rm=TRUE)) ## Take the mean across the prime weeks.
		sgdf_PW@data <- ddPW_mean
		plotName <- paste0(unlist(strsplit(f, ".rData")),  '.png') #plot filename becomes filename.png
		theTitle <- paste0(unlist(strsplit(f, ".rData")),  ' during the prime season') #plot title becomes filename + prime season
	}
	
	PW.r<-raster(sgdf_PW)


##working on it...	
for (i in 1:length(SpToler$Sci_Name)){ 
low<-SpToler$Min_TempC[i]
high<-SpToler$Max_TempC[i]
threshold <- c(-Inf, low, 0,  low, high, 1,  high, Inf, 0)
tmat <- matrix(threshold, ncol=3, byrow=TRUE)
Toler.rast <- reclassify(PW.r, tmat)
plot(Toler.rast,main=paste0('Suitability for ', SpToler$Sci_Name[i]))
}






################################		
############JUNK################
################################

##try using reclassify
# all values >= 0 and <= 0.25 become 1, etc.
threshold <- c(-Inf, low, 0,  low, high, 1,  high, Inf, 0)
tmat <- matrix(threshold, ncol=3, byrow=TRUE)
Toler.rast <- reclassify(PW.r, tmat)
plot(Toler.rast)

##raster calc
low<-11
high<-16
PW.r[PW.r > low&PW.r < high] <- 0 ##something doesn't work because less is shown if low is lower
PW.r[PW.r > 0] <- 1
plot(PW.r)