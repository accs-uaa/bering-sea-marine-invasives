##Extracting predictions for every species
##For final report .pdf maps

require(sp)
require(rgdal)

#####################################
#The following is for weekly survival
#####################################

ff <- list.files(path=file.path(baseDir, 'rOut', 'SurvivalWeeks'), pattern = 'rData')
#prj.crs<-CRS("+init=epsg:3571") ##set projection for rasters: NAD 83 Alaska Albers

for (i in 1:length(ff)){
f<-ff[i]
load(file.path(baseDir, 'rOut', 'SurvivalWeeks',f)) #loads s_nWeeks.S raster stack
    taxa <- unlist(strsplit(f, '_'))[1]
	modname <- substr(unlist(strsplit(f, '_'))[5], 1, 5)
    study.period <- substr(unlist(strsplit(f, '_'))[5], 7, 15)
    
cat(i, 'Working on...', f, fill =T)
	
	out<-mean(s_nWeeks.S) #collapse 10 year data into average
	out.clip<-out*Bering_200m
	
	out.r<-out.clip
	#out.r<-projectRaster(from=out.clip, res=10000, crs=prj.crs, method="bilinear", over=TRUE) 
	
	name<- paste(taxa,'_',modname,'_',study.period, '.tif',sep='')
	file.name <- file.path(file.path('C:/Users/adroghini/Documents/Marine-invasives/GIS/Species_Maps/Rasters/Weekly_Survival', name))
	writeRaster(out.r, file.name, format = "GTiff",overwrite=TRUE)
	
	rm(taxa,modname,study.period,f,out,out.clip,out.r,name,file.name)
	
	}
	

#rm(ff,prj.crs,i)
rm(ff,i)

#####################################
#The following is for reproduction
#####################################

ff <- list.files(path=file.path(baseDir, 'rOut', 'Reproduction'), pattern = 'rData')
#prj.crs<-CRS("+init=epsg:3571") ##set projection for rasters: NAD 83 Alaska Albers

for (i in 1:length(ff)){
f<-ff[i]
load(file.path(baseDir, 'rOut', 'Reproduction',f)) #loads s_OK.Repro raster stack
    taxa <- unlist(strsplit(f, '_'))[1]
	modname <- substr(unlist(strsplit(f, '_'))[3], 1, 5)
    study.period <- substr(unlist(strsplit(f, '_'))[3], 7, 15)
    
cat(i, 'Working on...', f, fill =T)
	
	out<-mean(s_OK.Repro) #collapse 10 year data into average
	out.clip<-out*Bering_200m
	
	out.r<-out.clip
	#out.r<-projectRaster(from=out.clip, res=10000, crs=prj.crs, method="bilinear", over=TRUE) 
	
	name<- paste(taxa,'_',modname,'_',study.period, '.tif',sep='')
	file.name <- file.path(file.path('C:/Users/adroghini/Documents/Marine-invasives/GIS/Species_Maps/Rasters/Repro', name))
	writeRaster(out.r, file.name, format = "GTiff",overwrite=TRUE)
	
	rm(taxa,modname,study.period,f,out,out.clip,out.r,name,file.name)
	
	}
	

#rm(ff,prj.crs,i)
rm(ff,i)