getwd()
library(raster)
library(rgdal)

# Objective: Obtain summary statistics for Results section of MS

# Calculate median
# Load year-round survival models for every taxa (n=42) and for each ROMS
# 
file_to_load<-list.files(file.path(baseDir,'rOut','OverallTaxa_Year'),full.names = TRUE)

# Generate Bering Sea 200m raster with which to clip results
dsn<-"C:/Users/adroghini/Documents/bering-invaders/rData/BeringSea_200m.shp"
Bering_200m<- readOGR(dsn) 
f <- file_to_load[i]
load(f)
r_mask<-sum(s_OK.S)
Bering<- spTransform(Bering, r_mask@crs)  ##Transform to same projection
Bering_200m<-rasterize(Bering,r_mask)
plot(Bering_200m)


# Calculate median & maximum taxa per pixel
# For each model and for each study period
for (i in 1:length(file_to_load)){
  f <- file_to_load[i]
  model<-substr(unlist(strsplit(f, '_'))[3],1,5)
  period<-substr(unlist(strsplit(f, '_'))[4], 1, 4)
  load(f)
 # cat("working on model", model, "\n for study period starting in", period)
#  fileName<-paste(model,"_year_surv_",period,".tif",sep="")
  # fileWrite <- file.path(file.path('C:/Users/adroghini/Documents/bering-invaders/gis_products/Yearly_Survival/', fileName))
  # writeRaster(nSurvival, fileWrite, format = "GTiff",overwrite=TRUE)
  nSurvival <- nSurvival*Bering_200m
  cat("\n median number of taxa for model", model, "\n for study period starting in", period, "\n is...")
  print(cellStats(nSurvival,median))
  cat("\n max number of taxa for model", model, "\n for study period starting in", period, "\n is...")
  print(cellStats(nSurvival,max))
  }


# Calculate change in suitable area
file_to_load<-list.files(path = "C:/Users/adroghini/Documents/bering-invaders/gis_products/OverallTaxa/YearlySurvival/Change_In",patter="Change_",full.names = TRUE)


#CGCM
rast_name<-"C:/Users/adroghini/Documents/bering-invaders/gis_products/OverallTaxa/YearlySurvival/Change_In/Change_CGCM3-t47.tif"
#ECHOG
rast_name<-"C:/Users/adroghini/Documents/bering-invaders/gis_products/OverallTaxa/YearlySurvival/Change_In/Change_ECHO-G.tif"
#MIROC
rast_name<-"C:/Users/adroghini/Documents/bering-invaders/gis_products/OverallTaxa/YearlySurvival/Change_In/Change_MIROC3.2.tif"
change<-raster(rast_name)
plot(change)
habt_tab<-table(values(change))
habt_tab<-as.data.frame(habt_tab)
habt_tab
habt_tab$Var1<-as.character(habt_tab$Var1)
habt_tab$Freq<-as.character(habt_tab$Freq)
habt_tab$Var1<-as.integer(habt_tab$Var1)
habt_tab$Freq<-as.integer(habt_tab$Freq)
#negative<-subset(habt_tab,Var1>0)
negative<-subset(habt_tab,Var1==0)
base::colSums(negative)