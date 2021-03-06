# This code uses the marmap package to query NOAA's ETOPO1 databases and create a bathymetry raster for the Bering Sea
# This raster is then exported and loaded into ESRI ArcMap 10.5, where it is used to clip the Bering Sea to only include areas with water depth <= 200 m
# Author: A.S. Fischbach (afischbach@usgs.gov)

## Source initial run script
source('rCode/init.R')


## Generate bathymetry raster

## Note: Due to a problem with antimeridian in current r version, I have run this portion of the script on an old R version (v. 3.2.5). This requires attention. 
## The solution to the previous post addressing projection of rasters across the antimeridian (http://r-sig-geo.2731867.n2.nabble.com/coercing-marmap-bathy-object-to-raster-spanning-the-antimeridian-td7589828.html#a7589841) no longer works (as of 24 June 2016).

## The ETOPO1.rData file can be accessed on our repository @ KNB Ecoinformatics.

BathymetryDataFile <- file.path(baseDir, 'rData/ETOPO1.rData')

if(!file.exists(BathymetryDataFile)){
  
  ## Define the study area polygon using geographic coordinates 
  xLimits <- sort(c(170, -135)) ## define the study area polygon using geographic coordinates (eastings WGS84)
  yLimits <- sort(c(52, 75)) ## (northings WGS84)
  
  ## Define projections
  prj.StudyArea <- CRS("+proj=aeqd +lat_0=70 +lon_0=-170 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") 
  prj.StudyAreaPositive <- CRS("+proj=aeqd +lat_0=70 +lon_0=190") # +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") #Lambert -170, 70
  prj.geo<-CRS('+proj=longlat +datum=WGS84')
  
  
  bath <- getNOAA.bathy(xLimits[1]+15, xLimits[2]-10, yLimits[1]-5, yLimits[2]+10, resolution = 1, antimeridian = TRUE, keep =T)
  bath.r.geo<-marmap::as.raster(bath)
  bath.r.1<-projectRaster(from=bath.r.geo, res=2000, crs=prj.StudyAreaPositive, method="bilinear", over=TRUE)
  bath.r<-projectRaster(from=bath.r.1, res=2000, crs=prj.StudyArea, method="bilinear", over=TRUE)
  
  bath200 <- rasterToContour(bath.r, levels=-200) # Extract only areas <= 200 m depth
  save(bath.r, bath200, file=BathymetryDataFile) # Export as .Rdata for use in R
  
  unloadNamespace('marmap')
  rm(bath, bath.r.geo, bath.r.1,xLimits,yLimits)
  
}else{
  load(BathymetryDataFile)

}

# Export as shapefile to generate Figure 1 in ESRI ArcMap
# Convert SpatialLines to Polygon in ArcMap using "Feature to Polygon" tool with bath2000 and beringSea shapefile as Inputs. The beringSea shapefile is also available on our KNB repository.
rgdal::writeOGR(bath200, 'rData', "bath200", driver="ESRI Shapefile") 

