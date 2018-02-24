#This code extracts bathymetry areas <= 200 m depth, and converts the study area shapefile into a raster for subsequent clipping in analyses
require(marmap)
require(raster)
require(sp)
require(rgdal)
### Read the bathymetry layer from the basemaps directory, a clipped copy of ETOPO1.
xLimits<-sort(c(150, -135))					## define the study area polygon using geographic coordinates (eastings WGS84)
yLimits<-sort(c(40, 70))					## (northings WGS84)
## Define projections
prj.StudyArea <- CRS("+proj=aeqd +lat_0=70 +lon_0=-170 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") 
prj.StudyAreaPositive <- CRS("+proj=aeqd +lat_0=70 +lon_0=190") # +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") #Lambert -170, 70
prj.geo<-CRS('+proj=longlat +datum=WGS84')
##

## Load bathymetry file
BathymetryDataFile<-file.path(baseDir, 'rCode/roms_habitat/ETOPO1.rData') 
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

##Extract only areas < 200 m depth
bath200<-rasterToContour(bath.r, levels=-200)
rgdal::writeOGR(bath200, 'C:/Users/adroghini/Documents/Marine-invasives/PMEL-Models/GIS', "bath200", driver="ESRI Shapefile")

##In ArcGIS, we used the XTools Pro package (https://xtools.pro/) to convert line feature to polygon, and clip Bering Sea study area to only include areas <= 200 m depth. Thank you Marcus Geist for the GIS help! 

##Load shapefile of Bering Sea shelf area
dsn<-"C:/Users/adroghini/Documents/Marine-invasives/PMEL-Models/rCode/roms_habitat/BeringSea_200m.shp"
Bering<- readOGR(dsn) 

##Convert Bering Sea polygon to raster
##Load any file to serve as mask for raster
##Using results for TaxaSuitability Survival analysis
ff <- list.files(file.path(baseDir, 'rOut', 'Survival'), pattern = 'rData')
f=ff[1]
load(file.path(baseDir, 'rOut', 'Survival', f)) 
r_mask<-sum(s_OK.S)
Bering<- spTransform(Bering, r_mask@crs)  ##Transform to same projection
Bering_200m<-rasterize(Bering,r_mask)
plot(Bering_200m)

rm(Bering,dsn,prj.StudyArea,s_OK.S,BathymetryDataFile,f,ff,r_mask)

rm(Bering,dsn,prj.StudyArea,s_OK.S,BathymetryDataFile,f,ff)