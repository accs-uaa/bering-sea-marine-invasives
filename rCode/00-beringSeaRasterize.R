# Load initial script
source('rCode/init.R')

# Load shapefile of the Bering Sea continental shelf
# Available as a .zip file on the ACCS catalog
# Extract to /rData folder
beringShelf <- readOGR('rData/beringSeaContinentalShelf.shp')

# Load any of the files from 02-evaluateHabitatSuitability.R to serve as a mask
ff <- list.files(file.path(baseDir, 'rOut', 'Survival'), pattern = 'rData')
f=ff[1]
load(file.path(baseDir, 'rOut', 'Survival', f)) 

# Convert beringShelf polygon to raster
# So that habitat suitability rasters can be clipped to only include our area of interest
rMask <- sum(s_OK.S)
beringShelf <- spTransform(beringShelf, rMask@crs)  # Transform to same projection
beringShelf <- rasterize(beringShelf,rMask) # Make into raster
# plot(beringShelf)

# Clean workspace
rm(s_OK.S,f,ff,rMask)