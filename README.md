# Non-native marine species in Alaska's Bering Sea
A collaboration between the Alaska Center for Conservation Science (University of Alaska Anchorage), the U.S. Geological Survey Alaska Science Center, and NOAA's Alaska Fisheries Science Center. Major funding was provided by the North Pacific Research Board (project #1532).

# Project summary
This repository contains R scripts that predict habitat suitability for non-native species in the Bering Sea.

Our habitat models use species-specific thresholds (obtained from the literature) and modeled ocean conditions from Regional Ocean Modeling Systems (ROMS).

We quantify habitat suitability for a) adult survival and b) reproduction and development life processes. Survival habitat is assessed at two temporal scales: year-round and weekly. Reproductive habitat is described in terms of "number of consecutive weeks". We consider two study periods: current (2003-2012) and future (2030-2039).

Habitat models and final project report can be accessed on our website: http://www.beringinvaders.org

# Technologies
- R (https://www.r-project.org/)
- R Studio (https://www.rstudio.com/)
- R Markdown (https://rmarkdown.rstudio.com/)
- ESRI ArcGIS Desktop 10.5 (https://www.esri.com/en-us/arcgis/about-arcgis/overview)

## R Packages
Install the following R contributed packages: doSNOW, dplyr, raster, rgdal, rgeos, sp, viridis, RColorBrewer, ggplot2, tidyr

# File organization
The following directory structure must be in place.
* ..\netCDF [contains netCDF extracts from the ROMS models, provided by Will Koeppen of AXIOM consulting.
* ..\GIS [for GIS geoTiff extracts of output and basemap spatial polygons files]
* ..\rCode [contains scripts]
* ..\rData [contains ROMS data cast as spatialGridDataFrames]
* ..\plots [for summary plots]
* ..\plots\taxa [for taxa specific plots]
* ..\plots\taxa\Reproduction [for taxa specific reproductive plots]
* ..\plots\taxa\Survival [for taxa specific Survival plots]
* ..\plots\taxa\SurvivalWeeks [for taxa specific SurvivalWeeks plots]
* ..\rOut\Reproduction [for taxa specific Reproduction .Rdata]
* ..\rOut\Survival [for taxa specific Survival .Rdata]
* ..\rOut\SurvivalWeeks [for taxa specific SurvivalWeeks .Rdata]
* ..\rOut\SurvivalWeeks_which [for taxa specific SurvivalWeeks_which .Rdata]
* ..\rOut\OverallTaxa_Year [for year-round suitability results]
* ..\rOut\Weekly_Survival [for weekly suitability results]
* ..\rOut\Repro_Plots [for reproduction results]
 
## File dependencies
* Species_Tolerances.csv [This file contains the summary data on temperature and salinity requirements for the marine invasive taxa considered]
* ports.csv [This file contains WGS84 coordinates and IATA abreviations for ports within the study area]
* ETOPO1.rData [This file contains bathymetry data that is called upon by the 'ExploreData.r' script.  It is built by the marmap package
* marmap_coord_-135;43;160;77_res_1_anti.csv data used in generating the ETOPO1 dataset by marmap.
* Continents.rData a spatialPolygon representation of the land within the study area.  This is built from a local copy of the ESRI continents dataset.
* beringSeaContinentalShelf.shp [Shapefile of the Bering Sea that determines the extent of our study area. Available [online through the ACCS Catalog](https://accscatalog.uaa.alaska.edu/dataset/resource/ab3f41f1-7dc6-4905-b307-ec99b430ec12).
* Files from the Bering 10K ROMS. We obtained the files from [Axiom Data Science](https://www.axiomdatascience.com/contact/) 

# Copyright
This software is in the public domain because it contains materials that originally came from the United States Geological Survey, an agency of the United States Department of Interior. For more information, see the official USGS copyright policy at http://www.usgs.gov/visual-id/credit_usgs.html#copyright

# Approved software
This software has NOT YET been approved for release by the U.S. Geological Survey (USGS). Although the software WILL BE subjected to rigorous review, the USGS reserves the right to update the software as needed pursuant to further analysis and review. No warranty, expressed or implied, is made by the USGS or the U.S. Government as to the functionality of the software and related material nor shall the fact of release constitute any such warranty. Furthermore, the software is released on condition that neither the USGS nor the U.S. Government shall be held liable for any damages resulting from its authorized or unauthorized use.  For more information, refer to IM OSQI 2015-03 (https://www2.usgs.gov/usgs-manual/im/IM-OSQI-2015-03.html) and IM OSQI 2015-05.

# Acknowledgements
Funding for this project was made available by the North Pacific Research Board (project #1532) and the Aleutian and Bering Sea Islands Landscape Conservation Cooperative. Tracey Gotthardt and Aaron Poe were involved with spearheading the project. Casey Greenstein, Lindsey Flagstad, Bonnie Bernard, Jaime Weltfelt, and Curtis Whisman contributed to the development of the ranking system and the species status reports.  J.W. thanks Jen Karnak of Marine Exchange of Alaska, who assisted with vessel identification. A.F. thanks Rob Bochenek and Dr. William Koeppen of Axiom Data Science for assistance extracting the ROMS data. A.D. thanks Marcus Geist for his GIS wizarding and Dr. Al Hermann for insightful conversations about the ROMS. Additional thanks goes to Dr. Matt Carlson for his valuable feedback on the manuscript.
