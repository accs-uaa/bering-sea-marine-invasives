# Non-native marine species in Alaska's Bering Sea
A collaboration between the Alaska Center for Conservation Science (University of Alaska Anchorage), the U.S. Geological Survey Alaska Science Center, and NOAA's Alaska Fisheries Science Center. Major funding was provided by the North Pacific Research Board (project #1523).

**Citation:** Droghini, A., A. S. Fischbach, J. T. Watson, and J. P. Reimer. *In press*. Regional ocean models indicate changing limits to biological invasions in the Bering Sea. 

# Project summary
This repository contains R scripts that predict temperature and salinity suitability for non-native species in the Bering Sea.

We evaluate suitable temperature and salinity conditions for a) year-round adult survival, b) weekly adult survival, and c) weekly reproduction. We consider two study periods: current (2003-2012) and future (2030-2039). Our models use species-specific thresholds obtained from the literature and ocean conditions predicted by three Regional Ocean Modeling Systems (ROMS).

# Technologies
- R (https://www.r-project.org/)
- R Studio (https://www.rstudio.com/)
- ESRI ArcGIS Desktop 10.5 (https://www.esri.com/en-us/arcgis/about-arcgis/overview)

## R Packages
Install the following R contributed packages: doSNOW, dplyr, ggplot2, marmap, ncdf4, plyr, raster, rgdal, rgeos, sp, RColorBrewer, tidyverse

# File dependencies
* Species_Tolerances.csv [This file contains the summary data on temperature and salinity requirements for the marine invasive taxa considered]
* ports.csv [This file contains WGS84 coordinates and IATA abreviations for ports within the study area]
* beringSeaContinentalShelf.shp [Shapefile of the Bering Sea that determines the extent of our study area. Available [online through the ACCS Catalog](https://accscatalog.uaa.alaska.edu/dataset/resource/ab3f41f1-7dc6-4905-b307-ec99b430ec12)].
* Files from the Bering 10K ROMS. We obtained the files from [Axiom Data Science](https://www.axiomdatascience.com/contact/)

## File organization
The following directory structure must be in place.
* ..\netCDF [contains netCDF extracts from the ROMS models, provided by Will Koeppen of AXIOM consulting.
* ..\gis_products [for geoTiff extracts of outputs, used for generating figures in ESRI ArcMap]
* ..\gis_products\allTaxa\yearlySurvival [for storing final raster results from year-round survival analyses]
* ..\rCode [contains scripts]
* ..\rData [contains ROMS data cast as spatialGridDataFrames]

* ..\rOut\Reproduction [for taxa specific Reproduction .Rdata]
* ..\rOut\Survival [for taxa specific Survival .Rdata]
* ..\rOut\SurvivalWeeks [for taxa specific SurvivalWeeks .Rdata]
* ..\rOut\SurvivalWeeks_which [for taxa specific SurvivalWeeks_which .Rdata]
* ..\rOut\OverallTaxa_Year [for summary year-round suitability results]
* ..\rOut\OverallTaxa_Repro [for summary reproductive suitability results]
* ..\rOut\OverallTaxa_Weekly [for summary weekly suitability results]

# Acknowledgements
Funding for this project was made available by the North Pacific Research Board (project #1523) and the Aleutian and Bering Sea Islands Landscape Conservation Cooperative. Tracey Gotthardt and Aaron Poe were involved with spearheading the project. Casey Greenstein, Lindsey Flagstad, Bonnie Bernard, Jaime Weltfelt, and Curtis Whisman contributed to the development of the ranking system and the species status reports. J.W. thanks Jen Karnak of Marine Exchange of Alaska, who assisted with vessel identification. A.F. thanks Rob Bochenek and Dr. William Koeppen of Axiom Data Science for assistance extracting the ROMS data. A.D. thanks Marcus Geist for his GIS wizarding and Dr. Al Hermann for insightful conversations about the ROMS.

# Contact
Amanda Droghini, adroghini@alaska.edu, 

Alaska Center for Conservation Science, University of Alaska Anchorage

# License
This repository and its contents are licensed under Creative Commons Attribution-Share Alike.