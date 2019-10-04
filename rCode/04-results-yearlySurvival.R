## Intent: For each ROMS: 1. Calculate how many NIS are predicted to have year-round suitable habitat in the Bering Sea shelf under a) Current (2003-2012) and b) Mid-century (2030-2039) conditions. 2. Calculate change in % suitable area.

## Last updated: 3 October 2019
## Created: 30 June 2017
## Authors:  A. Droghini (adroghini@alaska.edu)
##			 	   A.S. Fischbach (afischbach@usgs.gov)

# This code requires outputs from Overall_Year_Survival_byModel

## Source initial run script
source('rCode/init.R')

#### Load files & make dataframe ----

source('rCode/00-beringSeaRasterize.R') # study area raster
ff <- list.files(file.path(baseDir, 'rOut', 'OverallTaxa_Year'), pattern = 'rData')

# For each model, iterate by 2's (i.e. by study period)

for(i in seq(from = 1, to = length(ff), by = 2)){
	f = ff[i]
	model<-substr(unlist(strsplit(f, '_'))[3],1,5)
  period<-substr(unlist(strsplit(f, '_'))[4], 1, 4)
	load(file.path(baseDir, 'rOut', 'OverallTaxa_Year', f))

	hindcast<-nSurvival * beringShelf #clip to study area extent

	cat("\n median number of taxa for model", model, "\n for study period starting in", period, "\n is...")
	print(cellStats(hindcast,median))
	cat("\n max number of taxa for model", model, "\n for study period starting in", period, "\n is...")
	print(cellStats(hindcast,max))

	f = ff[i+1] # grab the next file name - same model, Mid-century Study Period (2030-2039)
	load(file.path(baseDir, 'rOut', 'OverallTaxa_Year', f))
	forecast<-nSurvival*beringShelf

	# calculate change in number of NIS from Current to Mid-century
	changeIn <- hindcast - forecast

# % suitable area
habt_tab<-table(values(changeIn))
habt_tab<-as.data.frame(habt_tab)
habt_tab
habt_tab$Var1<-as.character(habt_tab$Var1)
habt_tab$Freq<-as.character(habt_tab$Freq)
habt_tab$Var1<-as.integer(habt_tab$Var1)
habt_tab$Freq<-as.integer(habt_tab$Freq)
#negative<-subset(habt_tab,Var1>0)
negative<-subset(habt_tab,Var1==0)
base::colSums(negative)

## Examine the distribution of change in number of taxa that may survive within each pixel
## Express relative to total area = proportion change
b_area<- (cellStats(beringShelf,sum)) #Total no. of pixels in study area
habt_tab<-table(values(changeIn))
habt_tab.P<-habt_tab/b_area
habt.df<-as.data.frame(habt_tab.P)
}

## Clean up workspace
rm(list=ls())
