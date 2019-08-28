## June 30th 2017
## Last updated: 14 November 2017
# Authors:  A. Droghini (adroghini@alaska.edu)
#			A.S. Fischbach (afischbach@usgs.gov)
# 	
## Intent: For each ROMS model, calculate how many species are predicted to have year-round suitable habitat in Bering Sea shelf under a) current(2003-2012) and b) future (2030-2039) conditions. Calculate change in % suitable area under a) current and b) future conditions. 
# This code requires outputs from Overall_Year_Survival_byModel

## load packages
require(raster)
require(sp)
require(rgdal)
require(rgeos)
require(plyr)
require(ggplot2)
#baseDir <-  'D:/HelpingOthers/ABSIlcc/NFWF2016/Models'
setwd(file.path(baseDir))

ff <- list.files(file.path(baseDir, 'rOut', 'OverallTaxa_Year'), pattern = 'rData')
nn <- c('Model', 'Max_Sp_2003', 'Max_Sp_2030','Change_Sp_Min','Change_Sp_Max','Habt_NoChange','Habt_Gain','Habt_Loss')
output <- data.frame(matrix(NA, ncol = length(nn), nrow = (length(ff)/2)))
names(output) <- nn
rm(nn)

for(i in seq(from = 1, to = length(ff), by = 2)){ 
	f = ff[i]
	
	load(file.path(baseDir, 'rOut', 'OverallTaxa_Year', f))
	
##Populate dataframe
	Model <- unlist(strsplit(unlist(strsplit(f, ".rData")), '_'))[2] ##create character string for model name
	if(Model=="CCCma"){
		ModelName<-"CGCM3-t47"
		j=1
		}else if(Model=="MIROC"){
		ModelName<-"MIROC3.2"
		j=3
		}else{
		ModelName<-"ECHO-G"
		j=2
	}
	ModelPeriod <- unlist(strsplit(unlist(strsplit(f, ".rData")), '_'))[3]
	if(ModelPeriod=="2003-2013"){
		ModelPeriod<-"2003-2012"
		}else{
		ModelPeriod<-"2030-2039"
	}
	output$Model[j]<-ModelName

cat(j, '=== Handling ===', ModelName, fill =T) 

hindcast<-nSurvival
hind.clip<-hindcast*Bering_200m #clip to Bering Sea continental shelf - see BeringSea_Raster R Code

f = ff[i+1] ## grab the next file name - same model, future conditions
load(file.path(baseDir, 'rOut', 'OverallTaxa_Year', f))
forecast<-nSurvival
fore.clip<-forecast*Bering_200m

#Save current & future projections
name<- paste('Current_',ModelName, '.tif',sep='')
fileName <- file.path(file.path(baseDir, 'rOut','OverallTaxa_Year', 'GIS', name))
writeRaster(hind.clip, fileName, format = "GTiff",overwrite=TRUE)
name<- paste('Future_',ModelName, '.tif',sep='')
fileName <- file.path(file.path(baseDir, 'rOut','OverallTaxa_Year', 'GIS', name))
writeRaster(fore.clip, fileName, format = "GTiff",overwrite=TRUE)


##Extract total number of species with year-round survival for each study period-model combination
output$Max_Sp_2003[j]<-cellStats(hind.clip,max)
output$Max_Sp_2030[j]<-cellStats(fore.clip,max)

##Calculate & plot change in number of species
change<-fore.clip-hind.clip
output$Change_Sp_Max[j]<-cellStats(change,max) 
output$Change_Sp_Min[j]<-cellStats(change,min) 

name<- paste('Change_',ModelName, '.tif',sep='')
fileName <- file.path(file.path(baseDir, 'rOut','OverallTaxa_Year', 'GIS', name))
writeRaster(change, fileName, format = "GTiff",overwrite=TRUE)
#In ArcGIS, reproject to WGS_1984_North_Pole_LAEA_Bering_Sea (EPSG 3571)

## Examine the distribution of change in number of taxa that may survive within each pixel
## Express relative to total area = proportion change
b_area<- (cellStats(Bering_200m,sum)) #Total no. of pixels in study area
habt_tab<-table(values(change))
habt_tab.P<-habt_tab/b_area 
habt.df<-as.data.frame(habt_tab.P)

#col.l<-c('#053061','#2166ac','#4393c3','#d1e5f0','#fdae61','#d6604d','#b2182b','#67001f')
#cols<-c('#053061','#4393c3','#d1e5f0','#fdae61','#d6604d','#b2182b','#67001f')
#col.l <-colorRampPalette(cols)(100)
#col.l <-c('#4393c3','#d1e5f0','#fdae61','#d6604d','#b2182b','#67001f')
col.l <-c('#053061','#4393c3','#fdae61','#d6604d','#b2182b','#67001f')

##Bin into categories
habt.df$Var1<-as.character(habt.df$Var1)
habt.df$Var1<-as.numeric(habt.df$Var1)
habt.df$Category[habt.df$Var1 < 0] <- "-20 to -1" 
#habt.df$Category[habt.df$Var1 >= -5 & habt.df$Var1 < 0] <- "[-5, 0[" 
#habt.df$Category[habt.df$Var1 == -1] <- "-1" 
habt.df$Category[habt.df$Var1 == 0] <- "0" 
#habt.df$Category[habt.df$Var1 == 1] <- "+1" 
habt.df$Category[habt.df$Var1 > 0 & habt.df$Var1 <= 5] <- "+1 to +5" 
habt.df$Category[habt.df$Var1 > 5 & habt.df$Var1 <= 10] <- "+6 to +10" 
habt.df$Category[habt.df$Var1 > 10] <- "+11 to +20" 
#habt.df$Category[habt.df$Var1 > 15] <- "]15,20]" 

habt.plot<-ddply(habt.df,~Category,summarize,add=sum(Freq))

##Order categories
habt.plot$Category = factor(habt.plot$Category,levels=c("-20 to -1","0","+1 to +5","+6 to +10","+11 to +20"),ordered=TRUE)
b_max<-max(habt.plot$add)

##Plot for final report mapping
##Increase font size & get rid of grid lines
ggplot(data=habt.plot, aes(x=Category, y=add,fill=Category)) +
geom_bar(stat="identity",color="black")+
scale_y_continuous(breaks=seq(0, (b_max+0.05), 0.05))+
scale_fill_manual(values=col.l)+
coord_cartesian(ylim=c(0, (b_max))) +
ylab("Proportion of total habitat")  +
xlab("Change in number of taxa")  +
guides(fill=FALSE)+
#theme_minimal()+ theme(panel.grid.minor=element_blank())
theme_minimal(base_size = 22)+ theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank())
  
##Save
#name<- paste('Barplot_',ModelName, ,sep='')
name<-paste('Histogram_',ModelName,'.png',sep='')
  filePath <- file.path(file.path(baseDir, 'rOut','OverallTaxa_Year'))
  #tiff(filename = fileName,width=1200,height=800,units="px",pointsize=12,res=120)
 ggsave(name, device = "png", path = filePath,
  scale = 1, width = 2500/300, height = 1940/300, units = 'in', dpi = 300, limitsize = TRUE)
  graphics.off()
		  	
##Extract values to add to dataframe
habt.df$Cat_Output<-"NA"
habt.df$Cat_Output[habt.df$Var1 < 0] <- "Habt_Loss" 
habt.df$Cat_Output[habt.df$Var1 == 0] <- "Habt_NoChange" 
habt.df$Cat_Output[habt.df$Var1 > 0] <- "Habt_Gain" 
habt.out<-ddply(habt.df,~Cat_Output,summarize,add=sum(Freq))
  
output$Habt_Loss[j]<-habt.out[2,2] 
output$Habt_NoChange[j]<-habt.out[3,2]
output$Habt_Gain[j]<-habt.out[1,2]
  }
  
#Export output dataframe
name<- paste('Results_Change_Survival','.csv',sep='')
fileName <- file.path(file.path(baseDir, 'rOut','OverallTaxa_Year', name))
write.csv(output,fileName,row.names=FALSE)

#Clean up workspace
rm(i,j, nSurvival,f,ff,Model,ModelName,ModelPeriod,forecast,fore.clip,hindcast,hind.clip,change,col.l,habt_tab,b_area,habt.df,fileName,filePath,habt.plot,b_max,name,habt_tab.P,output)


###################################################
##################JUNK - DO NOT RUN################
###################################################

#Tried to plot change raster in R but turned out being a graphical quagmire with study area spanning the 
#International Date Line. Decided to export raster & plot in ArcGIS instead.

##Set extent to best view study area
e <-  extent(change)
e[1] <- e[1] + 400*1000
e[2] <- e[2] - 650*1000
e[3] <- e[3] + 200*1000
e[4] <- e[4] - 250*1000
plotResX = 2500 
plotResY = 1.04*(plotResX * ((e[4] - e[3])/ (e[2] - e[1])))
##cheat legend so that it's evenly spaced. Code from: https://stackoverflow.com/questions/22542919/how-to-adjust-legend-spacing-in-r-raster-plots 
sbreak1<-c(-6,-1,0,5,10,15,20)
sbreak2<-c(1,2,3,4,5,6,7) 
legendbreak<-c('-5','-1','0','5','10','15','20') ##too hard to read?
tiff(filename = fileName,width=plotResX,height=plotResY,units="px",pointsize=12,res=300)
print(levelplot(change,margin=FALSE,col.regions=col.l,at=sbreak1,par.settings=list(layout.heights=list(xlab.key.padding=1),col=col.l), colorkey=list(at=sbreak2,labels=legendbreak,col=col.l,axis.line=list(col='black') ),main=paste0(ModelName),xlim=c(e[1],e[2]),ylim=c(e[3],e[4])))
##want to add XY coordinates to yaxt and xaxt
##erase bounding box - add to par.settings: axis.line = list(col='transparent')
#change <- projectRaster(change, crs=prj.geo) #this gives me good xy coordinates but i end up on the other side of the world...
graphics.off()

