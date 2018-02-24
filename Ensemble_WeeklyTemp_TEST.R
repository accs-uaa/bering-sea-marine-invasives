#Last updated: 26 January 2018
#Author: A. Droghini (adroghini@alaska.edu)
#		 A.S. Fischbach (afischbach@usgs.gov)

##Intent: For each study period, generate average, min. & max. temps averaged across 3 models and all years
require(dplyr)
require(plyr)
require(ggplot2)
require(rgdal)
baseDir <- 'C:/Users/adroghini/Documents/PMEL Models'

##load Bering Sea polygon for clipping
dsn<-"C:/Users/adroghini/Documents/Marine-invasives/PMEL-Models/rCode/roms_habitat/BeringSea_200m.shp"
Bering<- readOGR(dsn) 
#reproject
ww <- list.files(file.path(baseDir, 'rOut', 'Survival'), pattern = 'rData')
w=ww[1]
load(file.path(baseDir, 'rOut', 'Survival', w)) 
r_mask<-sum(s_OK.S)
Bering<- spTransform(Bering, r_mask@crs)  ##Transform to same projection
rm(dsn,ww,w,r_mask)

##load model prediction files
ff<-data.frame(fileName = list.files(path = file.path(baseDir, 'rData'), pattern = 'temperature.rData'), 	
	stringsAsFactors =F) ## retain only temperature files
ff$model <- NA
ff$studyPeriod <- NA

for(i in 1:nrow(ff)){
	f <- ff$fileName[i]
	fs <- unlist(strsplit(unlist(strsplit(f, ".rData")), '_'))
	ff[i, ]$model <- fs[2]
	ff[i, ]$studyPeriod <- fs[3]
}
ff<-ff[order(ff$studyPeriod),]
#
spp <- unique(ff$studyPeriod)

##Generate dataframe for storing
#nn=c("Temp","metric")
#WeekTemp<-data.frame(matrix(NA, ncol = length(nn), nrow = length(ff)))

for (sp in spp){ ##study period loop
f <- ff[ff$studyPeriod == sp, ]$fileName

	for (i in 1:length(f)){
	  load(file.path(baseDir, 'rData', f[i]))
	  cat(i, '=== Handling ===', f[i], fill =T) 
	  ##clip model predictions to Bering Sea shelf
	  fullgrid(sgdf.Max) = FALSE #I have no idea how to clip a sgdf.....
	  #clip <- sgdf.Max[Bering,drop=TRUE]
	  clip <- sgdf.Max[Bering,]
	  ddAllWeeks<-dplyr::tbl_df(clip@data[,])

	  #compute weekly averages, max, and min
	  WeekAvg<-as.data.frame(colMeans(ddAllWeeks, na.rm = TRUE)) ##I need to do the ColumnsToSelect here to exclude 2013 and 2029.....
	  colnames(WeekAvg)[1]<-"Temp"
	  WeekAvg$metric<-"Average"
	  WeekMax<-as.data.frame(apply(ddAllWeeks, 2, max, na.rm=TRUE))
	  colnames(WeekMax)[1]<-"Temp"
	  WeekMax$metric<-"Max"
	  WeekMin<-as.data.frame(apply(ddAllWeeks, 2, min, na.rm=TRUE))
	  colnames(WeekMin)[1]<-"Temp"
	  WeekMin$metric<-"Min"
	  WeekTemp<-rbind(WeekAvg,WeekMax,WeekMin)
	  rm(ddAllWeeks,WeekAvg,WeekMax,WeekMin)
	  
	  #generate time attributes
	  WeekTemp$Date<-row.names(WeekTemp)
	  WeekTemp$Model<-unlist(strsplit(unlist(strsplit(f[i], ".rData")), '_'))[2]
	  
	  for (j in 1:nrow(WeekTemp)){
	  WeekTemp$Year[j]<-unlist(strsplit(WeekTemp$Date[j], split='_'))[1]
	  WeekTemp$Week[j]<-unlist(strsplit(WeekTemp$Date[j], split='_'))[2]
	  WeekTemp$Day[j]<-unlist(strsplit(WeekTemp$Date[j], split='_'))[3]
	  }
	  
	  WeekTemp<-WeekTemp[order(WeekTemp$metric,WeekTemp$Year,WeekTemp$Week),]
	  
	  if (i==1){
	  Week_Mod<-WeekTemp 
	  } 
	  else {
	  Week_Mod<-rbind(Week_Mod,WeekTemp)
	  }
	  
	  } ##end of model loop
	  
	  week.m<-subset(Week_Mod,metric=="Average") ##restrict to average?

	  if (sp==spp[1]){ 
		
		week.max<-ddply(week.m,~Week,summarize,Avg_Temp=mean(Temp),stdev=sd(Temp))
		week.max$StudyPeriod="2003-2012"
	  }
	  else {
	  week.s<-ddply(week.m,~Week,summarize,Avg_Temp=mean(Temp),stdev=sd(Temp))
	  week.s$StudyPeriod="2030-2039"
	  week.max<-rbind(week.max,week.s)
	  }
 
 } ##end loop

##Workspace clean-up
rm(fileName,week.m,week.s,Week_Mod,WeekTemp,sp,f,ff,spp,sgdf.Max)

week.max$Week<-as.numeric(week.max$Week)
week.max$Week<-week.max$Week-1 ##Weeks go from 0 to 52 to watch WhichOfSurvival_Summary plots


##plotting
p_temp<-ggplot(week.max, aes(x=Week, y=Avg_Temp, group=StudyPeriod))+
	geom_ribbon(aes(ymax = Avg_Temp + stdev, ymin = Avg_Temp - stdev, fill = StudyPeriod), alpha = 0.22)+
	geom_line(aes(linetype=StudyPeriod,colour=StudyPeriod), lwd=0.8)+
	scale_x_continuous(breaks=seq(0, 52, 4),limits=c(-0.5,52.5),expand = c(0,0),name="Week of the Year") +
	#scale_y_continuous(breaks=seq(0, 18, 1),limits=c(1,10.5),expand = c(0,0),name="Average water temperature (°C)") +
	scale_y_continuous(breaks=seq(-5, 18, 1),limits=c(-1,9.75),name="Water Temperature (°C)") +
	scale_color_manual(values=c("black", "black"))+ #"grey40
	scale_fill_manual(values=c("dodgerblue4", "dodgerblue1"))+
	scale_linetype_manual(values=c("dotdash","solid"))+ #"dotdash"
	theme_bw(base_size = 8)+ ##for multi-panel plotting
	theme(legend.position="none",axis.line = element_line(colour = "black"),
    panel.border = element_blank(),
    panel.background = element_blank(),panel.grid.minor=element_blank()) 

## save output
fileName<-file.path(file.path(baseDir, 'rOut', 'Weekly_Survival',"WeeklyTemps.csv")) 
write.csv(week.max, file = fileName,row.names=FALSE)

