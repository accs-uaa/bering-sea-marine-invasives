## Objective: Generate plot showing reproductive suitability as a function of
## species' minimum reproductive temperature tolerances.
## Reproductive suitability is expressed as number of consecutive weeks of
## suitable habitat.

## This script requires outputs from ReproWeeks_Summary.R manuscript

##Authors: A. Droghini (adroghini@alaska.edu)
##				 A. S. Fischbach (afischbach@usgs.gov)
##Last updated: 31 March 2018

#load required packages and files
require(ggplot2)

repro<-read.csv('C:/Users/adroghini/Documents/Marine-invasives/PMEL-Models/rOut/Dataframes_Reproduction/AllTaxa_Repro.csv') #note that in original Repro_Summary script Dataframes_Reproduction is referred to as Repro_Plots
repro<-repro[,1:14] #include only relevant columns

sp.tol<-read.csv('C:/Users/adroghini/Documents/Marine-invasives/PMEL-Models/rCode/roms_habitat/Species_Tolerances.csv') ##read species tolerance file
sp.tol<-subset(sp.tol,!is.na(Min_ReproTempC)) #include only taxa with known reproductive thresholds

#create blank dataframe in which to place results
nn <- c('Taxa', 'StudyPeriod', 'Avg.No.Of.Weeks','Min_ReproTempC','Max_ReproSal')
repro.current <- data.frame(matrix(NA, ncol = length(nn), nrow = length(unique(repro$Taxa))))
names(repro.current) <- nn
repro.future <- data.frame(matrix(NA, ncol = length(nn), nrow = length(unique(repro$Taxa))))
names(repro.future) <- nn
rm(nn)

## For each taxon, calculate number of consecutive weeks of suitable habitat for
## each study period: a) current (2003-2012) and b) mid-century (2030-2039).
## Number of weeks is averaged across the three climate models used for the
## predictions.

taxa<-unique(repro$Taxa)
spp<-unique(repro$StudyPeriod)

for (i in 1:length(taxa)){ ##begin taxa loop
	taxon=as.character(taxa[i])
	for (j in 1:length(spp)){
	sp=as.character(spp[j])
	repro.T<-subset(repro,Taxa==taxon & StudyPeriod==sp)
	repro.TS<-as.data.frame(colMeans(repro.T[sapply(repro.T, is.numeric)]))

	if (j==1){ ##for current study period
	repro.current$Taxa[i]<-taxon
	repro.current$StudyPeriod[i]<-sp
	repro.current$Avg.No.Of.Weeks[i]<-as.numeric(colMeans(repro.TS))
	repro.current$Min_ReproTempC[i]<-sp.tol$Min_ReproTempC[i]
	repro.current$Max_ReproSal[i]<-sp.tol$Max_ReproSal[i]
	} else { ##for future study period
	repro.future$Taxa[i]<-taxon
	repro.future$StudyPeriod[i]<-sp
	repro.future$Avg.No.Of.Weeks[i]<-as.numeric(colMeans(repro.TS))
	repro.future$Min_ReproTempC[i]<-sp.tol$Min_ReproTempC[i]
	repro.future$Max_ReproSal[i]<-sp.tol$Max_ReproSal[i]
	}
	} #close study period loop
	} #close taxa loop

repro.summary<-rbind(repro.current,repro.future)

## Workspace clean-up
rm(repro.current,repro.future,repro,taxa,spp,i,j,taxon,sp,repro.T,repro.TS)


## Create category based on taxa's maximum salinity threshold (Max_ReproSal)
## If salinity tolerance == 0 ppt, "Freshwater"
## If salinity tolerance < 31 ppt, "Brackish"
## If salinity tolerance >= 31 ppt, "Marine"
## These categories are based on the Venice system of salinity classification,
## but we set the threshold for "marine" at 31 ppt. The Bering Sea is a saline
## waterbody, and taxa that cannot tolerate salinities of at least 31 ppt
## have very little suitable habitat.
repro.summary$Salinity<-as.character(NA)
repro.summary$Salinity[repro.summary$Max_ReproSal == 0] <- "Freshwater"
repro.summary$Salinity[repro.summary$Max_ReproSal != 0 & repro.summary$Max_ReproSal < 31] <- "Brackish"
repro.summary$Salinity[repro.summary$Max_ReproSal >= 31] <- "Marine"

###########################
####### plot graph ########

# - Avg.No.Of.Weeks on y axis
# - Min_ReproTempC on x axis

pd <- position_dodge(width = 0.2) ##for jitter
ggplot(repro.summary, aes(x=Min_ReproTempC, y=Avg.No.Of.Weeks, group=Taxa))+
	geom_point(aes(color=StudyPeriod,shape=Salinity),size=3.25, position=pd)+
	#geom_point(aes(color=StudyPeriod,shape=Salinity),size=3.5)+
	scale_color_manual(values=c("dodgerblue1","midnightblue"),name="Study period")+
	scale_shape_manual(values=c(7,4,20))+
	scale_x_continuous(breaks=seq(0, 18, 2),name="Minimum reproductive temperature (°C)")+
	scale_y_continuous(breaks=seq(0, 52, 5),name="Number of consecutive suitable weeks")+
	#coord_cartesian(x = c(0, 19))+
	theme_bw(base_size = 10)+
	theme(legend.position="bottom",axis.line = element_line(colour = "black"),
    panel.border = element_blank(),
    panel.background = element_blank(),panel.grid.minor=element_blank())

#export graph to folder
filePath <- file.path(file.path(baseDir, 'rCode','cleaned_up_code','manuscript'))
#tiff(filename = fileName,width=1200,height=800,units="px",pointsize=12,res=120)
ggsave("figures-reproduction.png", device = "png", path = filePath,scale = 1, width = 2500/300, height = 1770/300, units = 'in', dpi = 300, limitsize = FALSE)
