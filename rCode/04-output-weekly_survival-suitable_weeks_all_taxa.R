#Created: 13 September 2017
#Last updated: 14 November 2017
#Author: Amanda Droghini (adroghini@alaska.edu)

#Intent:
#Determine which weeks of the year have highest habitat suitability across all taxa that cannot currently 
#survive year-round in the Bering Sea

#This script requires AllTaxa_WeeklySurvival.csv from WhichOfSurvival_.R script
##Also requires "p_temp" plot from WhichOfSurvival_Ensemble_WeeklyTemp.R script (for final plotting)

require(plyr)
require(ggplot2)

#baseDir <-  'D:/HelpingOthers/ABSIlcc/NFWF2016/Models'
out<-read.csv("rOut/Weekly_Survival/AllTaxa_WeeklySurvival.csv")
out$YearSurvival=ifelse(out$noYearRoundSurvival=="TRUE",0,1) ##Change logical to numerical. 
Sp.NoSurv<-subset(out,YearSurvival==0) #which species can't survive in Bering Sea?
Sp.NoSurv$Taxa<-factor(Sp.NoSurv$Taxa)
length(unique(Sp.NoSurv$Taxa)) #7 species have no year-round survival for at least some years

df.nosurv<-subset(out,Taxa %in% Sp.NoSurv$Taxa) #subset data to only include no survival species

##Summarize results for MS
##How many years can taxa survive? For 5/7, survival = 0 for all 10 years of current study period
comp_sp<-ddply(df.nosurv,c("Taxa","Model","StudyPeriod"),summarize,years=sum(YearSurvival))
rm(Sp.NoSurv)

##Plot # of species ~ week of the year for each model? With separate line for study period?

####Calculate # of species that can survive each week, averaged across all years. This allows us to computer standard deviations and express our y-axis as No. of Species
for(i in 1:3){
	for(j in 1:2){
SubModelPeriod<-subset(df.nosurv,Model==unique(df.nosurv$Model)[i] & StudyPeriod == unique(df.nosurv$StudyPeriod)[j] ) 
	for(y in 1:10){
year_data<-subset(SubModelPeriod,Year==unique(SubModelPeriod$Year)[y])
surv<-as.data.frame(colSums(year_data[,6:58],na.rm = TRUE)) ##sum number of species that can survive in each week
#surv$Week<-row.names(surv)
tmp<-row.names(surv)
tmp <- matrix(unlist(strsplit(as.character(tmp), 'week')), ncol=2,byrow=TRUE) ##Generate numeric week column - easier for plotting
surv$Week<-tmp[,2]
surv$Week<-as.numeric(surv$Week) 
colnames(surv)[1]<-"N_Species"
surv$Model=unique(SubModelPeriod$Model)
surv$StudyPeriod = unique(SubModelPeriod$StudyPeriod)
surv$Year = unique(SubModelPeriod$Year)[y]

if(i ==1 & j==1 & y==1){ ##if first model
			Sp_Surv<- surv
		}else{ 
			Sp_Surv <- rbind(Sp_Surv,surv) ##add to previous results
		}
}

}
}

##Delete temporary files
rm(tmp,surv,i,j,SubModelPeriod,year_data,y)

#Summarize across years - calculate average no. of species and st. dev.
df.plot<-ddply(Sp_Surv,c("StudyPeriod","Model","Week"), summarize, nspecies=mean(N_Species),stdev=sd(N_Species))
df.plot<-df.plot[with(df.plot, order(Week)), ]
df.plot$Week<-df.plot$Week-1 ##Start at week 0

##Create plots - loop through models
for (i in 1:length(unique(df.plot$Model))){
	mod.name<-unique(df.plot$Model)[i]
	df_mod<-subset(df.plot,Model==mod.name)
	#rib <- aes(ymax = nspecies + stdev, ymin = nspecies - stdev)
	
	plot1<-ggplot(data=df_mod, aes(x=Week, y=nspecies, group=as.factor(StudyPeriod))) +
	#geom_smooth(aes(x=Week,y=nspecies, colour=StudyPeriod), se=F, method="loess",show.legend = FALSE,lwd=1.1) +
	geom_line(aes(x=Week,y=nspecies, linetype=StudyPeriod,colour=StudyPeriod), lwd=0.6)+
	geom_point(aes(x=Week,y=nspecies,colour=StudyPeriod),size=1)+
	#geom_ribbon(rib, alpha = 0.5)+ #hideous!
	scale_y_continuous(breaks=seq(0, 10, 1),limits=c(0,8),expand = c(0,0),name="Number of Species") +
	scale_x_continuous(breaks=seq(0, 52, 4),limits=c(-0.5,52.5),expand = c(0,0),name="Week of the Year") +
	scale_colour_manual(values=c("black", "dodgerblue1"))+ #"grey40
	scale_linetype_manual(values=c("dotdash","solid"))+ #"dotdash"
	#scale_size_manual(values=c(1.1,1.45,1.2)) +
    #ggtitle("") +     
    theme_bw(base_size = 8)+
	theme(legend.position="none",axis.line = element_line(colour = "black"),
    panel.border = element_blank(),
    panel.background = element_blank(),panel.grid.minor=element_blank()) 
	
	##Assign individual name for mulitplot
	p_name<-paste('Weekly_',mod.name, '.png',sep='')
	g_name<-paste('plot_',mod.name,sep='')
	assign(g_name,plot1)
	
	##Save individual plots
	#filePath <- file.path(file.path(baseDir, 'rOut','Weekly_Survival'))
	#ggsave(p_name, device = "png", path = filePath, scale = 1, width = 2500/300, height = 1770/300, units = 'in', dpi = 300, limitsize = TRUE)
	#graphics.off()
	
	rm(plot1,mod.name,df_mod,g_name,p_name)
	
	}


##multiplot function from: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
# source(file.path(baseDir,'/rCode/functionMultiplot.R'))
p_name<-paste('Panel_Week_Plot','.png',sep='') 
filePath <- file.path(file.path(baseDir, 'rOut','Weekly_Survival'))
p_n<-paste(filePath,p_name,sep='/')
png(file=p_n,width = 2500/300, height = 1770/300,units = 'in', res = 300)
multiplot(plot_CCCma, plot_MIROC,plot_ECHOG,p_temp, cols=2)
dev.off()


##Clean up workspace
rm(plot_CCCma,plot_ECHOG,plot_MIROC,p_n,p_name,i,j,filePath,y,yy,df.plot,SubModelPeriod)

##remove dataframes
rm(comp_sp,df.nosurv,out2,Sp_Surv)
rm(out)

