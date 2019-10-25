#Last updated: 8 October 2017
#Author: Amanda Droghini (adroghini@alaska.edu)

##Intent: How many species can reproduce under a) current and b) future conditions? (Assume minimum 1 week required)
##What is the frequency distribution for the # of consecutive weeks under a) current and b) future conditions?
##Maximum/average/etc. # of consecutive weeks?

#### Start up requirements----

## Source initial run script
source('rCode/init.R')

## Load results of reproductive habitat suitability
ff <- list.files(file.path(baseDir, 'rOut', 'Reproduction'), pattern = 'rData')

#### Summary results for each taxon----
# For each model & study period combination
# Identify how many consecutive weeks of reproduction are available for each taxon
# Number of weeks chosen as maximum pixel value in our study area, for that year-model combination

## Create dataframe in which to place results
nn <- c('Taxa', 'Model', 'StudyPeriod', 'noRepro', eval(paste0('year', 1:10)))
resultsByTaxon <- data.frame(matrix(NA, ncol = length(nn), nrow = length(ff)))
names(resultsByTaxon) <- nn
rm(nn)

## Iterate through each taxon-model-study period combination

for(r in 1:nrow(resultsByTaxon)){
  f <- ff[r]

  # Parse out the taxon, model, and study period for the file
  resultsByTaxon$Taxa[r] <- unlist(strsplit(f, '_'))[1]
  resultsByTaxon$Model[r] <- substr(unlist(strsplit(f, '_'))[3], 1, 5)
  resultsByTaxon$StudyPeriod[r] <- substr(unlist(strsplit(f, '_'))[3], 7, 15)

  # Load file
  # Each .Rdata file contains a raster stack (s_OK.Repro) that has one layer for every year in the study period (10 years per period)
  # Pixel values represent the number of consecutive weeks of suitable reproductive habitat based on species' reproductive temperature and salinity requirements
  load(file.path(baseDir, 'rOut', 'Reproduction', f))
  cat(r, '=== Handling ===', f, fill =T)

  # Clip to study area
  reproClip <- s_OK.Repro * beringShelf

  # For each year in study period, extract pixel with the highest value
  # This represents the maximum number of weeks with suitable reproductive conditions found in at least one pixel in the Bering Sea
  # Place values into appropriate 'year' columns in resultsByTaxon dataframe
  # If max value = 0 for all years, column noRepro == TRUE

  cs <- cellStats(reproClip,max)
  resultsByTaxon[r, eval(paste0('year', 1:length(cs))) ] <- cs
  resultsByTaxon[r, 'noRepro'] <- all(cs == 0)

  # Calculate summary stats across all years in the study period
  resultsByTaxon$Average<-rowMeans(resultsByTaxon[,5:14])
  resultsByTaxon$stdev[r]=sd(resultsByTaxon[r,5:14])
}

#Create taxa ID key for graphing purposes
Key<-as.data.frame(unique(resultsByTaxon$Taxa))
Key$UID<-as.numeric(row.names(Key))
colnames(Key)[1]<-"Taxa"
resultsByTaxon<-merge(resultsByTaxon, Key, by='Taxa')
rm(Key)

## save output
fileName<-file.path(file.path(baseDir, 'rOut', 'Repro_Plots',"AllTaxa_Repro.csv"))
#########check ReadMe script to make sure this folder dependency is added in
write.csv(resultsByTaxon, file = fileName,row.names=FALSE)

#Generate plot for each model
model_i<-unique(resultsByTaxon$Model)
col.l<-c("#D55E00", "#0072B2", "#009E73") #each model uses a different colour
shp.l<-c(16,17,15)
pd <- position_dodge(width = 0.4) ##for jitter
for (i in 1:length(model_i)) {
	mod.name=model_i[i]
	repro.dat<-subset(resultsByTaxon,Model==mod.name) #plot each model separately

	##define plotting parameters
	y.max=max(repro.dat$Average)
	index<-which.max(repro.dat$Average)
	st.dev<-repro.dat[index,16]
	y.max<-y.max+st.dev
	my.col<-c(col.l[i],"grey50")
	my.shp<-shp.l[i]

	plot1<-ggplot(repro.dat, aes(x=UID, y=Average, group=StudyPeriod)) +
	geom_errorbar(aes(ymin=Average-stdev, ymax=Average+stdev),width=1.2,position = pd)+
	#geom_point(shape=16,size=3,position = pd)+
	geom_point(aes(color=StudyPeriod),size=1.5,shape=my.shp,position = pd)+
	#scale_shape_manual(values=c(15,0)) +
	scale_color_manual(values=my.col)+
	scale_x_continuous(breaks=seq(1, 29, 1),limits=c(0.5,29.5),name="Species ID")+
	scale_y_continuous(breaks=seq(0, 60, 5),limits=c(-1,55),name="Number of suitable weeks")+
	#coord_cartesian(y = c(0, ymax))+
	theme_bw(base_size = 8)+
	#theme_minimal()+
	theme(legend.position="none",axis.line = element_line(colour = "black"),
    panel.border = element_blank(),
    panel.background = element_blank(),panel.grid.minor=element_blank())

	p_name<-paste('Repro_',mod.name,'.png',sep='')
	g_name<-paste('plot_',mod.name,sep='')
	assign(g_name,plot1)
	filePath <- file.path(file.path(baseDir, 'rOut','Repro_Plots'))
	#tiff(filename = fileName,width=1200,height=800,units="px",pointsize=12,res=120)
	ggsave(p_name, device = "png", path = filePath,scale = 1, width = 2500/300, height = 1770/300, units = 'in', dpi = 300, limitsize = TRUE)

rm(plot1,y.max,st.dev,index,repro.dat,g_name,p_name)
}

#Calculate change in # of weeks between future & current conditions
#Plot all models on same graph
nn <- c('Taxa', 'UID','Model', 'Avg_Weeks')
#change <- data.frame(matrix(NA, ncol = length(nn), nrow = nrow(resultsByTaxon)/2))
change <- data.frame(matrix(NA, ncol = length(nn), nrow = nrow(resultsByTaxon)/2))
names(change) <- nn
rm(nn)

y=seq(1,87,1)
w=1

##Populate change dataframe that calculates the difference in the average number of weeks for each taxa-model combo
for (j in 2:nrow(resultsByTaxon)){
	if (resultsByTaxon$Taxa[j]==resultsByTaxon$Taxa[j-1] & resultsByTaxon$Model[j]==resultsByTaxon$Model[j-1]&resultsByTaxon$StudyPeriod[j]=="2029-2039"){
		if (j==2){
		w=w
		}
		else{
		w=w+1
		}

		z=y[w]
		change$Taxa[z]<-resultsByTaxon$Taxa[j]
		change$UID[z]<-resultsByTaxon$UID[j]
		change$Model[z]<-resultsByTaxon$Model[j]
		change$Avg_Weeks[z]<-resultsByTaxon$Average[j]-resultsByTaxon$Average[j-1]
		}
	else {}
	}

##Set parameters for plotting
y.min<-min(change$Avg_Weeks)-0.1
##Plot
p4<-ggplot(change, aes(x=UID, y=Avg_Weeks, group=Model))+
	geom_point(aes(color=Model,shape=Model),size=1.5,position=pd)+
	scale_color_manual(values=c(col.l))+
	scale_x_continuous(breaks=seq(1, 29, 1),name="Species ID")+
	scale_y_continuous(breaks=seq(0, 5, 1),name="Change in suitable weeks")+
	coord_cartesian(y = c(y.min, 5))+
	theme_bw(base_size = 8)+
	theme(legend.position="none",axis.line = element_line(colour = "black"),
    panel.border = element_blank(),
    panel.background = element_blank(),panel.grid.minor=element_blank())

	#Create 4-panel plot

##multiplot function from: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
##Function is also copy-pasted at the bottom of this code
p_name<-paste('Panel_Repro_Plot','.png',sep='')
filePath <- file.path(file.path(baseDir, 'rOut','Repro_Plots'))
p_n<-paste(filePath,p_name,sep='/')
png(file=p_n,width = 2500/300, height = 2000/300,units = 'in', res = 300)
multiplot(plot_CCCma, plot_MIROC,plot_ECHOG,p4, cols=2) ##not working very well...
dev.off()

#Summary results - exclude species that cannot reproduce & two outliers that have nearly year-round repro
repro.ex<-subset(resultsByTaxon,Average!=0 & Average<49)
repro.sum<-ddply(repro.ex,c("Model","StudyPeriod"),summarize,avg=mean(Average),std=sd(Average),N=length(Average))
fileName<-file.path(file.path(baseDir, 'rOut', 'Repro_Plots',"Repro_Summary.csv"))
write.csv(repro.sum, file = fileName,row.names=FALSE)

#Clean up workspace
rm(r,cs,repro.clip,s_OK.Repro,fileName,filePath,name,pd,mod.name,i,ff,f,col.l,my.col,shp.l,w,x,y,z,p_name,p_n,my.shp,p4,j,plot_CCCma,plot_ECHOG,plot_MIROC,model_i,ymin,)

##remove dataframes
rm(change,resultsByTaxon)


















##Multiplot function
##From: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
