#Last updated: 4 October 2017
# Authors: A.S. Fischbach (afischbach@usgs.gov)
# 		   A. Droghini (adroghini@alaska.edu)

##Intent: For each taxa-year-model combination, identify which weeks of the year (1-53) contain suitable habitat

##Read in files of weekly survival for each taxa, year and model
#baseDir <-  'D:/HelpingOthers/ABSIlcc/NFWF2016/Models'
ff <- list.files(file.path(baseDir, 'rOut', 'SurvivalWeeks_which'), pattern = 'rData')
dsn<-"C:/Users/adroghini/Documents/Marine-invasives/PMEL-Models/rCode/roms_habitat/BeringSea_200m.shp"
Bering_200m <- readOGR(dsn) #see BeringSea_Raster code

##Create dataframe into which to place results
nn <- c('Taxa', 'Model', 'Year', 'StudyPeriod', 'noYearRoundSurvival', eval(paste0('week', 1:53)))
out <- data.frame(matrix(NA, ncol = length(nn), nrow = length(ff)))
names(out) <- nn

#####lots of files to run through
	for(r in 1:nrow(out)){
		f <- ff[r]
		## parse out the taxa, model  and year for each listed files
		out$Taxa[r] <- unlist(strsplit(f, '_'))[1]
		out$Model[r] <- unlist(strsplit(f, '_'))[4] 
		out$Year[r] <- substr(unlist(strsplit(f, '_'))[5], 1, 4)
		if(as.numeric(out$Year[r]<2013)){ ##generate study period based on file year
		out$StudyPeriod[r]<-"2003-2012"
		}else{
		out$StudyPeriod[r]<-"2030-2039"
	}
		##load each taxa-model-year combination
		##each .Rdata file contains 1 raster layer for every week of the year with 1/0 cells for suitable/unsuitable habitat
		load(file.path(baseDir, 'rOut', 'SurvivalWeeks_which', f)) ##load file (s_y_OK) 
		cat(r, '=== Handling ===', f, fill =T) 
		
		weeks.clip<-s_y_OK*Bering_200m  ##clip to Bering Sea continental shelf (see: BeringSea_Raster.R code)
	
		cs <- cellStats(weeks.clip,max) ##why doesn't this work?
		
		## places cell stats into columns evaluate(paste0('week', 1:53))
		out[r, eval(paste0('week', 1:length(cs))) ] <- cs
		out[r, 'noYearRoundSurvival'] <- any(cs == 0) ##if 0 for at least 1 week of the year, noYearRoundSurvival == TRUE
		}

## save output
fileName<-file.path(file.path(baseDir, 'rOut', 'Weekly_Survival',"AllTaxa_WeeklySurvival.csv")) 
write.csv(out, file = fileName,row.names=FALSE)
rm(fileName)

##Workspace clean-up
rm(fileName,nn,cs,f,ff,r,s_y_OK,weeks.clip)
