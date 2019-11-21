# Objective: Create a figure that summarizes number of NIS predicted to have suitable habitat for every week and every latitude.
# Value (number of NIS) is averaged across years and models within a study period.
# Present a panel for each of the two study periods and one panel that shows the 'change in NIS' from current to mid-century

# Author: A. S. Fischbach, A. Droghini


# Clear workspace ---------------------------------------------------------
rm(list=ls())

# Source in required code and packages ----------------------------------------------------
source('rCode/04-output-weeklySurvivalByLatitude.R')
source('rCode/functionMultiplot.R')

# Plot mid-century vs current  --------------------------------------------

# Subset out Latitude 51 (so few pixels that you end up getting little survival predicted) and Week 53 (Most years do not have 53 weeks)

color.pal <- brewer.pal(11,"RdYlBu")[c(11,9,8,7,5,4,2)]

the.breaks<-c(0,6,12,18,24,30,36,42)
#breaks are (included,excluded]
the.labels<-c("0 - 6","6 - 12","12 - 18", "18 - 24", "24 - 30","30 - 36", "36 - 42")

plotPeriod <- weeklySurvival %>%
  mutate(spRichf=cut(speciesRichness,breaks=the.breaks),
         periodf=factor(Period,levels=c(0,1),labels=c("Recent (2003-2012)","Mid-Century (2030-2039)")),
         weekZero = Week - 1) %>%
  subset(Week<53 & lat > 51) %>%
  ggplot(aes(weekZero, lat, as.factor(spRichf))) +
  geom_tile(aes(fill = as.factor(spRichf))) +
  scale_fill_manual(values=color.pal,name="NIS with Suitable Conditions",labels=factor(the.labels)) +
  scale_y_continuous(breaks=seq(from=52,to=66,by=2),name="Latitude (°N)",expand=c(0,0))+
  scale_x_continuous(breaks=seq(from=0,to=50,by=10),name="Time of the Year (Weeks / Months)",expand=c(0,0))+
  theme_classic() +
  theme(legend.position = "bottom",
        panel.spacing = unit(2, "lines"), #sets spacing between facet panels
        panel.grid.major = element_line(size = 0.5, colour = '#00441155')) +
  facet_wrap(~periodf)

rm(color.pal,the.breaks,the.labels)

# Plot: Change between periods --------------------------------------------

# Wow, who would have thought I would use Reddit to solve my R issues...: https://www.reddit.com/r/rstats/comments/3pgjgj/how_to_create_a_new_column_whose_value_is_the/

changeFromRecent <- weeklySurvival %>%
  group_by(lat, Week) %>% # Only compare within a Week-latitude
  arrange(Week,lat) %>% # Arrange such that values for Mid-century come right after Current
  mutate(change = speciesRichness - lag(speciesRichness, 1)) %>%  # subtract Current value from Mid-century value
  filter(!is.na(change))  # Drop NAs (Current rows have no change value)

summary(changeFromRecent)
# Max value is 7, Minimum is -8.3
# Most latitudes predict no change

# Set breaks and graphical parameters
color.pal <- brewer.pal(11,"RdYlBu")[c(9,7,4,3,2,1)]

the.breaks<-c(-10,-0.34,0.34,8) # Considering 0 (no change) to be any value between -0.33 and +0.33, which means that the majority of the models (2/3 if abs(0.3) or 3/3 if 0) agreed there was no change.
#breaks are (included,excluded]

the.labels<-c("Fewer NIS","No Change","More NIS")

plotChange <- changeFromRecent %>%
  mutate(spRichf=cut(change,breaks=the.breaks), weekZero=Week-1) %>%
  subset(Week < 52 & lat > 51) %>%
  ggplot(aes(weekZero, lat, as.factor(spRichf))) +
  geom_tile(aes(fill = as.factor(spRichf))) +
  scale_fill_manual(values=color.pal,name="Mid-Century Change in NIS",
                    labels=factor(the.labels)) +
  scale_y_continuous(breaks=seq(from=52,to=66,by=2),name="Latitude (°N)",expand=c(0,0))+
  scale_x_continuous(breaks=seq(from=0,to=50,by=10),name="Time of the Year (Weeks / Months)",expand=c(0,0))+
  theme_classic() +
  theme(legend.position = "right",
        panel.grid.major = element_line(size = 0.5, colour = '#00441155'))

# Combine  and export plots -----------------------------------------------------------
plotName<-paste('weeklySurvival','.tif',sep='')
plotName<-paste(baseDir,plotName,sep='/')
tiff(file=plotName,width = 5000, height = 3500,units = 'px', res = 500)
multiplot(plotPeriod, plotChange, layout=matrix(c(1,3,2,3), ncol=2, byrow=TRUE))
dev.off()


# Workspace clean-up ------------------------------------------------------

rm(list=ls())

# Addition of month names below the x-axis done in Photoshop.
