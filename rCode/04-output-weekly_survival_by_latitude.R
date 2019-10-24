# Objective: Summarize the number of NIS predicted to have suitable habitat for every week and every latitude.
# To generate heatmap figure.

# Author: A.S. Fischbach (afischbach@usgs.gov)
#          A. Droghini (adroghini@alaska.edu)

## Source initial run script
source('rCode/init.R')

# Load data ---------------------------------------------------------------

# The dat.frame df_ee was built by the .\rCode\03-weeklySurvivalbyLatitude.R script, which summarized weekly survival into 1 degree latitudinal bands for all taxa, modeled across study years within each study period.

# Each row of this dat.frame represents survival across weeks (columns 1:53) for each taxon, model, and year (indicated in the columns 'taxa', 'model', and 'year'). The column 'studyPeriodOne' is TRUE if year is part of the study period 2003-2012

load(file.path(baseDir, '/rOut/week_survival_lat_taxa_model_studyPeriod.rData'))

cat(nrow(df_ee), 'rows of data read in from', length(unique(df_ee$taxa)), 'taxa', fill =T)
glimpse(df_ee)

# Reshape data ------------------------------------------------------------

# Reshape into long form with a column for weeks, a column for latitudinal bands, and the columns for the taxa, model, year and study period.

df_tidy <- df_ee %>%
  gather(key = Week, value = survival, -taxa, -model, -studyPeriodOne, -lat, -year)
df_tidy$Week <- as.numeric(substr(df_tidy$Week, start = 6, stop = 7))
df_tidy$lat <- as.numeric(df_tidy$lat)
glimpse(df_tidy)
summary(df_tidy)


# Summarize data by model ----------------------------------------------------------

# Summarize weekly survival over all taxa by StudyPeriod (False = future, True = current) and latitudinal band.

# For each taxon, take the mean of weekly survival across each year within each study period. If that mean is >= 0.7, classify the latitudinal band as having habitat (1, 0 otherwise). This 7/10 threshold is the same threshold that was used to define year-round survival.

# Within a taxon-model-study period, and for each week:
# How many years are predicted to have suitable habitat?
df_byPeriodModel <- df_tidy %>%
  mutate(Period = as.integer(!studyPeriodOne) ) %>%
  group_by(Period, taxa, Week, lat, model) %>%
  summarize(nSurvival = sum(survival, na.rm = T))

# Is number of years >= 7?
df_byPeriodModel <-df_byPeriodModel %>%
  mutate(survival = if_else(nSurvival >= 7, 1, 0))

# Average across the 3 ROMS models
df_byPeriodTaxa <- df_byPeriodModel %>%
  group_by(Period, taxa, Week, lat) %>%
  summarize(MeanAcrossModelsSurvival = mean(survival, na.rm = T))

# Summarize across taxa ---------------------------------------------------
weeklySurvival <- df_byPeriodTaxa %>%
  group_by(Period, Week, lat) %>%
  summarize(speciesRichness = sum(MeanAcrossModelsSurvival, na.rm = T))

rm(df_ee,df_tidy,df_byPeriodModel,df_byPeriodTaxa)
