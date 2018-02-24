#Author: Jordan Watson (jordan.watson@noaa.gov)

library(tidyverse)
library(circlize)
library(RColorBrewer)
library(forcats)
library(readxl)


#Download Ship arrival records from:
#"https://invasions.si.edu/cgi/search-nbic?do=arrivals&format=text&state=AK&mgmt_method=Any%20method&t0=2014-01-01&tend=2016-12-31"

#  2016 ballast water query results 
mydat <- read.csv("Data/search_results_2014_2016.csv",skip=2)
#mydat <- read.csv("Data/ballast_search_results_2014_2016.csv",skip=2)


#  Make column names lowercase.
names(mydat) <- tolower(names(mydat))

#  Many entries are missing the port name and are either blank or just say "LAT LON" - delete these
mydat <- mydat %>% filter(last.port!="LAT LON" & last.port!="")
mydat$port2 <- gsub(" \\(USA, AK\\)","",as.character(mydat$port))

#  Make arrival date a date field and extract the year
mydat$arrival.date <- as.Date(mydat$arrival.date,format="%Y-%m-%d")
mydat$year <- format(mydat$arrival.date,format="%Y")

#  Convert country abbreviations to names for last region
mydat$LastRegion <- NA
mydat$LastRegion[mydat$last.country=="CN"] <- "China"
mydat$LastRegion[mydat$last.country=="JP"] <- "Japan"
mydat$LastRegion[mydat$last.country=="KR"] <- "South Korea"
mydat$LastRegion[mydat$last.country=="SG"] <- "Singapore"
mydat$LastRegion[mydat$last.country=="ES"] <- "Spain"
mydat$LastRegion[mydat$last.country=="RU"] <- "Russia"
mydat$LastRegion[mydat$last.country=="CA"] <- "Canada"
mydat$LastRegion[mydat$last.country=="MX"] <- "Mexico"
mydat$LastRegion[mydat$last.country=="PA"] <- "Panama"
mydat$LastRegion[mydat$last.country=="NZ"] <- "New Zealand"
mydat$LastRegion[mydat$last.country=="DE"] <- "Germany"
mydat$LastRegion[mydat$last.country=="GU"] <- "Guam"
mydat$LastRegion[mydat$last.country=="TW"] <- "Taiwan"

#  Extract U.S. state names from the last region
mydat$LastRegion[mydat$last.port %in% unique(mydat$last.port[grep("(USA, WA)",mydat$last.port)])] <- "Washington"
mydat$LastRegion[mydat$last.port %in% unique(mydat$last.port[grep("(USA, HI)",mydat$last.port)])] <- "Hawaii"
mydat$LastRegion[mydat$last.port %in% unique(mydat$last.port[grep("(USA, AK)",mydat$last.port)])] <- "Alaska"
mydat$LastRegion[mydat$last.port %in% unique(mydat$last.port[grep("(USA, CA)",mydat$last.port)])] <- "California"
mydat$LastRegion[mydat$last.port %in% unique(mydat$last.port[grep("(USA, OR)",mydat$last.port)])] <- "Oregon"
mydat$LastRegion[mydat$last.port %in% unique(mydat$last.port[grep("(USA, FL)",mydat$last.port)])] <- "Florida"
mydat$LastRegion[mydat$last.port %in% unique(mydat$last.port[grep("(USA, DE)",mydat$last.port)])] <- "Delaware"

#  Separate country from port name as it's already recorded in last.country
mydat$last.port <- as.character(mydat$last.port)
mydat$last.port[mydat$last.port=="Victoria (Canada)"] <- "Victoria"
mydat$last.port[mydat$last.port=="Vancouver (Canada, BC)"] <- "Vancouver"
mydat$last.port[mydat$last.port=="Vancouver (Canada)"] <- "Vancouver"
mydat$last.port[mydat$last.port=="Manzanillo (Panama)"] <- "Manzanillo"
mydat$last.port[mydat$last.port=="Manzanillo (Mexico)"] <- "Manzanillo"
mydat$last.port[mydat$last.port=="San Francisco Cotp Zone"] <- "San Francisco (USA, CA)"
mydat$last.port[mydat$last.port=="San Francisco"] <- "San Francisco (USA, CA)"
mydat$LastRegion[mydat$last.port %in% unique(mydat$last.port[grep("(USA, CA)",mydat$last.port)])] <- "California"

#  Get ride of the USA and state abbreviation from last.port and leave just the port name
mydat$last.port[mydat$last.port %in% unique(mydat$last.port[grep("(USA, WA)",mydat$last.port)])] <- gsub(" \\(USA, WA\\)","",mydat$last.port[mydat$last.port %in% unique(mydat$last.port[grep("(USA, WA)",mydat$last.port)])])
mydat$last.port[mydat$last.port %in% unique(mydat$last.port[grep("(USA, HI)",mydat$last.port)])] <- gsub(" \\(USA, HI\\)","",mydat$last.port[mydat$last.port %in% unique(mydat$last.port[grep("(USA, HI)",mydat$last.port)])])
mydat$last.port[mydat$last.port %in% unique(mydat$last.port[grep("(USA, AK)",mydat$last.port)])] <- gsub(" \\(USA, AK\\)","",mydat$last.port[mydat$last.port %in% unique(mydat$last.port[grep("(USA, AK)",mydat$last.port)])])
mydat$last.port[mydat$last.port %in% unique(mydat$last.port[grep("(USA, CA)",mydat$last.port)])] <- gsub(" \\(USA, CA\\)","",mydat$last.port[mydat$last.port %in% unique(mydat$last.port[grep("(USA, CA)",mydat$last.port)])])
mydat$last.port[mydat$last.port %in% unique(mydat$last.port[grep("(USA, OR)",mydat$last.port)])] <- gsub(" \\(USA, OR\\)","",mydat$last.port[mydat$last.port %in% unique(mydat$last.port[grep("(USA, OR)",mydat$last.port)])])
mydat$last.port[mydat$last.port %in% unique(mydat$last.port[grep("(USA, FL)",mydat$last.port)])] <- gsub(" \\(USA, FL\\)","",mydat$last.port[mydat$last.port %in% unique(mydat$last.port[grep("(USA, FL)",mydat$last.port)])])

#  Standardize port names
mydat$port2[mydat$port2=="Prudhoe Bay Offshore"] <- "Prudhoe Bay"
mydat$port2[mydat$port2=="Red Dog"] <- "Kivalina"
mydat$port2[mydat$port2=="Norton Sound"] <- "Nome"
#mydat$port2[mydat$port2 %in% c("Dillingham","Naknek","Egegik")] <- "Bristol Bay"
mydat$port2[mydat$port2=="Captains Bay"] <- "Dutch Harbor"
mydat$port2[mydat$port2=="Drift River Terminal"] <- "Cook Inlet"
mydat$port2[mydat$port2=="Kazakof Bay"] <-"Afognak"
mydat$port2[mydat$port2=="Sawmill Bay"] <- "Valdez"
mydat$port2[mydat$port2=="Womans Bay"] <- "Kodiak"
mydat$port2[mydat$port2=="Togiak Bay"] <- "Togiak"
mydat$port2[mydat$port2=="Dutch Harbour"] <- "Dutch Harbor"

mydat$last.port[mydat$last.port=="Long Beach Anchorage"] <- "Long Beach"
mydat$last.port[mydat$last.port=="Norton Sound"] <- "Nome"
mydat$last.port[mydat$last.port=="Captains Bay"] <- "Dutch Harbor"
mydat$last.port[mydat$last.port=="Drift River Terminal"] <- "Cook Inlet"
mydat$last.port[mydat$last.port=="Kazakof Bay"] <-"Afognak"
mydat$last.port[mydat$last.port=="Womans Bay"] <- "Kodiak"
mydat$last.port[mydat$last.port=="Hubbard Glacier"] <- "Yakutat"
mydat$last.port[mydat$last.port=="Dutch Harbour"] <- "Dutch Harbor"

#  If the last.port was simply called "Alaska" then remove
mydat <- mydat %>% filter(last.port!="Alaska")

#  Identify Alaskan ports by region
arctic <- c("Point Barrow","Kotzebue","Prudhoe Bay","Kivalina","Barrow")
bsai <- c("Adak Island","Akutan","Dutch Harbor","Naknek","Saint Paul","Togiak","Togiak Bay","Nome","Wainwright","Kiska","Platinum","Dillingham","Nunivak Island","Bethel","Point Hope","King Island","Cape Constantine","Udagak Bay","Egegik","Port Clarence","Captains Bay","Dutch Harbour")
goa <- c("Afognak","Anchorage","Chignik","Cordova","Cook Inlet","Homer","Kodiak","Sand Point","Seward","Valdez","Whittier","Nikiski","King Cove","Port Baily","Ouzinkie","Cape Hinchinbrook","Old Harbor","Seldovia","Port Lions","Larsen Bay","Kachemak Bay","Cold Bay","Alitak","Port Graham" )
seak <- c("Craig","Auke Bay","Hawk Inlet","Juneau","Ketchikan","Klawock","Sitka","Skagway","Thorne Bay","Yakutat","Tolstoi","Tolstoi Bay","Glacier Bay","Wrangell","Tracy Arm","Petersburg","Haines","Hoonah","Icy Strait")

mydat$NextRegion <- NA
mydat$NextRegion[mydat$port2 %in% arctic] <- "arctic"
mydat$NextRegion[mydat$port2 %in% bsai] <- "bsai"
mydat$NextRegion[mydat$port2 %in% goa] <- "goa"
mydat$NextRegion[mydat$port2 %in% seak] <- "seak"

#  Append the state or country to the port name
mydat$LastPort <- paste0(mydat$last.port," (",mydat$LastRegion,")")

mydat$LastRegion[mydat$last.port %in% arctic] <- "Arctic (AK)"
mydat$LastRegion[mydat$last.port %in% bsai] <- "BSAI (AK)"
mydat$LastRegion[mydat$last.port %in% goa] <- "GOA (AK)"
mydat$LastRegion[mydat$last.port %in% seak] <- "Southeast AK"

mydat$LastRegion1 <- ifelse(mydat$LastRegion%in%c("Delaware","Florida","Spain","Germany","Guam","Hawaii","New Zealand","Oregon"),"Other",mydat$LastRegion)
mydat$LastRegion5 <- ifelse(mydat$LastRegion%in%c("Delaware","Florida","Spain","Germany","Guam","Hawaii","New Zealand","Oregon","Singapore","Panama","Mexico","Arctic (AK)","Alaska"),"Other",mydat$LastRegion)
