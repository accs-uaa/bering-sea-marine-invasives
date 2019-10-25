# Intent: This code cleans up the NBCI ballast water data and produces figures showing patterns of vessel traffic to U.S. Bering Sea ports ("bsai" region)
# Author: Jordan Watson (jordan.watson@noaa.gov)

#===================================================================
#  Load packages and data
#===================================================================

library(tidyverse)
library(circlize)
library(RColorBrewer)

#  Load 2016 ballast water query results
mydat <- read.csv("Data/search_results_2015_2016b.csv")
#===================================================================

#===================================================================
#  Data clean-up
#===================================================================

# Standardize naming of ports and countries

names(mydat) <- tolower(names(mydat))
mydat <- mydat %>% filter(last.port!="LAT LON" & last.port!="")
mydat$port2 <- gsub(" \\(USA, AK\\)","",as.character(mydat$port))

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

mydat$LastRegion[mydat$last.port %in% unique(mydat$last.port[grep("(USA, WA)",mydat$last.port)])] <- "Washington"
mydat$LastRegion[mydat$last.port %in% unique(mydat$last.port[grep("(USA, HI)",mydat$last.port)])] <- "Hawaii"
mydat$LastRegion[mydat$last.port %in% unique(mydat$last.port[grep("(USA, AK)",mydat$last.port)])] <- "Alaska"
mydat$LastRegion[mydat$last.port %in% unique(mydat$last.port[grep("(USA, CA)",mydat$last.port)])] <- "California"
mydat$LastRegion[mydat$last.port %in% unique(mydat$last.port[grep("(USA, OR)",mydat$last.port)])] <- "Oregon"
mydat$LastRegion[mydat$last.port %in% unique(mydat$last.port[grep("(USA, FL)",mydat$last.port)])] <- "Florida"

mydat$last.port <- as.character(mydat$last.port)
mydat$last.port[mydat$last.port=="Victoria (Canada)"] <- "Victoria"
mydat$last.port[mydat$last.port=="Vancouver (Canada, BC)"] <- "Vancouver"
mydat$last.port[mydat$last.port=="Vancouver (Canada)"] <- "Vancouver"
mydat$last.port[mydat$last.port=="Manzanillo (Panama)"] <- "Manzanillo"
mydat$last.port[mydat$last.port=="Manzanillo (Mexico)"] <- "Manzanillo"
mydat$last.port[mydat$last.port=="San Francisco Cotp Zone"] <- "San Francisco (USA, CA)"
mydat$last.port[mydat$last.port=="San Francisco"] <- "San Francisco (USA, CA)"
mydat$LastRegion[mydat$last.port %in% unique(mydat$last.port[grep("(USA, CA)",mydat$last.port)])] <- "California"

mydat$last.port[mydat$last.port %in% unique(mydat$last.port[grep("(USA, WA)",mydat$last.port)])] <- gsub(" \\(USA, WA\\)","",mydat$last.port[mydat$last.port %in% unique(mydat$last.port[grep("(USA, WA)",mydat$last.port)])])
mydat$last.port[mydat$last.port %in% unique(mydat$last.port[grep("(USA, HI)",mydat$last.port)])] <- gsub(" \\(USA, HI\\)","",mydat$last.port[mydat$last.port %in% unique(mydat$last.port[grep("(USA, HI)",mydat$last.port)])])
mydat$last.port[mydat$last.port %in% unique(mydat$last.port[grep("(USA, AK)",mydat$last.port)])] <- gsub(" \\(USA, AK\\)","",mydat$last.port[mydat$last.port %in% unique(mydat$last.port[grep("(USA, AK)",mydat$last.port)])])
mydat$last.port[mydat$last.port %in% unique(mydat$last.port[grep("(USA, CA)",mydat$last.port)])] <- gsub(" \\(USA, CA\\)","",mydat$last.port[mydat$last.port %in% unique(mydat$last.port[grep("(USA, CA)",mydat$last.port)])])
mydat$last.port[mydat$last.port %in% unique(mydat$last.port[grep("(USA, OR)",mydat$last.port)])] <- gsub(" \\(USA, OR\\)","",mydat$last.port[mydat$last.port %in% unique(mydat$last.port[grep("(USA, OR)",mydat$last.port)])])
mydat$last.port[mydat$last.port %in% unique(mydat$last.port[grep("(USA, FL)",mydat$last.port)])] <- gsub(" \\(USA, FL\\)","",mydat$last.port[mydat$last.port %in% unique(mydat$last.port[grep("(USA, FL)",mydat$last.port)])])

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
#===================================================================

#===================================================================
#  Identify Alaskan ports by region
#===================================================================

arctic <- c("Point Barrow","Kotzebue","Prudhoe Bay","Kivalina")
bsai <- c("Dillingham","Naknek","Egegik","Adak Island","Akutan","Barrow","Dutch Harbor","Naknek","Saint Paul","Togiak","Togiak Bay","Nome","Wainwright","Kiska","Platinum","Dillingham","Nunivak Island","Bethel","Point Hope","King Island","Cape Constantine","Udagak Bay","Egegik","Port Clarence","Captains Bay","Dutch Harbour")
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
#===================================================================

#===================================================================
#  Tally trips by port
#===================================================================

mydat %>%
  filter(NextRegion=="bsai") %>%
  group_by(LastRegion) %>%
  summarise(N=n()) %>%
  arrange(-N) %>%
  data.frame


#===================================================================
#  Plot trips from outside the bsai into the bsai
#===================================================================

newdat <- mydat %>% filter(NextRegion=="bsai" & !LastPort%in%paste(bsai,"(Alaska)")) %>% dplyr::select(from=LastRegion,to=port2)
#  Create adjacency matrix for chord diagram
adj <- table(newdat$from,newdat$to)
attributes(adj)$class <- 'matrix'

# To specify the size of the gap in the chord diagram, sum the rows/columns dimensions of the adjacency matrix.
my.gap <-sum(dim(adj))

#  Plot
x11(width=10,height=10);
circos.par(gap.degree=rep(4,my.gap))
chordDiagram(adj, annotationTrack = "grid",
             preAllocateTracks = list(track.height = 0.3))
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise",
              niceFacing = TRUE, adj = c(0, 0.5),cex=1.5)
}, bg.border = NA)
circos.clear()
#===================================================================



#===================================================================
#  Plot trips from Alaska (including the bsai) into the bsai
#===================================================================

mydat$temp <- NA
mydat$temp[mydat$last.port %in% arctic] <- "Arctic"
mydat$temp[mydat$last.port %in% bsai] <- "BSAI"
mydat$temp[mydat$last.port %in% goa] <- "GOA"
mydat$temp[mydat$last.port %in% seak] <- "SEAK"

newdat <- mydat %>%
  filter(NextRegion=="bsai" & LastRegion%in%c("Arctic (AK)","BSAI (AK)","GOA (AK)","Southeast AK")) %>%
  dplyr::select(-arrival.date,-imo.number,-last.country,-vessel.type) %>%
  mutate(LastPort2=paste0(last.port," (",temp,")")) %>%
  dplyr::select(from=LastPort2,to=port2)

#newdat <- mydat %>% filter(NextRegion=="bsai" & LastRegion%in%c("Arctic (AK)","BSAI (AK)","GOA (AK)","Southeast AK")) %>% dplyr::select(from=LastPort,to=port2)
adj <- table(newdat$from,newdat$to)
attributes(adj)$class <- 'matrix'

# To specify the size of the gap in the chord diagram, sum the rows/columns dimensions of the adjacency matrix.
my.gap <-sum(dim(adj))

x11(width=20,height=20);
circos.par(gap.degree=rep(4,my.gap))
chordDiagram(adj, annotationTrack = "grid",
             preAllocateTracks = list(track.height = 0.3))
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise",
              niceFacing = TRUE, adj = c(0, 0.5),cex=1)
}, bg.border = NA)
circos.clear()
#===================================================================
