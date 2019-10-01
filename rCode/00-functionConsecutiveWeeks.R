# Author: A.S. Fischbach (afischbach@usgs.gov)
# With insights on base r's rle function and parallel processing from Vijay Patil (vpatil@usgs.gov), 16 Febuary 2017

## Build functions to evaluate number of consecutive weeks of suitable habitat.

newRLEfunction <- function(x){
  RLE <- rle(x) 	## summarize runs lengths using the run length encoding function
  if(1 %in% RLE$values ){
    nConsecutiveWeeksGrowth<-max(RLE$lengths[which(RLE$values == 1)]) ## extract the runs of weeks of habitat
  }else{
    nConsecutiveWeeksGrowth <- 0
  }
  #as.numeric(max(nConsecutiveWeeksGrowth) >= nWeeks) ## does the maximum run of habitat weeks exceed the threshold?
  return(nConsecutiveWeeksGrowth)
}

ConsecutiveWeeksofHabitatCalc <- function(rb){
  calc(stack(rb), fun=newRLEfunction)
}