library(tidyverse)
library(jsonlite)

theData = read_csv('./data/train.csv')

theData$channelGrouping = as.factor( theData$channelGrouping )
theData$fullVisitorId = as.factor( theData$fullVisitorId )
theData$socialEngagementType = as.factor( theData$socialEngagementType )

theData = unpackJSONCol( theData, theData$totals )
theData$totals = NULL
theData = unpackJSONCol( theData, theData$device )

ggplot( theData, aes( channelGrouping ) ) + geom_bar()










unpackJSONCol = function( theDF, theDFCol )
{
  l = lapply( theDFCol, fromJSON )
  n = unique( unlist( sapply( l, names )))
  theDF[ , n ] = lapply( n, function(x) sapply(l, function(y) y[[x]] ))
  
  return ( theDF )
}
