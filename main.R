library(tidyverse)
library(jsonlite)
library(data.table)

theData = read_csv('./data/train.csv')

theData$channelGrouping = as.factor( theData$channelGrouping )
theData$fullVisitorId = as.factor( theData$fullVisitorId )
theData$socialEngagementType = as.factor( theData$socialEngagementType )

totals_col = json_clean( theData$totals )
device_col = json_clean( theData$device )
geonet_col = json_clean( theData$geoNetwork )
traffsrc_col = json_clean( theData$trafficSource )

# k = theData$totals
# d = data.frame( jsoncol = k, stringsAsFactors = FALSE )
# 
# theData = unpackJSONCol( theData, theData$totals )
# theData$totals = NULL
# theData = unpackJSONCol( theData, theData$device )

gc()

ggplot( theData, aes( channelGrouping ) ) + geom_bar()







json_clean =  function(column){
  col <- column %>% gsub('\"\"','\"',.) #turn into json format
  rs <- lapply(col, function(x) jsonlite::fromJSON(x)) # read json into data format
  ff <- rbindlist(rs, fill=TRUE) # turn list into dataframe
  gc()
  return(ff)
}


unpackJSONCol = function( theDF, theDFCol )
{
  l = lapply( theDFCol, fromJSON )
  n = unique( unlist( sapply( l, names )))
  theDF[ , n ] = lapply( n, function(x) sapply(l, function(y) y[[x]] ))
  
  return ( theDF )
}
