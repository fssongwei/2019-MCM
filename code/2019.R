path="/Users/david/Desktop/2019/"
setwd(path)
library(openxlsx)
library(zoo) #for missing value processing
library(factoextra) #for pca
library(ggrepel) #scatter plot
library(ggplot2)
library(ggpubr)
library(reshape) # used for cast function
library(plyr) # used for ddply function
library(RColorBrewer) # used to customize heatmap colors
rawdata=read.xlsx("MCM_NFLIS_Data.xlsx", sheet="Data")

Dataprocessing = function(rawdata, state)
{
  state = "PA"
  matrix = subset(rawdata, State == state)
  years = unique(matrix[,"YYYY"])
  counties = as.matrix(unique(matrix[,"FIPS_Combined"]))
  counties_ID = as.matrix(unique(matrix[,"COUNTY"]))
  
  for (y in years) {
    sub_matrix =  matrix = subset(rawdata, YYYY == y)
    result = apply(counties, 1, calculateCounty, sub_matrix)
    counties = cbind(counties, result)
  }
  
  counties = counties[,2:ncol(counties)]
  counties = apply(counties, 2, as.numeric)
  colnames(counties) = years
  rownames(counties) = counties_ID
  return (counties)
}

calculateCounty = function(x, matrix)
{
  sub_matrix = subset(matrix, FIPS_Combined == x)
  result = sum(sub_matrix[,"DrugReports"]) / sub_matrix[,"TotalDrugReportsCounty"][1]
  if(is.na(result)) return(0)
  return(as.matrix(result))
}

removeCounties = function(matrix)
{
  average_threshold = mean(apply(matrix, 2, mean)) #counties average drug smaller than this will be remove
  variance_threshold = mean(apply(matrix, 2, var)) #counties variance drug smaller than this will be remove
  newdata = matrix[which(apply(matrix, 1, mean) >= average_threshold),]
  newdata = matrix[which(apply(newdata, 1, var) >= variance_threshold),]
  return(newdata)
}

#=======================================================================
#get location
locationRawData = read.csv("uscitiesv1.4.csv")
locationRawData = locationRawData[,cbind('state_id','county_fips','county_name', 'lat', 'lng')]

getStateLocationData = function(matrix, state)
{
  locationRaw = subset(matrix, state_id == state)
  #locationRaw = matrix
  countyID = as.matrix(unique(locationRaw[,"county_fips"]))
  result = apply(countyID, 1, calculateLocation, locationRaw)
  colnames(result) = countyID
  rownames(result) = c("lat", "lng")
  return(result)
}

calculateLocation = function(x, matrix){
  countySet = subset(matrix, county_fips == x)
  lat = mean(countySet[,"lat"])
  lng = mean(countySet[,"lng"])
  return(c(lat,lng))
}

#write.csv(t(getStateLocationData(locationRawData, "all")), file = "location.csv")

#========================
#Bayesian network
getBNresult = function(stateData, locationData)
{
  library("bnlearn")
  stateData = as.data.frame(t(stateData))
  print(stateData)
  bn_df <- stateData
  res <- hc(bn_df)
  print(res)
  plot(res)
  fittedbn <- bn.fit(res, data = bn_df) #BN result
  
  result = as.matrix(colnames(stateData))
  result = as.matrix(apply(result, 1, processBN, locationData, fittedbn))
  rownames(result) = colnames(stateData)
  return (result)
}

processBN = function(x, locationData, fittedbn)
{
  BNresult = getElement(fittedbn, x)
  neighborPoints = as.matrix(c(BNresult$parents, BNresult$children))
  neighborPointsWeighted = apply(neighborPoints, 1, calculateDistance, locationData, x)
  return (sum(neighborPointsWeighted))
}

calculateDistance = function(x, locationData, center)
{
  centerLocation = locationData[,center]
  neighborLocation = locationData[,x]
  distance = 1/sqrt((centerLocation[1] - neighborLocation[1])^2 + (centerLocation[2] - neighborLocation[2])^2)
}



#heatmap

#states = unique(rawdata[,"State"])
for (state in states) {
  data = Dataprocessing(rawdata, "WV")
  heatmap(data, Colv = NA, Rowv = T,
          scale = "none",
          col = rev(brewer.pal(11, "RdBu")),
          main = paste(state, "(full version)"),
          xlab = "Year",
          ylab = "County")
  data = removeCounties(data) #delete some conties
  heatmap(data, Colv = NA, Rowv = T,
          scale = "none",
          col = rev(brewer.pal(11, "RdBu")),
          main = paste(state, "(Simplified version)"),
          xlab = "Year",
          ylab = "County")
}

#output
for (state in states) {
  state = "WV"
  data = Dataprocessing(rawdata, state)
  data = removeCounties(data) #delete some conties
  

  location = getStateLocationData(locationRawData, state) #coordinate
  result = getBNresult(data, location)
  result = as.matrix(result[order(result[,1]),]) #order
  #write.csv(result, file = paste(state,".csv"))
  
  test = cbind(t(getStateLocationData(locationRawData, "PA"))[rownames(result),],result)
  
}


data = subset(rawdata, State == "PA")
data = subset(data, YYYY == "2017")
data = subset(data, SubstanceName == "Heroin")[,c("FIPS_Combined", "DrugReports")]
t = t(getStateLocationData(locationRawData, "PA"))[data[,"FIPS_Combined"],]

data = cbind(t[,c("lng","lat")],data)[,c(1,2,4)]



