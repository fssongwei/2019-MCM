path="/Users/david/Desktop/2019/"
setwd(path)

#Processing data
d10 = read.csv(file = "part2_data/ACS_10_5YR_DP02_with_ann.csv")
d11 = read.csv(file = "part2_data/ACS_11_5YR_DP02_with_ann.csv")
d12 = read.csv(file = "part2_data/ACS_12_5YR_DP02_with_ann.csv")
d13 = read.csv(file = "part2_data/ACS_13_5YR_DP02_with_ann.csv")
d14 = read.csv(file = "part2_data/ACS_14_5YR_DP02_with_ann.csv")
d15 = read.csv(file = "part2_data/ACS_15_5YR_DP02_with_ann.csv")
d16 = read.csv(file = "part2_data/ACS_16_5YR_DP02_with_ann.csv")

Year = rep(2010, times=nrow(d10))
d10 = cbind(Year, d10)
Year = rep(2011, times=nrow(d11))
d11 = cbind(Year, d11)
Year = rep(2012, times=nrow(d12))
d12 = cbind(Year, d12)
Year = rep(2013, times=nrow(d13))
d13 = cbind(Year, d13)
Year = rep(2014, times=nrow(d14))
d14 = cbind(Year, d14)
Year = rep(2015, times=nrow(d15))
d15 = cbind(Year, d15)
Year = rep(2016, times=nrow(d16))
d16 = cbind(Year, d16)

data = merge(d10, d11, all="T")
data = merge(data, d12, all="T")
data = merge(data, d13, all="T")
data = merge(data, d14, all="T")
data = merge(data, d15, all="T")
data = merge(data, d16, all="T")

remove(d10,d11,d12,d13,d14,d15,d16,Year)

#processing metadada
metadata = read.csv(file = "metadata.csv")
metadata = metadata[!grepl('Estimate',metadata$Id),]
metadata = metadata[!grepl('Margin',metadata$Id),]
metadata = metadata[!grepl('Geography',metadata$Id),]

#elimiate data
data = data[,rbind("Year", as.matrix(metadata[,1]))]

#heatmap:
library(superheat)
superheat(apply(data, 2, as.numeric),scale = T,heat.na.col = "white",
          title = "Common set of socio-economic factors",
          column.title = "Features",
          row.title = "Years & Counties")

#filling missing value

fillMissingData = function(x)
{
  missingRate = sum(is.na(x)) / length(x)
  tmpVec = x
  if(missingRate > 0 & missingRate <= 0.2)
  {
    tmpVec = na.spline(tmpVec) # Cubic Spline Interpolation
  }
  else if(missingRate > 0.2 & missingRate <= 0.5)
  {
    tmpVec = na.approx(tmpVec, rule = 2) #curve fitting
  }
  else if(missingRate > 0.5)
  {
    if(length(tmpVec[!is.na(tmpVec)]) == 0){
      tmpVec[is.na(tmpVec)] = 0
    }
    else
    {
      tmpVec[is.na(tmpVec)] = sum(tmpVec[!is.na(tmpVec)]) / length(tmpVec[!is.na(tmpVec)])
    }
  }
  return(tmpVec)
}

countyID = as.numeric(as.vector(unique(data[,"GEO.id2"])))
countyID = countyID[which(is.na(countyID) == FALSE)]

data_filled = as.matrix(subset(data, GEO.id2 == countyID[1]))
data_filled = apply(county_data, 2, as.numeric)
data_filled = apply(county_data, 2, fillMissingData)

write.csv(data_filled, file = "filled_data.csv")

for (county in countyID[2:length(countyID)]) {
  tmp_county_data = as.matrix(subset(data, GEO.id2 == county))
  tmp_county_data = apply(tmp_county_data, 2, as.numeric)
  tmp_county_data = apply(tmp_county_data, 2, fillMissingData)
  data_filled = merge(data_filled, tmp_county_data, all = "T")
}

data_filled = data_filled[which(colSums(data_filled) != 0)]
write.csv(data_filled, file = "data")

#heatmap:
library(superheat)
superheat(apply(data_filled, 2, as.numeric),scale = T,heat.na.col = "white",
          title = "Common set of socio-economic factors (without missing value)",
          column.title = "Features",
          row.title = "Years & Counties")

factors = c("HOUSEHOLDS BY TYPE", "RELATIONSHIP", "MARITAL STATUS", "FERTILITY", "GRANDPARENTS", 
               "SCHOOL ENROLLMENT", "EDUCATIONAL ATTAINMENT", "VETERAN STATUS", "DISABILITY STATUS", "RESIDENCE",
               "PLACE OF BIRTH", "U.S. CITIZENSHIP STATUS", "YEAR OF ENTRY", "WORLD REGION OF BIRTH OF FOREIGN BORN",
               "LANGUAGE SPOKEN AT HOME", "ANCESTRY", "COMPUTERS AND INTERNET USE")


#Step 1 find the categories represent in metadata
factor_metadata = metadata[grepl(factors[1],metadata$Id),]

#Step 2 按年份，找出所有county包含上面metadata的数据（横轴为metadata，纵轴为）


countyID = unique(merge_data[,"GEO.id2"])
county<-subset(merge_data, GEO.id2=="21001")

county[,"HC03_VC14"]


  if(length(county[,1])>1){
    
    county <- county[,4:length(county)]
    data_nums <- lapply(county, as.numeric)
    nums <- unlist(lapply(data_nums, is.numeric))
    county=as.matrix(county[,nums])
    county_processed = apply(county,2,as.numeric)
    #county_processed = processingData(county)
    county_filled = apply(county_processed, 2, fillMissingData)
    #Delete zero columns
    nonzero_county = subset(county_filled, select=colMeans(is.na(county_filled)) == 0) 
    county_normalized = apply(nonzero_county, 2, normalization)
    write.csv(county_normalized,file = paste("./county_",i,"_normalized.csv",sep = ""))
    county_test = prcomp(county_normalized,cor=TRUE)
    summary(county_test,loadings=TRUE)
    pca = as.data.frame(predict(county_test)[,1:60])
    
    write.csv(pca,file = paste("./county_",i,"_pca.csv",sep = ""))

}



processingData = function(dataset)
{
  data_nums <- lapply(dataset, as.numeric)
  nums <- unlist(lapply(data_nums, is.numeric))
  mtrx=as.matrix(dataset[,nums])
  merge_sub = apply(mtrx,2,as.numeric)
  return(merge_sub)
}


#data_nums <- lapply(merge_data, as.numeric)
#nums <- unlist(lapply(data_nums, is.numeric))
#merge_data=as.matrix(merge_data[,nums])
#merge_sub = apply(merge_data,2,as.numeric)


#library(superheat)
#superheat(merge_sub, scale = T)
#write.csv(merge_data,file = "./merge.csv")



library(reshape) # used for cast function
library(plyr) # used for ddply function
library(RColorBrewer) # used to customize heatmap colors
library(zoo)

#missing data processing
fillMissingData = function(x)
{
  x[x == "(X)"] = NA
  missingRate = sum(is.na(x))/length(x)
  tmpVec = x
  if(missingRate > 0 & missingRate <= 0.2)
  {
    tmpVec = na.spline(tmpVec) #Cubic Spline Interpolation
  }
  else if(missingRate > 0.2 & missingRate <= 0.5)
  {
    tmpVec = na.approx(tmpVec, rule = 2) #curve fitting
  }
  else if(missingRate > 0.5)
  {
    return (tmpVec)
  }
  return (tmpVec)
}

#merge_filled = apply(merge_sub, 2, fillMissingData)

#Delete zero columns
#nonzero_sub = subset(merge_filled, select=colMeans(is.na(merge_filled)) == 0) 
#write.csv(nonzero_sub,file = "./nonzero.csv")

library(superheat)
superheat(nonzero_sub, scale = T)

#normalization
normalization = function(x)
{
  min = min(x)
  max = max(x)
  if((max-min) == 0) return (x)
  return ((x-min) / (max-min))
}
# #merge_normalized = apply(nonzero_sub, 2, normalization)
# write.csv(merge_normalized,file = "./normalized.csv")
# 
data1 = read.csv(file = "county_21003_normalized.csv",header=T,sep=",")
test = princomp(data1,cor=TRUE)
summary(test,loadings=TRUE)
pca = as.data.frame(predict(test)[,1:60])

# write.csv(pca,file = "./pca.csv")

PCA = function(matrix)
{
  pca = prcomp(matrix,scale=TRUE)
  eigs = pca$sdev^2
  var_pca=round(cumsum(eigs)/sum(eigs),4)
  this_var <- get_pca_var(pca)
  n=min(which(var_pca>0.85))#n is the dimension that expalained 85% variance
  mc=rowSums(this_var$cor[,1:n]) #this should be the weight for the 3 columns
  return(mc)
}
