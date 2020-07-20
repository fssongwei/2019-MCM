path="/Users/david/Desktop/2019/"
setwd(path)

Alldata = read.csv(file = "data.csv")

getStateData = function(data_matrix, state)
{
  rawdata=subset(read.xlsx("MCM_NFLIS_Data.xlsx", sheet="Data"), State == state)[,c("State", "FIPS_Combined")]
  countyID = unique(rawdata["FIPS_Combined"])
  t1 = apply(as.matrix(Alldata[,"GEO.id2"]), 1, as.numeric)
  t2 = apply(as.matrix(countyID), 1, as.numeric)
  data = Alldata[which(t1 %in% t2),]
  return(data)
}

data = Alldata

#processing metadada
metadata = read.csv(file = "metadata.csv")
metadata = metadata[!grepl('Estimate',metadata$Id),]
metadata = metadata[!grepl('Margin',metadata$Id),]
metadata = metadata[!grepl('Geography',metadata$Id),]

countyID = as.numeric(as.vector(unique(data[,"GEO.id2"])))

#Step 1 find the categories represent in metadata

all_result = countyID

for (factor in colnames(data)[3:ncol(data)]) {
  result = as.matrix(countyID)
  colnames(result) = "GEO.id2"
  for (year in c(2010,2011,2012,2013,2014,2015,2016)) {
    tmp_data = subset(data, Year == year)
    tmp_data = tmp_data[,cbind("GEO.id2",factor)]

    #normalization
    normalization = function(x)
    {
      min = min(x)
      max = max(x)
      if((max-min) == 0) return (x)
      return ((x-min) / (max-min))
    }
    nor_result = cbind(tmp_data[,1], apply(as.matrix(tmp_data[,2]),2,normalization))
    colnames(nor_result) = c("GEO.id2", factor)
    result =  merge(result, nor_result, all="T", by = "GEO.id2")
  }
  result = as.matrix(apply(result[,2:ncol(result)], 1, sum))
  colnames(result) = factor
  all_result = cbind(all_result, result)
}

colnames(all_result)[1] = "FIPS_Combined"


#get ratio of drug for every counties
rawdata=read.xlsx("MCM_NFLIS_Data.xlsx", sheet="Data")
rawdata = rawdata[,c("YYYY", "COUNTY", "FIPS_Combined", "DrugReports", "TotalDrugReportsCounty")]

calculateTotalDrugs = function(x, rawdata)
{
  TotalDrugReportsCounty = sum(unique(subset(rawdata, FIPS_Combined == x)[,"TotalDrugReportsCounty"]))
  DrugReports = sum(subset(rawdata, FIPS_Combined == x)[,"DrugReports"])
  return(DrugReports/TotalDrugReportsCounty)
}
drug_ratio = cbind(as.matrix(countyID), apply(as.matrix(countyID), 1, calculateTotalDrugs, rawdata))
colnames(drug_ratio) = c("FIPS_Combined", "Drug Ratio")

test = as.matrix(merge(drug_ratio, all_result, all = "T", by = "FIPS_Combined"))
library(Hmisc)
t = rcorr(test[,2:ncol(test)])$r
corrleation = as.matrix(sort(t[,"Drug Ratio"]))
corrleation_name = as.matrix(apply(as.matrix(rownames(corrleation)), 1, getFeatureName, metadata))

corrleation = cbind(corrleation_name, corrleation)

getFeatureName = function(x, metadata)
{
  return (subset(metadata, GEO.id == x)[,2][1])
}






