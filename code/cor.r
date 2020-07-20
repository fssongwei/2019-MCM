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

data = getStateData(Alldata, "KY")

#processing metadada
metadata = read.csv(file = "metadata.csv")
metadata = metadata[!grepl('Estimate',metadata$Id),]
metadata = metadata[!grepl('Margin',metadata$Id),]
metadata = metadata[!grepl('Geography',metadata$Id),]

countyID = as.numeric(as.vector(unique(data[,"GEO.id2"])))

factors = c("HOUSEHOLDS BY TYPE", "RELATIONSHIP", "MARITAL STATUS", "FERTILITY", "GRANDPARENTS", 
            "SCHOOL ENROLLMENT", "EDUCATIONAL ATTAINMENT", "VETERAN STATUS", "DISABILITY STATUS", "RESIDENCE",
            "PLACE OF BIRTH", "U.S. CITIZENSHIP STATUS", "YEAR OF ENTRY", "WORLD REGION OF BIRTH OF FOREIGN BORN",
            "LANGUAGE SPOKEN AT HOME", "ANCESTRY", "COMPUTERS")

#Step 1 find the categories represent in metadata

all_result = countyID

for (factor in factors) {
  factor_metadata = metadata[grepl(factor,metadata$Id),]
  result = rep(0, times=length(countyID))
  for (year in c(2010,2011,2012,2013,2014,2015,2016)) {
    tmp_data = subset(data, Year == year)
    tmp_data = tmp_data[,c("Year", "GEO.id2", intersect(as.matrix(factor_metadata[,1]), colnames(data)))]
    
    #normalization
    normalization = function(x)
    {
      min = min(x)
      max = max(x)
      if((max-min) == 0) return (x)
      return ((x-min) / (max-min))
    }
    
    tmp_data = cbind( tmp_data[,1:2], apply(tmp_data[,3:ncol(tmp_data)],2,normalization))
    
    #PCA降维
    PCA = function(matrix)
    {
      pca = prcomp(matrix,scale=TRUE)
      eigs = pca$sdev^2
      var_pca=round(cumsum(eigs)/sum(eigs),4)
      this_var <- get_pca_var(pca)
      n=min(which(var_pca>0.85))#n is the dimension that expalained 85% variance
      if(n == 1){
        mc = this_var$cor[,1:n]
      } else
      {
        mc=rowSums(this_var$cor[,1:n]) #this should be the weight for the 3 columns
      }
      return(mc)
    }
    weight = PCA(apply(tmp_data, 2, as.numeric)[,3:ncol(tmp_data)])
    result = cbind(result, as.vector(as.matrix(tmp_data[,3:ncol(tmp_data)]) %*% weight) )
  }
  result = as.matrix(apply(result, 1, sum))
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
