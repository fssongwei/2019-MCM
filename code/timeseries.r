library(vars)

#the most important features:

features = as.matrix(
          cbind(
            # DISABILITY STATUS OF THE CIVILIAN NONINSTITUTIONALIZED POPULATION 
            "HC03_VC115",
            #GRANDPARENTS
            "HC03_VC63", "HC03_VC67", "HC03_VC62",
            #MARITAL STATUS
            "HC03_VC48", "HC03_VC40", "HC03_VC47", "HC03_VC41",
            #PLACE OF BIRTH
            "HC03_VC131"))
                 
substance = "Hydrocodone"
state = "VA"

data = read.csv(file = "data.csv")

getStateData = function(data_matrix, state)
{
  rawdata=subset(read.xlsx("MCM_NFLIS_Data.xlsx", sheet="Data"), State == state)[,c("State", "FIPS_Combined")]
  countyID = unique(rawdata["FIPS_Combined"])
  t1 = apply(as.matrix(Alldata[,"GEO.id2"]), 1, as.numeric)
  t2 = apply(as.matrix(countyID), 1, as.numeric)
  data = Alldata[which(t1 %in% t2),]
  return(data)
}

data = getStateData(data, "KY")

getFeature = function(x, data)
{
  years = as.matrix(c("2010", "2011", "2012", "2013", "2014", "2015", "2016"))
  
  getData = function(year,data)
  {
    tmp_data = subset(data, Year == year)
    tmp_data = sum(tmp_data[,x])
    return(tmp_data)
  }
  return(result = apply(years, 1, getData, data))
}

feature_data = apply(features, 2, getFeature, data)

gm11<-function(x,k)
{
  n<-length(x)
  x1<-numeric(n);
  for(i in 1:n)
  {
    x1[i]<-sum(x[1:i]);
  }
  z1<-numeric(n)
  m<-n-1
  for(j in 1:m)
  {
    z1[j+1]<-(0.5*x1[j+1]+0.5*x1[j])
  }
  Yn=t(t(x[2:n]))
  B<-matrix(1,nrow=n-1,ncol=2)
  B[,1]<-t(t(-z1[2:n]))
  u<-solve(t(B)%*%B)%*%t(B)%*%Yn;
  a<-u[1];
  b<-u[2];
  x2<-numeric(k);
  x2[1]<-x[1];
  for(i in 1:k-1)
  {
    x2[1+i]=(x[1]-b/a)*exp(-a*i)+b/a;
  }
  x2=c(0,x2);
  y=diff(x2);
  y
}

#feature_data = rbind(feature_data, (apply(feature_data, 2, gm11, 1))
#colnames(feature_data) = features
#rownames(feature_data) = c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017")
#write.csv(feature_data, "feature_data.csv")

feature_data = read.csv("feature_data.csv")
feature_data = apply(feature_data, 2, as.numeric)[,2:ncol(feature_data)]

normalization = function(x)
{
  q = 0.99
  return (c(x[1] * q, x[2] * q^2, x[3] * q^3, x[4] * q^4, x[5] * q^5, x[6] * q^6, x[7] * q^7, x[8] * q^8))
}
feature_data = apply(feature_data, 2, normalization)



rawdata=read.xlsx("MCM_NFLIS_Data.xlsx", sheet="Data")

countDrug = function(x, data)
{
  return (c(x, sum(subset(data, SubstanceName == x)[,"DrugReports"])))
}

substance = "Fentanyl"
#data = subset(rawdata, State=="PA")
data = rawdata
year = as.matrix(c("2010","2011","2012","2013","2014","2015","2016","2017"))
getNofDrug = function(x, data, substance)
{
  data = subset(data, YYYY == x)
  data = subset(data, SubstanceName == substance)
  total_count = 100 * sum(data[,"DrugReports"]) / sum(data[,"TotalDrugReportsCounty"])
  #total_count = sum(data[,"DrugReports"])
  return(total_count)
}
count = apply(year, 1, getNofDrug, data, substance)
count = as.numeric(count)
count = as.matrix(count)

for (i in 1:ncol(feature_data)) {
  x = grangertest(count, feature_data[,i])
  print(x$`Pr(>F)`)
}

#library(tseries)
#data_rate_ts = ts(cbind(count, feature_data[,2:ncol(feature_data)]))
#var = VAR(cbind(count, feature_data[,2:ncol(feature_data)]), p=1, type="const")

#var.p=predict(var,n.ahead=10,ci=0.95)

combind = cbind(count, feature_data)
colnames(combind)[1] = "drug ratio"

#VARselect(uschange[,1:2], lag.max=8, type="const")[["selection"]]

#VAR(combind[,1:5])
var.predict<-predict(VAR(combind[,c(1, 2)]),n.ahead=10,ci=0.2)
test =var.predict$fcst$drug.ratio

