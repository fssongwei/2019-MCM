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


countDrug = function(x, data)
{
  return (c(x, sum(subset(data, SubstanceName == x)[,"DrugReports"])))
}

state_name = as.matrix(c("all","VA", "PA", "OH", "WV", "KY"))

get_Data = function(x)
{
  substance = "Oxycodone"
  if(x == "all")
  {
    data = rawdata
  }
  else
  {
    data = subset(rawdata, State==x)
  }
year = as.matrix(c("2010","2011","2012","2013","2014","2015","2016","2017"))
getNofDrug = function(x, data, substance)
{
  data = subset(data, YYYY == x)
  data = subset(data, SubstanceName == substance)
  #total_count = 100 * sum(data[,"DrugReports"]) / sum(data[,"TotalDrugReportsCounty"])
  total_count = sum(data[,"DrugReports"])
  return(total_count)
}
count = apply(year, 1, getNofDrug, data, substance)


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
result = as.matrix(gm11(count,18))
return(result)
}

result = apply(state_name, 1, get_Data)
