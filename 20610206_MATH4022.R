#------------ Question 1 --------------
#Reading the cet_temp.csv dataset 
cet_df<-read.csv("cet_temp.csv",header=TRUE)

#Print the dataset 
print(cet_df)

#Setting the annual mean temperature as Time Series data
cet_temp<- ts(cet_df$avg_annual_temp_C,start=1900,frequency=1)

