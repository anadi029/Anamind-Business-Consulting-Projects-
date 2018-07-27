data= read.csv("analogyinput.csv", na.strings = ".",header = TRUE) #taking input
a=data$launch.date[1]
t=data$Total.Forecasting.Months[1]
a
c=0
j=0
c1=0
c2=0
c3=0
c4=0
mm=c()
mn=c()
month<- strftime(a,"%m")
month
month=as.numeric(month)
data1=data.frame()
startDate <- ymd(a)
myDates <- startDate %m+% months(c(0:(t-1)))
data1=data.frame()
for (i in 1:t)
  data1[i,'Months']=myDates[i]

for (i in 1:t)
  data1[i,'normal forecast']=data[i,'Forecast']
data1

for(i in 1:t)
  c=c+data1[i,'normal forecast']
data1[t+3,'normal forecast']=c
data1
data1$b1 <- ""
for (i in 1:t)
  data1[i,'Months1']=myDates[i]
b=data$New.Launch.Date[1]
month1<- strftime(b,"%m")
month1=as.numeric(month1)
z=month1-month
z
for (i in 1:z)
  data1[i,'launch date forecasting']=0
for (i in 1:(t-z))
  data1[i+z,'launch date forecasting']=data[i,'Forecast']
#data1[16,'launch date forecasting']=c
for (i in 1:t)
  c1=c1+data1[i,'launch date forecasting']
data1[t+3,'launch date forecasting']=c1
data1$b2 <- ""
for (i in 1:t)
  data1[i,'Months2']=myDates[i]
x=data$launch.horizon[1]
x
y=0
y1=0
y2=0
for(i in 1:x)
  y=y+data[i,'Forecast']
y
for (i in 1:x)
  data1[i,'launch horizon forecasting']=(data[i,'Forecast']/y)*c
data1
for (i in (x+1):t)
  data1[i,'launch horizon forecasting']=0
#data1[16,'launch horizon forecasting']=c
for (i in 1:t)
  c2=c2+data1[i,'launch horizon forecasting']
data1[t+3,'launch horizon forecasting']=c2
data1$b3 <- ""
for (i in 1:t)
  data1[i,'Months3']=myDates[i]
for (i in 1:z)
  data1[i,'launch date horizon forecasting']=0

if (x<(t-z)){
  for (i in 1:x)
    data1[z+i,'launch date horizon forecasting']=(data[i,'Forecast']/y)*c
  for (i in (z+x+1):t)
    data1[i,'launch date horizon forecasting']=0
}else{
  for (i in 1:x)
    data1[z+i,'launch date horizon forecasting']=(data[i,'Forecast']/y)*c
}
#data1[16,'launch date horizon forecasting']=c
for (i in 1:t)
  c3=c3+data1[i,'launch date horizon forecasting']
data1[t+3,'launch date horizon forecasting']=c3
data1$b4 <- ""
for (i in 1:t)
  data1[i,'Month4']=myDates[i]
p=length(data$Months)
for (i in 1:p)
  mm[i]<- strftime(data$Months[i],"%m")
for (i in 1:p)
  mn[i]=as.numeric(mm[i])
mn
month
#for (i in 1:p)
for (i in 1:length(mn)){
  if(mn[i]==month)break
}
j=i
j
for (i in 1:t)
  y1=y1+data[i,'Forecast']
for (i in 1:t)
  y2=y2+data[j+i-1,'Forecast']
y1
y2
data1
for (i in 1:t)
  data1[i,'seasonal forecasting']=(y1/y2)*data[j+i-1,'Forecast']
for (i in 1:t)
  c4=c4+data1[i,'seasonal forecasting']
data1[t+3,'seasonal forecasting']=c4
data1
data1 <- sapply(data1, as.character)
data1[is.na(data1)] <- " "
data1=as.data.frame(data1)
data1
#write.csv(data1, file = "Analogy.csv")
