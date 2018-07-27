dataset = read.csv("data-marketing-budget-12mo.csv", header=T,
                   colClasses = c("numeric", "numeric","numeric"))

dataset2 = read.csv("outputdemand5.csv")
a=ncol(dataset)
a
#
j=c(0)
if (a==3){
l=(dataset2[,2])
o=(dataset2[,3])
# k=(dataset2$weather)
# u=(dataset2$temperature)
p=(dataset2[,1])
length(l)
l
dataset

simple.fit = lm(dataset[,1]~dataset[,2]+dataset[,3],dataset)
#forecast(simple.fit)
#summary(simple.fit)

#a=fitted(simple.fit)
#a


#plot(dataset$Demand, xlab="Months")
#plot(a)
for (i in 1:length(l))
  p[i]=coefficients(simple.fit)[1]+coefficients(simple.fit)[2]*l[i]+coefficients(simple.fit)[3]*o[i]
  
bestfit=simple.fit
}
if (a==4){
  l=(dataset2[,2])
  o=(dataset2[,3])
  k=(dataset2[,4])
  # u=(dataset2$temperature)
  p=(dataset2[,1])
  length(l)
  l
  dataset
  
  simple.fit = lm(dataset[,1]~dataset[,2]+dataset[,3]+dataset[,4],dataset)
  #forecast(simple.fit)
  #summary(simple.fit)
  
  #a=fitted(simple.fit)
  #a
  
  
  #plot(dataset$Demand, xlab="Months")
  #plot(a)
  for (i in 1:length(l))
    p[i]=coefficients(simple.fit)[1]+coefficients(simple.fit)[2]*l[i]+coefficients(simple.fit)[3]*o[i]+coefficients(simple.fit)[4]*k[i]
  
  bestfit=simple.fit
}
if (a==5){
  l=(dataset2[,2])
  o=(dataset2[,3])
  k=(dataset2[,4])
  u=(dataset2[,5])
  p=(dataset2[,1])
  length(l)
  l
  dataset
  
  simple.fit = lm(dataset[,1]~dataset[,2]+dataset[,3]+dataset[,4]+dataset[,5],dataset)
  #forecast(simple.fit)
  #summary(simple.fit)
  
  #a=fitted(simple.fit)
  #a
  
  
  #plot(dataset$Demand, xlab="Months")
  #plot(a)
  for (i in 1:length(l))
    p[i]=coefficients(simple.fit)[1]+coefficients(simple.fit)[2]*l[i]+coefficients(simple.fit)[3]*o[i]+coefficients(simple.fit)[4]*k[i]+coefficients(simple.fit)[5]*u[i]
  
  bestfit=simple.fit
}
if (a==6){
  l=(dataset2[,2])
  o=(dataset2[,3])
  k=(dataset2[,4])
  u=(dataset2[,5])
  t=(dataset2[,6])
  p=(dataset2[,1])
  length(l)
  l
  dataset
  
  simple.fit = lm(dataset[,1]~dataset[,2]+dataset[,3]+dataset[,4]+dataset[,5]+dataset[,6],dataset)
  #forecast(simple.fit)
  #summary(simple.fit)
  
  #a=fitted(simple.fit)
  #a
  
  
  #plot(dataset$Demand, xlab="Months")
  #plot(a)
  for (i in 1:length(l))
    p[i]=coefficients(simple.fit)[1]+coefficients(simple.fit)[2]*l[i]+coefficients(simple.fit)[3]*o[i]+coefficients(simple.fit)[4]*k[i]+coefficients(simple.fit)[5]*u[i]+coefficients(simple.fit)[6]*t[i]
  
  bestfit=simple.fit
}
if (a==7){
  l=(dataset2[,2])
  o=(dataset2[,3])
  k=(dataset2[,4])
  u=(dataset2[,5])
  t=(dataset2[,6])
  b=(dataset2[,7])
  p=(dataset2[,1])
  length(l)
  l
  dataset
  
  simple.fit = lm(dataset[,1]~dataset[,2]+dataset[,3]+dataset[,4]+dataset[,5]+dataset[,6]+dataset[,7],dataset)
  #forecast(simple.fit)
  #summary(simple.fit)
  
  #a=fitted(simple.fit)
  #a
  
  
  #plot(dataset$Demand, xlab="Months")
  #plot(a)
  for (i in 1:length(l))
    p[i]=coefficients(simple.fit)[1]+coefficients(simple.fit)[2]*l[i]+coefficients(simple.fit)[3]*o[i]+coefficients(simple.fit)[4]*k[i]+coefficients(simple.fit)[5]*u[i]+coefficients(simple.fit)[6]*t[i]+coefficients(simple.fit)[7]*b[i]
  
  bestfit=simple.fit
}
if (a==8){
  l=(dataset2[,2])
  o=(dataset2[,3])
  k=(dataset2[,4])
  u=(dataset2[,5])
  t=(dataset2[,6])
  b=(dataset2[,7])
  c=(dataset2[,8])
  p=(dataset2[,1])
  length(l)
  l
  dataset
  
  simple.fit = lm(dataset[,1]~dataset[,2]+dataset[,3]+dataset[,4]+dataset[,5]+dataset[,6]+dataset[,7]+dataset[,8],dataset)
  #forecast(simple.fit)
  #summary(simple.fit)
  
  #a=fitted(simple.fit)
  #a
  
  
  #plot(dataset$Demand, xlab="Months")
  #plot(a)
  for (i in 1:length(l))
    p[i]=coefficients(simple.fit)[1]+coefficients(simple.fit)[2]*l[i]+coefficients(simple.fit)[3]*o[i]+coefficients(simple.fit)[4]*k[i]+coefficients(simple.fit)[5]*u[i]+coefficients(simple.fit)[6]*t[i]+coefficients(simple.fit)[7]*b[i]+coefficients(simple.fit)[8]*c[i]
  
  bestfit=simple.fit
}
if (a==9){
  l=(dataset2[,2])
  o=(dataset2[,3])
  k=(dataset2[,4])
  u=(dataset2[,5])
  t=(dataset2[,6])
  b=(dataset2[,7])
  c=(dataset2[,8])
  e=(dataset2[,9])
  p=(dataset2[,1])
  length(l)
  l
  dataset
  
  simple.fit = lm(dataset[,1]~dataset[,2]+dataset[,3]+dataset[,4]+dataset[,5]+dataset[,6]+dataset[,7]+dataset[,8]+dataset[,9],dataset)
  #forecast(simple.fit)
  #summary(simple.fit)
  
  #a=fitted(simple.fit)
  #a
  
  
  #plot(dataset$Demand, xlab="Months")
  #plot(a)
  for (i in 1:length(l))
    p[i]=coefficients(simple.fit)[1]+coefficients(simple.fit)[2]*l[i]+coefficients(simple.fit)[3]*o[i]+coefficients(simple.fit)[4]*k[i]+coefficients(simple.fit)[5]*u[i]+coefficients(simple.fit)[6]*t[i]+coefficients(simple.fit)[7]*b[i]+coefficients(simple.fit)[8]*c[i]+coefficients(simple.fit)[9]*e[i]
  
  bestfit=simple.fit
}
if (a==10){
  l=(dataset2[,2])
  o=(dataset2[,3])
  k=(dataset2[,4])
  u=(dataset2[,5])
  t=(dataset2[,6])
  b=(dataset2[,7])
  c=(dataset2[,8])
  e=(dataset2[,9])
  r=(dataset2[,10])
  p=(dataset2[,1])
  length(l)
  l
  dataset
  
  simple.fit = lm(dataset[,1]~dataset[,2]+dataset[,3]+dataset[,4]+dataset[,5]+dataset[,6]+dataset[,7]+dataset[,8]+dataset[,9]+dataset[,10],dataset)
  #forecast(simple.fit)
  #summary(simple.fit)
  
  #a=fitted(simple.fit)
  #a
  
  
  #plot(dataset$Demand, xlab="Months")
  #plot(a)
  for (i in 1:length(l))
    p[i]=coefficients(simple.fit)[1]+coefficients(simple.fit)[2]*l[i]+coefficients(simple.fit)[3]*o[i]+coefficients(simple.fit)[4]*k[i]+coefficients(simple.fit)[5]*u[i]+coefficients(simple.fit)[6]*t[i]+coefficients(simple.fit)[7]*b[i]+coefficients(simple.fit)[8]*c[i]+coefficients(simple.fit)[9]*e[i]+coefficients(simple.fit)[10]*r[i]
  
  bestfit=simple.fit
}
if (a==11){
  l=(dataset2[,2])
  o=(dataset2[,3])
  k=(dataset2[,4])
  u=(dataset2[,5])
  t=(dataset2[,6])
  b=(dataset2[,7])
  c=(dataset2[,8])
  e=(dataset2[,9])
  r=(dataset2[,10])
  g=(dataset2[,11])
  p=(dataset2[,1])
  length(l)
  l
  dataset
  
  simple.fit = lm(dataset[,1]~dataset[,2]+dataset[,3]+dataset[,4]+dataset[,5]+dataset[,6]+dataset[,7]+dataset[,8]+dataset[,9]+dataset[,10]+dataset[,11],dataset)
  #forecast(simple.fit)
  #summary(simple.fit)
  
  #a=fitted(simple.fit)
  #a
  
  
  #plot(dataset$Demand, xlab="Months")
  #plot(a)
  for (i in 1:length(l))
    p[i]=coefficients(simple.fit)[1]+coefficients(simple.fit)[2]*l[i]+coefficients(simple.fit)[3]*o[i]+coefficients(simple.fit)[4]*k[i]+coefficients(simple.fit)[5]*u[i]+coefficients(simple.fit)[6]*t[i]+coefficients(simple.fit)[7]*b[i]+coefficients(simple.fit)[8]*c[i]+coefficients(simple.fit)[9]*e[i]+coefficients(simple.fit)[10]*r[i]+coefficients(simple.fit)[11]*g[i]
  
  bestfit=simple.fit
}
summary(bestfit)
fitted(bestfit)
plot(fitted(bestfit))
for (i in 1:length(p))
  print(p[i])
