dat1 = read.csv("storeclustering2.csv", na.strings = ".",header = TRUE) #taking input
#dat1[1,6]
x=ncol(dat1)      #counting the no of columns in the dataframe
x
v=c()
set.seed(20)          #setting a fixed seed 
for (i in col(dat1))  #finding the columns that are numeric in nature in order to remove the outliers
  if(sapply(dat1[1,i], is.numeric)){
    e = dat1[,i]
    train_ts <- ts(e, frequency=12)
    y <- ts(e, frequency=12)
    remove_outliers <- function(x, na.rm = TRUE, ...) {
      qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
      H <- 1.5 * IQR(x, na.rm = na.rm)
      y <- x
      y[x < (qnt[1] - H)] <- NA
      y[x > (qnt[2] + H)] <- NA
      y
    }
    y <- remove_outliers(y)     
    dat1[,i]=y                     #all outliers removed
    
  }
dat1                               #outliers free dataframe                         

dat1<- sapply(dat1, as.character)    #Removing the "NA" values as these are not computed bu function
dat1[is.na(dat1)] <- " "

dat1=as.data.frame(dat1)
dat1                                 #putting blank values in place of "NA" 


for (i in 3:x)   #we will start from a particular column (column from where we have to give it in the function,it wpuld be defined from which index we have to input columns)
  dat1[,i]<- scale(as.numeric(dat1[,i]))   
dat1             #scaled dataset (scaling only for the columns that are to be passed in the function)

nb <- NbClust(dat1[,3:x], distance = "euclidean", min.nc = 2, 
              max.nc = 12, method = "kmeans")     #it calculates the optimal no of clusters
nb                                                #it gives the require plot as well as the most optimal clusters arrangemnts                                             
a=nb$Best.nc
a
# nb$Best.partition
# wss <- function(d) 
#   sum(scale(d, scale = FALSE)^2)
# }
# res <- sapply(seq.int(1, nrow(dat1)), wrap, h = nb, x = dat1)
# plot(seq_along(res), res, type = "b", pch = 19)
# 
# wss <- (nrow(dat1[,4])-1)*sum(apply(dat1[,4:8],2,var))
# for (i in 2:24) wss[i] <- sum(kmeans(data1[,4:8], 
#                                      centers=i)$withinss)
# plot(1:24, wss, type="b", xlab="Number of Clusters",
#      ylab="Within groups sum of squares")


print("input  number of clusters")
a=dat1$InputCluster[1]
a

irisCluster <- kmeans(dat1[,4:8], 4, nstart = 20)  #inputting the desired no of clusters from clients through a csv file
irisCluster 
#plot(irisCluster)
# irisCluster$centers
# for(i in 1:length(v))
#   print(v[i])