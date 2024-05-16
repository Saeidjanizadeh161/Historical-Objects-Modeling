####Working directory###############
setwd("D:\\castel\\50m\\layers")
####required packages##########
library(raster)
library(xlsx)
install.packages("rgdal",dependences=T)
library(rgdal)
####Importing raster data#######
DEM<-raster("DEM.tif")
River<-raster("disriver.tif")
Settelment<-raster("dissettelment.tif")
Lithology<-raster("lithology.tif")
Repges<-raster("repges.tif")
####Stacking raster files####
Historical.data<-stack(DEM,River,Settelment,Lithology,Repges)
####Importing the locations as points##########
Castlep<-shapefile("pointcastel.shp")
####Converting points data to data frame#####
Castlep1<-as.data.frame(Castlep)
write.csv(Castlep1,"pointcas.csv")
####Extracting values based on raster files and locations of the Castels#####
variables.castp<-extract(Historical.data, Castlep)
write.csv(variables.castp, "historicalvars.csv")

######Converting raster files to data frame#######
Data2<-as.data.frame(Historical.data, xy=T)
dim(Data3)
Data3<-na.omit(Data2)
names(Data3)
dataall<-Data3[,c(-1,-2)]
xy<-Data3[,1:2]
xy
write.csv(xy, "XY.csv")
write.csv(dataall, "dataall.csv")


#####Importing prepared data################
datah<- read.csv("D:\\castel\\50m\\layers/historicalvars.csv", 1)

head(datah)

datah1<-datah[,-6]

####Multicolinearity analysis#########
library(usdm)
vi<-vifstep(datah1, th=10)
vi

##### Splitimg data########
dataf<-sort(sample(nrow(datah), nrow(datah)*.7))
train<-datah[dataf,]
test<-datah[-dataf,]
trainX<-train[,-6]
trainY<-train[,6]
testX<-test[,-6]
testY<-test[,6]

######RF model###########
library(caret)

library(randomForest)



RF <- caret::train(
  Castel~., 
  data = train,
  method = "rf")


varImp(RF)
testRF<-predict(RF,testX)
trainRF<-predict(RF,trainX)



allRF<-predict(RF,dataall)
write.csv(allKNN, "RFall.csv")

RFpred<- read.csv("D:\\castel\\50m\\layers\\RFall.csv", 1)
names(RFpred)[names(RFpred) == "x"] <- "RF"

XY3<- read.csv("D:\\castel\\50m\\layers\\XY3.csv", 1)
head(XY3)

RFmerge<-merge(RFpred, XY3,by="X")
head(RFmerge)


DEM
coordinates(RFmerge) <- ~x+y
rast <- raster(ncol = 8519, nrow = 4043)
extent(rast) <- extent(DEM)
RFst<-rasterize(RFmerge, rast, RFmerge$RF, fun = mean)
plot(RFst)
writeRaster(RFst, file="RF.asc", format="ascii")


############KNN model########################
library(kknn)

KNN <- caret::train(
  Castel~., 
  data = train,
  method = "kknn")


varImp(KNN)
testKNN<-predict(KNN,testX)
trainKNN<-predict(KNN,trainX)


library(pROC)
mroc<-roc(testY, testKNN, plot=T)
coords(mroc, .5, "threshold", ret=c("sensitivity","specificity","ppv","npv"))




library(PRROC)
PRROC_obj <- roc.curve(scores.class0 = testKNN, weights.class0=testY,
                       curve=TRUE)
plot(PRROC_obj)

memory.size() ### Checking your memory size
memory.limit() ## Checking the set limit
memory.limit(size=56000) ### expanding your memory _ here it goes beyond to your actually memory. This 56000 is proposed for 64Bit.


allKNN<-predict(KNN,dataall)
write.csv(allKNN, "KNNall.csv")

KNNpred<- read.csv("D:\\castel\\50m\\layers\\KNNall.csv", 1)
head(KNNpred)
dim(KNNpred)
names(KNNpred)[names(KNNpred) == "x"] <- "KNN"

XY3<- read.csv("D:\\castel\\50m\\layers\\XY3.csv", 1)
head(XY3)

KNNmerge<-merge(KNNpred, XY3,by="X")
head(XGBGSmerge)


DEM
coordinates(KNNmerge) <- ~x+y
rast <- raster(ncol = 8519, nrow = 4043)
extent(rast) <- extent(DEM)
KNNst<-rasterize(KNNmerge, rast, KNNmerge$KNN, fun = mean)
NBrast
plot(KNNst)
writeRaster(KNNst, file="KNN.asc", format="ascii")


##########SVM model###############
library(kernlab)

SVM <- caret::train(
  Castel~., 
  data = train,
  method = "svmRadial")


varImp(SVM)
testSVM<-predict(SVM,testX)
trainSVM<-predict(SVM,trainX)




library(pROC)
mroc<-roc(testY, testSVM, plot=T)
coords(mroc, .5, "threshold", ret=c("sensitivity","specificity","ppv","npv"))


library(PRROC)
PRROC_obj <- roc.curve(scores.class0 = testSVM, weights.class0=testY,
                       curve=TRUE)
plot(PRROC_obj)



allSVM<-predict(SVM,dataall)
write.csv(allSVM, "SVMall.csv")

SVMpred<- read.csv("D:\\castel\\50m\\layers\\SVMall.csv", 1)
head(SVMpred)
dim(KNNpred)
names(SVMpred)[names(SVMpred) == "x"] <- "SVM"

XY3<- read.csv("D:\\castel\\50m\\layers\\XY3.csv", 1)
head(XY3)

SVMmerge<-merge(SVMpred, XY3,by="X")
head(XGBGSmerge)


DEM
coordinates(SVMmerge) <- ~x+y
rast <- raster(ncol = 8519, nrow = 4043)
extent(rast) <- extent(DEM)
SVMst<-rasterize(SVMmerge, rast, SVMmerge$SVM, fun = mean)
NBrast
plot(SVMst)
writeRaster(SVMst, file="SVM.asc", format="ascii")


########ROC plot######
library(pROC)
mroc<-roc(testY, testRF, plot=T)
coords(mroc, .5, "threshold", ret=c("sensitivity","specificity","ppv","npv"))


library(PRROC)
PRROC_obj <- roc.curve(scores.class0 = testRF, weights.class0=testY,
                       curve=TRUE)
plot(PRROC_obj)


library(tidyverse)


library(pROC)

my_auc <- roc(testY, testKNN)
sen_spec_df <- data_frame(TPR = my_auc$sensitivities, 
                          FPR = 1 - my_auc$specificities, 
                          total = my_auc$sensitivities + my_auc$specificities, 
                          cutoff = my_auc$thresholds)


sen_spec_df %>% 
  ggplot(aes(x = FPR, ymin = 0, ymax = TPR))+
  geom_polygon(aes(y = TPR), fill = "red", alpha = 0.3) +
  geom_path(aes(y = TPR), col = "firebrick", size = 1.2) +
  geom_abline(intercept = 0, slope = 1, color = "gray37", size = 1, linetype = "dashed") + 
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  theme_bw() +
  coord_equal() +
  labs(x = "FPR (1 - Specificity)", 
       y = "TPR (Sensitivity)", 
       title = "", 
       subtitle = paste0("AUC Value: ", my_auc$auc %>% round(4)))

