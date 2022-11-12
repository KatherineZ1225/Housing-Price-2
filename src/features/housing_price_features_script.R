library(data.table)
library(ggplot2)
train<-fread('./project/volume/data/raw/Stat_380_train2021.csv')
test <- fread('./project/volume/data/raw/Stat_380_test2021.csv')
test$SalePrice <- 0

train$train <- 1
test$train <- 0

master <- rbind(train, test)
master$newId <- 1:15000

# feature 1: BldgType
unique(train$BldgType)
type <- dcast(master,newId ~ BldgType, length)

# feature 2: Heating
unique(train$Heating)
heat <- dcast(master,newId ~ Heating, length)

# feature 3: CentralAir
unique(train$CentralAir)
cen <- dcast(master,newId ~ CentralAir, length)

# merging
master<- merge(master, type, all.x=T, by = "newId")
master<- merge(master, heat, all.x=T, by = "newId")
master<- merge(master, cen, all.x=T, by = "newId")
master$BldgType<-NULL
master$Heating<-NULL
master$CentralAir<-NULL

# Split back to train and test --------------------------------------------
train <- master[train==1]
test <- master[train==0]

### clean up columns
train$train <- NULL
test$train <- NULL
test$SalePrice <- NULL


fwrite(train,'./project/volume/data/interim/train.csv')
fwrite(test,'./project/volume/data/interim/test.csv')
