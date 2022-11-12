library(data.table)

DT<-fread('./project/volume/data/raw/Stat_380_train.csv')
Pr<-fread('./project/volume/data/raw/Stat_380_test.csv')

DT[is.na(DT$LotFrontage)]$LotFrontage<-median(DT$LotFrontage)

fwrite(DT,'./project/volume/data/interim/train.csv')
fwrite(Pr,'./project/volume/data/interim/test.csv')

