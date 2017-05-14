devtools::install_github("hoxo-m/easyRFM")
library(easyRFM)
df <- rfm_generate_data(10000, begin="2014-10-01", end="2016-01-01", seed=42536)
head(df, 3)
str(df)
df$date <- as.Date(df$date)
#1.計算最近一次消費(Recency)
#將日期排序由大到小
df <- df[order(df[,'date'],decreasing = T),]
#將重複id值除去留著離日期最近的ID
newdf <- df[!duplicated(df$id),]
Recency<-as.numeric(difftime(df$date[1],newdf$date,units="days"))
newdf <-cbind(newdf,Recency)
newdf <- newdf[order(newdf$id),]
#2.計算消費頻率
fre <- as.data.frame(table(df$id))
Frequency <- fre[,2]
newdf <- cbind(newdf,Frequency)
#3.計算消費金額
Monetary <- aggregate(df$payment, list(df$id), sum)$x
##Monetary <- m/Frequency
newdf <- cbind(newdf,Monetary)

head(newdf)
