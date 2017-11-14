#######################
##
#
# source:http://www.dunnhumby.com/sourcefiles
# 網站有一個Breakfast at the Frat資料集
install.packages("readxl")
library(readxl)
library(plotly)
wbname <- 'C:/Users/user/Desktop/R/Breakfast.xlsx'

# db Store Lookup
storeData <- read_excel(wbname, skip = 1, sheet = 2)

# db Products Lookup
productData <- read_excel(wbname, skip = 1, sheet = 3)

# db Transactions Lookup
transData <- read_excel(wbname, skip = 1, sheet = 4)

#######################
#df <- read_excel('dunnhumby - Breakfast at the Frat.xlsx', skip = 1, sheet = 4)
df <- transData
str(df)

df <- df[,c('STORE_NUM', 'HHS', 'SPEND', 'WEEK_END_DATE')]

summary(df) # 157371 NA's
df <- na.omit(df)
summary(df)

uid <- unique(df$STORE_NUM)

#挑選其中一家來觀察
df <- df[df$STORE_NUM == 623, 2:4]

df$WEEK_END_DATE <- as.Date(df$WEEK_END_DATE)
df <- df[order(df[,3], decreasing = F),]
df <- df[order(df$WEEK_END_DATE, decreasing = F),]

newdf <- aggregate(df$WEEK_END_DATE, list(df$HHS), max)
names(newdf) <- c('HHS', 'WEEK_END_DATE')
#Recncy計算
Recency <- as.numeric(difftime(max(df$WEEK_END_DATE), newdf$WEEK_END_DATE, units="days"))

newdf <-cbind(newdf,Recency)
newdf <- newdf[order(newdf$HHS),]
#Frequency計算
fre <- as.data.frame(table(df$HHS))
Frequency <- fre[,2]
newdf <- cbind(newdf,Frequency)
#Monetary總數計算
Monetary <- aggregate(df$SPEND, list(df$HHS), sum)$x

newdf <- cbind(newdf,Monetary)

library(rgl)
plot3d(newdf$Frequency, newdf$Monetary, newdf$Recency)
p <- plot_ly(newdf, x = ~Frequency, y = ~Monetary, z = ~Recency) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Frequency'),
                      yaxis = list(title = 'Monetary'),
                      zaxis = list(title = 'Recency')))
p


#分群
dat <- apply(newdf[, 3:5], 2, scale)
rownames(dat) <- newdf$HHS
kmeans<-kmeans(dat, centers = 5)
plot(dat,col=kmeans$cluster)
cluster<-as.factor(kmeans$cluster)
newdata<-data.frame(dat,cluster)

newdata%>%
plot_ly( x = ~Frequency, y = ~Recency,color=~cluster,mode="markers")

