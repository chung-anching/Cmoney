Sys.getlocale()
library(xlsx)
#匯入資料(head=T: 把第一列作為行名)
df <- read.table("week_20210216_0912.csv",sep=",", encoding = "UTF-8",head=T)
df_day <- read.table("day_20210216_0912.csv",sep=",", encoding = "UTF-8",head=T)
df_day2 <- read.table("day2_20210216_0912.csv",sep=",", encoding = "UTF-8",head=T)
df_day2 <- read.csv("day2_20210216_0912.csv",sep=",", encoding = "UTF-8",head=T)

head(df)
summary(df)
ggplot(data = df) + geom_bar(mapping = aes(x = total_revenue))

#Plots & See the correlations
#use geom_point() to see the distribution of scatterplot and use geom_smooth to make it linear
ggplot(data=df) + geom_point(mapping = aes(x = Sum, y = total_revenue))
gplot(data=df) + geom_point(mapping = aes(x = Sum, y = total_revenue))

ggplot(data = df) + 
    geom_smooth(mapping = aes(x = Sum, y = total_revenue))


#use cor to check the correlation
cor(df$Sum, df$total_revenue, use="complete.obs")
cor(df_day2$Sum, df_day2$fb_touch, use="complete.obs")
cor(df_day2$Sum, df_day2$live_stream, use="complete.obs")
cor(df_day2$Sum, df_day2$fb_like, use="complete.obs")
cor(df_day2$Sum, df_day2$EDM_successfully_send, use="complete.obs")


#Linear square regression
y = (slope) x + (intercept)
lm(y~x)

regressor<- lm()
# 建立迴歸模型
df.lm <- lm(Sum ~ total_revenue + lineat_newuser, data = df)
summary(df.lm)
# Sum = 0.0008916 * total_revenue - 13.39 * lineat_newuser + 9387

df_day2.lm <- lm(Sum ~ live_stream + fb_likes + EDM_successfully_send, data = df_day2)
summary(df_day2.lm)
# Sum = 0.07837 * live_stream + 0.28650 * fb_likes - 0.01654 * EDM_successfully_send + 2702.9

# 畫出模型診斷用的圖
library(ggfortify)
autoplot(df.lm)

# 殘差常態性檢定
shapiro.test(df.lm$residual)

# 殘差獨立性檢定
require(car)
durbinWatsonTest(df.lm)

# 殘差變異數同質性檢定
require(car)
ncvTest(df.lm)

#對wt列用均值進行插補，na.rm=TRUE表示忽略缺失值，該引數必須寫上
df_day2$live_stream[is.na(df_day2$live_stream)]=mean(df_day2$live_stream,na.rm = TRUE)
df_day2$fb_touch[is.na(df_day2$fb_touch)]=mean(df_day2$fb_touch,na.rm = TRUE)
df_day2$fb_likes[is.na(df_day2$fb_likes)]=mean(df_day2$fb_likes,na.rm = TRUE)
df_day2$EDM_open[is.na(df_day2$EDM_open)]=mean(df_day2$EDM_open,na.rm = TRUE)
head(df_day2)

#先把資料轉成數值型態
as.numeric(df_day2$Date)
as.numeric(df_day2$Android)
as.numeric(df_day2$iOS)
df_day2$Sum = as.numeric(df_day2$Sum)
df_day2$live_stream = as.numeric(df_day2$live_stream)
df_day2$fb_touch = as.numeric(df_day2$fb_touch)
df_day2$fb_likes = as.numeric(df_day2$fb_likes)
df_day2$fb_comments = as.numeric(df_day2$fb_comments)
df_day2$EDM_open = as.numeric(df_day2$EDM_open)
df_day2$EDM_EDM_successfully_send = as.numeric(df_day2$EDM_EDM_successfully_send)
#檢查屬性是否改變
class(df_day2$fb_likes)

#第二種方法
df_day2["fb_likes"]<-as.numeric(df_day2["fb_likes"])

#這裡我們選用三列numeric資料進行測試
library(caret)
data_cor=df_day2[,names(df_day2)%in%c("fb_touch","fb_likes","fb_comments","live_stream","EDM_open","Sum")]
head(data_cor)

#確認是否為數字
is.numeric(df_day2$live_stream)
is.numeric(df_day2$fb_touch)
is.numeric(df_day2$Sum)

#假如要刪掉第151列(沒資料)，就直接將除了第二列以外的數據直接指定給points
data_cor <- data_cor[-151,]
data_cor <- data_cor[-151,]
data_cor <- data_cor[-151,]   #data_cor <- data_cor[-153,]會移除到更後面的欄位
data_cor <- data_cor[-214,]
df_day2 <- df_day2[-151,]
df_day2 <- df_day2[-151,]
df_day2 <- df_day2[-151,]
data_cor <- data_cor[-211,]
df_day2[,"fb_likes"]

#不要第一欄的日期(無法算cor)
df_day2[,2:7] 
cor=cor(df_day2[,2:10] ) 
#先計算data的相關係數
cor=cor(data_cor)  
cor(data_cor,data_cor, use="complete.obs")
cor
