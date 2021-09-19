Sys.getlocale()
library(xlsx)
df <- read.table("week_20210216_0912.csv",sep=",", encoding = "UTF-8",head=T)

ggplot(data = df) + geom_bar(mapping = aes(x = total_revenue))

#Plots & See the correlations
#we use geom_point() to see the distribution of scatterplot and use geom_smooth to make it linear
ggplot(data=df) + geom_point(mapping = aes(x = Sum, y = total_revenue))
gplot(data=df) + geom_point(mapping = aes(x = Sum, y = total_revenue))

ggplot(data = df) + 
    geom_smooth(mapping = aes(x = Sum, y = total_revenue))

#we can use cor to check the correlation between two variables.
