library(tidyverse)
library(corrplot)
library(car)
library(performance)



df = read.csv('../data/djo_fdf.csv')
tail(df)
summary(df)

sdf = df[, !(names(df) %in% c('Odd_Djo', 'Odd_Opp', 'Odd_Pred', 'Odd_Acc'))]






cor(df$Match_Importance, df$Win)
ggplot(df, aes(x = as.factor(Match_Importance), fill = as.factor(Win))) + geom_bar()
ggplot(df, aes(x = as.factor(Match_Importance), fill = as.factor(Win))) + geom_bar(position = 'fill')
ggplot(df, aes(x = Match_Importance, color = as.factor(Win), fill = as.factor(Win))) + geom_density(linewidth = 0.75, alpha = 0.5) 


cor(df$Rank_Djo, df$Win)
cor(df$Log_Rank_Djo, df$Win)
ggplot(df, aes(x = as.factor(Win), y = Log_Rank_Djo)) + geom_boxplot() 
ggplot(df, aes(x = Rank_Djo)) + geom_histogram(bins = 30) 
ggplot(df, aes(x = Log_Rank_Djo, color = as.factor(Win), fill = as.factor(Win))) + geom_density(linewidth = 0.75, alpha = 0.5) 


cor(df$Rank_Opp, df$Win)
cor(df$Log_Rank_Opp, df$Win)
ggplot(df, aes(x = as.factor(Win), y = Log_Rank_Opp)) + geom_boxplot() 
ggplot(df, aes(x = Rank_Opp)) + geom_histogram(bins = 50) 
ggplot(df, aes(x = Log_Rank_Opp, color = as.factor(Win), fill = as.factor(Win))) + geom_density(linewidth = 0.75, alpha = 0.5) 


cor(df$Five_Set, df$Win)
ggplot(df, aes(x = as.factor(Five_Set), fill = as.factor(Win))) + geom_bar()
ggplot(df, aes(x = as.factor(Five_Set), fill = as.factor(Win))) + geom_bar(position = 'fill')


cor(df$Hard, df$Win)
cor(df$Grass, df$Win)
cor(df$Clay, df$Win)
cor(df$Indoor, df$Win)
ggplot(df, aes(x = as.factor(Hard), fill = as.factor(Win))) + geom_bar()
ggplot(df, aes(x = as.factor(Hard), fill = as.factor(Win))) + geom_bar(position = 'fill')
ggplot(df, aes(x = as.factor(Grass), fill = as.factor(Win))) + geom_bar()
ggplot(df, aes(x = as.factor(Grass), fill = as.factor(Win))) + geom_bar(position = 'fill')
ggplot(df, aes(x = as.factor(Clay), fill = as.factor(Win))) + geom_bar()
ggplot(df, aes(x = as.factor(Clay), fill = as.factor(Win))) + geom_bar(position = 'fill')
ggplot(df, aes(x = as.factor(Indoor), fill = as.factor(Win))) + geom_bar()
ggplot(df, aes(x = as.factor(Indoor), fill = as.factor(Win))) + geom_bar(position = 'fill')

cor(df$Opp_Set_Perc, df$Win)
ggplot(df, aes(x = as.factor(Win), y = Opp_Set_Perc)) + geom_boxplot()
ggplot(df, aes(x = Opp_Set_Perc)) + geom_histogram(bins = 50) 
ggplot(df, aes(x = Opp_Set_Perc, color = as.factor(Win), fill = as.factor(Win))) + geom_density(linewidth = 0.75, alpha = 0.5) 


cor(df$Opp_Court_Perc, df$Win)
ggplot(df, aes(x = as.factor(Win), y = Opp_Court_Perc)) + geom_boxplot() 
ggplot(df, aes(x = Opp_Court_Perc)) + geom_histogram(bins = 50) 
ggplot(df, aes(x = Opp_Court_Perc, color = as.factor(Win), fill = as.factor(Win))) + geom_density(linewidth = 0.75, alpha = 0.5) 


cor(df$H2H_Djo, df$Win)
ggplot(df, aes(x = as.factor(Win), y = H2H_Djo)) + geom_boxplot() 
ggplot(df, aes(x = (H2H_Djo))) + geom_histogram(bins = 50) 
ggplot(df, aes(x = H2H_Djo, color = as.factor(Win), fill = as.factor(Win))) + geom_density(linewidth = 0.75, alpha = 0.5) 


cor(df$Pre_Djo, df$Win)
ggplot(df, aes(x = as.factor(Pre_Djo), fill = as.factor(Win))) + geom_bar()
ggplot(df, aes(x = as.factor(Pre_Djo), fill = as.factor(Win))) + geom_bar(position = 'fill')

cor(df$Pre5_Djo, df$Win)
ggplot(df, aes(x = as.factor(Pre5_Djo), fill = as.factor(Win))) + geom_bar()
ggplot(df, aes(x = as.factor(Pre5_Djo), fill = as.factor(Win))) + geom_bar(position = 'fill')

cor(df$Pre_Opp, df$Win)
ggplot(df, aes(x = as.factor(Pre_Opp), fill = as.factor(Win))) + geom_bar()
ggplot(df, aes(x = as.factor(Pre_Opp), fill = as.factor(Win))) + geom_bar(position = 'fill')

cor(df$Pre5_Opp, df$Win)
ggplot(df, aes(x = as.factor(Pre5_Opp), fill = as.factor(Win))) + geom_bar()
ggplot(df, aes(x = as.factor(Pre5_Opp), fill = as.factor(Win))) + geom_bar(position = 'fill')



corrplot(cor(df), type = "upper", tl.col = "black", tl.srt = 60)
corrplot(cor(sdf), type = "upper", tl.col = "black", tl.srt = 60)

corrplot(cor(sdf[, !(names(sdf) %in% c('Rank_Djo', 'Rank_Opp'))]), type = "upper", tl.col = "black", tl.srt = 60)



# outliers in both ranks   => log tranformation
# maybe outliers in opp court and set perc, h2h



