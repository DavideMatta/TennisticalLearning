library(tidyverse)
source('../rfunctions.r')

data = read.csv('../data/atp_tennis.csv')
head(data)



data$Player_1 = apply(data, 1, p1ruud)
data$Player_2 = apply(data, 1, p2ruud)
data$Winner = apply(data, 1, wruud)


data = data[data$Odd_1 >= 1 & data$Odd_2 >= 1, ]
data = data[data$Rank_1 >= 1 & data$Rank_2 >= 1, ]
data = data[data$Surface == 'Grass' | (data$Surface == 'Clay' & data$Court == 'Outdoor') | data$Surface == 'Hard', ]
head(data)


data = data[, c('Date', 'Series', 'Court', 'Surface', 'Round', 'Best.of', 'Player_1', 'Player_2', 'Winner', 'Rank_1', 'Rank_2', 'Odd_1', 'Odd_2')]
head(data)



data$court_type = apply(data, 1, crtsrf)
data$Five_Set = apply(data, 1, five)
data = data[, !(names(data) %in% c("Court", "Surface", "Best.of"))]
colnames(data)[colnames(data) == "court_type"] = "Court"
data$Match_Importance = apply(data, 1, imp)
data = data[, !(names(data) %in% c('Series', 'Round'))]
head(data)



djf = data[((data$Player_1 == 'Djokovic N.') | (data$Player_2 == 'Djokovic N.')), ]
djf = tibble::rownames_to_column(djf, var = "index")
djf = djf[, !(names(djf) %in% c('index'))]
head(djf)







djf$Opponent = apply(djf, 1, function(djf) opponent('Djokovic N.', djf))
djf$Odd_Djo = apply(djf, 1, function(djf) odd_thisplayer('Djokovic N.', djf))
djf$Odd_Opp = apply(djf, 1, function(djf) odd_opponent('Djokovic N.', djf))
head(djf)





djf$Rank_Djo = apply(djf, 1, function(djf) rank_thisplayer('Djokovic N.', djf))
djf$Rank_Opp = apply(djf, 1, function(djf) rank_opponent('Djokovic N.', djf))
djf$Win = apply(djf, 1, function(djf) win('Djokovic N.', djf))
djf$Odd_Pred = apply(djf, 1, function(djf) odd_pred(djf['Odd_Djo'], djf))
head(djf)



djf$Grass = apply(djf, 1, grass)
djf$Clay = apply(djf, 1, clay)
djf$Indoor = apply(djf, 1, indoor)
djf$Hard = apply(djf, 1, hard)
djf$Odd_Acc = apply(djf, 1, odd_acc)
head(djf)




djf$Opp_Court_Perc = apply(djf, 1, function(djf) courtwperc(djf['Opponent'], djf['Court'], djf['Date']))
djf$Opp_Set_Perc = apply(djf, 1, function(djf) setwperc(djf['Opponent'], djf['Five_Set'], djf['Date']))
head(djf)



djf$H2H_Djo = apply(djf, 1, function(djf) h2hpercplayer('Djokovic N.', djf['Opponent'], djf['Date']))
djf$Pre_Djo = prewplayer(djf$Win)
djf$Pre5_Djo = pre5wplayer(djf$Win)
tail(djf)
head(djf)




djf$Pre_Opp = apply(djf, 1, function(djf) prewopp(djf['Opponent'], djf['Date']))
djf$Pre5_Opp = apply(djf, 1, function(djf) pre5wopp(djf['Opponent'], djf['Date']))
tail(djf)
head(djf)



table(djf$Odd_Acc) / length(djf$Odd_Acc)

djf$Rank_Djo = as.numeric(djf$Rank_Djo)
djf$Rank_Opp = as.numeric(djf$Rank_Opp)
djf$Odd_Djo = as.numeric(djf$Odd_Djo)
djf$Odd_Opp = as.numeric(djf$Odd_Opp)



djf$Log_Rank_Djo = log(djf$Rank_Djo)
djf$Log_Rank_Opp = log(djf$Rank_Opp)


fdf = djf[, c('Match_Importance', 'Rank_Djo', 'Rank_Opp', 'Log_Rank_Djo', 'Log_Rank_Opp', 
              'Five_Set', 'Hard', 'Grass', 'Clay', 'Indoor', 'Opp_Set_Perc', 'Opp_Court_Perc', 
               'H2H_Djo', 'Pre_Djo', 'Pre5_Djo', 'Pre_Opp', 'Pre5_Opp', 
              'Odd_Djo', 'Odd_Opp', 'Odd_Pred', 'Odd_Acc', 'Win')]
tail(fdf)

write.csv(data, '../data/data.csv', row.names = FALSE)
write.csv(djf, '../data/djo_df.csv', row.names = FALSE)
write.csv(fdf, '../data/djo_fdf.csv', row.names = FALSE)




