library(tidyverse)
source('../rfunctions.r')


data = read.csv('../data/data.csv')
head(data)



drf = data[((data$Player_1 == 'Ruud C.') | (data$Player_2 == 'Ruud C.')), ]
drf = tibble::rownames_to_column(drf, var = "index")
drf = drf[, !(names(drf) %in% c('index'))]
head(drf)







drf$Opponent = apply(drf, 1, function(drf) opponent('Ruud C.', drf))
drf$Odd_Ruud = apply(drf, 1, function(drf) odd_thisplayer('Ruud C.', drf))
drf$Odd_Opp = apply(drf, 1, function(drf) odd_opponent('Ruud C.', drf))
head(drf)





drf$Rank_Ruud = apply(drf, 1, function(drf) rank_thisplayer('Ruud C.', drf))
drf$Rank_Opp = apply(drf, 1, function(drf) rank_opponent('Ruud C.', drf))
drf$Win = apply(drf, 1, function(drf) win('Ruud C.', drf))
drf$Odd_Pred = apply(drf, 1, function(drf) odd_pred(drf['Odd_Ruud'], drf))
head(drf)



drf$Grass = apply(drf, 1, grass)
drf$Clay = apply(drf, 1, clay)
drf$Indoor = apply(drf, 1, indoor)
drf$Hard = apply(drf, 1, hard)
drf$Odd_Acc = apply(drf, 1, odd_acc)
head(drf)




drf$Opp_Court_Perc = apply(drf, 1, function(drf) courtwperc(drf['Opponent'], drf['Court'], drf['Date']))
drf$Opp_Set_Perc = apply(drf, 1, function(drf) setwperc(drf['Opponent'], drf['Five_Set'], drf['Date']))
head(drf)



drf$H2H_Ruud = apply(drf, 1, function(drf) h2hpercplayer('Ruud C.', drf['Opponent'], drf['Date']))
drf$Pre_Ruud = prewplayer(drf$Win)
drf$Pre5_Ruud = pre5wplayer(drf$Win)
tail(drf)
head(drf)




drf$Pre_Opp = apply(drf, 1, function(drf) prewopp(drf['Opponent'], drf['Date']))
drf$Pre5_Opp = apply(drf, 1, function(drf) pre5wopp(drf['Opponent'], drf['Date']))
tail(drf)
head(drf)



table(drf$Odd_Acc) / length(drf$Odd_Acc)

drf$Rank_Ruud = as.numeric(drf$Rank_Ruud)
drf$Rank_Opp = as.numeric(drf$Rank_Opp)
drf$Odd_Ruud = as.numeric(drf$Odd_Ruud)
drf$Odd_Opp = as.numeric(drf$Odd_Opp)



drf$Log_Rank_Ruud = log(drf$Rank_Ruud)
drf$Log_Rank_Opp = log(drf$Rank_Opp)


fdf = drf[, c('Match_Importance', 'Rank_Ruud', 'Rank_Opp', 'Log_Rank_Ruud', 'Log_Rank_Opp', 
              'Five_Set', 'Hard', 'Grass', 'Clay', 'Indoor', 'Opp_Set_Perc', 'Opp_Court_Perc', 
               'H2H_Ruud', 'Pre_Ruud', 'Pre5_Ruud', 'Pre_Opp', 'Pre5_Opp', 
              'Odd_Ruud', 'Odd_Opp', 'Odd_Pred', 'Odd_Acc', 'Win')]
tail(fdf)


write.csv(drf, '../data/ruud_df.csv', row.names = FALSE)
write.csv(fdf, '../data/ruud_fdf.csv', row.names = FALSE)




