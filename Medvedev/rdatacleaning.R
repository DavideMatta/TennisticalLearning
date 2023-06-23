library(tidyverse)
source('../rfunctions.r')


data = read.csv('../data/data.csv')
head(data)



dmf = data[((data$Player_1 == 'Medvedev D.') | (data$Player_2 == 'Medvedev D.')), ]
dmf = tibble::rownames_to_column(dmf, var = "index")
dmf = dmf[, !(names(dmf) %in% c('index'))]
head(dmf)







dmf$Opponent = apply(dmf, 1, function(dmf) opponent('Medvedev D.', dmf))
dmf$Odd_Med = apply(dmf, 1, function(dmf) odd_thisplayer('Medvedev D.', dmf))
dmf$Odd_Opp = apply(dmf, 1, function(dmf) odd_opponent('Medvedev D.', dmf))
head(dmf)





dmf$Rank_Med = apply(dmf, 1, function(dmf) rank_thisplayer('Medvedev D.', dmf))
dmf$Rank_Opp = apply(dmf, 1, function(dmf) rank_opponent('Medvedev D.', dmf))
dmf$Win = apply(dmf, 1, function(dmf) win('Medvedev D.', dmf))
dmf$Odd_Pred = apply(dmf, 1, function(dmf) odd_pred(dmf['Odd_Med'], dmf))
head(dmf)



dmf$Grass = apply(dmf, 1, grass)
dmf$Clay = apply(dmf, 1, clay)
dmf$Indoor = apply(dmf, 1, indoor)
dmf$Hard = apply(dmf, 1, hard)
dmf$Odd_Acc = apply(dmf, 1, odd_acc)
head(dmf)




dmf$Opp_Court_Perc = apply(dmf, 1, function(dmf) courtwperc(dmf['Opponent'], dmf['Court'], dmf['Date']))
dmf$Opp_Set_Perc = apply(dmf, 1, function(dmf) setwperc(dmf['Opponent'], dmf['Five_Set'], dmf['Date']))
head(dmf)



dmf$H2H_Med = apply(dmf, 1, function(dmf) h2hpercplayer('Medvedev D.', dmf['Opponent'], dmf['Date']))
dmf$Pre_Med = prewplayer(dmf$Win)
dmf$Pre5_Med = pre5wplayer(dmf$Win)
tail(dmf)
head(dmf)




dmf$Pre_Opp = apply(dmf, 1, function(dmf) prewopp(dmf['Opponent'], dmf['Date']))
dmf$Pre5_Opp = apply(dmf, 1, function(dmf) pre5wopp(dmf['Opponent'], dmf['Date']))
tail(dmf)
head(dmf)



table(dmf$Odd_Acc) / length(dmf$Odd_Acc)

dmf$Rank_Med = as.numeric(dmf$Rank_Med)
dmf$Rank_Opp = as.numeric(dmf$Rank_Opp)
dmf$Odd_Med = as.numeric(dmf$Odd_Med)
dmf$Odd_Opp = as.numeric(dmf$Odd_Opp)



dmf$Log_Rank_Med = log(dmf$Rank_Med)
dmf$Log_Rank_Opp = log(dmf$Rank_Opp)


fdf = dmf[, c('Match_Importance', 'Rank_Med', 'Rank_Opp', 'Log_Rank_Med', 'Log_Rank_Opp', 
              'Five_Set', 'Hard', 'Grass', 'Clay', 'Indoor', 'Opp_Set_Perc', 'Opp_Court_Perc', 
               'H2H_Med', 'Pre_Med', 'Pre5_Med', 'Pre_Opp', 'Pre5_Opp', 
              'Odd_Med', 'Odd_Opp', 'Odd_Pred', 'Odd_Acc', 'Win')]
tail(fdf)


write.csv(dmf, '../data/med_df.csv', row.names = FALSE)
write.csv(fdf, '../data/med_fdf.csv', row.names = FALSE)




