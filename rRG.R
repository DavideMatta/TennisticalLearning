library(tidyverse)
source('rfunctions.r')

data = read.csv('data/atp_tennis_update.csv')
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




djf$Rank_Djo = as.numeric(djf$Rank_Djo)
djf$Rank_Opp = as.numeric(djf$Rank_Opp)
djf$Odd_Djo = as.numeric(djf$Odd_Djo)
djf$Odd_Opp = as.numeric(djf$Odd_Opp)



djf$Log_Rank_Djo = log(djf$Rank_Djo)
djf$Log_Rank_Opp = log(djf$Rank_Opp)


djf = subset(djf, (djf$Date > '2023-05-27' & djf$Date < '2023-06-12'))
head(djf)

table(djf$Odd_Acc) / length(djf$Odd_Acc)
# 0.8571429

fdf = djf[, c('Match_Importance', 'Rank_Djo', 'Rank_Opp', 'Log_Rank_Djo', 'Log_Rank_Opp', 
              'Five_Set', 'Hard', 'Grass', 'Clay', 'Indoor', 'Opp_Set_Perc', 'Opp_Court_Perc', 
              'H2H_Djo', 'Pre_Djo', 'Pre5_Djo', 'Pre_Opp', 'Pre5_Opp', 
              'Odd_Djo', 'Odd_Opp', 'Odd_Pred', 'Odd_Acc', 'Win')]
tail(fdf)

write.csv(data, 'data/data_update.csv', row.names = FALSE)
write.csv(fdf, 'data/djo_fdf_RG.csv', row.names = FALSE)




# MEDVEDEV




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



dmf = subset(dmf, (dmf$Date > '2023-05-27' & dmf$Date < '2023-06-12'))
dmf



table(dmf$Odd_Acc) / length(dmf$Odd_Acc)
# 0

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


write.csv(fdf, 'data/med_fdf_RG.csv', row.names = FALSE)





# RUUD






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


drf = subset(drf, (drf$Date > '2023-05-27' & drf$Date < '2023-06-12'))
drf

table(drf$Odd_Acc) / length(drf$Odd_Acc)
# 1

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


write.csv(fdf, 'data/ruud_fdf_RG.csv', row.names = FALSE)






