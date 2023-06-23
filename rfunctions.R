library(tidyverse)

p1ruud = function(df) {
  if (df['Player_1'] == 'Ruud C.' && df['Date'] < '2010-01-01') {
    return('Ruud Cr.')
  } else {
    return(df['Player_1'])
  }
}

p2ruud = function(df) {
  if (df['Player_2'] == 'Ruud C.' && df['Date'] < '2010-01-01') {
    return('Ruud Cr.')
  } else {
    return(df['Player_2'])
  }
}

wruud = function(df) {
  if (df['Winner'] == 'Ruud C.' && df['Date'] < '2010-01-01') {
    return('Ruud Cr.')
  } else {
    return(df['Winner'])
  }
}



crtsrf = function(df) {
  if (df['Surface'] == 'Grass') {
    return('Grass')
  }
  if (df['Surface'] == 'Clay') {
    return('Clay')
  }
  if (df['Surface'] == 'Hard' && df['Court'] == 'Indoor') {
    return('Hard Indoor')
  }
  if (df['Surface'] == 'Hard' && df['Court'] == 'Outdoor') {
    return('Hard Outdoor')
  }
}

five = function(df) {
  if (df['Best.of'] == 5) {
    return(1)
  } else {
    return(0)
  }
}


imp = function(df) {
  if (df['Series'] %in% c('International', 'ATP250')) {
    if (df['Round'] == 'The Final') {
      return(250)
    } else if (df['Round'] == 'Semifinals') {
      return(150)
    } else if (df['Round'] == 'Quarterfinals') {
      return(90)
    } else {
      return(45)
    }
  } else if (df['Series'] %in% c('International Gold', 'ATP500')) {
    if (df['Round'] == 'The Final') {
      return(500)
    } else if (df['Round'] == 'Semifinals') {
      return(300)
    } else if (df['Round'] == 'Quarterfinals') {
      return(180)
    } else {
      return(90)
    }
  } else if (df['Series'] %in% c('Masters', 'Masters 1000')) {
    if (df['Round'] == 'The Final') {
      return(1000)
    } else if (df['Round'] == 'Semifinals') {
      return(600)
    } else if (df['Round'] == 'Quarterfinals') {
      return(360)
    } else if (df['Round'] %in% c('4th Round', '3rd Round')) {
      return(180)
    } else {
      return(90)
    }
  } else if (df['Series'] == 'Grand Slam') {
    if (df['Round'] == 'The Final') {
      return(2000)
    } else if (df['Round'] == 'Semifinals') {
      return(1200)
    } else if (df['Round'] == 'Quarterfinals') {
      return(720)
    } else if (df['Round'] == '4th Round') {
      return(360)
    } else if (df['Round'] == '3rd Round') {
      return(180)
    } else {
      return(90)
    }
  } else if (df['Series'] == 'Masters Cup') {
    if (df['Round'] == 'The Final') {
      return(1500)
    } else if (df['Round'] == 'Semifinals') {
      return(1000)
    } else if (df['Round'] == 'Round Robin') {
      return(600)
    }
  }
}


opponent = function(this_player, df) {
  if (df['Player_1'] == this_player) {
    return(df['Player_2'])
  }
  if (df['Player_2'] == this_player) {
    return(df['Player_1'])
  }
}

odd_thisplayer = function(this_player, df) {
  if (df['Player_1'] == this_player) {
    return(df['Odd_1'])
  }
  if (df['Player_2'] == this_player) {
    return(df['Odd_2'])
  }
}

odd_opponent = function(this_player, df) {
  if (df['Player_1'] == this_player) {
    return(df['Odd_2'])
  }
  if (df['Player_2'] == this_player) {
    return(df['Odd_1'])
  }
}


rank_thisplayer = function(this_player, df) {
  if (df['Player_1'] == this_player) {
    return(df['Rank_1'])
  }
  if (df['Player_2'] == this_player) {
    return(df['Rank_2'])
  }
}

rank_opponent = function(this_player, df) {
  if (df['Player_1'] == this_player) {
    return(df['Rank_2'])
  }
  if (df['Player_2'] == this_player) {
    return(df['Rank_1'])
  }
}

win = function(this_player, df) {
  if (df['Winner'] == this_player) {
    return(1)
  } 
  else {
    return(0)
  }
}

odd_pred = function(this_player_odd, df) {
  if (this_player_odd > df['Odd_Opp']) {
    return(0)
  } 
  else {
    return(1)
  }
}


grass = function(df) {
  if (df['Court'] == 'Grass') {
    return(1)
  } 
  else {
    return(0)
  }
}

clay = function(df) {
  if (df['Court'] == 'Clay') {
    return(1)
  } 
  else {
    return(0)
  }
}

indoor = function(df) {
  if (df['Court'] == 'Hard Indoor') {
    return(1)
  } 
  else {
    return(0)
  }
}

hard = function(df) {
  if (df['Court'] == 'Hard Outdoor') {
    return(1)
  } 
  else {
    return(0)
  }
}

odd_acc = function(df) {
  if (df['Win'] == df['Odd_Pred']) {
    return(1)
  } 
  else {
    return(0)
  }
}


courtwperc = function(player2, court, date) {
  df = subset(data, (data$Player_2 == player2 | data$Player_1 == player2) & data$Court == court & data$Date < date)
  if (nrow(df) == 0) {
    return(0)
  } else {
    player2_wins = sum(df$Winner == player2)
    return((player2_wins / nrow(df)) * 100)
  }
}

setwperc = function(player2, sets, date) {
  df = subset(data, (data$Player_2 == player2 | data$Player_1 == player2) & data$Five_Set == sets & data$Date < date)
  if (nrow(df) == 0) {
    return(0)
  } else {
    player2_wins = sum(df$Winner == player2)
    return((player2_wins / nrow(df)) * 100)
  }
}


h2hpercplayer = function(this_player, opponent, date) {
  df = data[((data$Player_2 == opponent) | (data$Player_1 == opponent)) & ((data$Player_2 == this_player) | (data$Player_1 == this_player)) & (data$Date < date), ]
  if (nrow(df) > 0) {
    player_wins = sum(df$Winner == this_player)
    opponent_wins = sum(df$Winner == opponent)
    total_matches = nrow(df)
    if (player_wins > 0) {
      return ((player_wins / total_matches) * 100)
    } else if (opponent_wins > 0) {
      return (100 - ((opponent_wins / total_matches) * 100))
    } else {
      return (50)
    }
  } else {
    return (50)
  }
}


prewplayer = function(column) {
  return(c(0, column[-length(column)]))
}


pre5wplayer = function(column) {
  result = numeric(length(column))
  for (i in 6:length(column)) {
    result[i] = mean(column[(i - 5):(i - 1)])
  }
  result[is.na(result)] = 0
  return(result)
}


prewopp = function(opponent, date) {
  df = subset(data, (Player_2 == opponent | Player_1 == opponent) & Date < date)
  if (nrow(df) > 0) {
    last_row = tail(df, 1)
    if (last_row$Winner == opponent) {
      return(1)
    } else {
      return(0)
    }
  } else {
    return(0)
  }
}


pre5wopp = function(opponent, date) {
  df = subset(data, (Player_2 == opponent | Player_1 == opponent) & Date < date)
  if (nrow(df) > 0) {
    last_five_rows = tail(df, 5)
    count = 0
    for (i in 1:length(last_five_rows)) {
      if (!is.na(last_five_rows[i, "Winner"]) && last_five_rows[i, "Winner"] == opponent) {
        count = count + 0.2
      }
    }
    return(count)
  } else {
    return(0)
  }
}



