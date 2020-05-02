# Beispiel KI

# 1) das R Skript kann sollte mit player_hierStehtDerPlayerNameInKurz benannt sein
# 2) es muss eine Funktion enthalten, die mit player_ beginnt (kann aber muss nicht dem Dateinamen entsprechen)
# 3) die Funktion muss aus den Inputs game_state (Matrix mit Werten -1, 0, 1), der player_id (-1 oder 1) und den möglichen Zügen (Teilmengen aus 1:7)
# einen der möglichen Züge auswählen. Wird keiner dieser Züge gewählt, wählt die Shiny App automatisch zufällig aus den möglichen Zügen einen Alternativzug aus
# 4) Das R Skript benötigt die Attribute name, author und description für die player funktion. Da diese nicht 'player_beispiel' heißen sollte, muss dieser Funktionsname unten auch geändert werden

# # Beispiel-Inputs
# game_state <- matrix(data = rep(0, 6*7), nrow = 6, ncol = 7)
# game_state[1, 1] <- -1
# game_state[1, 2] <- 1
# game_state
# player_id <- -1
# player_id
# possible <- 1:7
# possible

n_col = 7
n_row = 6
diagonal_ind_tr_bl = row(game_state)-col(game_state)
diagonal_ind_tl_br = col(game_state)+row(game_state)

player_beispiel <- function(game_state,
                    player_id,
                    possible){
  # store number of rows
  n_row <<- nrow(game_state)
  # store number of cols
  n_col <<- ncol(game_state)
  
  # get diagonals in top right to bottom left (increasing order)
  diagonal_ind_tr_bl <<- row(game_state)-col(game_state)
  # get diagonals top left to bottom right (increasing order)
  diagonal_ind_tl_br <<- col(game_state)+row(game_state)
  
  # if only one move is possible, return that one
  if (length(possible) == 1) selected_move <- possible
  
  # if you are the first to move, select the middle
  if (all(game_state==0)) selected_move <- 4
  
  # get scores for different state
  scores = vector()
  for (move in possible){
    
    game_state_after_move = make_move(game_state, move, player_id) 
    # check if we have to reduce possible
    score_move = evaluate(game_state_after_move)
    scores = c(scores, score_move)

  }
  print(paste0("Scores: ", scores))
  best_move = possible[which(scores==max(scores))[1]]
    
  return(selected_move = best_move)
  
}


attr(player_beispiel, 'name')        <- 'Beispiel PleaseDontKillMe'              
attr(player_beispiel, 'author')      <- 'Beispiel MaxK'
attr(player_beispiel, 'description') <- 'Verliert vielleicht nicht' # In den Attributen werden derzeit ä, ö, ü, ß noch falsch dargestellt.



make_move <- function(game_state, move, player){
  board_col = game_state[, move]
  if (0%in%board_col){
    print(paste0("Throw coin in column ",move))
    coin_pos = min(which(board_col==0))
    game_state[coin_pos,move] = player
  } else {
    print(paste("ERROR - MOVE ",move,"  NOT POSSIBLE"))
    print(game_state)
    stop("Stopped")
  }
  return(game_state)
}


evaluate_row <- function(board_row, board_row_below=rep(1,n_col), len_row=n_col){
  window_values <- vector()
  # go through row in windows of size n_win
  for (i in 0:(len_row-n_win)){
    window = board_row[(i+1):(n_win+i)]
    # if both players have coins inside the window, you can't win/loose there
    if (all(c(1,-1)%in%window)) window_values = c(window_values,0)
    else {
      window_value = sum(window)
      
      if (0%in%board_row_below[i:(n_win+i)]){
        # if you can't place a coin there, because there is no coin below, punish by 1
        punish = (-1)*sign(window_value)
        window_values = c(window_values,(window_value+punish))
      } else {
        window_values = c(window_values,window_value)
      }
    }
  }
  return(window_values)
}


evaluate_col <- function(board_col){
  # return sum of the coins of the same player at the top
  player_pos = max(0,which(board_col==1))
  player_neg = max(0,which(board_col==-1))
  
  if (player_pos == player_neg) return(0)
  
  player_max = max(player_pos, player_neg)
  player_min = min(player_pos, player_neg)
  
  eval_value = sum(board_col[(player_min+1):player_max])
  
  return(eval_value)
}

evaluate <- function(game_state){
  scores = vector()
  # evaluate verticals
  
  scores_vertical = vector()
  for (board_col_pos in 1:n_col){
    board_col = game_state[,board_col_pos]
    top_coin_pos = max(which(board_col!=0),0)
    if (top_coin_pos == 0) scores_vertical = c(scores_vertical, 0)
    
    board_col_value = evaluate_col(board_col[1:top_coin_pos])
    if (board_col_value >= n_win) return(Inf)
    if (board_col_value <= -n_win) return(-Inf)
    
    if (abs(board_col_value)+(n_row-top_coin_pos) < 4){
      # if I can't stack coins on top to win there is no value
      scores_vertical = c(scores_vertical, 0)
      next
    }
    scores_vertical = c(scores_vertical, board_col_value)
  }
  scores = c(scores, scores_vertical)
  
  # evaluate horizontals
  scores_horizontal = vector()
  for (board_row_pos in 1:n_row){
    board_row = game_state[board_row_pos,]
    # when row is empty all rows above are empty
    if (all(board_row==0)) break
    
    if (board_row_pos >= 1) {
      row_values = evaluate_row(board_row, game_state[board_row_pos-1,])
    } else {
      row_values = evaluate_row(board_row)
    }
    if (4%in%row_values) return(Inf)
    if ((-4)%in%row_values) return(-Inf)
    scores_horizontal = c(scores_horizontal, row_values)
  }
  scores = c(scores, scores_horizontal)
  
  # evaluate diagonal top right to bottom left
  scores_diagonal_tr_bl = vector()

  for (board_dia_pos in (1-n_col):(n_row-1)){
    # get diagonal
    diag_pos = game_state[diagonal_ind_tr_bl==board_dia_pos]
    if (length(diag_pos)<n_win){
      # diagonals that are <4 are not relevant
      scores_diagonal_tr_bl = c(scores_diagonal_tr_bl,0)
      next
    }
    # diags with pos one lower are below
    diag_pos_below = game_state[diagonal_ind_tr_bl==(board_dia_pos-1)]
    # when board_diag > 0 we are on the left side of the diagonal.
    # The diagonal below will be shorter (not important since evaluate_row doesn't care)
    # when board_diag<=0, we are on the right side of the diagonal,
    # and the row below needs extension on the right side
    if (board_dia_pos>0){
    #if (length(diag_pos_below)>n_win){
      diag_values = evaluate_row(diag_pos, diag_pos_below, len_row=length(diag_pos))
    } else {
      # for lb_rt diagonals, we extend on the left side (bottom_row)
      diag_values = evaluate_row(diag_pos, c(1,diag_pos_below), len_row=length(diag_pos))
    }
    if (4%in%diag_values) return(Inf)
    if ((-4)%in%diag_values) return(-Inf)
    scores_diagonal_tr_bl = c(scores_diagonal_tr_bl, diag_values)
  }
  scores = c(scores, scores_diagonal_tr_bl)
    
  # evaluate diagonal top left top to bottom right
  scores_diagonal_tl_br = vector()

  for (board_dia_pos in (1+1):(n_row+n_col)){
    # get diagonal
    diag_pos = game_state[diagonal_ind_tl_br==board_dia_pos]
    if (length(diag_pos)<n_win){
      # diagonals that are <4 are not relevant
      scores_diagonal_tl_br = c(scores_diagonal_tl_br,0)
      next
    }
    # diags with pos one lower are below
    diag_pos_below = game_state[diagonal_ind_tl_br==(board_dia_pos-1)]
    
    # when board_dia_pos <= 8, diag_below is shorter by one and
    # needs extension on the right side (bottom_row)
    # when board_dia_pos > 8, diag_below has extra entry on left side, so we cut
    if (board_dia_pos>(n_col+1)){
      diag_values = evaluate_row(diag_pos, diag_pos_below[2:length(diag_pos_below)], len_row=length(diag_pos))
    } else {
      # for lb_rt diagonals, we extend on the right side (bottom_row)
      diag_values = evaluate_row(diag_pos, c(diag_pos_below,1), len_row=length(diag_pos))
    }
    scores_diagonal_tl_br = c(scores_diagonal_tl_br, diag_values)
  }
  scores = c(scores, scores_diagonal_tl_br)
  
  # Compute total score. Square (so3 has more value than 2), 
  # with respect to sign, and add scores.
  
  score = sum(scores*abs(scores))
  return(score)
}



plot_game <- function(game_state){
  plot.new()
  plot.window(c(0,n_col),c(0,n_row))
  abline(v=(seq(1,(n_col-1),1)), col="lightgray")
  abline(h=(seq(1,(n_row-1),1)), col="lightgray")
  #plot(0,type='n',axes=FALSE)
  for (i in 0:(n_row-1)){
    board_row = game_state[(i+1),] 
    x_coords = seq(0.5,n_row+0.5,1)[board_row==1]
    y_coords = rep((i+0.5),sum(board_row==1))
    points(x_coords,y_coords,type="p", col="red", pch=19, cex=2.5)
    
    x_coords = seq(0.5,n_row+0.5,1)[board_row==-1]
    y_coords = rep((i+0.5),sum(board_row==-1))
    points(x_coords,y_coords,type="p", col="blue", pch=19, cex=2.5)
  }
  for (j in 1:n_col){
    text(j-0.5,0,(j))
  }
}

game_state <- matrix(data = rep(0, 6*7), nrow = 6, ncol = 7)
#game_state[1, 1] <- -1
#game_state[1, 2] <- 1
#game_state[,1] = c(-1,-1,-1,1,0,0)
#game_state[,2] = c(1,1,-1,1,1,0)
#game_state[,3] = c(-1,1,-1,1,0,0)
#game_state[,3] = c(0,0,0,0,0,0)
#game_state[,3] = c(-1,1,-1,1,0,0)
#game_state[,4] = c(1,-1,1,0,0,0)
#game_state[,5] = c(1,-1,1,-1,-1,1)
#game_state[,6] = c(1,-1,1,0,0,0)
#game_state[,7] = c(1,-1,1,-1,-1,1)
#game_state
player_id <- -1
player_id
possible <- 1:7
#possible <- 
#plot_game(game_state)
#game_state <- make_move(game_state = game_state, move = player_beispiel(game_state, 1, possible), player = 1)
#game_state <- make_move(game_state, move = 2, player = -1)
#game_state_bak <- game_state
#write.csv("game_state_bak.csv",game_state_bak)
#write.table(game_state_bak,"game_state_bak.csv")
#game_state <- read.table("game_state_bak.csv")
#possible <- possible[possible!=4]
#game_state <- make_move(game_state = game_state, move = player_beispiel(game_state, 1, possible), player = 1)
