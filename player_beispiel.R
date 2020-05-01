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

player_beispiel <- function(game_state,
                    player_id,
                    possible){
  n_row <<- n_row(game_state)
  n_col <<- n_col(game_state)
  if (length(possible) == 1) selected_move <- possible # Einschränkung, weil sample(6, size = 1) in Wirklichkeit sample(1:6, size = 1) macht
  if (length(possible) > 1)  selected_move <- sample(possible, size = 1)
  
  return(selected_move = selected_move)
  
}


attr(player_beispiel, 'name')        <- 'Beispiel PleaseDontKillMe'              
attr(player_beispiel, 'author')      <- 'Beispiel MaxK'
attr(player_beispiel, 'description') <- 'Verliert vielleicht nicht' # In den Attributen werden derzeit ä, ö, ü, ß noch falsch dargestellt.



game_state <- matrix(data = rep(0, 6*7), nrow = 6, ncol = 7)
game_state[1, 1] <- -1
game_state[1, 2] <- 1
game_state
player_id <- -1
player_id
possible <- 1:7
possible


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


evaluate <- function(game_state, possible){
  scores = vector()
  # evaluate verticals
  
  scores_vertical = vector()
  for (board_col_pos in possible){
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
  # get diagonals in increasing order
  diagonal_ind = row(game_state)-col(game_state)
  for (board_dia_pos in (1-n_col):(n_row-1)){
    if (board_dia_pos==0){
      print("hi")
    }
    # get diagonal
    diag_pos = game_state[diagonal_ind==board_dia_pos]
    if (length(diag_pos)<n_win){
      # diagonals that are <4 are not relevant
      scores_diagonal_tr_bl = c(scores_diagonal_tr_bl,0)
      next
    }
    # diags with pos one lower are below
    diag_pos_below = game_state[diagonal_ind==(board_dia_pos-1)]
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
  # get diagonals in decreasing order
  diagonal_ind = col(game_state)+row(game_state)
  for (board_dia_pos in (1+1):(n_row+n_col)){
    # get diagonal
    diag_pos = game_state[diagonal_ind==board_dia_pos]
    if (length(diag_pos)<n_win){
      # diagonals that are <4 are not relevant
      scores_diagonal_tl_br = c(scores_diagonal_tl_br,0)
      next
    }
    # diags with pos one lower are below
    diag_pos_below = game_state[diagonal_ind==(board_dia_pos-1)]
    
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
  
  
  return(scores_diagonal_tl_br)
}

game_state[,1] = c(-1,-1,-1,1,1,0)
game_state[,2] = c(1,1,-1,1,1,1)
game_state[,3] = c(-1,1,-1,1,0,0)
game_state[,3] = c(0,0,0,0,0,0)
game_state[,3] = c(-1,1,-1,1,0,0)
game_state[,4] = c(1,-1,1,0,0,0)
game_state[,5] = c(1,-1,1,-1,-1,-1)

evaluate(game_state,possible)


# TODO: loss function to compute final score (square (with respect to sign) and add)