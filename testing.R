game_over(game_state){
  
  # check horizontal lines
  #for (i in 1:(n_col-n_win)){
    #b <- rep(0,n_col)
    #b[i:(i+n_win-1)] = rep(1,n_win)
    # check horizontal
    #h_sum = game_state%*%b
    
    #if (4 %in% h_sum) {return(c(TRUE,Inf))}
    #if (-4 %in% h_sum) {return(c(TRUE,-Inf))}
  #}
}
  
  # check vertical
  for (j in 1:(n_row-n_win)){
    b <- rep(0,n_row)
    b[j:(j+n_win-1)] = rep(1,n_win)
    v_sum = b%*%game_state
    
    if (4 %in% v_sum) return(c(TRUE,Inf))
    if (-4 %in% h_sum) return(c(TRUE,-Inf))
  }
  
  # check diagonals
  for (k in 1:(n_row-n_win)){
    for (l in 1:(n_col-n_win)){
      # get sub matrix window
      sub_matrix = game_state[k:(k+n_win-1), l:(l+n_win-1)]
      # compute diagonals from bottom left to top right
      sub_matrix_diag = diag(sub_matrix)
      if (4%in%sub_matrix_diag) return(c(TRUE,Inf))
      if (-4%in%sub_matrix_diag) return(c(TRUE,-Inf))
      # compute diagonals from top left to bottom right
      sub_matrix_diag = diag(sub_matrix[,n_win:1])
      sub_matrix_diag = diag(sub_matrix)
      if (4%in%sub_matrix_diag) return(c(TRUE,Inf))
      if (-4%in%sub_matrix_diag) return(c(TRUE,-Inf))
    }
  }
  return(FALSE)
}