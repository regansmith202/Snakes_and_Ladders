library(dplyr)
library(tibble)

# matrix of moves
move_matrix = matrix(NA, ncol = 2, nrow = 500)
colnames(move_matrix) = c("on_square", "move_to")
move_matrix[,1:2] = seq_len(500)

non_rando_matrix = matrix(NA, ncol = 5, nrow = 1000000)
for(i in 1:1000000){
  non_rando_matrix[i,1] = sample(c(1:6), 5, replace = TRUE)[1]
  non_rando_matrix[i,2] = sample(c(1:6), 5, replace = TRUE)[2]
  non_rando_matrix[i,3] = sample(c(1:6), 5, replace = TRUE)[3]
  non_rando_matrix[i,4] = sample(c(1:6), 5, replace = TRUE)[4]
  non_rando_matrix[i,5] = sample(c(1:6), 5, replace = TRUE)[5]
}

colnames(non_rando_matrix) = paste("move:", c(1:5))
move1_non_rando = non_rando_matrix %>% 
  as_tibble() %>%
  unique()

# loop for all possible 5 throws
for(j in 1:6^5){
  
  moves1 = c(move1_non_rando$`move: 1`[j],
             move1_non_rando$`move: 2`[j],
             move1_non_rando$`move: 3`[j],
             move1_non_rando$`move: 4`[j],
             move1_non_rando$`move: 5`[j])
  
  
  # Up values
  move_matrix[4,2] = 75
  move_matrix[5,2] = 15
  move_matrix[19,2] = 41
  move_matrix[28,2] = 50
  move_matrix[35,2] = 96
  move_matrix[44,2] = 82
  move_matrix[53,2] = 94
  move_matrix[59,2] = 95
  move_matrix[70,2] = 91
  # Down values
  move_matrix[98,2] = 12
  move_matrix[88,2] = 67
  move_matrix[81,2] = 62
  move_matrix[76,2] = 41
  move_matrix[52,2] = 23
  move_matrix[47,2] = 30
  move_matrix[31,2] = 8
  move_matrix[21,2] = 3
  
 
  for(i in 1:5){
    if(i == 1){
      player1_current_position <<- 0
      player1_rolls_a = moves1[i]
      player1_moves_to <<- move_matrix[moves1[1],2]
      
      # rule for ladder/snake remove - if condition they hit a snake or ladder
      if(player1_current_position + player1_rolls_a != player1_moves_to){
        move_matrix[player1_current_position + player1_rolls_a,2] = player1_current_position + player1_rolls_a
      }
      
    } else{
      player1_current_position <<- player1_moves_to
      player1_rolls_a = moves1[i]
      player1_moves_to <<- move_matrix[player1_current_position + player1_rolls_a,2]
      
      # rule for ladder/snake remove - if condition they hit a snake or ladder
      if(player1_current_position + player1_rolls_a != player1_moves_to){
        move_matrix[player1_current_position + player1_rolls_a,2] = player1_current_position + player1_rolls_a
      }
    }
  }
      
      
      
  
  if(player1_moves_to >= 100){
    for(z in 1:6^5){
      moves2 = c(move1_non_rando$`move: 1`[z],
                 move1_non_rando$`move: 2`[z],
                 move1_non_rando$`move: 3`[z],
                 move1_non_rando$`move: 4`[z],
                 move1_non_rando$`move: 5`[z])

      for(m in 1:5){
        if(m == 1){
          player2_current_position <<- 0
          player2_rolls_a = moves2[m]
          player2_moves_to <<- move_matrix[moves2[1],2]
        } else{
          player2_current_position <<- player2_moves_to
          player2_rolls_a = moves2[m]
          player2_moves_to <<- move_matrix[player2_current_position + player2_rolls_a,2]
        }
      }

      if(player2_moves_to >= 100){
        message(list(moves1, moves2))
        return(list(moves1,moves2))
      }

    }
  }
  
}



