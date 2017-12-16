#-------------------------------------------------------------------------------

# purpose of this script is to simulate an answer to the puzzle posted here:
# https://www.actuaries.digital/2017/12/14/the-critical-line-volume-17-christmas-edition/

#-------------------------------------------------------------------------------

library(data.table)

# Funcitons to do simulation:
create_xy_probs <- function() {
    
    
    x <- -2:2
    y <- -2:2
    
    xy <- data.table(expand.grid(X = x, Y = y))
    
    xy[, PROB_UP := 0.25 - 0.03 * Y]
    xy[, PROB_DOWN := 0.3 + 0.05 * Y]
    xy[, PROB_LEFT := 0.3 + 0.02 * X - 0.02 * Y]
    xy[, PROB_RIGHT := 0.15 - 0.02 * X]
    # xy[,SUM_PROB:=PROB_UP+PROB_DOWN+PROB_LEFT+PROB_RIGHT]
    
    xy[, RANGE_UP := PROB_UP]
    xy[, RANGE_DOWN := PROB_DOWN + RANGE_UP]
    xy[, RANGE_LEFT := PROB_LEFT + RANGE_DOWN]
    xy[, RANGE_RIGHT := PROB_RIGHT + RANGE_LEFT]
    
    return(xy)
    
}
find_move_to <- function(xy,x=1,y=0,rand_01=0.2){
    
    xy_entry <- data.table(X = x, Y = y, RAND = rand_01)
    xy_entry <- merge(xy_entry, xy, by = c("X","Y"), all.x=T)
    
    xy_entry[RAND < RANGE_UP, MOVE_TO := "UP"]
    xy_entry[is.na(MOVE_TO) & RAND < RANGE_DOWN, MOVE_TO := "DOWN"]
    xy_entry[is.na(MOVE_TO) & RAND < RANGE_LEFT, MOVE_TO := "LEFT"]
    xy_entry[is.na(MOVE_TO), MOVE_TO := "RIGHT"]
    
    return(xy_entry[,MOVE_TO])
    
    
}
move_to_xy <- function(row_info){ 
    
    row_info[MOVE_TO == "UP", Y := Y + 1]
    row_info[MOVE_TO == "DOWN", Y := Y - 1]
    row_info[MOVE_TO == "RIGHT", X := X + 1]
    row_info[MOVE_TO == "LEFT", X := X - 1]
    
    row_info[Y == -3, Y := 2]
    row_info[Y == 3, Y := -2]
    row_info[X == -3, X := 2]
    row_info[X == 3, X := -2]
    
    return(row_info[,.(X, Y)])
    
}
one_move <- function(db, xy){
    
    db[nrow(db), RAND := runif(1, 0, 1)]    
    db[nrow(db), MOVE_TO := find_move_to(xy, X, Y, RAND)]
    
    next_move <- move_to_xy(db[nrow(db)])
    
    next_move[X==-2 & Y ==2 , FINISH := 1]
    next_move[X==2 & Y ==2 , FINISH := 2]
    next_move[X==2 & Y ==-2 , FINISH := 3]
    next_move[X==-2 & Y ==-2 , FINISH := 4]
    
    db <- rbind(db, next_move,fill=T)
    
    return(db)
}
one_simulation <- function(xy){
    
    db <- data.table(X=0, Y=0, FINISH = NA)
        
    while(nrow(db[!is.na(FINISH)]) ==0){
        
        db <- one_move(db, xy)
        
    }    
    
    results <- data.table(FINAL_POS = db[!is.na(FINISH),FINISH],
                          NO_MOVES = nrow(db))
    
}
do_simulation <- function(n_sims = 100){
    
    xy <- create_xy_probs()
    
    results_db <- data.table()
    
    for(ii in 1:n_sims){
        
        print(ii)
        
        results_sim <- one_simulation(xy)
        results_db <- rbind(results_db, results_sim)
        
    }
    
    return(results_db)
    
}

#-------------------------------------------------------------------------------

# run simulation
BEGIN <- Sys.time()
results_db <- do_simulation(10^4)
END <- Sys.time()

print(END - BEGIN)

# probability of finishing in each of the four goals:
results_db[,.(COUNT=.N,PROB=.N/nrow(results_db)),FINAL_POS]

# probability that walk takes more than 20 steps:
results_db[,.(COUNT=.N, PROB=.N/nrow(results_db)), .(MORE_20_MOVES = NO_MOVES>20)]

# expected number of steps in walk
results_db[,.(EXPECTED_STEPS = mean(NO_MOVES))]
results_db[,.(EXPECTED_STEPS = median(NO_MOVES))]
