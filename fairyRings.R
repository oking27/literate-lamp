# This simulates the formation of mushroom rings.

probSpore <- 0.2
probSporeToHyphae <- 0.5
probMushroom <- 0.4
probSpread <- 0.6

EMPTY <- 0
SPORE <- 1
YOUNG <- 2
MATURING <- 3
MUSHROOMS <- 4
OLDER <- 5
DECAYING <- 6
DEAD <- 7
LONG_DEAD <- 8
INERT <- 9

# lawn dimensions
height <- 50
width <- 50
numGen <- 50
lawn <- array(9, c(height, width, numGen))

lawn[2:(height-1), 2:(width-1), 1] <- rbinom((height-2)*(width-2), 1, probSpore)

for(gen in 2:numGen) {
  for (col in 2:(width-1)) {
    for(row in 2:(height-1)) {
      lawn[row, col, gen] <- switch(lawn[row, col, gen-1]+1,
                                      2*max(rep(rbinom(1, 1, probSpread), length(grep(2, lawn[(row-1):(row+1), (col-1):(col+1), gen-1]))), 0), # EMPTY to YOUNG
                                    ifelse(rbinom(1, 1, probSporeToHyphae), 2, 1), # SPORE to YOUNG
                                    3, # YOUNG to MATURING
                                    ifelse(rbinom(1, 1, probMushroom), 4, 5), # MATURING to MUSHROOMS/OLDER
                                    6, # MUSHROOMS to DECAYING
                                    6, # OLDER to DECAYING
                                    7, # DECAYING to DEAD
                                    8, # DEAD to LONG_DEAD
                                    ifelse(lawn[row, col, gen-2] == 8, 0, 8), # LONG_DEAD to EMPTY
                                    9 # INERT
                                    )
    }
  }
}

pointsForGrid <- function(grid, val) {
  xcoords <- vector()
  ycoords <- vector()
  for (row in 1:nrow(grid)) {
    for (col in 1:ncol(grid)) {
      if (grid[row, col] == val) {
        xcoords[length(xcoords)+1] <- col
        ycoords[length(ycoords)+1] <- nrow(grid)-row+1
      }
    }
  }
  return(list(xcoords, ycoords))
}

pointsForGrid <- function(grid, val) {
  xcoords <- vector()
  ycoords <- vector()
  for (row in 1:nrow(grid)) {
    for (col in 1:ncol(grid)) {
      if (grid[row, col] == val) {
        xcoords[length(xcoords)+1] <- col
        ycoords[length(ycoords)+1] <- nrow(grid)-row+1
      }
    }
  }
  return(list(xcoords, ycoords))
}

plot.gen <- function(lawn, gen) {
  all.empty <- pointsForGrid(lawn[, , gen], EMPTY)
  plot(all.empty[[1]], all.empty[[2]], pch=20, col="lawngreen",
       xlim=c(0, width), ylim=c(0, height), main=paste("Mushroom Generation", gen),
       xlab="", ylab="")
  all.spore <- pointsForGrid(lawn[, , gen], SPORE)
  points(all.spore[[1]], all.spore[[2]], pch=20, col="floralwhite",
       xlim=c(0, width), ylim=c(0, height))
  all.young <- pointsForGrid(lawn[, , gen], YOUNG)
  points(all.young[[1]], all.young[[2]], pch=20, col="palegoldenrod",
         xlim=c(0, width), ylim=c(0, height))
  all.maturing <- pointsForGrid(lawn[, , gen], MATURING)
  points(all.maturing[[1]], all.maturing[[2]], pch=20, col="orange",
         xlim=c(0, width), ylim=c(0, height))
  all.mushrooms <- pointsForGrid(lawn[, , gen], MUSHROOMS)
  points(all.mushrooms[[1]], all.mushrooms[[2]], pch=20, col="red",
         xlim=c(0, width), ylim=c(0, height))
  all.older <- pointsForGrid(lawn[, , gen], OLDER)
  points(all.older[[1]], all.older[[2]], pch=20, col="indianred",
         xlim=c(0, width), ylim=c(0, height))
  all.decaying <- pointsForGrid(lawn[, , gen], DECAYING)
  points(all.decaying[[1]], all.decaying[[2]], pch=20, col="rosybrown",
         xlim=c(0, width), ylim=c(0, height))
  all.dead <- pointsForGrid(lawn[, , gen], DEAD)
  points(all.dead[[1]], all.dead[[2]],pch=20, col="saddlebrown",
         xlim=c(0, width), ylim=c(0, height))
  all.long_dead <- pointsForGrid(lawn[, , gen], LONG_DEAD)
  points(all.long_dead[[1]], all.long_dead[[2]], pch=20, col="darkolivegreen",
         xlim=c(0, width), ylim=c(0, height))
  all.inert <- pointsForGrid(lawn[, , gen], INERT)
  points(all.inert[[1]], all.inert[[2]], pch=20, col="black",
         xlim=c(0, width), ylim=c(0, height))
}

for (gen in 1:numGen) {
  plot.gen(lawn, gen)
  Sys.sleep(0.1)
}
# end
