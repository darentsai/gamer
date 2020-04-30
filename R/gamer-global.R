tetris_global <- new.env(parent = emptyenv())

## rotation matrix (clockwise 90 degrees)
tetris_global$ROTATEMAT <- matrix(round(c(cos(-pi/2), -sin(-pi/2),
                                          sin(-pi/2), cos(-pi/2))), 2, 2, byrow = T)

## probability of each tetromino
tetris_global$PROB <- rep(1, 7)

## vector of each tetromino
tetris_global$VEC_LIST = list('I' = matrix(c(c(-1.5, -0.5, 0.5, 1.5), c(-0.5, -0.5, -0.5, -0.5)), 2, 4, byrow = T),
                              'J' = matrix(c(c(0, -1, -1, 1), c(0, 1, 0, 0)), 2, 4, byrow = T),
                              'L' = matrix(c(c(0, 1, -1, 1), c(0, 1, 0, 0)), 2, 4, byrow = T),
                              'O' = matrix(c(c(-0.5, -0.5, 0.5, 0.5), c(-0.5, 0.5, 0.5, -0.5)), 2, 4, byrow = T),
                              'S' = matrix(c(c(0, 1, 0, -1), c(0, 0, -1, -1)), 2, 4, byrow = T),
                              'T' = matrix(c(c(0, -1, 1, 0), c(0, 0, 0, 1)), 2, 4, byrow = T),
                              'Z' = matrix(c(c(0, -1, 0, 1), c(0, 0, -1, -1)), 2, 4, byrow = T))
