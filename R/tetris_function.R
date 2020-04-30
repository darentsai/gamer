# Initialization -----------------------------

init <- function(env, nhint, speed){
  env$stage <- 0
  env$score <- 0 ; env$line <- 0
  env$combo <- -1
  env$time <- Sys.time()
  env$speed <- speed
  env$mat <- matrix(rep("", 23*10), 10, 23)
  env$dir <- ""
  env$type.cur <- env$type.hold <- NA_character_
  env$type.hint <- sample(names(tetris_global$VEC_LIST), nhint, T, prob = tetris_global$PROB)
  env$base <- c(5, 21)
  env$vec <- matrix(0, 2, 4)
  env$pos.next <- env$pos.prev <- matrix(0, 2, 4)
  env$proj <- env$proj.prev <- matrix(NA_real_, 2, 4)
  env$shift <- FALSE
  env$no_action <- FALSE
  env$stop <- FALSE
}

# Background -----------------------------

draw_table <- function(env, nhint){
  if(env$stage == 0){
    plot.new()
    plot.window(c(-7, 16), c(-2, 22), asp = 1)
    grid(50, col = "grey80", lty = 1, lwd = 1)
    rect(-4, c(17, 20), -1, c(20, 21), col = c("grey30", "grey70"), lwd = 3)
    rect(11, 20, 14, 21, col = "grey70", lwd = 3)
    rect(seq(14, by = -0.25, len = 5)[1:nhint],
         c(17.00, 13.75, 10.75, 8.00, 5.50)[1:nhint],
         seq(14.7, by = -0.25, len = 5)[1:nhint],
         c(20.00, 16.50, 13.25, 10.25, 7.50)[1:nhint],
         col = "grey70", lwd = 3)
    rect(11, c(17.00, 13.75, 10.75, 8.00, 5.50)[1:nhint],
         seq(14, by = -0.25, len = 5)[1:nhint],
         c(20.00, 16.50, 13.25, 10.25, 7.50)[1:nhint],
         col = "grey30", lwd = 3)
    rect(-4, 0, -1, 9, col = "grey10", lwd = 3)
    rect(-4, c(2, 5, 8), -1, c(3, 6, 9), col = "grey70", lwd = 3)
    text(-2.5, 20.5, "Hold", font = 2)
    text(-2.5, 12.5, "COMBO", font = 2, cex = 1.2)
    text(-2.5, 11.5, "0", font = 2, cex = 2, col = "red3")
    text(seq(14.35, by = -0.25, len = 5)[1:nhint],
         c(18.500, 15.125, 12.000, 9.125, 6.500)[1:nhint],
         (1:5)[1:nhint], font = 2, cex = 0.7, srt = -90)
    text(12.5, 20.5, "Next", font = 2)
    text(-2.5, c(2.5, 5.5, 8.5), c("SPEED", "LINES", "SCORE"), font = 2)
    text(-2.5, c(1, 4, 7), c("LVL  1", "0", "0"), font = 2, cex = c(1.3, 2, 2), col = "orange")
  }
  rect(c(-0.1, -0.1), c(-0.1, 20.1), c(10.1, 10.1), c(20.1, 21.1), col = c("grey10", "grey70"), lwd = 3)
  segments(0:10, 0, 0:10, 20, col = "grey30", lwd = 2)
  segments(0, 0:20, 10, 0:20, col = "grey30", lwd = 2)
  text(5.5, 20.55, "Tetris  Field", font = 2)
}

# Hold Area -----------------------------

hold_area <- function(env){
  ## exchange type.cur and type.store
  tmp <- env$type.cur
  env$type.cur <- env$type.hold
  env$type.hold <- tmp
  env$mat[t(env$pos.next)] <- "" ## Warning
  env$dir <- ""
  env$base <- c(5, 21)
  env$vec <- tetris_global$VEC_LIST[[env$type.cur]]
  env$shift <- TRUE

  ## display on the "hold" area
  x <- tetris_global$VEC_LIST[[env$type.hold]]
  r <- 0.6
  x.scaling <- x * r ## contraction
  dist <- c(-2.5, 18.5) - rowMeans(x.scaling)
  x.new <- x.scaling + dist
  offset <- 0.5 * r
  rect(-4, 17, -1, 20, col = "grey30", lwd = 3)
  rect(x.new[1, ] - offset, x.new[2, ] - offset, x.new[1, ] + offset, x.new[2, ] + offset,
       col = env$color$BLOCK[env$type.hold], lwd = 2)
}

# Clear lines -----------------------------

clear_lines <- function(env, nhint){
  if(env$stop){
    line.ind <- which(colSums(env$mat != "") == 10)
    nline <- length(line.ind)
    if(nline > 0){
      env$combo <- env$combo + 1
      if(env$combo > 0){
        rect(-4, 11, -1, 12, col = env$color$BG, border = NA)
        text(-2.5, 11.5, env$combo, font = 2, cex = 2, col = "red3")
      }

      line.pre <- env$line
      env$line <- env$line + nline

      if(nline >= 4){
        env$score <- env$score + 8
      } else if(nline >= 3){
        env$score <- env$score + 4
      } else if (nline >= 2){
        env$score <- env$score + 2
      } else {
        env$score <- env$score + 1
      }

      if(env$combo >= 7){
        env$score <- env$score + 4
      } else if(env$combo >= 5){
        env$score <- env$score + 3
      } else if (env$combo >= 3){
        env$score <- env$score + 2
      } else if (env$combo >= 1){
        env$score <- env$score + 1
      }

      if(env$speed < 9 & env$line %/% 10 - line.pre %/% 10 == 1){
        env$speed <- env$speed + 1
        rect(-4, 0, -1, 2, col = "grey10", lwd = 3)
        text(-2.5, 1, paste0("LVL  ", env$speed), font = 2, cex = 1.3, col = "orange")
      }

      env$mat <- cbind(env$mat[, -line.ind], matrix("", 10, nline))
      ind <- which(env$mat[, 1:20] != "", arr.ind = T)
      draw_table(env, nhint)
      rect(ind[, 1] - 1 + 0.1, ind[, 2] - 1 + 0.1, ind[, 1] - 0.1, ind[, 2] - 0.1,
           col = env$color$BLOCK[env$mat[ind]], lwd = 2)

      ## display the score
      rect(-4, c(3, 6), -1, c(5, 8), col = "grey10", lwd = 3)
      text(-2.5, c(4, 7), c(env$line, env$score), font = 2, cex = 2, col = "orange")

    } else {
      if(env$combo >= 0){
        env$combo <- -1
        rect(-4, 11, -1, 12, col = env$color$BG, border = NA)
        text(-2.5, 11.5, "0", font = 2, cex = 2, col = "red3")
      }
    }
  }
}

# Determine whether the game is over
# and when to resample a new block -----------------------------

resample <- function(env, nhint){
  if(is.na(env$type.cur)){
    if(nhint == 0){
      env$type.cur <- sample(names(tetris_global$VEC_LIST), 1, prob = tetris_global$PROB)
    } else {
      env$type.cur <- env$type.hint[1]
      env$type.hint <- c(env$type.hint[-1], sample(names(tetris_global$VEC_LIST), 1, prob = tetris_global$PROB))

      ## display on the "next" area
      center.list <- list(c(12.5, 18.5), c(12.375, 15.125), c(12.25, 12), c(12.125, 9.125), c(12, 6.5))
      x.list <- tetris_global$VEC_LIST[env$type.hint]
      r.list <- seq(0.7, by = -0.075, len = 5)

      rect(11, c(17.00, 13.75, 10.75, 8.00, 5.50)[1:nhint],
           seq(14, by = -0.25, len = 5)[1:nhint],
           c(20.00, 16.50, 13.25, 10.25, 7.50)[1:nhint],
           col = "grey30", lwd = 3)

      Map(function(x, type, r, center){
        x.scaling <- x * r ## contraction
        dist <- center - rowMeans(x.scaling)
        x.new <- x.scaling + dist
        offset <- 0.5 * r
        rect(x.new[1, ] - offset, x.new[2, ] - offset, x.new[1, ] + offset, x.new[2, ] + offset,
             col = env$color$BLOCK[type], lwd = 2)
      }, x.list, env$type.hint, r.list[1:nhint], center.list[1:nhint])
    }

    env$dir <- ""
    if(env$type.cur %in% c('S', 'Z')) env$base <- c(5, 22)
    else env$base <- c(5, 21)
    env$vec <- tetris_global$VEC_LIST[[env$type.cur]]
    env$shift <- FALSE

    if(any(env$mat[t(ceiling(env$vec + env$base))] != ""))
      return(TRUE)
  }
  return(FALSE)
}

position <- function(env){

  env$pos.prev <- env$pos.next
  env$proj.prev <- env$proj

  if(env$dir == "down")  tmp.base <- env$base + c(0, -1)
  else if(env$dir == "left")  tmp.base <- env$base + c(-1, 0)
  else if(env$dir == "right") tmp.base <- env$base + c(1, 0)
  else tmp.base <- env$base

  if(env$dir == "up") tmp.vec <- tetris_global$ROTATEMAT %*% env$vec
  else tmp.vec <- env$vec

  tmp.pos <- ceiling(tmp.vec + tmp.base) ## treat 'O' and 'I' with ceiling function
  tmp.mat <- env$mat
  tmp.mat[t(env$pos.prev)] <- ""
  out_of_bounds <- tmp.pos[1, ] < 1 | tmp.pos[1, ] > 10 | tmp.pos[2, ] < 1
  in_bounds <- tmp.pos[, !out_of_bounds, drop = F] # Warning: drop = F

  if(any(out_of_bounds) | any(tmp.mat[t(in_bounds)] != "")){
    env$no_action <- TRUE
  } else {
    env$pos.next <- tmp.pos
    env$base <- tmp.base
    env$vec <- tmp.vec
    env$no_action <- FALSE
  }
}

project_down <- function(env){
  bottom <- as.matrix(aggregate(env$pos.next[2, ] ~ env$pos.next[1, ], FUN = min))
  dist <- apply(bottom, 1, function(row){
    x <- row[1] ; y <- row[2]
    if(y > 1){
      ind <- which(env$mat[x, 1:(y-1)] != "")
      return(y - ifelse(length(ind) == 0, 1, max(ind)+1))
    } else return(0)
  })
  env$proj <- env$pos.next - c(0, min(dist))
  ind <- env$proj[, env$proj[2, ] >= 1 & env$proj[2, ] <= 20, drop = F] ## Warning : drop = F
  ind.prev <- env$proj.prev[, env$proj.prev[2, ] >= 1 & env$proj.prev[2, ] <= 20, drop = F]
  if(!env$stop)
    rect(ind.prev[1, ] - 1, ind.prev[2, ] - 1, ind.prev[1, ], ind.prev[2, ],
         col = "grey10", border = "grey30", lwd = 2)
  rasterImage(projection_style, ind[1, ] - 1 + 0.1, ind[2, ] - 1 + 0.1, ind[1, ] - 0.1, ind[2, ] - 0.1)
}

draw_matrix <- function(env){
  ind <- env$pos.next[, env$pos.next[2, ] >= 1 & env$pos.next[2, ] <= 20, drop = F] ## Warning : drop = F
  ind.prev <- env$pos.prev[, env$pos.prev[2, ] >= 1 & env$pos.prev[2, ] <= 20, drop = F]
  if(!env$stop)
    rect(ind.prev[1, ] - 1, ind.prev[2, ] - 1, ind.prev[1, ], ind.prev[2, ],
         col = "grey10", border = "grey30", lwd = 2)
  rect(ind[1, ] - 1 + 0.1, ind[2, ] - 1 + 0.1, ind[1, ] - 0.1, ind[2, ] - 0.1,
       col = env$color$BLOCK[env$type.cur], lwd = 2)
  env$stop <- FALSE
}

# Stage -----------------------------

.stage0 <- function(env, nhint){
  env$stage <- 0
  draw_table(env, nhint)
  text(5, 10, "Any Buttons to Start", col = "gold", font = 2, cex = 1.5)
}

.stage1 <- function(env, nhint){
  clear_lines(env, nhint)
  if(!resample(env, nhint)){
    position(env)
    if(!env$no_action){
      ## update env$mat
      if(!env$stop) env$mat[t(env$pos.prev)] <- ""
      env$mat[t(env$pos.next)] <- env$type.cur
      if(env$dir != "down") project_down(env)
      draw_matrix(env)
    }
  } else {
    .stage2(env)
  }
}

.stage2 <- function(env){
  env$stage <- 2
  draw_table(env)
  text(5, 12, "Game Over", col = "red", font = 2, cex = 2)
  text(5, 8, "Space to Restart\nQ to Quit", col = "white", font = 2, cex = 1.7)
}
