# Keyboard Event  -----------------------------

.keydown <- function(key, env, nhint, speed){
  lkey <- tolower(key)

  if(env$stage == 0){
    init(env, nhint, speed)
    env$stage <- 1
    draw_table(env)
    .stage1(env, nhint)
    return(NULL)
  }

  if(env$stage == 2){
    if(lkey == 'q') dev.off()
    if(lkey == ' ') .stage0(env, nhint)
  }

  if(env$stage == 1){

    if(lkey == 'q') {
      env$stage <- 2
      .stage2(env)
    }

    if(lkey == 'p'){
      gra <- getGraphicsEventEnv()

      getGraphicsEvent(
        prompt = 'Paused: P to resume',
        onKeybd = function(key)
          if (tolower(key) == 'p')
            getGraphicsEvent(
              prompt = gra$prompt,
              onKeybd = gra$onKeybd,
              onIdle = gra$onIdle
            )
      )
    }

    if(lkey %in% c("up", "down", "left", "right")){
      ## override original direction
      env$dir <- lkey
      if(lkey == "down"){
        if(all(env$pos.next == env$proj)){
          env$stop <- TRUE
          env$type.cur <- NA_character_
        }
      }
      .stage1(env, nhint)
    }

    if(lkey == "a"){
      if(!env$shift) hold_area(env)
      else return(NULL)
      .stage1(env, nhint)
    }

    if(lkey == ' '){
      env$pos.prev <- env$pos.next
      env$pos.next <- env$proj
      env$mat[t(env$pos.prev)] <- ""
      env$mat[t(env$pos.next)] <- env$type.cur
      draw_matrix(env)
      env$stop <- TRUE
      env$type.cur <- NA_character_
      .stage1(env, nhint)
      return(NULL)
    }
  }
  # Console Check
  # cat("keydown:", sprintf("%5s", key), ", stage:", env$stage,
  #     ", type:", env$type.cur, ", score:", env$score, "\n", sep = "")
}

# Idle Event  -----------------------------
# The idle event handler will be called whenever the event queue
# of the graphics device was found to be empty, i.e. in an infinite loop.

.idle <- function(env, nhint){
  # cat("loop is running", format(Sys.time(), format = "%H:%M:%OS2"), "\n")
  if(env$stage == 1){
    if(Sys.time() - env$time >= 1.1 - env$speed * 0.1){
      env$time <- Sys.time()
      .keydown("Down", env, nhint)
    }
  }
}
