get_field = function(fieldstring = "nexgen") {
  field <- list()
    if(fieldstring == "nexgen") {
    field$endzone_left_back <- 0
    field$endzone_left_front <- 210
    field$endzone_right_front <- 790
    field$endzone_right_back <- 1000
    field$x_length <- field$endzone_right_back - field$endzone_left_back
    field$x_mid <- field$endzone_left_back + field$x_length/2
    field$x_step <- 10
    field$y_step <- 10
    field$ob_top <- 0
    field$ob_bottom <- 360
    field$y_length <- field$ob_bottom - field$ob_top
    field$y_mid <- field$ob_top + field$y_length/2
    return(field)
  } else if(fieldstring == "ultiapps") {
    field$endzone_left_back <- 440
    field$endzone_left_front <- 2500
    field$endzone_right_front <- 7500
    field$endzone_right_back <- 9560
    field$x_length <- field$endzone_right_back - field$endzone_left_back
    field$x_mid <- field$endzone_left_back + field$x_length/2
    field$x_step <- 20
    field$y_step <- 40
    field$ob_top <- 1000
    field$ob_bottom <- 9000
    field$y_length <- field$ob_bottom - field$ob_top
    field$y_mid <- field$ob_top + field$y_length/2
    return(field)
  } else {
    stop(paste("fieldstring ==",fieldstring,"not implemented.")) 
  }
}
