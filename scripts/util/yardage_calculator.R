yardMapper <- function(coord1, coord2, field, x_yards=110, y_yards=40) {
  x <- yardXMapper(coord1[1], coord2[1], field, x_yards)
  y <- yardYMapper(coord1[2], coord2[2], field, y_yards)
  sqrt(x^2 + y^2)
}
yardXMapper <- function(x1, x2, field, x_yards=110) {
  x <- (x2[1]-x1[1])/field$x_length * x_yards
  x
}
yardYMapper <- function(y1, y2, field, y_yards=40) {
  y <- (y2[1]-y1[1])/field$y_length * y_yards
  y
}