pts <- matrix(
  c(
    0, 0,
    0, 1,
    1, 0),
  nrow = 3,
  ncol = 2,
  byrow = TRUE)

basis <- function(n, v, t) {
  return(choose(n, v) * (t^v) * (1-t)^(n-v))
}

n <- nrow(pts)-1
i <- 0:n

curvex <- function(t) sum(pts[,1] * basis(n, i, t))
curvey <- function(t) sum(pts[,2] * basis(n, i, t))

path <- data.frame(t=seq(0, 1, by=0.05))

path$x = sapply(path$t, curvex)
path$y = sapply(path$t, curvey)

plot(pts[,1], pts[,2])
with(path, lines(x,y, type="l"))
