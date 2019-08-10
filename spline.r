pts <- matrix(
  c(
    0, 0,
    0, 1,
    1, 0,
    1, 1,
    2, 0,
    2, -1
    ),
  nrow = 6,
  ncol = 2,
  byrow = TRUE)

basis <- function(n, v, t) {
  return(choose(n, v) * (t^v) * (1-t)^(n-v))
}

npts <- nrow(pts)/2

curvex <- function(t) {
  i <- 1
  if(t %% 1 == 0) {
    return(pts[t*2+1,i])
  } else {
    offs <- t %/% 1 * 2 + 1
    controls <- c(pts[offs, i],
                  pts[offs+1, i],
                  2*pts[offs+2, i] - pts[offs+3, i],
                  pts[offs+2, i])
    return(sum(controls * basis(3, 0:3, t %% 1)))
  }
}
curvey <- function(t) {
  i <- 2
  if(t %% 1 == 0) {
    return(pts[t*2+1,i])
  } else {
    offs <- t %/% 1 * 2 + 1
    controls <- c(pts[offs,i],
                  pts[offs+1, i],
                  (2*pts[offs+2, i]) - pts[offs+3, i],
                  pts[offs+2, i])
    return(sum(controls * basis(3, 0:3, t %% 1)))
  }
}

path <- data.frame(t=seq(0, npts - 1, by=0.05))

path$x = sapply(path$t, curvex)
path$y = sapply(path$t, curvey)

with(path, plot(x,y, type="l", asp=1))

lines(pts[,1], pts[,2], type="b", col="darkgray")