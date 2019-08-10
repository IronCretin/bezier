pts <- matrix(
  c(
    0, 0,
    .25, .25,
    1, 0,
    2, 1,
    2, 0,
    2, -.5
    ),
  nrow = 6,
  ncol = 2,
  byrow = TRUE)

xsc <- c(-.5, 2.5)
ysc <- c(-1, 1)

basis <- function(n, v, t) {
  return(choose(n, v) * (t^v) * (1-t)^(n-v))
}

npts <- nrow(pts)/2

curve <- function(t) {
  if(t %% 1 == 0) {
    return(pts[t*2+1,])
  } else {
    offs <- t %/% 1 * 2 + 1
    controls <- matrix(
      c(pts[offs,],
        pts[offs+1,],
        (2*pts[offs+2,]) - pts[offs+3,],
        pts[offs+2,]),
      nrow = 4,
      ncol = 2,
      byrow = TRUE)
    bas <- matrix(basis(3, 0:3, t %% 1), nrow = 4, ncol = 2)
    return(colSums(controls * bas))
  }
}


path <- data.frame(t=seq(0, npts - 1, by=0.05))

path$x = sapply(path$t, curve)[1,]
path$y = sapply(path$t, curve)[2,]

with(path, plot(x,y, type="l", asp=1, xlim = xsc, ylim = ysc))
for (i in 1:npts) {
  offs <- i * 2 - 1
  controls <- matrix(
                c((2*pts[offs,]) - pts[offs+1,],
                  pts[offs,],
                  pts[offs+1,]),
                  nrow = 3,
                  ncol = 2,
                  byrow = TRUE)
  lines(controls[,1], controls[,2], type="b", col="darkgray")
}