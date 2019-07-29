pts <- matrix(c(0, 0,
                0, 1,
                1,-1,
                1, 0),
              nrow = 4,
              ncol = 2,
              byrow = TRUE)

cvt <- matrix(c(1, 0, 0, 0,
               -3, 3, 0, 0,
                3,-6, 3, 0,
               -1, 3,-3, 1),
              nrow = 4,
              ncol = 4,
              byrow = TRUE)

coeffs <- cvt %*% pts

curvex <- function(t) coeffs[1,1] + coeffs[2,1]*t + coeffs[3,1]*t^2 + coeffs[4,1]*t^3
curvey <- function(t) coeffs[1,2] + coeffs[2,2]*t + coeffs[3,2]*t^2 + coeffs[4,2]*t^3

path <- data.frame(t=seq(0, 1, by=0.05))

path$x = curvex(path$t)
path$y = curvey(path$t)

with(path, plot(x,y, type="l"))