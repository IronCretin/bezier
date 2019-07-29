p0 <- c(0, 0)
p1 <- c(0, 1)
p2 <- c(1, 0)

#curve <- function(t) p0 + 2*(p1 - p0)*t + (p2 - 2*p1 + p0)*t^2
curvex <- function(t) p0[1] + 2*(p1[1] - p0[1])*t + (p2[1] - 2*p1[1] + p0[1])*t^2
curvey <- function(t) p0[2] + 2*(p1[2] - p0[2])*t + (p2[2] - 2*p1[2] + p0[2])*t^2

path <- data.frame(t=seq(0, 1, by=0.05))

path$x = curvex(path$t)
path$y = curvey(path$t)

with(path, plot(x,y, type="l"))
points(p0[1], p0[2])
points(p1[1], p1[2])
points(p2[1], p2[2])