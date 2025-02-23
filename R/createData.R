set.seed(1337)

# Generate boundary of cone
std.limit <- 1.5
xstd <- seq(0, std.limit, length.out = 10)
ystd <- seq(0, std.limit, length.out = 10)
cone <- expand.grid(x = xstd, y = ystd)
cone$r <- cone$x * cone$y

# Start and end points for transport/interpolation
start <- matrix(c(.5, -.3, 
                  -.3, 1.5), byrow = T, nrow = 2, ncol = 2)
end <- matrix(c(2, -.4, 
                -.4, .5), byrow = T, nrow = 2, ncol = 2)

# Interpolate between start and end points
interp.euclid <- lapply(seq(0, 1, .1), function(t) 
    spd.interpolate(start, end, t, method = 'euclidean'))
interp.rieman <- lapply(seq(0, 1, .1), function(t) 
    spd.interpolate(start, end, t, method = 'riemannian'))
coords.interp.euclid <- t(sapply(interp.euclid, function(m) m[lower.tri(m, diag = T)]))
coords.interp.rieman <- t(sapply(interp.rieman, function(m) m[lower.tri(m, diag = T)]))

# Sample covariance matrices around the initial point. Here, we simply
# generate centered normal random vectors in the tangent space, and then 
# exponential map onto the positive cone
n <- 5
sample.cov <- matrix(rnorm(3*n, 0, .15), ncol = 3)
sample.cov <- scale(sample.cov, center = T, scale = F)
sample.cov <- lapply(1:n, function(i) {
    v <- sample.cov[i,]
    m <- spd.vectorize(v)
    spd.expmap(m, p = start)
})
coords.sample <- t(sapply(sample.cov, function(m) m[lower.tri(m, diag = T)]))

# Transport covariance matrices to the endpoint. Either by simple translation
# (Euclidean), or by parallel transport along a geodesic (Riemannian).
transported.cov.rieman <- lapply(sample.cov, spd.translate, from = start, to = end)
transported.cov.euclid <- lapply(sample.cov, function(m) m - start + end)
coords.transp.rieman  <- t(sapply(transported.cov.rieman, function(m) m[lower.tri(m, diag = T)]))
coords.transp.euclid <- t(sapply(transported.cov.euclid, function(m) m[lower.tri(m, diag = T)]))

# Helper function to compute ellipses
drawEllipse <- function(m) {
    l <- eigen(m)$values
    if (m[1,2] == 0 & m[1,1] >= m[2,2]) {
        theta <- 0
    } else if (m[1,2] == 0 & m[1,1] < m[2,2]) {
        theta <- pi/2
    } else {
        theta <- atan2(l[1] - m[1,1], m[1,2])
    }
    t <- seq(0, 2*pi, length.out = 40)
    
    x <- sqrt(l[1]) * cos(theta)*cos(t) - sqrt(l[2]) * sin(theta)*sin(t)
    y <- sqrt(l[1]) * sin(theta)*sin(t) + sqrt(l[2]) * cos(theta)*cos(t)
    data.frame(x = x, y = y)
}

sample.ellipses <- lapply(sample.cov, drawEllipse)
euclid.ellipses <- lapply(transported.cov.euclid, drawEllipse)
rieman.ellipses <- lapply(transported.cov.rieman, drawEllipse)
start.ellipse <- drawEllipse(start)
end.ellipse <- drawEllipse(end)

# Create spherical grid in tangent space for distance visualization
tangentsphere <- fibonaccisphere(n = 500, r = .4, out.xyz = TRUE, out.sph = FALSE)
examplepoints <- list(matrix(c(.5, .3, 
                               .3, 1.5), byrow = T, nrow = 2, ncol = 2),
                      matrix(c(.5, .2, 
                               .2, .5), byrow = T, nrow = 2, ncol = 2),
                      matrix(c(1.5, 0, 
                               0, 1.5), byrow = T, nrow = 2, ncol = 2),
                      matrix(c(1.75, -1, 
                               -1, 1), byrow = T, nrow = 2, ncol = 2))
isodist <- lapply(examplepoints, function(p) {
    t(sapply(1:nrow(tangentsphere), function(i) {
        x <- spd.vectorize(tangentsphere[i,])
        v <- spd.expmap(x, p)
        return(spd.vectorize(v))
    }))
})
examplepoints <- t(sapply(examplepoints, spd.vectorize))

save(cone, start, end, sample.cov, coords.sample,
     interp.euclid, interp.rieman, drawEllipse,
     coords.interp.euclid, coords.interp.rieman,
     transported.cov.rieman, transported.cov.euclid,
     coords.transp.rieman, coords.transp.euclid, 
     sample.ellipses, euclid.ellipses, rieman.ellipses,
     start.ellipse, end.ellipse, isodist, examplepoints,
     file = 'data/datafiles.RData')
rm(list = ls())