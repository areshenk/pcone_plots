library(plotly)
library(RColorBrewer)
library(spdm)

set.seed(1)

xstd <- seq(0, 1.5, length.out = 10)
ystd <- seq(0, 1.5, length.out = 10)

grid <- expand.grid(x = xstd, y = ystd)
grid$r <- grid$x * grid$y

# Initial distribution of points
n <- 5
start <- matrix(c(.4, .4, 
                  .4, 1.75), 
                byrow = T, nrow = 2, ncol = 2)
end <- matrix(c(1.5, -.3, 
                -.3, .5), 
              byrow = T, nrow = 2, ncol = 2)

sample.cov <- matrix(rnorm(3*n, 0, .1), ncol = 3)
sample.cov <- scale(sample.cov, center = T, scale = F)
sample.cov <- lapply(1:n, function(i) {
    v <- sample.cov[i,]
    m <- spd.vectorize(v)
    spd.expmap(m, p = start)
})

# Transported covariance matrices
transported.cov.rieman <- lapply(sample.cov, spd.translate, from = start, to = end)
transported.cov.euclid <- lapply(sample.cov, function(m) m - start + end)

# Interpolation
interp.euclid <- lapply(seq(0, 1, .1), function(t) {
    m <- spd.interpolate(start, end, t, method = 'euclidean')
})
interp.rieman <- lapply(seq(0, 1, .1), function(t) {
    m <- spd.interpolate(start, end, t, method = 'riemannian')
})

# Coordinates
coords.sample <- t(sapply(sample.cov, function(m) m[lower.tri(m, diag = T)]))
coords.transp.rieman  <- t(sapply(transported.cov.rieman, function(m) m[lower.tri(m, diag = T)]))
coords.transp.euclid <- t(sapply(transported.cov.euclid, function(m) m[lower.tri(m, diag = T)]))
coords.interp.euclid  <- t(sapply(interp.euclid, function(m) m[lower.tri(m, diag = T)]))
coords.interp.rieman  <- t(sapply(interp.rieman, function(m) m[lower.tri(m, diag = T)]))

plot_ly() %>%
    layout(scene = list(xaxis = list(title = 'Var(x)'), 
                        yaxis = list(title = 'Var(y)'),
                        zaxis = list(title = 'Cov(x,y)'))) %>%
    
    # Degenerate surface
    add_trace(x = ~grid$x^2, y = ~grid$y^2, z = ~grid$r,
              type = 'mesh3d', opacity = .5,
              intensity = ~(grid$r), colorscale = 'Reds') %>%
    add_trace(x = ~grid$x^2, y = ~grid$y^2, z = ~-grid$r,
              type = 'mesh3d', opacity = .5,
              intensity = ~(grid$r), colorscale = 'Reds') %>%
    
    # Add correlation matrices
    add_trace(x = ~coords.interp.euclid[,1], y = ~coords.interp.euclid[,3], z = ~coords.interp.euclid[,2], 
              type = 'scatter3d', mode = 'lines',
              line = list(color = 'darkblue', width = 4)) %>%
    add_trace(x = ~coords.interp.rieman[,1], y = ~coords.interp.rieman[,3], z = ~coords.interp.rieman[,2], 
              type = 'scatter3d', mode = 'lines',
              line = list(color = 'darkred', width = 4)) %>%
    
    # Add initial distribution
    add_trace(x = ~start[1,1], y = ~start[2,2], z = ~start[1,2], 
              type = 'scatter3d', mode = 'markers',
              marker = list(size = 5, color = "black")) %>%
    add_trace(x = ~coords.sample[,1], y = ~coords.sample[,3], z = ~coords.sample[,2], 
              type = 'scatter3d', mode = 'markers',
              marker = list(size = 3, color = "black")) %>%
    
    # Add parallel transport
    add_trace(x = ~end[1,1], y = ~end[2,2], z = ~end[1,2], 
              type = 'scatter3d', mode = 'markers',
              marker = list(size = 5, color = "black")) %>%
    add_trace(x = ~coords.transp.rieman[,1], y = ~coords.transp.rieman[,3], z = ~coords.transp.rieman[,2], 
              type = 'scatter3d', mode = 'markers',
              marker = list(size = 3, color = "darkred")) %>%
    add_trace(x = ~coords.transp.euclid[,1], y = ~coords.transp.euclid[,3], z = ~coords.transp.euclid[,2], 
              type = 'scatter3d', mode = 'markers',
              marker = list(size = 3, color = "darkblue")) %>%
    
    hide_colorbar() %>% layout(showlegend = FALSE)
