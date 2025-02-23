library(plotly)
library(RColorBrewer)
library(spdm)

xstd <- seq(0, 1.5, length.out = 10)
ystd <- seq(0, 1.5, length.out = 10)

grid <- expand.grid(x = xstd, y = ystd)
grid$r <- grid$x * grid$y

# Start and end points
start <- matrix(c(.4, .4, 
                  .4, 1.75), 
                byrow = T, nrow = 2, ncol = 2)
end <- matrix(c(1.5, -.3, 
                -.3, .5), 
              byrow = T, nrow = 2, ncol = 2)

# Euclidean and Riemannian interpolation
interp.euclid <- t(sapply(seq(0, 1, .1), function(t) {
    m <- spd.interpolate(start, end, t, method = 'euclidean')
    return(m[lower.tri(m, diag = T)])
}))
interp.rieman <- t(sapply(seq(0, 1, .1), function(t) {
    m <- spd.interpolate(start, end, t, method = 'riemannian')
    return(m[lower.tri(m, diag = T)])
}))

plot_ly() %>%
    layout(scene = list(xaxis = list(title = 'Var(x)'), 
                        yaxis = list(title = 'Var(y)'),
                        zaxis = list(title = 'Cov(x,y)'))) %>%
    
    # Degenerate surface
    add_trace(x = ~grid$x^2, y = ~grid$y^2, z = ~grid$r,
              type = 'mesh3d', opacity = .5,
              intensity = ~(grid$r), colorscale = 'Greys') %>%
    add_trace(x = ~grid$x^2, y = ~grid$y^2, z = ~-grid$r,
              type = 'mesh3d', opacity = .5,
              intensity = ~(grid$r), colorscale = 'Greys') %>%
    
    # Add correlation matrices
    add_trace(x = ~interp.euclid[,1], y = ~interp.euclid[,3], z = ~interp.euclid[,2], 
              type = 'scatter3d', mode = 'lines',
              line = list(color = 'darkblue', width = 4)) %>%
    add_trace(x = ~interp.rieman[,1], y = ~interp.rieman[,3], z = ~interp.rieman[,2], 
              type = 'scatter3d', mode = 'lines',
              line = list(color = 'darkorange', width = 4)) %>%
    
    hide_colorbar() %>% layout(showlegend = FALSE)

