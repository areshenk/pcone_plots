library(plotly)
library(RColorBrewer)

xstd <- seq(0, 1.5, length.out = 10)
ystd <- seq(0, 1.5, length.out = 10)

grid <- expand.grid(x = xstd, y = ystd)
grid$r <- grid$x * grid$y

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
    
    # Isosurface 2
    add_trace(x = ~grid$x^2, y = ~grid$y^2, z = ~.75*grid$r,
              type = 'mesh3d', opacity = .5,
              intensity = ~(grid$r), colorscale = 'Blues') %>%
    add_trace(x = ~grid$x^2, y = ~grid$y^2, z = ~-.75*grid$r,
              type = 'mesh3d', opacity = .5,
              intensity = ~(grid$r), colorscale = 'Blues') %>%
    
    # Isosurface 3
    add_trace(x = ~grid$x^2, y = ~grid$y^2, z = ~.5*grid$r,
              type = 'mesh3d', opacity = .5,
              intensity = ~(grid$r), colorscale = 'Greens') %>%
    add_trace(x = ~grid$x^2, y = ~grid$y^2, z = ~-.5*grid$r,
              type = 'mesh3d', opacity = .5,
              intensity = ~(grid$r), colorscale = 'Greens') %>%
    
    hide_colorbar() %>% layout(showlegend = FALSE)
