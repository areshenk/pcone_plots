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
    
    # Upper cone segment
    add_trace(x = ~grid$x^2, y = ~grid$y^2, z = ~grid$r,
              type = 'mesh3d', opacity = .5,
              intensity = ~(grid$r), colorscale = 'Reds') %>%
    
    # Lower cone segment
    add_trace(x = ~grid$x^2, y = ~grid$y^2, z = ~-grid$r,
              type = 'mesh3d', opacity = .5,
              intensity = ~-(grid$r), colorscale = 'Blues') %>%
    
    # Add correlation matrices
    add_trace(x = ~c(1,1), y = ~c(1,1), z = ~c(-1,1), 
              type = 'scatter3d', mode = 'lines',
              line = list(color = 'black', dash = "dash", width = 4)) %>%
    
    # Add isotropic matrices
    add_trace(x = ~c(0,1.5), y = ~c(0,1.5), z = ~c(0,0), 
              type = 'scatter3d', mode = 'lines',
              line = list(color = 'black', width = 4)) %>% 
    
    hide_colorbar() %>% layout(showlegend = FALSE)
