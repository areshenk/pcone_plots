pl.detshells <- plot_ly() %>%
    layout(scene = list(xaxis = list(title = 'Var(x)'), 
                        yaxis = list(title = 'Var(y)'),
                        zaxis = list(title = 'Cov(x,y)'))) %>%
    
    # Degenerate surface
    add_trace(x = ~cone$x^2, y = ~cone$y^2, z = ~cone$r,
              type = 'mesh3d', opacity = .5,
              intensity = ~(cone$r), colorscale = 'Reds') %>%
    add_trace(x = ~cone$x^2, y = ~cone$y^2, z = ~-cone$r,
              type = 'mesh3d', opacity = .5,
              intensity = ~(cone$r), colorscale = 'Reds') %>%
    
    # Isosurface 2
    add_trace(x = ~cone$x^2, y = ~cone$y^2, z = ~.75*cone$r,
              type = 'mesh3d', opacity = .5,
              intensity = ~(cone$r), colorscale = 'Blues') %>%
    add_trace(x = ~cone$x^2, y = ~cone$y^2, z = ~-.75*cone$r,
              type = 'mesh3d', opacity = .5,
              intensity = ~(cone$r), colorscale = 'Blues') %>%
    
    # Isosurface 3
    add_trace(x = ~cone$x^2, y = ~cone$y^2, z = ~.5*cone$r,
              type = 'mesh3d', opacity = .5,
              intensity = ~(cone$r), colorscale = 'Greens') %>%
    add_trace(x = ~cone$x^2, y = ~cone$y^2, z = ~-.5*cone$r,
              type = 'mesh3d', opacity = .5,
              intensity = ~(cone$r), colorscale = 'Greens') %>%
    
    hide_colorbar() %>% layout(showlegend = FALSE)
