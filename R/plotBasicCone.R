pl.positivecone <- plot_ly() %>%
    layout(scene = list(xaxis = list(title = 'Var(x)'), 
                        yaxis = list(title = 'Var(y)'),
                        zaxis = list(title = 'Cov(x,y)'))) %>%
    
    # Upper cone segment
    add_trace(x = ~cone$x^2, y = ~cone$y^2, z = ~cone$r,
              type = 'mesh3d', opacity = .5,
              intensity = ~(cone$r), colorscale = 'Reds') %>%
    
    # Lower cone segment
    add_trace(x = ~cone$x^2, y = ~cone$y^2, z = ~-cone$r,
              type = 'mesh3d', opacity = .5,
              intensity = ~-(cone$r), colorscale = 'Blues') %>%
    
    # Add correlation matrices
    add_trace(x = ~c(1,1), y = ~c(1,1), z = ~c(-1,1), 
              type = 'scatter3d', mode = 'lines',
              line = list(color = 'black', dash = "dash", width = 4)) %>%
    
    # Add isotropic matrices
    add_trace(x = ~c(0,1.5), y = ~c(0,1.5), z = ~c(0,0), 
              type = 'scatter3d', mode = 'lines',
              line = list(color = 'black', width = 4)) %>% 
    
    hide_colorbar() %>% layout(showlegend = FALSE)
