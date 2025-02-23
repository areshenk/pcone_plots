pl.transport <- plot_ly() %>%
    layout(scene = list(xaxis = list(title = 'Var(x)'), 
                        yaxis = list(title = 'Var(y)'),
                        zaxis = list(title = 'Cov(x,y)'))) %>%
    
    # Degenerate surface
    add_trace(x = ~cone$x^2, y = ~cone$y^2, z = ~cone$r,
              type = 'mesh3d', opacity = .5,
              intensity = ~(cone$r), colorscale = 'Greys') %>%
    add_trace(x = ~cone$x^2, y = ~cone$y^2, z = ~-cone$r,
              type = 'mesh3d', opacity = .5,
              intensity = ~(cone$r), colorscale = 'Greys') %>%
    
    # Add correlation matrices
    add_trace(x = ~coords.interp.euclid[,1], y = ~coords.interp.euclid[,3], z = ~coords.interp.euclid[,2], 
              type = 'scatter3d', mode = 'lines',
              line = list(color = 'darkblue', width = 4)) %>%
    add_trace(x = ~coords.interp.rieman[,1], y = ~coords.interp.rieman[,3], z = ~coords.interp.rieman[,2], 
              type = 'scatter3d', mode = 'lines',
              line = list(color = 'darkorange', width = 4)) %>%
    
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
              marker = list(size = 4, color = "darkorange")) %>%
    add_trace(x = ~coords.transp.euclid[,1], y = ~coords.transp.euclid[,3], z = ~coords.transp.euclid[,2], 
              type = 'scatter3d', mode = 'markers',
              marker = list(size = 4, color = "blue")) %>%
    
    hide_colorbar() %>% layout(showlegend = FALSE)
