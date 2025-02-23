colpal <- c('#d95f02','#7570b3','#e7298a','#66a61e')
isodistpoints <- do.call(rbind, isodist)

pl.sphere <- plot_ly() %>%
    layout(scene = list(xaxis = list(title = 'Var(x)'), 
                        yaxis = list(title = 'Var(y)'),
                        zaxis = list(title = 'Cov(x,y)'))) %>%
    
    # Cone surface
    add_trace(x = ~cone$x^2, y = ~cone$y^2, z = ~cone$r,
              type = 'mesh3d', opacity = .5,
              intensity = ~(cone$r), colorscale = 'Greys') %>%
    add_trace(x = ~cone$x^2, y = ~cone$y^2, z = ~-cone$r,
              type = 'mesh3d', opacity = .5,
              intensity = ~(cone$r), colorscale = 'Greys') %>%
    
    # Interpolation paths
    add_trace(x = ~examplepoints[,1], y = ~examplepoints[,3], z = ~examplepoints[,2], 
              type = 'scatter3d', mode = 'markers',
              marker = list(color = 'black', size = 6)) %>%
    
    # Add spheres
    add_trace(x = ~isodistpoints[,1], y = ~isodistpoints[,3], z = ~isodistpoints[,2], 
              type = 'scatter3d', mode = 'markers', opacity = .5,
              marker = list(color = rep(colpal, each = nrow(isodist[[1]])), size = 3)) %>%
    
    hide_colorbar() %>% layout(showlegend = FALSE)