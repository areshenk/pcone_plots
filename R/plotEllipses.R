# Draw ellipses for sample covariance matrices before and after transport
df.cov <- data.frame(Observation = rep(1:5, 3),
                     Sample = rep(c('Source', 'Target (Riemannian)', 
                                    'Target (Euclidean)'), each = 5),
                     Condition = rep(c('Pre', 'Post', 'Post'), each = 5))
df.cov$Cov <- c(sample.cov, transported.cov.rieman, transported.cov.euclid)
df.cov$Ref <- rep(list(start, end, end), each = 5)

df.cov <- ddply(df.cov, c('Observation', 'Condition'), mutate,
                'Dist' = spd.dist(Cov[[1]], Ref[[1]], method = 'riemannian'),
                'Det'  = det(Cov[[1]]))

df.ellipse <- ddply(df.cov, c('Observation', 'Sample', 'Condition'), summarize,
                    'x' = drawEllipse(Cov[[1]])$x, 'y' = drawEllipse(Cov[[1]])$y)
df.ellipse$Condition <- factor(df.ellipse$Condition, c('Pre', 'Post'))

# Draw ellipses for start and end points
l.startend <- list(drawEllipse(start), drawEllipse(end))
l.startend[[1]] <- cbind(l.startend[[1]], 
                         data.frame(Observation = rep(NA, nrow(l.startend[[1]])),
                                    Sample = NA, Condition = 'Pre'))
l.startend[[2]] <- cbind(l.startend[[2]], 
                         data.frame(Observation = rep(NA, nrow(l.startend[[2]])),
                                    Sample = NA, Condition = 'Post'))
df.startend <- do.call(rbind, l.startend)
df.startend <- do.call(rbind, lapply(1:5, function(i) {
    d <- df.startend
    d$Observation <- i
    return(d)
}))
df.startend$Condition <- factor(df.startend$Condition, c('Pre', 'Post'))


pl.ellipse <- ggplot(df.ellipse, aes(x = x, y = y, group = interaction(Observation, Sample),
                       color = Sample)) +
    facet_grid(Observation ~ Condition, scales = 'free') + theme_classic() +
    geom_path(data = df.startend, aes(x = x, y = y), 
              color = grey(.5), linetype = 'dashed') +
    geom_path() +
    scale_color_manual(values = c('black', 'darkblue', 'darkorange')) +
    theme(legend.position = 'bottom',
          legend.title = element_blank())
