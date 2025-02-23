library(plyr)
library(ggplot2)

m <- transported.cov.rieman[[1]]

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

df.sample <- data.frame(Observation = rep(1:5, 3),
                        Condition = rep(c('Source', 'Target (Riemannian)', 
                                          'Target (Euclidean)'), each = 5))
df.sample$Cov <- c(sample.cov, transported.cov.rieman, transported.cov.euclid)
df.sample$Ref <- rep(list(start, end, end), each = 5)
df.sample <- ddply(df.sample, c('Observation', 'Condition'), mutate,
                   'Dist' = spd.dist(Cov[[1]], Ref[[1]], method = 'riemannian'),
                   'Det' = det(Cov[[1]]))

df.ellipse <- ddply(df.sample, c('Observation', 'Condition'), summarize,
                    'x' = drawEllipse(Cov[[1]])$x, 'y' = drawEllipse(Cov[[1]])$y)

ggplot(df.ellipse, aes(x = x, y = y, group = Observation,
                       color = as.factor(Observation))) +
    facet_grid(Observation ~ Condition) + theme_classic() +
    geom_path() +
    theme(legend.position = 'none')
