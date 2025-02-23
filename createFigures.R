#plotting libraries
library(plotly)
library(ggplot2)
library(RColorBrewer)

# Data management/analysis
library(plyr)
library(spdm)

# For sampling points on a sphere
library(cooltools)

source('R/createData.R')
load('data/datafiles.RData')

source('R/plotBasicCone.R')
source('R/plotShells.R')
source('R/plotInterpolation.R')
source('R/plotTransport.R')
source('R/plotSpheres.R')
source('R/plotEllipses.R')

pl.interpolation
pl.transport
pl.ellipse

figs <- list('positivecone'  = pl.positivecone,
             'detshells'     = pl.detshells,
             'interpolation' = pl.interpolation,
             'transport'     = pl.transport,
             'sphere'        = pl.sphere,
             'ellipses'      = pl.ellipse)

saveRDS('figs', file = 'figures/figs.rds')
