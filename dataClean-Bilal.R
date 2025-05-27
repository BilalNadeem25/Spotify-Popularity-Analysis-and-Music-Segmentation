options(repos = c(CRAN = "https://cloud.r-project.org/"))

install.packages('naniar')
install.packages('ggplot2')
install.packages('dplyr')
install.packages("tidyr") 
library(tidyr)            
library(naniar)
library(ggplot2)
library(dplyr)

songsdf <- read.csv('30000 spotify songs.csv')

# missing value overview
vis_miss(songsdf)


