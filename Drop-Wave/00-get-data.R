library(tidyverse)
library(pbapply)
library(parallel)

# Script from: https://www.sfu.ca/~ssurjano/Code/dropr.html
source("drop-wave.R") 

# Define parameters for iterations ---------------------------------------------
step_size = 0.005
init_size = 0.5 

res = 50 # resolution

pboptions(type = "timer")

cl = makeForkCluster(detectCores())

# Collect the data for the gif -------------------------------------------------
data_list = tibble(
  seq(-init_size, -2, by = -step_size),
  seq(init_size, 2, by = step_size)
    ) %>% 
  pbapply(cl = cl, MARGIN = 1, FUN = function(row) {
    index1 = row[1] %>% as.numeric()
    index2 = row[2] %>% as.numeric()
    
    x1 = x2 = seq(from = index1, to = index2, length.out = res)
    
    df = expand_grid(x1 = x1, x2 = x2)
    df$y = apply(df, 1, drop)
    
    return(df)
  })
  
stopCluster(cl)

saveRDS(data_list, "data-list.RDS")
