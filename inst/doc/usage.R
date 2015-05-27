## ----tidy=FALSE----------------------------------------------------------
library(biogrid)
# interactions table is returned by default
interactions <- biogrid()             # Interactions
organisms    <- biogrid('organisms')  # NCBI organism IDs
systems      <- biogrid('systems')    # Experimental systems
log          <- biogrid('log')        # Record of all downloads

## ----message=FALSE-------------------------------------------------------
library(dplyr)

# I want the NCBI ID for S. cerevisiae!
yeastId <- organisms %>% 
  filter(grepl('cerevisiae', organism)) %>% 
  select(id) %>%
  as.integer

# Now I want all yeast interactions where a or b is 'CTF4'
example1 <- biogrid(collect = FALSE) %>%
  filter(a == 'CTF4' | b == 'CTF4',
         a_organism == yeastId,
         b_organism == yeastId) %>%
  select(id, a, b) %>%
  collect
example1

