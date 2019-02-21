## ADB
## Feb 20, 2019

## Function to standardise vector of values

std <- function(x){
  (x - mean(x, na.rm = T)) / sd(x, na.rm = T)
}
