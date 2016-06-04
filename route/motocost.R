library(MASS, pos="package:base")
library(memisc, pos="package:base")
motocost <- function(tv) cases((tv<=1) -> 2,
                               (tv>1 & tv<=3) -> 3,
                               (tv> 3 & tv <=7) -> 6,
                               (tv> 7 & tv <=10) -> 9,
                               (tv > 10 & tv <= 15) -> 15,
                               (tv > 15) -> NA)