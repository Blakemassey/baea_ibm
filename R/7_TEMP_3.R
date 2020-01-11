i <- unique(move_dens$behavior_behavior)[1]
test <- move_dens[move_dens$behavior_behavior == i, ]

any(is.na(test$dens))
