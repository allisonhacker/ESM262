

speeds <- runif(min = 0, max = 100, n = 300)

sum_speed=0

for (i in 1:length(speeds)){
  sum_speed = sum_speed + speeds[i]
  mean_speed = sum_speed/i
}

mean_speed

##############
sum_speed = 0

for (i in 1:length(speeds)){
  sum_speed = sum_speed + speeds[i]
}

mean <- sum_speed/length(speeds)
