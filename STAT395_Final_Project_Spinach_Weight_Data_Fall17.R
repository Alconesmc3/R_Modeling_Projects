## Spinach Weight Data 
## Linear Model


library(readr)
weight <- read_csv("~/Desktop/ucsc/spinach_weight.csv")
View(spinach_weight)

attach(weight)
names(weight)

new_group = ifelse(group == 1 | group == 4 ,1,0)

new_group

#Create model for weight versus what group they are in
wt2grouplm <- lm(grams ~ new_group)
summary(wt2grouplm)
plot(new_group, grams,xlab= "groups(Control = 0, Treatment = 1)", ylab = "grams",
     main = "Grams for Each Group")
abline(wt2grouplm, col = "red")
summary(grams)
