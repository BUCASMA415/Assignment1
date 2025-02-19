#####

# Discussion 3

# Name: Aidan Patt
# Wed Feb 12 14:33:49 2025 ------------------------------


flower = read.table(file = 'data/flower.txt',
                    header = TRUE, 
                    sep = "\t",
                    stringsAsFActors = TRUE)

# header = TRUE, tells first row is a header
# sep tells type of seperator

library(tidyverse)
library(palmerpenguins)

plot(penguins$bill_depth_mm) # scatter plot of  value against index

# many baese R dont have data = argument
# but can use the with() function with plot() as a shortcut

with(penguins, plot(bill_depth_mm))


# from book
my_x = 1:10
my_y = seq(1,20,2)

par(mfrow = c(1,1))

plot(my_x, my_y, type = "l")
plot(my_x, my_y, type = "b")
plot(my_x, my_y, type = "o")
plot(my_x, my_y, type = "c")


# histogram and density plot

# generate data
x = rnorm(100, 50, 5)
dens = density(x)
hist(x, freq = FALSE)
lines(dens)


# Boxplots
colnames(penguins)
pairs(penguins[, colnames(penguins)])

pairs(penguins[, c('bill_length_mm', 'bill_depth_mm', 'flipper_length_mm',
                   'body_mass_g')])


# co plot for isolating, 
# when feel like 3rd variable that could affect other two
# use to isolate the relationship without hte thrd

coplot(penguins$body_mass_g ~ penguins$bill_length_mm | penguins$bill_depth_mm)


#save('1stcoplot.png')

#?save()
#save.image('1stcoplot.png')


###### HW #######
# 1. make a boxplot
library(ggplot2)
ggplot(penguins, aes(species, bill_length_mm)) +
  geom_boxplot()

# 2. Use ggplot and make a plot with a legend
ggplot(penguins, aes(species, bill_length_mm)) +
  geom_col(mapping = aes(color = species)) 
 


  
