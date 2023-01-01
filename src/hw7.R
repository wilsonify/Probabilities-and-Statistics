# ---
# jupyter:
#   jupytext:
#     text_representation:
#       extension: .R
#       format_name: light
#       format_version: '1.5'
#       jupytext_version: 1.14.4
#   kernelspec:
#     display_name: R
#     language: R
#     name: ir
# ---

library(tidyverse)

# Suppose that an accounting firm does a study to determine the time needed to complete one person's tax forms. 
#
# It randomly surveys 150 people. 
#
# The sample mean is 22.3 hours. 
#
# There is a known population standard deviation of 6.8 hours. 
#
# The population distribution is assumed to be normal.

# Part (a)
#
# Find $\bar{x}$,$\sigma$,n. (Enter exact numbers as integers, fractions, or decimals.)

x_bar=22.3
sigma=6.8
n=150

# Part (b)
# In words, define the random variables X and $\bar{X}$
#
#
# X is the time needed to complete one person's tax forms, and $\bar{X}$ is the mean time needed to complete tax forms from a sample of 150 customers. 

# Part (c)
# Which distribution should you use for this problem? (Round your answers to two decimal places.)
# X ~
# Explain your choice.
#
# The standard normal distribution should be used because the population standard deviation is known. 

(sigma/sqrt(n)) %>% round(2)

# Part (d)
#
# Construct a 90% confidence interval for the population mean time to complete the tax forms.
# (i) State the confidence interval. (Round your answers to two decimal places.)

CL=0.90
alpha=1-CL
half_alpha=alpha/2
half_alpha
z=qnorm(1-half_alpha)
error_bound=z*sigma/sqrt(n)
x_bar-error_bound %>% round(2)
x_bar+error_bound %>% round(2)
error_bound %>% round(2)

# (ii) Sketch the graph. (Round your answers to two decimal places.)

curve(dnorm(x,x_bar,sigma/sqrt(n)),from = x_bar-3*sigma/sqrt(n),to =x_bar+3*sigma/sqrt(n) )
abline(v =x_bar-error_bound)
abline(v=x_bar+error_bound)

# (iii) Calculate the error bound. (Round your answer to two decimal places.)

error_bound=z*sigma/sqrt(n)
error_bound %>% round(2)

# Part (e)
#
# If the firm wished to increase its level of confidence and keep the error bound the same by taking another survey, what change should it make?
#
# It should increase the number of people surveyed. 

# Part (f)
#
# If the firm did another survey, kept the error bound the same, and only surveyed 49 people, what would happen to the level of confidence? Why?
#
# The level of confidence would be smaller because we have collected a smaller sample, obtaining less accurate information. 

# Part (g)
# Suppose that the firm decided that it needed to be at least 96% confident of the population mean length of time to within one hour. 
#
# How would the number of people the firm surveys change? Why?
#
# The number of people surveyed would increase because more accurate information requires a larger sample.


