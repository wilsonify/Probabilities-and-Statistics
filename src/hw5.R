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

library("tidyverse")

# # Homework 5
# ## Problem 1
# Yoonie is a personnel manager in a large corporation. Each month she must review 16 of the employees. From past experience, she has found that the reviews take her approximately four hours each to do with a population standard deviation of 1.2 hours. Let X be the random variable representing the time it takes her to complete one review. Assume X is normally distributed. Let X be the random variable representing the mean time to complete the 16 reviews. Assume that the 16 reviews represent a random set of reviews.
#
# Complete the distributions. (Enter exact numbers as integers, fractions, or decimals.) 
#
#
# X ~ N(4,1.2)
#
# $\bar{X}$~ N(4,$\frac{1.2}{\sqrt{16}}$)
#

1.2/4

# ## Problem 2
# Suppose that the distance of fly balls hit to the outfield (in baseball) is normally distributed with a mean of 236 feet and a standard deviation of 46 feet. We randomly sample 49 fly balls.
#
# X ~ N(236,46)
#
# $\bar{X}$ ~ N(236,$\frac{46}{\sqrt{49}}$)

# (a) If X
# = average distance in feet for 49 fly balls, then give the distribution of
# X.
# Round your standard deviation to two decimal places.

(46/sqrt(49)) %>% round(3)

# (b)What is the probability that the 49 balls traveled an average of less than 226 feet?
#
# Sketch the graph. Scale the horizontal axis for
# X.
# Shade the region corresponding to the probability. 

pnorm(226,236,(46/sqrt(49))) %>% round(3)

# (c)Find the 80th percentile of the distribution of the average of 49 fly balls. (Round your answer to two decimal places.)

qnorm(0.80,236,(46/sqrt(49))) %>% round(2)

# ## Problem 3
# Suppose that a category of world class runners are known to run a marathon (26 miles) in an average of 142 minutes with a standard deviation of 12 minutes. Consider 49 of the races.
#
# Let
# X
# = the average of the 49 races.
#
# X ~ N(142,12)
#
# $\bar{X}$ ~ N(142,$\frac{12}{\sqrt(49)}$)

x_bar=142
sigma=12/sqrt(49)

# Part (a)
# Give the distribution of
# X.
# (Round your standard deviation to two decimal places.)
# X
# ~
# ,

sigma %>% round(2)

# Part (b)
# Find the probability that the runner will average between 140 and 143 minutes in these 49 marathons. (Round your answer to four decimal places.)

(pnorm(143,x_bar,sigma)-pnorm(140,x_bar,sigma)) %>% round(4)

# Part (c)
# Find the 60th percentile for the average of these 49 marathons. (Round your answer to two decimal places.)
# min

(qnorm(0.6,x_bar,sigma)) %>% round(2)

# Part (d)
# Find the median of the average running times.
# min

qnorm(0.5,x_bar,sigma)

# # Problem 4
# The cost of unleaded gasoline in the Bay Area once followed an unknown distribution with a mean of \$4.24 and a standard deviation of \$0.08. Sixteen gas stations from the Bay Area are randomly chosen. We are interested in the average cost of gasoline for the 16 gas stations. What is the distribution to use for the average cost of gasoline for the 16 gas stations?
#
#
# X ~ N(4.24,0.08)
#
# $\bar{X}$ ~ N(4.24,$\frac{0.08}{\sqrt{16}}$)

# # Problem 5
# The cost of unleaded gasoline in the Bay Area once followed an unknown distribution with a mean of \$4.59 and a standard deviation of \$0.10. Sixteen gas stations from the Bay Area are randomly chosen. We are interested in the average cost of gasoline for the 16 gas stations.
#
# Find the probability that the average price for 30 gas stations is less than \$4.53.
#
# X ~ N(4.59,0.10)
#
# $\bar{X}$ ~ N(4.59,$\frac{0.10}{\sqrt{30}}$)

# +
avg=4.59
std=0.10/sqrt(30)
              
(pnorm(4.53,avg,std))
# -

# # Problem 6 
# Salaries for teachers in a particular elementary school district are normally distributed with a mean of \$42,000 and a standard deviation of \$5,500. We randomly survey ten teachers from that district. (Round your answers to the nearest dollar.) 
#
# X ~ N(42000,5500)
#
# $\bar{X}$ ~ N(42000,$\frac{5500}{\sqrt{10}}$)

avg=42000
std=5500/sqrt(10)

# (a) Find the 90th percentile for an individual teacher's salary.

qnorm(0.9,42000,5500) %>% round(0)

# (b) Find the 90th percentile for the average teacher's salary. 

qnorm(0.9,avg,std) %>% round(0)


