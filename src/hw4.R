# -*- coding: utf-8 -*-
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

# # Homework 4
# ## Problem 1
#
# Suppose the reaction temperature X (in °C) in a certain chemical process has a uniform distribution with
# A = −6
# and B = 6.



# (a) Compute P(X < 0).



# (b) Compute
# P(−3 < X < 3).



# (c) Compute P(−4 ≤ X ≤ 5). (Round your answer to two decimal places.)



# (d) For k satisfying
# −6 < k < k + 4 < 6,
# compute P(k < X < k + 4). (Round your answer to two decimal places.)



# An article considered the use of a uniform distribution with
# A = 0.20
# and
# B = 4.25



# for the diameter X of a certain type of weld (mm).
# (a) Determine the pdf of X. (Round your answers to three decimal places.)
# f(x) = 	0.20 < x < 4.25
#  	    	otherwise



# Graph the pdf of X.



# (b) What is the probability that diameter exceeds 2 mm? (Round your answer to three decimal places.)



# (c) What is the probability that diameter is within 1 mm of the mean diameter? (Round your answer to three decimal places.)



# (d) For any value a satisfying
# 0.20 < a < a + 3 < 4.25,
# what is
# P(a < X < a + 3)?
# (Round your answer to three decimal places.)



#  
# An article suggests the uniform distribution on the interval (7.5, 20) as a model for depth (cm) of the bioturbation layer in sediment in a certain region.



# (a) What are the mean and variance of depth? (Round your variance to two decimal places.)



# (b) What is the cdf of depth?
# F(x) = 
#      0	    	x < 7.5
#  		7.5 ≤ x < 20
#      1		20 ≤ x



# (c) What is the probability that observed depth is at most 10? (Round your answer to four decimal places.)



# What is the probability that observed depth is between 10 and 15? (Round your answer to four decimal places.)



# (d) What is the probability that the observed depth is within 1 standard deviation of the mean value? (Round your answer to four decimal places.)



# What is the probability that the observed depth is within 2 standard deviations of the mean value?



# Determine zα for the following of α. (Round your answers to two decimal places.)
# (a)    α = 0.0075



# (b)    α = 0.15



# (c)    α = 0.686



# There are two machines available for cutting corks intended for use in wine bottles. The first produces corks with diameters that are normally distributed with mean 3 cm and standard deviation 0.10 cm. The second machine produces corks with diameters that have a normal distribution with mean 3.04 cm and standard deviation 0.02 cm. Acceptable corks have diameters between 2.9 cm and 3.1 cm.



# What is the probability that the first machine produces an acceptable cork? (Round your answer to four decimal places.)



# What is the probability that the second machine produces an acceptable cork? (Round your answer to four decimal places.)



# Which machine is more likely to produce an acceptable cork?
# the first machine the second machine     



# If bolt thread length is normally distributed, what is the probability that the thread length of a randomly selected bolt is



# (a) Within 1.3 SDs of its mean value?
#



# (b) Farther than 2.1 SDs from its mean value?
#
# (c) Between 1 and 2 SDs from its mean value?



# Let X = the time between two successive arrivals at the drive-up window of a local bank. If X has an exponential distribution with λ = 1, (which is identical to a standard gamma distribution with α = 1), compute the following. (If necessary, round your answer to three decimal places.)



# (a) The expected time between two successive arrivals
#



# (b) The standard deviation of the time between successive arrivals
#



# (c)    P(X ≤ 3)



# (d)    P(2 ≤ X ≤ 4)



# Let X denote the distance (m) that an animal moves from its birth site to the first territorial vacancy it encounters. Suppose that for banner-tailed kangaroo rats, X has an exponential distribution with parameter λ = 0.01392.



# (a) What is the probability that the distance is at most 100 m? At most 200 m? Between 100 and 200 m? (Round your answers to four decimal places.)



# at most 100 m 	     	



# at most 200 m 	     	



# between 100 and 200 m 	     	



# (b) What is the probability that distance exceeds the mean distance by more than 2 standard deviations? (Round your answer to four decimal places.)



# (c) What is the value of the median distance? (Round your answer to two decimal places.)



# Let X denote the data transfer time (ms) in a grid computing system (the time required for data transfer between a "worker" computer and a "master" computer). Suppose that X has a gamma distribution with mean value 37.5 ms and standard deviation 21.6 (suggested by the article "Computation Time of Grid Computing with Data Transfer Times that Follow a Gamma Distribution,"†).



# (a)
# What are the values of α and β? (Round your answers to four decimal places.)
#
# α =
# β = 



# (b) What is the probability that data transfer time exceeds 50 ms? (Round your answer to three decimal places.)
#



# (c)
# What is the probability that data transfer time is between 50 and 74 ms? (Round your answer to three decimal places.)



# Suppose the time spent by a randomly selected student who uses a terminal connected to a local time-sharing computer facility has a gamma distribution with mean 15 min and variance 45 min2.
# (a) What are the values of α and β?
# α =
# β =



# (b) What is the probability that a student uses the terminal for at most 21 min? (Round your answer to three decimal places.)
#



# (c) What is the probability that a student spends between 15 and 27 min using the terminal? (Round your answer to three decimal places.)



# The lifetime X (in hundreds of hours) of a certain type of vacuum tube has a Weibull distribution with parameters α = 2 and β = 3.† Compute the following. (Round your answers to three decimal places.)



# (a)    
# E(X) and V(X)



# (b)    
# P(X ≤ 5)



# (c)    
# P(1.5 ≤ X ≤ 5)



# An article suggests the lognormal distribution as a model for SO2 concentration above a certain forest. Suppose the parameter values are μ = 2.1 and σ = 0.9.



# (a) What are the mean value and standard deviation of concentration? (Round your answers to three decimal places.)



# (b) What is the probability that concentration is at most 10? Between 5 and 10? (Round your answers to four decimal places.)
# at most 10 	     	Correct: Your answer is correct.
# between 5 and 10 	     	Correct: Your answer is correct.



# What condition on \alpha and \beta is necessary for the standard beta pdf to be symmetric?
# Since the standard beta distribution lies on (0, 1),
# the point of symmetry must be
# $\frac{1}{2}$ 
# ,so $\mu =\frac{1}{2}.$
#
# Therefore, solve for \alpha to obtain
#
# $\frac{\alpha}{\alpha + \beta} = 	0.5$
#
# $ \alpha	 = 	0.5\alpha + 0.5\beta$
#
# $ \alpha - 0.5\alpha = 	(0.5\alpha - 0.5\alpha) + 0.5\beta$
#
# $0.5\alpha	 = 	 0.5\beta$
#
# $\alpha	 = 	\beta$



# Suppose the proportion X of surface area in a randomly selected quadrat that is covered by a certain plant has a standard beta distribution with α = 5 and β = 3.



# (a) Compute E(X) and V(X). (Round your answers to four decimal places.)



# (b) Compute P(X ≤ 0.5). (Round your answer to four decimal places.)



# (c) Compute P(0.5 ≤ X ≤ 0.9). (Round your answer to four decimal places.)



# (d) What is the expected proportion of the sampling region not covered by the plant? (Round your answer to four decimal places.)


