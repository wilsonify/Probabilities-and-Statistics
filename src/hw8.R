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

library(tidyverse)

# ## Homework 8

#  ## Problem 1.
#  
# A group of divers is exploring an old sunken ship. Suppose the null hypothesis, $H_0$, is: the sunken ship does not contain buried treasure.
#
# State the Type I error.
#
# The ship does not contain buried treasure, but the divers think it does. 
#
# State the Type II error.
#
# The ship does contain buried treasure, but the divers think it does not.

# ## Problem 2.
#  
# Let $\mu$ denote the true average radioactivity level (picocuries per liter). The value 5 pCi/L is considered the dividing line between safe and unsafe water. Would you recommend testing
# $H_0$: $\mu$ = 5 versus $H_a$: $\mu$ > 5 or $H_0$: $\mu$ = 5 versus $H_a$: $\mu$ < 5?
# Explain your reasoning. [Hint: Think about the consequences of a type I and type II error for each possibility.]
#
#
# One should test
# $H_0$: $\mu$ = 5 versus $H_a$: $\mu$ < 5.
# A type I error in this case involves deciding the water is safe when it isn't. This is a very serious error, so a test which ensures that this error is highly unlikely is desirable. We prefer that the most serious error be a type I error because it can be explicitly controlled. 

# ## Problem 3.
#  
# Many older homes have electrical systems that use fuses rather than circuit breakers. A manufacturer of 40-amp fuses wants to make sure that the mean amperage at which its fuses burn out is in fact 40. If the mean amperage is lower than 40, customers will complain because the fuses require replacement too often. If the mean amperage is higher than 40, the manufacturer might be liable for damage to an electrical system due to fuse malfunction. To verify the amperage of the fuses, a sample of fuses is to be selected and inspected. If a hypothesis test were to be performed on the resulting data, w$H_a$t null and alternative hypotheses would be of interest to the manufacturer?
#
#    
# $H_0$: $\mu$ = 40;
# $H_a$: $\mu$ ≠ 40
#
# Describe type I error in the context of this problem situation.
#
# A type I error would be declaring a fuse as being defective when in fact there is nothing wrong with the fuse. 
#
# Describe type II error in the context of this problem situation.
#
# A type II error would be declaring a fuse to be satisfactory when in fact it is defective. 

# ## Problem 4.
#  
# Let $\mu$ denote the true average reaction time to a certain stimulus. For a z test of $H_0$: $\mu$ = 5 versus $H_a$: $\mu$ > 5,
# determine the P-value for each of the following values of the z test statistic. (Round your answers to four decimal places.)

# (a)
z=1.49 #given
alpha=1-pnorm(z) #right-tail probability
alpha %>% round(4)

#(b)
z=0.96 #given
alpha=1-pnorm(z)
alpha %>% round(4)

# (c)
z=1.96
alpha = 1-pnorm(z)
alpha %>% round(4)

#(d)
z=2.49
alpha = 1-pnorm(z)
alpha %>% round(4)

# (e)
z=-0.18
alpha = 1-pnorm(z)
alpha %>% round(4)

# ## Problem 5.
#  
# Newly purchased tires of a particular type are supposed to be filled to a pressure of 30 psi. Let $\mu$ denote the true average pressure. A test is to be carried out to decide whether $\mu$ differs from the target value. Determine the P-value for each of the following z test statistic values. (Round your answers to four decimal places.)

#(a)
z=2.19
half_alpha=1-pnorm(z) #right tail of two-tailed prob
alpha =2*half_alpha
alpha %>% round(4)

#(b)
z=-1.76
half_alpha=pnorm(z) #left-tail of two-tailed prob
alpha =2*half_alpha
alpha %>% round(4)

#(c)
z=-0.59
half_alpha=pnorm(z) #left-tail of two-tailed prob
alpha =2*half_alpha
alpha %>% round(4)

#(d)
z=1.48
half_alpha=1-pnorm(z) #right tail of two-tailed prob
alpha =2*half_alpha
alpha %>% round(4)

#(e)
z=-5.06
half_alpha=pnorm(z) #left-tail of two-tailed prob
alpha =2*half_alpha
alpha %>% round(4)

# ## Problem 6.
#  
# Pairs of P-values and significance levels, $\alpha$, are given. For each pair, state whether the observed P-value would lead to rejection of $H_0$ at the given significance level.

#(a)
P_value = 0.097
alpha = 0.05
P_value<alpha # if TRUE, reject H_0    

#(b)    
P_value = 0.009
alpha = 0.001
P_value<alpha # if TRUE, reject H_0    

#(c)
P_value = 0.498
alpha = 0.05
P_value<alpha # if TRUE, reject H_0

#(d)    
P_value = 0.097
alpha = 0.10
P_value<alpha # if TRUE, reject H_0

#(e)
P_value = 0.036
alpha = 0.01
P_value<alpha # if TRUE, reject H_0

#(f)
P_value = 0.221
alpha = 0.10
P_value<alpha # if TRUE, reject H_0

# ## Problem 7.
#  
# To obtain information on the corrosion-resistance properties of a certain type of steel conduit, 45 specimens are buried in soil for a 2-year period. The maximum penetration (in mils) for each specimen is then measured, yielding a sample average penetration of x = 53.8 and a sample standard deviation of s = 4.7. The conduits were manufactured with the specification that true average penetration be at most 50 mils. They will be used unless it can be demonstrated conclusively that the specification $H_a$s not been met. What would you conclude? (Use $\alpha$ = 0.05.)
# State the appropriate null and alternative hypotheses.
#
# $H_0$: $\mu$ = 50;
# $H_a$: $\mu$ > 50     
#
# Calculate the test statistic and determine the P-value. (Round your test statistic to two decimal places and your P-value to four decimal places.)

n=45
x_bar=53.8
H0=50
s=4.7
standard_error=s/sqrt(n)
z=(x_bar-H0)/standard_error
z %>% round(2)
alpha = 1-pnorm(z) #right-tail, one-sided
alpha %>% round(4)

# State the conclusion in the problem context.
#
# Reject the null hypothesis. There is sufficient evidence to conclude that the true average penetration is more than 50 mils. 
#

# ## Problem 8.
#  
# The recommended daily dietary allowance for zinc among males older than age 50 years is 15 mg/day. An article reports the following summary data on intake for a sample of males age 65−74 years: n = 119, x = 12.4, and s = 6.96. Does this data indicate that average daily zinc intake in the population of all males age 65−74 falls below the recommended allowance? (Use $\alpha$ = 0.05.)
# State the appropriate null and alternative hypotheses.
#
# $H_0$: $\mu$ = 15;
# $H_a$: $\mu$ < 15
#
# Calculate the test statistic and determine the P-value. (Round your test statistic to two decimal places and your P-value to four decimal places.)

n=119
x_bar=12.4
H0=15
s=6.96
standard_error=s/sqrt(n)
z=(x_bar-H0)/standard_error
z %>% round(2)
alpha = pnorm(z) #left-tail, one-sided
alpha %>% round(4)

# State the conclusion in the problem context.
#
# Reject the null hypothesis. There is sufficient evidence that average daily zinc intake falls below 15 mg/day. 

# ## Problem 9.
#  
# The true average diameter of ball bearings of a certain type is supposed to be 0.5 in. A one-sample t test will be carried out to see whether this is the case. What conclusion is appropriate in each of the following situations?
#
# assume the true mean is equal to 0.5:
#
# null hypothesis, $H_0: \mu=0.5$
#
# alt hypothesis, $H_a: \mu \neq 0.5$

#(a)    
n       = 20
df      = n-1
t       = 1.64
alpha   = 0.05
P_value = 2*(1-pt(t,df)) #right-tail of two-tailed prob
P_value<alpha #if TRUE, reject null hypothesis

# Do not reject the null hypothesis. There is not sufficient evidence that the true diameter differs from 0.5 in.

#(b)    
n = 20
df=n-1
t = -1.64
alpha = 0.05
P_value = 2*(pt(t,df)) #left-tail of two-tailed prob
P_value<alpha #if TRUE, reject null hypothesis

# Do not reject the null hypothesis. There is not sufficient evidence that the true diameter differs from 0.5 in.

#(c)    
n = 30
df=n-1
t = -2.58
alpha = 0.01
P_value = 2*(pt(t,df)) #left-tail of two-tailed prob
P_value<alpha #if TRUE, reject null hypothesis

# Do not reject the null hypothesis. There is not sufficient evidence that the true diameter differs from 0.5 in.

#(d)    
n = 30
df=n-1
t = -3.95
P_value = 2*(pt(t,df)) #left-tail of two-tailed prob
P_value<alpha #if TRUE, reject null hypothesis

# Reject the null hypothesis. There is sufficient evidence that the true diameter differs from 0.5 in. 

# ## Problem 10.
#  
# An article described an investigation into the coating weights for large pipes resulting from a galvanized coating process. Production standards call for a true average weight of 200 lb per pipe. The accompanying descriptive summary and boxplot are from Minitab.
#
# |Variable |N |Mean |Median |TrMean |StDev |SEMean|Min |Max |Q1 |Q3|
# |---------|--|-----|-------|-------|------|------|----|----|---|--|
# |ctg wt |30 |206.97 |206.00 |206.81 |6.35 |1.16|193.00 |218.00 |202.75 |212.00|
#
# (a) What does the boxplot suggest about the status of the specification for true average coating weight?
#
# It appears that the true average weight could be significantly off from the production specification of 200 lb per pipe.      

# (b) A normal probability plot of the data was quite straight. Use the descriptive output to test the appropriate hypotheses. (Use $\alpha$ = 0.05.) State the appropriate hypotheses.
#
# $H_0$: $\mu$ = 200;
# $H_a$: $\mu$ ≠ 200

# Calculate the test statistic and determine the P-value. (Round your test statistic to two decimal places and your P-value to three decimal places.)

n=30
df=n-1
x_bar=206.97
H0=200
s=6.35
SE=s/sqrt(n)
t=(x_bar-H0)/SE
t %>% round(2)
half_p_value = 1-pt(t,df)
p_value=2*half_p_value
p_value %>% round(3)
p_value<0.05 #if TRUE, reject null hypothesis

# What can you conclude?
#
# Reject the null hypothesis. There is sufficient evidence to conclude that the true average weight differs from 200 lb per pipe.     

# ## Problem 11.
#
# The accompanying data is on cube compressive strength (MPa) of concrete specimens.

comp_str <- c(112.5,97.0,92.8,86.0,102.0
              ,99.5,95.8,103.5,89.0,86.9)
qqnorm(comp_str)
qqline(comp_str)

# (a) Is it plausible that the compressive strength for this type of concrete is normally distributed?
#
# The normal probability plot is acceptably linear, suggesting that a normal population distribution is plausible.     

# (b) Suppose the concrete will be used for a particular application unless there is strong evidence that true average strength is less than 100 MPa. Should the concrete be used? Carry out a test of appropriate hypotheses.
# State the appropriate hypotheses.
#
# $H_0$: $\mu$ = 100;
# $H_a$: $\mu$ < 100

# Calculate the test statistic and determine the P-value. (Round your test statistic to two decimal places and your P-value to four decimal places.)

n=length(comp_str)
df=n-1
x_bar=mean(comp_str)
s=sd(comp_str)
H0=100
SE=s/sqrt(n)
t=(x_bar-H0)/SE
t %>% round(2)
P_value=pt(t,df) #left-tail of one-sided test
P_value %>% round(4)
P_value < 0.05 #use standard alpha=0.05. if TRUE, reject null hypothesis

# What can you conclude?
#
# There is not strong evidence that the true average strength is less than 100 MPa. The concrete should be used.    

# ## Problem 12.
#
# A statistical program is recommended.
# A random sample of soil specimens was obtained, and the amount of organic matter (%) in the soil was determined for each specimen, resulting in the accompanying data.

organic <- c( 1.19,5.09,0.97,1.59,4.60,0.32,0.55,1.45
             ,0.16,4.47,1.20,3.50,5.02,4.67,5.22,2.69
             ,3.99,3.17,3.03,2.21,0.69,4.47,3.31,1.17
             ,0.78,1.17,1.57,2.62,1.66,2.05)
qqnorm(organic)
qqline(organic)

# The values of the sample mean, sample standard deviation, and (estimated) standard error of the mean are 2.486, 1.612, and 0.294, respectively. Does this data suggest that the true average percentage of organic matter in such soil is something other than 3%? Carry out a test of the appropriate hypotheses at significance level 0.10. [Note: A normal probability plot of the data shows an acceptable pattern in light of the reasonably large sample size.]
#
# State the appropriate hypotheses.
#
# $H_0$: $\mu$ = 3;
# $H_a$: $\mu$ ≠ 3

# Calculate the test statistic and determine the P-value. (Round your test statistic to two decimal places and your P-value to three decimal places.)

n=length(organic)
df=n-1
x_bar=mean(organic)
s=sd(organic)
SE=s/sqrt(n)
H0=3
t=(x_bar-H0)/SE
t %>% round(2)
half_p_value=pt(t,df) #left-tail of two-sided prob
p_value=2*half_p_value
p_value %>% round(3)
p_value<0.1 # if TRUE, reject null

# What can you conclude?
#
# Reject the null hypothesis. There is sufficient evidence to conclude that the true average percentage of organic matter in this type of soil is something other than 3%. 
#
# Would your conclusion be different if $\alpha$ = 0.05 had been used?
#
# Do not reject the null hypothesis. There is not sufficient evidence to conclude that the true average percentage of organic matter in this type of soil is something other than 3%.

# ## Problem 13.
#
# Consider using a z test to test $H_0$: p = 0.9.
#
# Determine the P-value in each of the following situations. (Round your answers to four decimal places.)

#(a)
#p > 0.9 implies right-tail
z = 1.46
(1-pnorm(z)) %>% round(4)

#(b)
#p < 0.9 implies left-tail
z = -2.79
pnorm(z) %>% round(4)

#(c)
#p ≠ 0.9 implies two-tailed
z = -2.79
half=pnorm(z)
(2*half) %>% round(4)

#(d)
#p < 0.9 implies left-tail
z = 0.28
pnorm(z) %>% round(4)

# ## Problem 14.
#
# A random sample of 147 recent donations at a certain blood bank reveals that 81 were type A blood. Does this suggest that the actual percentage of type A donations differs from 40%, the percentage of the population having type A blood? Carry out a test of the appropriate hypotheses using a significance level of 0.01.
#
# State the appropriate null and alternative hypotheses.
#
# $H_0$: p = 0.40;
# $H_a$: p ≠ 0.40
#
# Calculate the test statistic and determine the P-value. (Round your test statistic to two decimal places and your P-value to four decimal places.)

n=147
p=81/n
H0=0.40
SE=sqrt(H0*(1-H0)/n) #sd of a binomial dist
alpha=0.01
z=(p-H0)/SE
z %>% round(2)
half_p_value = 1-pnorm(z) #right-tail of two-tailed prob
p_value=2*half_p_value
p_value %>% round(4)

# State the conclusion in the problem context.
#
# Reject the null hypothesis. There is sufficient evidence to conclude that the percentage of type A donations differs from 40%. 
#
# Would your conclusion have been different if a significance level of 0.05 had been used?
#
# No    

# ## Problem 15.
#  
# It is known that roughly 2/3 of all human beings have a dominant right foot or eye. Is there also right-sided dominance in kissing behavior? An article reported that in a random sample of 136 kissing couples, both people in 87 of the couples tended to lean more to the right than to the left. (Use $\alpha$ = 0.05.)

# (a) If 2/3 of all kissing couples exhibit this right-leaning behavior, what is the probability that the number in a sample of 136 who do so differs from the expected value by at least as much as what was actually observed? (Round your answer to three decimal places.)

# +
n <- 136
p_hat <- 2/3
expected <- p_hat*n
observed  <- 87 
difference <- abs(observed-expected)

#binomial dist is discrete
left_tail <-  0:floor(expected-difference)
right_tail <- ceiling(expected+difference):n
p_value <- sum(dbinom(left_tail,n,p_hat))+sum(dbinom(right_tail,n,p_hat))
round(p_value,3)
# -

# (b) Does the result of the experiment suggest that the 2/3 figure is implausible for kissing behavior?
#
# State the appropriate null and alternative hypotheses.
#
# $H_0$: p = 2/3;
# $H_a$: p ≠ 2/3
#
# Calculate the test statistic and determine the P-value. (Round your test statistic to two decimal places and your P-value to four decimal places.)
#
# **see calc above**
#
# State the conclusion in the problem context.
#
# Do not reject the null hypothesis. There is not sufficient evidence to conclude that the true proportion of right-leaning be$H_a$vior differs from 2/3. 

n=136
x_bar=87/n
H0=2/3
SE=sqrt(H0*(1-H0)/n)
z=(x_bar-H0)/SE
z %>% round(2)
(2*pnorm(z))%>% round(3)

# ## Problem 16.
#
# In a sample of 176 students at an Australian university that introduced the use of plagiarism-detection software in a number of courses, 59 students indicated a belief that such software unfairly targets students. Does this suggest that a majority of students at the university do not share this belief? Test appropriate hypotheses at level 0.05. (Let p be the proportion of students at this university who do not share this belief.)
#
# State the appropriate hypotheses.
#
# $H_0$: p = 0.50;
# $H_a$: p > 0.50
#

# Calculate the test statistic and determine the P-value. (Round your test statistic to two decimal places and your P-value to four decimal places.)

n=176
H0=0.5
x_bar=59/n
SE=sqrt(H0*(1-H0)/n)
z=(H0-x_bar)/SE
z %>% round(2)
p_value=pnorm(z) #right-tail of one-sided
p_value %>% round(4)

# What can you conclude?
#
# Do not reject the null hypothesis. There is not sufficient evidence that more than 50% of all students do not share the belief that the plagiarism-detection software unfairly targets students. 
#
# Reject the null hypothesis. There is not sufficient evidence that more than 50% of all students do not share the belief that the plagiarism-detection software unfairly targets students.     
#
# Reject the null hypothesis. There is sufficient evidence that more than 50% of all students do not share the belief that the plagiarism-detection software unfairly targets students. 
#
# Do not reject the null hypothesis. There is sufficient evidence that more than 50% of all students do not share the belief that the plagiarism-detection software unfairly targets students.


