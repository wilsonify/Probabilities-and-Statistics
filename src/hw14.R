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

# # Homework 14
# ## 1.
# What conclusion would be appropriate for an upper-tailed chi-squared test in each of the following situations?

# (a)    α = 0.05, df = 4, χ2 = 11.25
# We would reject H0. 

# +
alpha <- 0.05
df <- 4
stat <- 11.25

p_value <- 1-pchisq(stat,df)
p_value
if ( p_value<alpha ){print("Reject H0")} else {print("Fail to Reject H0")}
# -

# (b)    α = 0.01, df = 3, χ2 = 8.53
# We would fail to reject H0.     

alpha <- 0.01
df <- 3
stat <- 8.53
p_value <- 1-pchisq(stat,df)
p_value
if ( p_value<alpha ){
    print("Reject H0")
} else {
    print("Fail to Reject H0")
}

# (c)    α = 0.10, df = 2, χ2 = 4.06
# We would fail to reject H0.     

alpha <- 0.01
df <- 2
stat <- 4.06
p_value <- 1-pchisq(stat,df)
p_value
if ( p_value<alpha ){
    print("Reject H0")
} else {
    print("Fail to Reject H0")
}

# (d)    α = 0.01, k = 6, χ2 = 12.50
# We would fail to reject H0.     

alpha <- 0.01
k <- 6
df <- k-1
stat <- 12.5
p_value <- 1-pchisq(stat,df)
p_value
if ( p_value<alpha ){
    print("Reject H0")
} else {
    print("Fail to Reject H0")
}

# ## 2.
# A six-sided die is rolled 120 times. Fill in the expected frequency column. Then, conduct a hypothesis test at the 5% level to determine if the die is fair. The data below are the result of the 120 rolls. (Enter exact numbers as integers, fractions, or decimals.)
#
# |Value | 	Frequency | 	Expected Frequency|
# |------|------------|---------------------|
# |1 |	14 |	120*1/6=20|
# |2 |	32 |	120*1/6=20|
# |3 |	15 |	120*1/6=20|
# |4 |	14 |	120*1/6=20|
# |5 |	30 |	120*1/6=20|
# |6 |	15 |	120*1/6=20|

observed_frequency <- c(14,32,15,14,30,15)
expected_frequency <- c(20,20,20,20,20,20)

# Part (a)
# State the null hypothesis.
#
# The data fit the distribution for a fair six-sided die.     

# Part (b)
# State the alternative hypothesis.
#
# The data do not fit the distribution for a fair six-sided die. 

# Part (c)
# What are the degrees of freedom? (Enter an exact number as an integer, fraction, or decimal.)
#
# $\nu = 6-1 = 5$

df <- length(observed_frequency)-1
df

# Part (d)
# State the distribution to use for the test.
#
# $\chi^2_5$

# Part (e)
# What is the test statistic? (Round your answer to two decimal places.)

stat <- sum((observed_frequency-expected_frequency)^2/expected_frequency)
stat

# Part (f)
# What is the p-value? (Round your answer to four decimal places.)

p_value <- 1 - pchisq(stat,df)
round(p_value,4)

# Explain what the p-value means for this problem.
#
# If H0 is true, then there is a chance equal to the p-value that the value of the test statistic will be equal to or greater than the calculated value. 

# Part (g)
# Sketch a picture of this situation. Label and scale the horizontal axis, and shade the region(s) corresponding to the p-value.

curve(dchisq(x,df),from = 0,to = 4*df)
abline(v=stat)

# Part (h)
# Indicate the correct decision ("reject" or "do not reject" the null hypothesis), the reason for it, and write the appropriate conclusion.

# (i) Alpha (Enter an exact number as an integer, fraction, or decimal.)
# α = 0.05

alpha <- 0.05 

# (ii) Decision:
# reject the null hypothesis 

if ( p_value<alpha ){
    print("Reject H0")
} else {
    print("Fail to Reject H0")
}

# (iii) Reason for decision:
# Since α > p-value, we reject the null hypothesis. 

# (iv) Conclusion:
# There is sufficient evidence to conclude that the data do not fit the distribution for a fair six-sided die. 

#check answer
chisq.test(x = observed_frequency,p = rep(1/6,6)) #for goodness of fit, use x=observed_frequency and p=expected_proportion

# ## 3.
# The marital status distribution of the U.S. male population, age 15 and older, is as shown below.
#
# |Marital Status| 	Percent|
# |--------------|---------|
# |never married |     31.3|
# |married       |     56.1|
# |widowed       |     2.5 |
# |divorced\separated| 10.1|
#
# Suppose that a random sample of 400 U.S. young adult males, 18 to 24 years old, yielded the following frequency distribution. We are interested in whether this age group of males fits the distribution of the U.S. adult population at the 5% level. Calculate the frequency one would expect when surveying 400 people. Fill in the table below, rounding to two decimal places.
#
# |Marital Status    | 	Frequency | Expected Frequency |
# |------------------|------------|--------------------|
# |never married     | 137 	      |125.2|
# |married 	         | 241 	      |224.4|
# |widowed 	         | 2 	        |10   |
# |divorced/separated| 20 	      |40.4 |

expected_proportion <- c(31.3,56.1,2.5,10.1)/100
n <- 400
expected_frequency <- n*expected_proportion
observed_frequency <- c(137,241,2,20) 

# Part (a)
# State the null hypothesis.
#
# The data fit the distribution of marital status for the U.S. adult population. 

# Part (b)
# State the alternative hypothesis.
#
# The data do not fit the distribution of marital status for the U.S. adult population.     

# Part (c)
# What are the degrees of freedom? (Enter an exact number as an integer, fraction, or decimal.)
# $\nu=4-1=3$

df <- length(observed_frequency) - 1

# Part (d)
# State the distribution to use for the test.
#
# $\chi^2_3$

# Part (e)
# What is the test statistic? (Round your answer to two decimal places.)

stat <- sum((observed_frequency-expected_frequency)^2/expected_frequency)
round(stat,2)

# Part (f)
# What is the p-value? (Round your answer to four decimal places.)

p_value <- 1-pchisq(stat,df)
round(p_value,4)

# Explain what the p-value means for this problem.
#
# If H0 is true, then there is a chance equal to the p-value that the value of the test statistic will be equal to or greater than the calculated value.

# Part (g)
# Sketch a picture of this situation. Label and scale the horizontal axis, and shade the region(s) corresponding to the p-value.

curve(dchisq(x,df),from = 0,to = 10*df)
abline(v=stat)

# Part (h)
# Indicate the correct decision ("reject" or "do not reject" the null hypothesis), the reason for it, and write the appropriate conclusion.

if ( p_value<alpha ){
    print("Reject H0")
} else {
    print("Fail to Reject H0")
}

# (i) Alpha (Enter an exact number as an integer, fraction, or decimal.)
# α = 0.05

alpha <- 0.05

# (ii) Decision:
# reject the null hypothesis 

# (iii) Reason for decision:
# Since α > p-value, we reject the null hypothesis.

# (iv) Conclusion:
# There is sufficient evidence to conclude that the data do not fit the distribution of marital status for the U.S. adult population. 

#check answer
chisq.test(x = observed_frequency,p = expected_proportion)

# ## 4.
# A university conducted a survey of more than 263,000 college freshmen from 385 colleges in a particular year. The results of student expected majors by gender were reported. Suppose a survey of 5000 graduating females and 5000 graduating males was done as a follow-up to determine what their actual majors were. The second column in the table does not add to 100% because of rounding.
#
# Conduct a goodness of fit test at the 5% level to determine if the male distribution fits the female distribution.
#
# |Major               | 	Women |	Men |
# |--------------------|--------|-----|
# |Arts & Humanities   |14.0% 	|11.3%|
# |Biological Sciences |8.4% 	  |6.7% |
# |Business 	         |13.1% 	|23.9%|
# |Education 	         |13.0% 	|5.7% |
# |Engineering 	       |2.6% 	  |15.5%|
# |Physical Sciences 	 |2.6% 	  |3.7% |
# |Professional 	     |18.9% 	|9.3% |
# |Social Sciences 	   |13.0% 	|7.5% |
# |Technical 	         |0.4% 	  |1.8% |
# |Other 	             |5.8% 	  |8.1% |
# |Undecided 	         |8.0%    |6.5% |

n <- 5000
expected_proportion <- c(14.0,8.4,13.1,13.0,2.6,2.6,18.9,13.0,0.4,5.8,8.0)/100 #women
observed_proportion <- c(11.3,6.7,23.9,5.7,15.5,3.7,9.3,7.5,1.8,8.1,6.5)/100 #men
expected_frequency  <- n*expected_proportion
observed_frequency  <- n*observed_proportion

# Part (a)
# State the null hypothesis.
#
# The male distribution fits the female distribution.     

# Part (b)
# State the alternative hypothesis.
#
# The male distribution does not fit the female distribution. 

# Part (c)
# What are the degrees of freedom?
# $\nu=11-1=10$

df <- length(observed_frequency)-1
df

# Part (d)
# State the distribution to use for the test.
#
# $\chi^2_{10}$

# Part (e)
# What is the test statistic? 

stat <- sum((observed_frequency-expected_frequency)^2/expected_frequency)
round(stat,2)

# Part (f)
# What is the p-value? (Round your answer to four decimal places.)

p_value <- 1-pchisq(stat,df)
round(p_value,4)

# Explain what the p-value means for this problem.
#
# If H0 is true, then there is a chance equal to the p-value that the value of the test statistic will be equal to or greater than the calculated value. 

# Part (g)
# Sketch a picture of this situation. Label and scale the horizontal axis, and shade the region(s) corresponding to the p-value. (Upload your file below.)

curve(dchisq(x,df),from=0,to=5000)
abline(v=stat)

# Part (h)
# Indicate the correct decision ("reject" or "do not reject" the null hypothesis) and write the appropriate conclusion.

if ( p_value<alpha ){
    print("Reject H0")
} else {
    print("Fail to Reject H0")
}

# (i) Alpha:
# α = 0.05

alpha <- 0.05

# (ii) Decision:
# reject the null hypothesis

# (iii) Reason for decision:
# Since α > p-value, we reject the null hypothesis. 

# (iv) Conclusion:
# There is sufficient evidence to conclude that the male distribution does not fit the female distribution.

#check answer
adjusted_expected_proportion=expected_frequency/sum(expected_frequency) # force expected proportion to add up to 1
chisq.test(observed_frequency,p=adjusted_expected_proportion)

# ## 5.
# Read the statement and decide whether it is true or false.
# In a goodness-of-fit test, the expected values are the values we would expect if the null hypothesis were true.
# True

# ## 6.
# Read the statement and decide whether it is true or false.
# In general, if the observed values and expected values of a goodness-of-fit test are not close together, then the test statistic can get very large and on a graph will be way out in the right tail.
# True

# ## 7.
# Read the statement and decide whether it is true or false.
# The test to use to determine if a six-sided die is fair is a goodness-of-fit test.
# True 

# ## 8.
# Read the statement and decide whether it is true or false.
# In a goodness-of fit test, if the p-value is 0.0113, do not reject the null hypothesis. (Use a significance level of 0.05.)
# False     

# ## 9.
# College students may be interested in whether or not their majors have any effect on starting salaries after graduation. Suppose that 296 recent graduates were surveyed as to their majors in college and their starting salaries after graduation. Below are the data. Conduct a test of independence. (Use a significance level of 0.05.)
#
# |Major       |<\$50,000|\$50,000-\$68,999|=>\$69,000|
# |------------|--------|---------------|---------|
# |English     |4 	    |20 	          |4
# |Engineering |9 	    |31 	          |60
# |Nursing     |9 	    |15 	          |14
# |Business    |9 	    |20 	          |30
# |Psychology  |21 	    |29 	          |21

observed_frequency <- cbind(c(4,9,9,9,21),c(20,31,15,20,29),c(4,60,14,30,21))
observed_frequency

# Part (a)
# State the null hypothesis.
# An individual's starting salary after graduation is independent of that individual's major in college. 

# Part (b)
# State the alternative hypothesis.
# An individual's starting salary after graduation is dependent of that individual's major in college.     

# Part (c)
# What are the degrees of freedom? (Enter an exact number as an integer, fraction, or decimal.)
# $\nu=(5-1)*(3-1)=8$

df <- prod(dim(observed_frequency)-1)
df

# Part (d)
# State the distribution to use for the test.
# $\chi^2_8$

# Part (e)
# What is the test statistic? (Round your answer to two decimal places.)

expected_frequency <- outer(rowSums(observed_frequency),colSums(observed_frequency))/sum(observed_frequency)
stat <- sum((observed_frequency-expected_frequency)^2/expected_frequency)
round(stat,2)

# Part (f)
# What is the p-value? (Round your answer to four decimal places.)

p_value <- 1-pchisq(stat,df)
round(p_value,4)

# Explain what the p-value means for this problem.
# If H0 is true, then there is a chance equal to the p-value that the value of the test statistic will be equal to or greater than the calculated value.

# Part (g)
# Sketch a picture of this situation. Label and scale the horizontal axis, and shade the region(s) corresponding to the p-value.

curve(dchisq(x,df),from = 0,to = 5*df)
abline(v=stat)

# Part (h)
# Indicate the correct decision ("reject" or "do not reject" the null hypothesis) and write the appropriate conclusion.

# (i) Alpha (Enter an exact number as an integer, fraction, or decimal.)
# α = 0.05

alpha <- 0.05

# (ii) Decision:
# reject the null hypothesis 

if ( p_value<alpha ){
    print("Reject H0")
} else {
    print("Fail to Reject H0")
}

# (iii) Reason for decision:
# Since α > p-value, we reject the null hypothesis. 

# (iv) Conclusion:
# There is sufficient evidence to conclude that an individual's starting salary after graduation is dependent on that individual's major in college. 

#check answer
chisq.test(observed_frequency)

# ## 10.
# Some travel agents claim that honeymoon hot spots vary according to the age of the bride. Suppose that 277 recent brides were interviewed as to where they spent their honeymoons. The information is given below. Conduct a test of independence at the 5% level.
#
# |Location      | 	20 – 29| 	30 – 39| 	40 – 49| 	50 and over|
# |--------------|---------|---------|---------|-------------|
# |Niagara Falls |14 	     |25 	     |24 	     |19           |
# |Poconos 	     |16 	     |25 	     |24 	     |10           |
# |Europe 	     |9 	     |24 	     |15 	     |5            |
# |Virgin Islands|21 	     |24 	     |16 	     |6            |

observed_frequency <- rbind(c(14,25,24,19),c(16,25,24,10),c(9,24,15,5),c(21,24,16,6))
observed_frequency

# Part (a)
# State the null hypothesis.
# The honeymoon location is independent of the age of the bride. 

# Part (b)
# State the alternative hypothesis.
# The honeymoon location is dependent on the age of the bride.     

# Part (c)
# What are the degrees of freedom? (Enter an exact number as an integer, fraction, or decimal.)
# $\nu=(4-1)*(4-1)=9$

df <- prod(dim(observed_frequency)-1)
df

# Part (d)
# State the distribution to use for the test.
# $\chi^2_9$

# Part (e)
# What is the test statistic? (Round your answer to two decimal places.)

expected_frequency <- outer(rowSums(observed_frequency),colSums(observed_frequency))/sum(observed_frequency)
stat <- sum((observed_frequency-expected_frequency)^2/expected_frequency)
round(stat,2)

# Part (f)
# What is the p-value? (Round your answer to four decimal places.)

p_value <- 1-pchisq(stat,df)
round(p_value,4)

# Explain what the p-value means for this problem.
#
# If H0 is true, then there is a chance equal to the p-value that the value of the test statistic will be equal to or greater than the calculated value. 

# Part (g)
# Sketch a picture of this situation. Label and scale the horizontal axis, and shade the region(s) corresponding to the p-value.

curve(dchisq(x,df),from = 0,to = 3*df)
abline(v=stat)

# Part (h)
# Indicate the correct decision ("reject" or "do not reject" the null hypothesis), the reason for it, and write the appropriate conclusion.

# (i) Alpha (Enter an exact number as an integer, fraction, or decimal.)
# α = 0.05

alpha <- 0.05

# (ii) Decision:
# do not reject the null hypothesis     

if ( p_value<alpha ){
    print("Reject H0")
} else {
    print("Fail to Reject H0")
}

# (iii) Reason for decision:
# Since α < p-value, we do not reject the null hypothesis. 

# (iv) Conclusion:
# There is not sufficient evidence to conclude that the honeymoon location is dependent on the age of the bride.     

#check answer
chisq.test(observed_frequency)

# ## 11.
# Suppose that 590 30-year-olds were surveyed to determine whether or not there is a relationship between the level of education an individual has and salary. Conduct a test of independence at the 5% level.
#
# |Annual Salary          |Not a high school graduate |	High school graduate | College graduate |	Masters or doctorate |
# |-----------------------|---------------------------|----------------------|------------------|----------------------|
# |< \$30,000 	            |10                         |  20                  | 	10              | 	5                  |
# |\$30,000 – \$40,000 	    |25                         |  40                  | 	70              | 	30                 |
# |\$40,000 – \$50,000 	    |5                          |  10                  | 	35              | 	55                 |
# |\$50,000 – \$60,000 	    |10                         |  10                  | 	20              | 	65                 |
# |\$60,000+ 	            |0                          |  10                  | 	10              | 	150                |

observed_frequency <- cbind(c(10,25,5,10,0),c(20,40,10,10,10),c(10,70,35,20,10),c(5,30,55,65,150))
observed_frequency

# Part (a)
# State the null hypothesis.
# The annual salary of the 30-year-old is independent of the level of education that the individual obtained.
#      

# Part (b)
# State the alternative hypothesis.
# The annual salary of the 30-year-old is dependent of the level of education that the individual obtained.     

# Part (c)
# What are the degrees of freedom? (Enter an exact number as an integer, fraction, or decimal.)
# $\nu=(5-1)*(4-1)=12$

df <- prod(dim(observed_frequency)-1)
df

# Part (d)
# State the distribution to use for the test.
# $\chi^2_12$

# Part (e)
# What is the test statistic? (Round your answer to two decimal places.)

expected_frequency <- outer(rowSums(observed_frequency),colSums(observed_frequency))/sum(observed_frequency)
stat <- sum((observed_frequency-expected_frequency)^2/expected_frequency)
round(stat,2)

# Part (f)
# What is the p-value? (Round your answer to four decimal places.)

p_value <- 1-pchisq(stat,df)
round(p_value,4)

# Explain what the p-value means for this problem.
#
# If H0 is true, then there is a chance equal to the p-value that the value of the test statistic will be equal to or greater than the calculated value.

# Part (g)
# Sketch a picture of this situation. Label and scale the horizontal axis, and shade the region(s) corresponding to the p-value.

curve(dchisq(x,df),from = 0,to = 20*df)
abline(v=stat)

# Part (h)
# Indicate the correct decision ("reject" or "do not reject" the null hypothesis) and write the appropriate conclusion.

# (i) Alpha (Enter an exact number as an integer, fraction, or decimal.)
# α = 0.05

alpha <- 0.05

# (ii) Decision:
# reject the null hypothesis 

if ( p_value<alpha ){
    print("Reject H0")
} else {
    print("Fail to Reject H0")
}

# (iii) Reason for decision:
# Since α > p-value, we reject the null hypothesis. 

# (iv) Conclusion:
# There is sufficient evidence to conclude that the annual salary of the 30-year-old is dependent on the level of education that the individual obtained.    

#check answer
chisq.test(observed_frequency)

# ## 12.
# A psychologist is interested in testing whether there is a difference in the distribution of personality types for business majors and social science majors. The results of the study are shown in the table below. Conduct a test of homogeneity. Test at a 5% level of significance.
#
# |Major          |Open |Conscientious |Extrovert |Agreeable |Neurotic|
# |---------------|-----|--------------|----------|----------|--------|
# |Business 	    |43 	|53 	         |45 	      |61 	     |59      |
# |Social Science |72 	|75 	         |65 	      |80 	     |66      |

observed_frequency <- rbind(c(43,53,45,61,59),c(72,75,65,80,66))
observed_frequency

# Part (a)
# State the null hypothesis.
# The distribution for personality types is the same for both majors. 
#     

# Part (b)
# State the alternative hypothesis.
# The distribution for personality types is not the same for both majors. 

# Part (c)
# What are the degrees of freedom? (Enter an exact number as an integer, fraction, or decimal.)
# $\nu=(2-1)*(5-1)=4$

df <- prod(dim(observed_frequency)-1)
df

# Part (d)
# State the distribution to use for the test.
#
# $\chi^2_4$

# Part (e)
# What is the test statistic? (Round your answer to two decimal places.)

expected_frequency <- outer(rowSums(observed_frequency),colSums(observed_frequency))/sum(observed_frequency)
stat <- sum((observed_frequency-expected_frequency)^2/expected_frequency)
round(stat,2)

# Part (f)
# What is the p-value? (Round your answer to four decimal places.)

p_value <- 1-pchisq(stat,df)
round(p_value,4)

# Explain what the p-value means for this problem.
#
# If H0 is true, then there is a chance equal to the p-value that the value of the test statistic will be equal to or greater than the calculated value. 

# Part (g)
# Sketch a picture of this situation. Label and scale the horizontal axis, and shade the region(s) corresponding to the p-value.

curve(dchisq(x,df),from = 0,to = 3*df)
abline(v=stat)

# Part (h)
# Indicate the correct decision ("reject" or "do not reject" the null hypothesis) and write the appropriate conclusion.

# (i) Alpha (Enter an exact number as an integer, fraction, or decimal.)
# α = 0.05

alpha <- 0.05

# (ii) Decision:
# do not reject the null hypothesis     

if ( p_value<alpha ){
    print("Reject H0")
} else {
    print("Fail to Reject H0")
}

# (iii) Reason for decision:
# Since α < p-value, we do not reject the null hypothesis. 

# (iv) Conclusion:
# There is insufficient evidence to conclude that the distribution of personality types is different for business and social science majors.    

#check answer
chisq.test(observed_frequency)

# ## 13.
# A fisherman is interested in whether the distribution of fish caught in Green Valley Lake is the same as the distribution of fish caught in Echo Lake. Of the 186 randomly selected fish caught in Green Valley Lake, 104 were rainbow trout, 27 were other trout, 34 were bass, and 21 were catfish. Of the 293 randomly selected fish caught in Echo Lake, 118 were rainbow trout, 58 were other trout, 64 were bass, and 53 were catfish. Perform a test for homogeneity at a 5% level of significance.
#
# |Lake\Fish  |Rainbow|Other Trout|Bass|Catfish|
# |-----------|-------|-----------|----|-------|
# |GreenValley|104    |27         |34  |21     |
# |Echo       |118    |58         |64  |53     |

observed_frequency <- rbind(c(104,27,34,21),c(118,58,64,53))
observed_frequency

# Part (a)
# State the null hypothesis.
#
# The distribution for fish caught is the same in Green Valley Lake and in Echo Lake.     

# Part (b)
# State the alternative hypothesis.
#
# The distribution for fish caught is not the same in Green Valley Lake and in Echo Lake.     

# Part (c)
# What are the degrees of freedom? (Enter an exact number as an integer, fraction, or decimal.)
# $\nu=()*()=3$

df <- (dim(observed_frequency)[1]-1)*(dim(observed_frequency)[2]-1)
df

# Part (d)
# State the distribution to use for the test.
#
# $\chi^2_3$

# Part (e)
# What is the test statistic? (Round your answer to two decimal places.)

expected_frequency <- outer(rowSums(observed_frequency),colSums(observed_frequency))/sum(observed_frequency) 
stat <- sum((observed_frequency-expected_frequency)^2/expected_frequency)
round(stat,2)

# Part (f)
# What is the p-value? (Round your answer to four decimal places.)

p_value <- 1-pchisq(stat,df)
round(p_value,4)

#check answer
chisq.test(observed_frequency) #check answer

# Explain what the p-value means for this problem.
#
# If H0 is true, then there is a chance equal to the p-value that the value of the test statistic will be equal to or greater than the calculated value. 

# Part (g)
# Sketch a picture of this situation. Label and scale the horizontal axis, and shade the region(s) corresponding to the p-value.

curve(dchisq(x,df),from = 0,to = 5*df)
abline(v=stat)

# Part (h)
# Indicate the correct decision ("reject" or "do not reject" the null hypothesis) and write the appropriate conclusion.

if ( p_value<alpha ){
    print("Reject H0")
} else {
    print("Fail to Reject H0")
}

# (i) Alpha (Enter an exact number as an integer, fraction, or decimal.)
# α = 0.05

alpha <- 0.05

# (ii) Decision:
# reject the null hypothesis 

# (iii) Reason for decision:
# Since α > p-value, we reject the null hypothesis. 

# (iv) Conclusion:
# There is sufficient evidence to conclude that the distribution of fish caught is different in Green Valley Lake and in Echo Lake. 

#check answer again
chisq.test(observed_frequency) #check answer

# ## 14.
# Read the statement and decide whether it is true or false. 
# If df = 2, the Chi-square distribution has a shape that reminds us of the exponential.
# True

df <- 2
curve(dchisq(x,df),from = 0,to = 3*df)

curve(dexp(x),from=0,to=3*df)

# ## 15.
# A plant manager is concerned her equipment may need recalibrating. It seems that the actual weight of the 15 oz. cereal boxes it fills has been fluctuating. The standard deviation should be at most 0.5 oz. In order to determine if the machine needs to be recalibrated, 83 randomly selected boxes of cereal from the next day's production were weighed. The standard deviation of the 83 boxes was 0.53. Does the machine need to be recalibrated? Conduct a hypothesis test at the 5% level.

sigma_0 <- 0.5
n <- 83
s <- 0.53
alpha <- 0.05

# Part (a)
# State the null hypothesis.
# H0: σ2 ≤ 0.5^2

# Part (b)
# State the alternative hypothesis.
# Ha: σ2 > 0.5^2 

# Part (c)
# What are the degrees of freedom? (Enter an exact number as an integer, fraction, or decimal.)
# $\nu=n-1=82$

df <- n-1
df

# Part (d)
# State the distribution to use for the test.
#
# $\chi^2_{82}$

# Part (e)
# What is the test statistic? (Round your answer to two decimal places.)
#
# $\nu \frac{s^2}{sigma^2} \sim \chi^2_{\nu}$

stat <- df*s^2/sigma_0^2
round(stat,2)

# Part (f)
# What is the p-value? (Round your answer to four decimal places.)

p_value <- 1-pchisq(stat,df)
round(p_value,4)

# Explain what the p-value means for this problem.
#
# If H0 is true, then there is a chance equal to the p-value of obtaining a sample standard deviation of 0.53 or more. 

# Part (g)
# Sketch a picture of this situation. Label and scale the horizontal axis, and shade the region(s) corresponding to the p-value.

curve(dchisq(x,df),from = 0,to = 2*df)
abline(v=stat)

# Part (h)
# Indicate the correct decision ("reject" or "do not reject" the null hypothesis), the reason for it, and write the appropriate conclusion.

if ( p_value<alpha ){
    print("Reject H0")
} else {
    print("Fail to Reject H0")
}

# (i) Alpha (Enter an exact number as an integer, fraction, or decimal.)
# α = 0.05

alpha <- 0.05

# (ii) Decision:
# do not reject the null hypothesis     

# (iii) Reason for decision:
# Since α < p-value, we do not reject the null hypothesis. 

# (iv) Conclusion:
# There is not sufficient evidence to conclude that the machine needs to be recalibrated.     

# ## 16.
# Isabella, an accomplished Bay to Breakers runner, claims that the standard deviation for her time to run the 7.5 mile race is at most 3 minutes. To test her claim, Rupinder looks up 5 of her race times. They are 54 minutes, 61 minutes, 57 minutes, 62 minutes, and 58 minutes. Conduct a hypothesis test at the 5% level.

n <- 5
s <- sd(c(54,61,57,62,58))
s
sigma_0 <- 3
alpha <- 0.05

# Part (a)
# State the null hypothesis.
# H0: σ^2 ≤ 3^2

# Part (b)
# State the alternative hypothesis.
# Ha: σ^2 > 3^2

# Part (c)
# What are the degrees of freedom? (Enter an exact number as an integer, fraction, or decimal.)
#
# $\nu=5-1=4$

n <- 5
df <- n-1
df

# Part (d)
# State the distribution to use for the test.
#
# $\chi^2_4$

# Part (e)
# What is the test statistic? (Round your answer to two decimal places.)

stat <- df*s^2/sigma_0^2
round(stat,2)

# Part (f)
# What is the p-value? (Round your answer to four decimal places.)

p_value <- 1-pchisq(stat,df)
round(p_value,4)

# Explain what the p-value means for this problem.
#
# If H0 is true, then there is a chance equal to the p-value of obtaining a sample standard deviation equal to the one calculated here or more.

# Part (g)
# Sketch a picture of this situation. Label and scale the horizontal axis, and shade the region(s) corresponding to the p-value.

curve(dchisq(x,df),from = 0,to = 3*df)
abline(v=stat)

# Part (h)
# Indicate the correct decision ("reject" or "do not reject" the null hypothesis), the reason for it, and write the appropriate conclusion.

# (i) Alpha (Enter an exact number as an integer, fraction, or decimal.)
# α = 0.05

if ( p_value<alpha ){
    print("Reject H0")
} else {
    print("Fail to Reject H0")
}

# (ii) Decision:
# do not reject the null hypothesis     

# (iii) Reason for decision:
# Since α < p-value, we do not reject the null hypothesis.

# (iv) Conclusion:
# There is not sufficient evidence to conclude that the standard deviation is greater than 3.     

# ## 17.
# The manager of "Frenchies" is concerned that patrons are not consistently receiving the same amount of French fries with each order. The chef claims that the standard deviation for a 10-ounce order of fries is at most 1.5 oz., but the manager thinks that it may be higher. He randomly weighs 52 orders of fries, which yields a mean of 11 oz. and a standard deviation of 1.9 oz. Conduct a hypothesis test at the 5% level.

sigma_0 <- 1.5
n <- 52
x_bar <- 11
s <- 1.9

# Part (a)
# State the null hypothesis.
# H0: σ^2 ≤ 1.5^2

# Part (b)
# State the alternative hypothesis.
# Ha: σ^2 > 1.5^2

# Part (c)
# What are the degrees of freedom? (Enter an exact number as an integer, fraction, or decimal.)
# $\nu=n-1=51$

df <- n-1
df

# Part (d)
# State the distribution to use for the test.
#
# $\chi^2_{51}$

# Part (e)
# What is the test statistic? (Round your answer to two decimal places.)

stat <- df*s^2/sigma_0^2
round(stat,2)

# Part (f)
# What is the p-value? (Round your answer to four decimal places.)

p_value <- 1-pchisq(stat,4)
round(p_value,4)

# Explain what the p-value means for this problem.
# If H0 is true, then there is a chance equal to the p-value of obtaining a sample standard deviation of 1.9 or more.

# Part (g)
# Sketch a picture of this situation. Label and scale the horizontal axis, and shade the region(s) corresponding to the p-value.

curve(dchisq(x,df),from = 0,to = 3*df)
abline(v=stat)

# Part (h)
# Indicate the correct decision ("reject" or "do not reject" the null hypothesis), the reason for it, and write the appropriate conclusion.

if ( p_value<alpha ){
    print("Reject H0")
} else {
    print("Fail to Reject H0")
}

# (i) Alpha (Enter an exact number as an integer, fraction, or decimal.)
# α = 0.05

# (ii) Decision:
# reject the null hypothesis

# (iii) Reason for decision:
# Since α > p-value, we reject the null hypothesis. 

# (iv) Conclusion:
# There is sufficient evidence to conclude that the standard deviation is greater than 1.5. 

# ## 18.
# (a) Explain why a goodness-of-fit test and a test of independence are generally right-tailed tests.
#
# The test statistic is always positive.
# If the expected and observed values are not close together, 
# the test statistic will be large and the null hypothesis will be rejected.  

# (b) If you did a left-tailed test, what would you be testing?
#
# testing to see if the data fits the distribution "too well"

# ## 19.
# Three different design configurations are being considered for a particular component. There are four possible failure modes for the component. An engineer obtained the following data on number of failures in each mode for each of the three configurations. Does the configuration appear to have an effect on type of failure?
#
# |Configuration\FailureMode |1  |2  |3 |4 |
# |--------------|---|---|--|--|
# |	1            |20 |44 |17|9 |
# |	2 	         |4  |18 |7 |12|
# |	3 	         |10 |31 |14|5 |

# +
observed_frequency <- rbind(c(20,44,17,9),c(4,18,7,12),c(10,31,14,5))
observed_frequency

df <- prod(dim(data)-1)
df
# -

# State the appropriate hypotheses.
# H0: $p_{ij} = p_i \cdot p_j$  i = 1, 2, 3; j = 1, 2, 3, 4
# Ha: at least one $p_{ij} ≠ p_i \cdot p_j$

# Compute the test statistic value. (Round your answer to three decimal places.)

expected_frequency <- outer(rowSums(observed_frequency),colSums(observed_frequency))/sum(observed_frequency)
stat <- sum((observed_frequency-expected_frequency)^2/expected_frequency)
round(stat,3)

# What can you say about the P-value?
# 0.025 < P-value < 0.05 

p_value <- 1-pchisq(stat,df)
round(p_value,4)

# State the conclusion in the problem context. (Use α = 0.05.)
# Reject H0. There is evidence that configuration has a significant effect on type of failure. 

alpha <- 0.05
if ( p_value<alpha ){
    print("Reject H0")
} else {
    print("Fail to Reject H0")
}

#check answer 
chisq.test(observed_frequency)

# ## 20.
# Consider the accompanying 2 × 3 table displaying the sample proportions that fell in the various combinations of categories (e.g., 13% of those in the sample were in the first category of both factors).
#
# |  |1     |2 	   |3   |
# |--|------|------|----|
# |1 |0.13 	|0.20  |0.28|
# |2 |0.08 	|0.10  |0.21|

observed_proportion <- rbind(c(0.13,0.20,0.28),c(0.08,0.10,0.21))
observed_proportion

# (a) Suppose the sample consisted of n = 100 people. Use the chi-squared test for independence with significance level 0.10. State the appropriate hypotheses.
# H0: pij = pi · pj for every pair (i, j)
# Ha: at least one pij ≠ pi · pj

n <- 100
alpha <- 0.1
df <- prod(dim(data)-1)
observed_frequency <- n*observed_proportion
expected_frequency <- outer(rowSums(observed_frequency),colSums(observed_frequency))/sum(observed_frequency)

# Calculate the test statistic. (Round your answer to two decimal places.)

stat <- sum((observed_frequency-expected_frequency)^2/expected_frequency)
round(stat,2)

# What can be said about the P-value for the test?
# P-value > 0.10 

p_value <- 1-pchisq(stat,df)
round(p_value,4)

# State the conclusion in the problem context.
#
# Fail to reject H0. 
#
# An individual's category with respect to factor 1 is independent of the category with respect to factor 2.

chisq.test(observed_frequency)

if ( p_value<alpha ){
    print("Reject H0")
} else {
    print("Fail to Reject H0")
}

# (b) Suppose the sample consisted of n = 1000 people. Use the chi-squared test for independence with significance level 0.10.
# Calculate the test statistic. (Round your answer to two decimal places.)
# χ2 = 

n <- 1000
alpha <- 0.1
observed_frequency <- n*observed_proportion
expected_frequency <- outer(rowSums(observed_frequency),colSums(observed_frequency))/sum(observed_frequency)
stat <- sum((observed_frequency-expected_frequency)^2/expected_frequency)
round(stat,2)
p_value <- 1-pchisq(stat,df)
round(p_value,4)

# What can be said about the P-value for the test?
# 0.025 < P-value < 0.05 

if ( p_value<alpha ){
    print("Reject H0")
} else {
    print("Fail to Reject H0")
}

# State the conclusion in the problem context.
#
# Reject H0. 
#
# An individual's category with respect to factor 1 is not independent of the category with respect to factor 2. 

#check answer
chisq.test(observed_frequency)

# (c) What is the smallest sample size n for which these observed proportions would result in rejection of the independence hypothesis? (Round your answer up to the next whole number.)
# n = 

alpha <- 0.1
stat <- qchisq(1-alpha,df)
expected_proportion <- outer(rowSums(observed_proportion),colSums(observed_proportion))/sum(observed_proportion)
pseudo_stat <- sum((observed_proportion-expected_proportion)^2/expected_proportion)
n <- ceiling(stat/pseudo_stat)
n

# ## 21.
# The accompanying data refers to leaf marks found on white clover samples selected from both long-grass areas and short-grass areas. Use a χ2 test to decide whether the true proportions of different marks are identical for the two types of regions. (Use α = 0.01.)
#
# 	 	
# |Grass\Mark       |L   |LL |Y+YL |O   |Others|
# |-----------------|----|---|-----|----|------|
# |Long-Grass Areas |406 |11 |22   |7   |277   |
# |Short-GrassAreas |509 |4  |14   |11  |220   |

observed_frequency <- rbind(c(406,11,22,7,277),c(509,4,14,11,220))
observed_frequency

# State the appropriate hypotheses.
# H0: p1j = p2j      j = 1, 2, 3, 4, 5
#
# Ha: at least one p1j ≠ p2j

# Calculate the test statistic. (Round your answer to two decimal places.)

df <- prod(dim(observed_frequency)-1)
expected_frequency <- outer(rowSums(observed_frequency),colSums(observed_frequency))/sum(observed_frequency)
stat <- sum((observed_frequency-expected_frequency)^2/expected_frequency)
round(stat,2)

# What can be said about the P-value for the test?
# P-value < 0.005 

p_value <- 1-pchisq(stat,df)
round(p_value,4)

# State the conclusion in the problem context.
# Reject H0. There is evidence that the proportions are different. 

if ( p_value<alpha ){
    print("Reject H0")
} else {
    print("Fail to Reject H0")
}

# ## 22.
# Each individual in a random sample of high school and college students was cross-classified with respect to both political views and marijuana usage, resulting in the data displayed in the accompanying two-way table. Does the data support the hypothesis that political views and marijuana usage level are independent within the population? Test the appropriate hypotheses using level of significance 0.01.
#
# |Political Views\Usage Level |Never 	|Rarely 	|Frequently|
# |----------------------------|--------|---------|----------|
# |Liberal                     |476 	  |173 	    |116       |
# |Conservative 	             |211 	  |49 	    |14        |
# |Other 	                     |171 	  |47 	    |86        |

observed_frequency <- rbind(c(476,173,116),c(211,49,14),c(171,47,86))
df <- prod(dim(observed_frequency)-1)
df

# State the appropriate hypotheses.
# H0: pij = pi· · p·j    i = 1, 2, 3; j = 1, 2, 3
# Ha: at least one pij ≠ pi· · p·j

# Calculate the test statistic. (Round your answer to two decimal places.)

df <- prod(dim(observed_frequency)-1)
expected_frequency <- outer(rowSums(observed_frequency),colSums(observed_frequency))/sum(observed_frequency)
stat <- sum((observed_frequency-expected_frequency)^2/expected_frequency)
round(stat,2)

# What can be said about the P-value for the test?
# P-value < 0.005 

p_value <- 1-pchisq(stat,df)
p_value

# State the conclusion in the problem context.
# Reject H0. There is evidence that political views and level of marijuana usage are related. 

if ( p_value<alpha ){
    print("Reject H0")
} else {
    print("Fail to Reject H0")
}

#check answer
chisq.test(observed_frequency)

# ## 23.
# Qualifications of male and female head and assistant college athletic coaches were compared in an article. Each person in random samples of 2217 male coaches and 1135 female coaches was classified according to number of years of coaching experience to obtain the accompanying two-way table. Is there enough evidence to conclude that the proportions falling into the experience categories are different for men and women? Use α = 0.01.
#
# |Gender\Years of Experience  |1–3   |4–6  |7–9  |10–12  |13+|  
# |----------------------------|------|-----|-----|-------|---|
# |Male 	                     |198 	|370 	|478 	|356 	  |815|
# |Female 	                   |230 	|245 	|240 	|162 	  |258|

observed_frequency <- rbind(c(198,370,478,356,815),c(230,245,240,162,258))
observed_frequency

# State the appropriate hypotheses.
# H0: p1j = p2j      j = 1, 2, 3, 4, 5
# Ha: at least one p1j ≠ p2j 

# Calculate the test statistic. (Round your answer to two decimal places.)

df <- prod(dim(observed_frequency)-1)
expected_frequency <- outer(rowSums(observed_frequency),colSums(observed_frequency))/sum(observed_frequency)
stat <- sum((observed_frequency-expected_frequency)^2/expected_frequency)
round(stat,2)

# What can be said about the P-value for the test?
# P-value < 0.005 

p_value <- 1-pchisq(stat,df)
p_value

# State the conclusion in the problem context.
#
# Reject H0. 
#
# There is evidence that the proportions of male and female coaches falling into these experience categories are very different. 

#check answer
chisq.test(observed_frequency)

# ## 24.
# It is hypothesized that when homing pigeons are disoriented in a certain manner, they will exhibit no preference for any direction of flight after takeoff (so that the direction X should be uniformly distributed on the interval from 0° to 360°). To test this, 120 pigeons are disoriented, let loose, and the direction of flight of each is recorded; the resulting data follows. Use the chi-squared test at level 0.10 to see whether the data supports the hypothesis.
#
# |Direction |0−<45° |45−<90° |90−<135° |135−<180° |180−<225° |225−<270° |270−<315° |315−<360° |
# |----------|-------|--------|---------|----------|----------|----------|----------|----------|
# |Frequency |12 	   |17 	    |16 	    |15        |13 	      |19 	     |17 	      |11        |

alpha <- 0.10
observed_frequency <- c(12,17,16,15,13,19,17,11)
n <- sum(observed_frequency)
df <- 8-1
expected_frequency <- rep(n/8,8)

#
# State the appropriate hypotheses.
# H0: p1 = p2 = ... = p8 = 0.125
# Ha: at least one pi ≠ 0.125     

# Calculate the test statistic. (Round your answer to two decimal places.)

stat <- sum((observed_frequency-expected_frequency)^2/expected_frequency)
round(stat,2)

# What can be said about the P-value for the test?
# P-value > 0.10 

p_value <- 1-pchisq(stat,df)
p_value

# State the conclusion in the problem context.
# Fail to reject H0. There is not enough evidence to disprove the claim. 

 if ( p_value<alpha ){
    print("Reject H0")
} else {
    print("Fail to Reject H0")
}

# ## 25.
# An article made a strong case for central statistical monitoring as an alternative to more expensive onsite data verification. It suggested various methods for identifying data characteristics such as outliers, incorrect dates, anomalous data patterns, unusual correlation structures, and digit preference. Benford's Law gives a probability model for the first significant digit in many large data sets:
# $p(x) = log_{10}\left(\frac{(x + 1)}{x}\right)$ for x = 1, 2, ..., 9.
#
# |Digit |1 	|2 	|3 	|4 	|5 	|6 	|7 	|8 	|9  |
# |------|----|---|---|---|---|---|---|---|---|
# |Freq. |343 |180|165|155|83 |65 |51 |49 |55 |
#
#
#
# Carry out a test of hypotheses to see whether or not these frequencies are consistent with Benford's Law using a significance level of 0.05. 

digits <- 1:9
df <- length(digits)-1
observed_frequency <- c(343,180,165,155,83,65,51,49,55)
n <- sum(observed_frequency)
expected_proportion <- log10((digits+1)/(digits))
round(expected_proportion,4)
expected_frequency <- n*expected_proportion

# State the appropriate hypotheses. (Round your answers to four decimal places.)
#
# H0: p1 = 0.3010, p2 = 0.1761, p3 = 0.1249, p4 = 0.0969, p5 = 0.0792, p6 = 0.0669, p7 = 0.0580, p8 = 0.0512, p9 = 0.0458
#
# Ha: at least one pi ≠ pi0

# Calculate the test statistic. (Round your answer to two decimal places.) 

stat <- sum((observed_frequency-expected_frequency)^2/expected_frequency)
round(stat,2)

# What can be said about the P-value for the test?
# P-value < 0.005 

p_value <- 1-pchisq(stat,df)
p_value

# State the conclusion in the problem context. 
#
# Reject H0. There is enough evidence to conclude that at least one of the first significant digits deviates from Benford's law.

# ## 26.
# Criminologists have long debated whether there is a relationship between weather conditions and the incidence of violent crime. The author of an article classified 1358 homicides according to season, resulting in the accompanying data. Test the null hypothesis of equal proportions using α = 0.01.
#
# |Winter |Spring |Summer |Fall |
# |-------|-------|-------|-----|
# |328 	  |333    |372    |325  |

alpha <- 0.01
observed_frequency <- c(328,333,372,325)
df <- length(observed_frequency)-1
n <- sum(observed_frequency)

# State the appropriate hypotheses.
# H0: p1 = p2 = p3 = p4 = 0.25
# Ha: at least one pi does not equal 0.25

expected_proportion <- rep(1/4,4)
expected_frequency  <- n*expected_proportion

# Compute the test statistic value. (Round your answer to three decimal places.)

stat <- sum((observed_frequency-expected_frequency)^2/expected_frequency)
round(stat,3)

# What can we say about the P-value?
# P-value > 0.10 

p_value <- 1-pchisq(stat,df)
p_value

# State the conclusion in the problem context.
#
# Fail to reject H0. 
#
# There is no evidence that there is a seasonal relationship between weather conditions and the incidence of violent crime.

# ## 27.
# The response time of a computer system to a request for a certain type of information is hypothesized to have an exponential distribution with parameter λ = 1 sec (so if X = response time, the pdf of X under H0 is f0(x) = e−x for x ≥ 0).

# (a) If you had observed X1, X2, . . . , Xn and wanted to use the chi-squared test with five class intervals having equal probability under H0, what would be the resulting class intervals?
#
# [0, 0.2231), [0.2231, 0.5108), [0.5108, 0.9163), [0.9163, 1.6094), [1.6094, ∞) 

quintile <- 1:4
bin <- c(0,qexp(quintile/5),Inf)
bin

# (b) Carry out the chi-squared test using the following data resulting from a random sample of 40 response times. (Use α = 0.10. Use H0: the population distribution is exponential versus Ha: the population distribution is not exponential.)

# +
df <- 5-1
observations <- c(0.10,0.99,1.16,1.26,3.24,0.12,0.26,0.80,0.79,1.16,1.56,0.41,0.59,0.2,2.22,0.66,0.71,2.21,0.68,0.43,0.11,0.46,0.69,0.38,0.99,0.55,0.81,2.51,2.77,0.16,1.11,0.02,2.13,0.19,1.21,1.13,2.93,2.14,0.14,0.44)
n <- length(observations)
observations <- sort(observations)
observed_frequency <- diff(findInterval(bin,observations)) # running difference of observations < bin
expected_proportion <- rep(1/5,5)
expected_frequency <- n*expected_proportion

#debug
observed_frequency
expected_frequency
(observed_frequency-expected_frequency)

# -

# Calculate the test statistic. (Round your answer to two decimal places.)

stat <- sum((observed_frequency-expected_frequency)^2/expected_frequency)
round(stat,2)

# What can be said about the P-value for the test?
# P-value > 0.10 

p_value <- 1-pchisq(stat,df)
p_value

# State the conclusion in the problem context.
# Fail to reject H0. The data is consistent with the specified exponential distribution.

curve(dchisq(x,df),from = 0,to = 3*df)
abline(v=stat)
