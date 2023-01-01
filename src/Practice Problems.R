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

# + jupyter={"outputs_hidden": true}

# -

#

# # 7.2: Large-Sample Confidence Intervals for a Population Mean and Proportion 
# A sample of 53 research cotton samples resulted in a sample average percentage elongation of 8.14 and a sample standard deviation of 1.42. Calculate a 95% large-sample CI for the true average percentage elongation μ. (Round your answers to three decimal places.)
#
# What assumptions are you making about the distribution of percentage elongation? 
#
# No assumptions needed because n is large (>40) 

n=53
df=n-1
x_bar=8.14
s=1.42
SE=s/sqrt(n)
alpha=1-0.95
half_alpha=alpha/2
t=qnorm(1-half_alpha)
CI=c(x_bar-t*SE,x_bar+t*SE)
round(CI,3)

# The following observations are lifetimes (days) subsequent to diagnosis for individuals suffering from blood cancer.
#
# (a)
# Can a confidence interval for true average lifetime be calculated without assuming anything about the nature of the lifetime distribution? Explain your reasoning. [Note: A normal probability plot of the data exhibits a reasonably linear pattern.]
#
# Yes, the sample size is large enough for the confidence interval to be reasonable. 
#
# (b)
# Calculate and interpret a confidence interval with a 99% confidence level for true average lifetime. [Hint:
# x = 1192.0
# and
# s = 506.6.]
# (Round your answers to one decimal place.)
#
# Interpret the resulting interval.
# We are 99% confident that this interval contains the true population mean. 

# + jupyter={"outputs_hidden": true}
data <- c(115,181,255,419,442,462,516,739,743,789,807
  ,865,925,984,1026,1062,1064,1166,1192,1222,1222,1251
  ,1278,1290,1358,1369,1408,1455,1478,1519,1578,1578,1599
  ,1604,1606,1696,1736,1799,1815,1853,1899,1925,1965)
# -

n=length(data)
df=n-1
x_bar=mean(data)
s=sd(data)
SE=s/sqrt(n)
alpha=1-0.99
half_alpha=alpha/2
t=qnorm(1-half_alpha)
CI=c(x_bar-t*SE,x_bar+t*SE)
round(CI,1)

# # 4.4: The Exponential and Gamma and Other Distributions
# Suppose the proportion X of surface area in a randomly selected quadrat that is covered by a certain plant has a standard beta distribution with α = 4 and β = 2.
#
# (a) Compute E(X) and V(X). (Round your answers to four decimal places.)

alpha=4
beta=2
average=alpha/(alpha+beta)
round(average,4)
variance=(alpha*beta)/((alpha+beta)^2*(alpha+beta+1))
round(variance,4)

# (b) Compute P(X ≤ 0.7). (Round your answer to four decimal places.)

pbeta(0.7,alpha,beta)

# (c) Compute P(0.7 ≤ X ≤ 0.9). (Round your answer to four decimal places.)

pbeta(0.9,alpha,beta)-pbeta(0.7,alpha,beta)

# (d) What is the expected proportion of the sampling region not covered by the plant? (Round your answer to four decimal places.)

1-average

# The lifetime X (in hundreds of hours) of a certain type of vacuum tube has a Weibull distribution with parameters α = 2 and β = 3. 
#
# Compute the following. (Round your answers to three decimal places.) 
#
# E(X) = 	

shape=2
scale=3
mu=scale*gamma(1+1/shape)
round(mu,3)

# V(X) = 	

variance=scale^2*(gamma(1+2/shape)-gamma(1+1/shape)^2)
round(variance,3)

# P(X ≤ 4)

round(pweibull(4,shape,scale),3)

# P(1.5 ≤ X ≤ 4)

pweibull(4,shape,scale)-pweibull(1.5,shape,scale)

#  Let X denote the data transfer time (ms) in a grid computing system (the time required for data transfer between a "worker" computer and a "master" computer). Suppose that X has a gamma distribution with mean value 37.5 ms and standard deviation 21.6 (suggested by the article "Computation Time of Grid Computing with Data Transfer Times that Follow a Gamma Distribution,"†).
#  
# $mu={\alpha}{\beta} \implies \alpha=\frac{\mu}{\beta}$
#
# $var={\alpha}{\beta^2} \implies \alpha = \frac{var}{\beta^2}$
#
# $ \frac{\beta}{\mu}  = \frac{\beta^2}{var} \implies$
# $\beta=\frac{var}{\mu}$

# (a)
# What are the values of α and β? (Round your answers to four decimal places.)

mu=37.5
sd=21.6
var=sd^2
beta=var/mu #scale
alpha=mu/beta
round(alpha,4)
round(beta,4) 

# (b)
# What is the probability that data transfer time exceeds 49 ms? (Round your answer to three decimal places.)

1-pgamma(49,shape=alpha,scale = beta)


# (c)
# What is the probability that data transfer time is between 49 and 79 ms? (Round your answer to three decimal places.) 

pgamma(79,alpha,scale = beta)-pgamma(49,alpha,scale = beta)


# Let X denote the distance (m) that an animal moves from its birth site to the first territorial vacancy it encounters. Suppose that for banner-tailed kangaroo rats, X has an exponential distribution with parameter λ = 0.01427.

# + jupyter={"outputs_hidden": true}
lambda=0.01427
# -

# (a) What is the probability that the distance is at most 100 m? 

pexp(100,lambda)

# at most 200 m 	     	

pexp(200,lambda)

# between 100 and 200 m 	     	

pexp(200,lambda)-pexp(100,lambda)

# (b) What is the probability that distance exceeds the mean distance by more than 2 standard deviations? (Round your answer to four decimal places.)

mu=1/lambda
var=1/lambda^2
1-pexp(mu+2*sqrt(var),lambda)

# (c) What is the value of the median distance? (Round your answer to two decimal places.)
# m 

qexp(0.5,lambda)

# Let X denote the distance (m) that an animal moves from its birth site to the first territorial vacancy it encounters. Suppose that for banner-tailed kangaroo rats, X has an exponential distribution with parameter λ = 0.01347.

# + jupyter={"outputs_hidden": true}
lambda=0.01347
# -

# (a) What is the probability that the distance is at most 100 m? 

round(pexp(100,lambda),4)

# At most 200 m? 

round(pexp(200,lambda),4)

# Between 100 and 200 m? (Round your answers to four decimal places.)

round(pexp(200,lambda)-pexp(100,lambda),4)

# (b) What is the probability that distance exceeds the mean distance by more than 2 standard deviations? (Round your answer to four decimal places.)

mu=1/lambda
var=1/lambda^2
round(1-pexp(mu+2*sqrt(var),lambda),4)

# (c) What is the value of the median distance? (Round your answer to two decimal places.)
# m 

qexp(0.5,lambda)

#  Let X denote the data transfer time (ms) in a grid computing system (the time required for data transfer between a "worker" computer and a "master" computer). Suppose that X has a gamma distribution with mean value 37.5 ms and standard deviation 21.6 (suggested by the article "Computation Time of Grid Computing with Data Transfer Times that Follow a Gamma Distribution,"†).

# + jupyter={"outputs_hidden": true}
mu=37.5
var=21.6^2
# -

# (a)
# What are the values of α and β? (Round your answers to four decimal places.)
# α = β =

beta=var/mu #scale
alpha=mu/beta
round(alpha,4)
round(beta,4) 

# (b)
# What is the probability that data transfer time exceeds 51 ms? (Round your answer to three decimal places.)
# (c)

1-pgamma(51,alpha,scale = beta)

#  (c)
# What is the probability that data transfer time is between 51 and 70 ms? (Round your answer to three decimal places.) 

pgamma(70,alpha,scale = beta)-pgamma(51,alpha,scale = beta)

# # 4.3: The Normal Distribution
# If bolt thread length is normally distributed, what is the probability that the thread length of a randomly selected bolt is

# (a) Within 0.7 SDs of its mean value?

round(pnorm(0.7)-pnorm(-0.7),4)

# (b) Farther than 2.5 SDs from its mean value?

round(pnorm(-2.5)+(1-pnorm(2.5)),4)

# (c) Between 1 and 2 SDs from its mean value?

round((pnorm(2)-pnorm(-2))-(pnorm(1)-pnorm(-1)),4)

# Suppose the force acting on a column that helps to support a building is a normally distributed random variable X with mean value 16.0 kips and standard deviation 1.25 kips. Compute the following probabilities by standardizing and then using a standard normal curve table from the Appendix Tables. (Round your answers to four decimal places.)

# + jupyter={"outputs_hidden": true}
mu=16
std=1.25
# -

# (a)    
# P(X ≤ 16)

pnorm(16,mu,std)

# (b)    
# P(X ≤ 18.5)

pnorm(18.5,mu,std)

# (c)    
# P(X ≥ 9.75)

1-pnorm(9.75,mu,std)

# (d)    
# P(15 ≤ X ≤ 19)

pnorm(19,mu,std)-pnorm(15,mu,std)

# (e)    
# P(|X − 16| ≤ 1)

pnorm(17,mu,std)-pnorm(15,mu,std)

# Let Z be a standard normal random variable and calculate the following probabilities, drawing pictures wherever appropriate. (Round your answers to four decimal places.)

# (a)    P(0 ≤ Z ≤ 2.94)

round(pnorm(2.94)-pnorm(0),4)

# (b)    P(0 ≤ Z ≤ 1)

round(pnorm(1)-pnorm(0),4)

# (c)    P(−2.40 ≤ Z ≤ 0)

round(pnorm(0)-pnorm(-2.40),4)

# (d)    P(−2.40 ≤ Z ≤ 2.40)

round(pnorm(2.4)-pnorm(-2.4),4)

# (e)    P(Z ≤ 1.03)

round(pnorm(1.03),4)

# (f)    P(−1.25 ≤ Z)

round(1-pnorm(-1.25),4)

# (g)    P(−1.40 ≤ Z ≤ 2.00)

round(pnorm(2)-pnorm(-1.4),4)

# (h)    P(1.03 ≤ Z ≤ 2.50)

round(pnorm(2.5)-pnorm(1.03),4)

# (i)    P(1.40 ≤ Z)

round(1-pnorm(1.4),4)

# (j)    P(|Z| ≤ 2.50)

round(pnorm(2.5)-pnorm(-2.5),4)

# In each case, determine the value of the constant c that makes the probability statement correct. (Round your answers to two decimal places.)

# (a)    Φ(c) = 0.9838

round(qnorm(0.9838),2)

# (b)    P(0 ≤ Z ≤ c) = 0.2967

round(qnorm(0.2967+pnorm(0)),2)

# (c)    P(c ≤ Z) = 0.1170

round(qnorm(1-0.1170),2)

# (d)    P(−c ≤ Z ≤ c) = 0.6372

round(qnorm(0.6372/2+pnorm(0)),2)

# (e)    P(c ≤ |Z|) = 0.0160

round(qnorm(1-0.0160/2),2)

# # Independence
# An oil exploration company currently has two active projects, one in Asia and the other in Europe. Let A be the event that the Asian project is successful and B be the event that the European project is successful. Suppose that A and B are independent events with P(A) = 0.2 and P(B) = 0.8.

# (a) If the Asian project is not successful, what is the probability that the European project is also not successful?
#
# P(B')=1-P(B)=1-0.8=0.2
#
# Explain your reasoning.
# Since the events are independent, then A' and B' are independent. 

# (b) What is the probability that at least one of the two projects will be successful?
#
# $P(A \cup B)=1-P(A' \cap B')$

1-(0.2*0.8)

# (c) Given that at least one of the two projects is successful, what is the probability that only the Asian project is successful? (Round your answer to three decimal places.)
#
# $P(A|A\cup B)=\frac{P(A)-P(A\cap B)}{P(A\cup B)}$

(0.2-(0.2*0.8))/(1-(0.2*0.8))

# A company that manufactures video cameras produces a basic model and a deluxe model. Over the past year, 32% of the cameras sold have been of the basic model. Of those buying the basic model, 30% purchase an extended warranty, whereas 35% of all deluxe purchasers do so. If you learn that a randomly selected purchaser has an extended warranty, how likely is it that he or she has a basic model? (Round your answer to four decimal places.)
#
# Let B represent basic model cameras
# Let W represent Warrantied cameras
#
# |Camera\Warranty|With Warranty|Without Warranty|Row Sum|
# |---------------|--------|-----------|---|
# |Basic   |$0.3\cdot0.32     $               |$(1-0.3)\cdot0.32$                           |$0.32$|
# |Deluxe  |$0.35\cdot(1-0.32)$               |$(1-0.35)\cdot(1-0.32)$                      |$1-0.32$|
# |Col Sum |$(0.3\cdot0.32)+0.35\cdot(1-0.32)$|$((1-0.3)\cdot0.32)+((1-0.35)\cdot(1-0.32)) $|$1$ |
#
#
# |Camera\Warranty|With Warranty|Without Warranty|Row Sum|
# |---------------|-------------|----------------|-------|
# |Basic          |$0.096$      |$0.224$         |$0.32$ |
# |Deluxe         |$0.238$      |$0.442$         |$0.68$ |
# |Col Sum        |$0.334$      |$0.666$         |$1$    |

(0.3*0.32)/((0.35*(1-0.32))+(0.3*0.32))

# # 2.2: Axioms, Interpretations, and Properties of Probability
#
# An academic department with five faculty members—Anderson, Box, Cox, Cramer, and Fisher—must select two of its members to serve on a personnel review committee. Because the work will be time-consuming, no one is anxious to serve, so it is decided that the representative will be selected by putting the names on identical pieces of paper and then randomly selecting two.

# + jupyter={"outputs_hidden": true}
names=c('Anderson',"Box","Cox","Cramer","Fisher")
experience=c(3,6,7,10,14)
# -

# (a) What is the probability that both Anderson and Box will be selected? [Hint: List the equally likely outcomes.]

omega=expand.grid(names,names)
omega=omega[omega$Var1!=omega$Var2,]
dim(omega)
omega

# (b) What is the probability that at least one of the two members whose name begins with C is selected?

# (c) If the five faculty members have taught for 3, 6, 7, 10, and 14 years, respectively, at the university, what is the probability that the two chosen representatives have a total of at least 18 years teaching experience there?

# Computer keyboard failures can be attributed to electrical defects or mechanical defects. A repair facility currently has 25 failed keyboards, 13 of which have electrical defects and 12 of which have mechanical defects.
#
# |       |  electric |mechanical | row sum |
# |-------|-----------|-----------|---------|
# |failed |     13    |   12      |      25 |
# |good   |           |           |         |
# |col sum|           |           |         |

# (a)
# How many ways are there to randomly select 7 of these keyboards for a thorough inspection (without regard to order)?
# ways

choose(25,7)

# (b)
# In how many ways can a sample of 7 keyboards be selected so that exactly two have an electrical defect?
# ways

choose(13,2)*choose(12,5)

# (c)
# If a sample of 7 keyboards is randomly selected, what is the probability that at least 6 of these will have a mechanical defect? (Round your answer to four decimal places.) 

(choose(12,6)*choose(13,1)+choose(12,7)*choose(13,0))/choose(25,7)

# The route used by a certain motorist in commuting to work contains two intersections with traffic signals. The probability that he must stop at the first signal is 0.35, the analogous probability for the second signal is 0.45, and the probability that he must stop at at least one of the two signals is 0.7.
#
# Let A be the event of stopping at the first sign
# Let B be the even of stopping at the second sign
#
# $P(A)=0.35$
#
# $P(B)=0.45$
#
# $P(A\cup B)=0.7$
#
# $P(A \cap B)=P(A)+P(B)-P(A\cup B)$

# (a) What is the probability that he must stop at both signals?

0.35+0.45-0.7

# (b) What is the probability that he must stop at the first signal but not at the second one?
#
# $P(A \cap B')= P(A) - P(A \cap B)$

0.35-0.1

# (c) What is the probability that he must stop at exactly one signal? 

0.7-0.1

# # Goodness of fit

# Consider the accompanying 2 × 3 table displaying the sample proportions that fell in the various combinations of categories (e.g., 12% of those in the sample were in the first category of both factors).
#
# | |1     | 2   | 3  |
# |-|------|-----|----|
# |1|	0.12 |0.20 |0.28|
# |2|	0.07 |0.11 |0.22|

# (a) Suppose the sample consisted of n = 100 people. Use the chi-squared test for independence with significance level 0.10. 
#
# State the appropriate hypotheses.
# H0: pij = pi · pj
# for every pair (i, j)
# Ha: at least one pij ≠ pi · pj

# Calculate the test statistic. (Round your answer to two decimal places.)
# χ2 =

n=100
observed_proportion <- rbind(c(0.12,0.20,0.28),c(0.07,0.11,0.22))
df <- prod(dim(observed_proportion)-1)
expected_proportion <- outer(rowSums(observed_proportion),colSums(observed_proportion))
observed_frequency  <- observed_proportion*n 
expected_frequency  <- expected_proportion*n
stat <- sum((observed_frequency-expected_frequency)^2/expected_frequency)
stat

# What can be said about the P-value for the test?
# P-value < 0.005 0.005 < P-value < 0.01     0.01 < P-value < 0.025 0.025 < P-value < 0.05 0.05 < P-value < 0.10 P-value > 0.10

p_value  <- 1-pchisq(stat,df)
p_value

# State the conclusion in the problem context.
# Fail to reject H0. An individual's category with respect to factor 1 is independent of the category with respect to factor 2.     

# (b) Suppose the sample consisted of n = 1000 people. Use the chi-squared test for independence with significance level 0.10.
# Calculate the test statistic. (Round your answer to two decimal places.)
# χ2 =

n=1000
expected_proportion <- outer(rowSums(observed_proportion),colSums(observed_proportion))
observed_frequency  <- observed_proportion*n 
expected_frequency  <- expected_proportion*n
stat <- sum((observed_frequency-expected_frequency)^2/expected_frequency)
stat

# What can be said about the P-value for the test?
# P-value < 0.005 0.005 < P-value < 0.01     0.01 < P-value < 0.025 0.025 < P-value < 0.05 0.05 < P-value < 0.10 P-value > 0.10

p_value  <- 1-pchisq(stat,df)
p_value

# State the conclusion in the problem context.
#
# Reject H0. An individual's category with respect to factor 1 is not independent of the category with respect to factor 2.

# (c) What is the smallest sample size n for which these observed proportions would result in rejection of the independence hypothesis? (Round your answer up to the next whole number.)
# n = 

alpha=0.1
critical_stat <-  qchisq(1-alpha,df)
n=critical_stat/sum((observed_proportion-expected_proportion)^2/expected_proportion)
ceiling(n)

# # Practice Problems
# An article considered regressing
# y = 28-day standard-cured strength (psi)
# against
# x = accelerated strength (psi).
# Suppose the equation of the true regression line is
# y = 1900 + 1.3x,
# and that the standard deviation of the random deviation ϵ is 350 psi.

# + jupyter={"outputs_hidden": true}
B0=1900
B1=1.3
s=350
# -

# (a)
# What is the probability that the observed value of 28-day strength will exceed 5000 psi when the value of accelerated strength is 2000? (Round your answer to four decimal places.)
# (No Response)

y_2000=B0+B1*2000
round(1-pnorm(5000,y_2000,s),4)

# (b)
# What is the probability that the observed value of 28-day strength will exceed 5000 psi when the value of accelerated strength is 2500? (Round your answer to four decimal places.)
# (No Response)

y_2500=B0+B1*2500
round(1-pnorm(5000,y_2500,s),4)

# (c)
# Consider making two independent observations on 28-day strength, the first for an accelerated strength of 2000 and the second for
# x = 2500.
# What is the probability that the second observation will exceed the first by more than 1000 psi? (Round your answer to four decimal places.)

s_diff=sqrt(s^2+s^2)
y_diff=B1*(2500-2000)
round(1-pnorm(1000,y_diff,s_diff),4)

# (d) 
# Let Y1 and Y2 denote observations on 28-day strength when
# x = x1
# and
# x = x2,
# respectively. By how much would
# x2
# have to exceed
# x1
# in order that
# P(Y2 > Y1) = 0.95?
# (Round your answer to two decimal places.)
#
# $(Y2-Y1) \sim N(0,s_{diff}/B1)$

round(qnorm(0.95,0,s_diff/B1),2)

# # 2. 
# An article reported that for a sample of 47 kitchens with gas cooking appliances monitored during a one-week period, the sample mean CO2 level (ppm) was 654.16, and the sample standard deviation 
# was 166.08.
#
#
# (a) Calculate and interpret a 95% (two-sided) confidence interval for true average CO2 level in the population of all homes from which the sample was selected. (Round your answers to two decimal places.)

# +
n=47
mu=654.16
s=166.08
SE=s/sqrt(n)

alpha=1-0.95
z=qnorm(1-alpha/2)
error=z*SE

round(c(mu-error,mu+error),2)
# -

# Interpret the resulting interval.
#
# We are 95% confident that this interval contains the true population mean.

# (b) Suppose the investigators had made a rough guess of 169 for the value of s before collecting data. What sample size would be necessary to obtain an interval width of 52 ppm for a confidence level of 95%? (Round your answer up to the nearest whole number.)
#  kitchens

width=52
alpha=1-0.95
z=qnorm(1-alpha/2)
s=169
n=ceiling((2*s*z/width)^2)
n

# # 3. –/5 points DevoreStat9 8.E.033.
# An article described an investigation into the coating weights for large pipes resulting from a galvanized coating process. Production standards call for a true average weight of 200 lb per pipe. The accompanying descriptive summary and boxplot are from Minitab.
#
# |Variable|N|Mean|Median|TrMean|StDev|SEMean|Min|Max|Q1|Q3|
# |--------|-|----|------|------|-----|------|---|---|--|--|
# |ctg wt|30|206.48|206.00|206.81|6.35|1.16|193.00|218.00|202.75|212.00|
#
#
#

# (a) What does the boxplot suggest about the status of the specification for true average coating weight?
#
# It appears that the true average weight could be significantly off from the production specification of 200 lb per pipe.

# (b) A normal probability plot of the data was quite straight. Use the descriptive output to test the appropriate hypotheses. (Use α = 0.05.)
# State the appropriate hypotheses.
#
# H0: μ = 200
# Ha: μ ≠ 200
#
# Calculate the test statistic and determine the P-value. (Round your test statistic to two decimal places and your P-value to three decimal places.)

alpha=0.05
x_bar=206.48
x_0=200
s=6.35
n=30
df=n-1
SE=s/sqrt(n)
t=(x_bar-x_0)/SE
round(t,2)
p_value=1-pt(t,df)
round(p_value,3)

# What can you conclude?
#
# Reject the null hypothesis. There is sufficient evidence to conclude that the true average weight differs from 200 lb per pipe.

if(p_value<alpha) {print("reject H0")} else {print("do not reject H0")}

# # 4.
# In an investigation of alcohol use among college students, each male student in a sample was categorized both according to age group and according to the number of heavy drinking episodes during the previous 30 days.
#
# |Episodes\Age Group| 18–20 | 21–23| ≥24 |
# |------------------|-------|------|-----|
# | None             | 354   | 293  | 592 |
# | 1–2              | 218   | 288  | 354 |
# | 3–4              | 182   | 218  | 185 |
# | ≥ 5              | 328   | 331  | 147 |

observed_frequency=cbind(c(354,218,182,328),c(293,288,218,331),c(592,354,185,147))
observed_frequency

# Does there appear to be an association between extent of binge drinking and age group in the population from which the sample was selected? Carry out a test of hypotheses at significance level 0.01.
#
# State the appropriate hypotheses.
#
# H0: pij =  pi·pj
#     i = 1, 2, 3, 4; j = 1, 2, 3
#
# Ha: at least one pij ≠ pi·pj

# Compute the test statistic value. (Round your answer to three decimal places.)

expected_frequency=outer(rowSums(observed_frequency),colSums(observed_frequency))/sum(observed_frequency)
stat=sum((observed_frequency-expected_frequency)^2/(expected_frequency))
round(stat,3)

# What can you say about the P-value?
# P-value < 0.005 0.005 < P-value < 0.01     0.01 < P-value < 0.025 0.025 < P-value < 0.05 0.05 < P-value < 0.10 P-value > 0.10

df=prod(dim(observed_frequency)-1)
p_value=1-pchisq(stat,df)
p_value

n=1361
observed_frequency=c(330,331,374,326)
expected_frequency=rep(n/4,4)
stat=sum((observed_frequency-expected_frequency)^2/expected_frequency)
stat
1-pchisq(stat,length(observed_frequency)-1)

# State the conclusion in the problem context.
# Reject H0. There is evidence of a significant association between binge drinking and age group.

# The bond behavior of reinforcing bars is an important determinant of strength and stability. The article "Experimental Study on the Bond Behavior of Reinforcing Bars Embedded in Concrete Subjected to Lateral Pressure"† reported the results of one experiment in which varying levels of lateral pressure were applied to 21 concrete cube specimens, each with an embedded 16 mm plain steel round bar, and the corresponding bond capacity was determined. Due to differing concrete cube strengths
# (fcu,
# in MPa), the applied lateral pressure was equivalent to a fixed proportion of the specimen's
# fcu
# (0, 0.1fcu,   , 0.6fcu).
# Also, since bond strength can be heavily influenced by the specimen's
# fcu,
# bond capacity was expressed as the ratio of bond strength (MPa) to
# 	fcu
# .

x <- c(0 ,0 ,0 ,0.1 ,0.1 ,0.1 ,0.2 ,0.2 ,0.2 ,0.3 ,0.3 ,0.3 ,0.4,0.4 ,0.4 ,0.5 ,0.5 ,0.5 ,0.6 ,0.6 ,0.6)
y <- c(0.123 ,0.100 ,0.101 ,0.172 ,0.133 ,0.107 ,0.217 ,0.172 ,0.151 ,0.263 ,0.227 ,0.252 ,0.310 ,0.365 ,0.239 ,0.365 ,0.319 ,0.312 ,0.394 ,0.386 ,0.320)
plot(x,y)

# (a)
# Does a scatterplot of the data support the use of the simple linear regression model?
# A scatterplot of the data shows a weak, positive, 
#
# A scatterplot of the data shows a reasonably strong, positive, linear relationship between pressure and the bond capacity ratio and supports the use of a simple linear regression model.     

# (b)
# Use the accompanying Minitab output to give point estimates of the slope and intercept of the population regression line. (Enter your answers to five decimal places.)
#
# The regression equation is Ratio = 0.101 + 0.461 Pressure
#
# Predictor 	Coef 	SE Coef 	T 	P
# Constant 	0.10121 	0.01308 	7.74 	0.000
# Pressure 	0.46071 	0.03627 	12.70 	0.000
# S = 0.0332397    R-Sq = 89.5%    R-Sq(adj) = 88.9%
#
# Analysis of Variance Source 	DF 	SS 	MS 	F 	P
# Regression 	1 	0.17830 	0.17830 	161.37 	0.000
# Residual Error 	19 	0.02099 	0.00110 		
# Total 	20 	0.19929 			
# slope intercept

# + jupyter={"outputs_hidden": true}
B0=0.10121
B1=0.46071
# -

# (c)
# Calculate a point estimate of the true average bond capacity when lateral pressure is
# 0.16fcu.
# (Round your answer to four decimal places.)

B0+B1*0.16

# (d)
# What is a point estimate of the error standard deviation σ? (Enter your answer to seven decimal places.)
#
# How would you interpret the point estimate of the error standard deviation σ?
#
# This represents the typical difference between a concrete specimen's actual bond capacity ratio and the ratio predicted by the least squares regression line.

# +
n <- length(x)
df <- n-2
sxx   <- sum((x-mean(x))^2) 
syy   <- sum((y-mean(y))^2)
sxy   <- sum((x-mean(x))*(y-mean(y)))
B1    <- sxy/sxx
B0    <- mean(y)-B1*mean(x)
predict_y <- B0+B1*x
sse <- sum((y-predict_y)^2)
s <- sqrt(sse/df)
s
s_B1 <- s/sqrt(sxx) 
B1_0 <- 0 #null hypothesis
t <- (B1-B1_0)/s_B1
p_value <- pt(-abs(t),df)+(1-pt(abs(t),df)) #two-tails

#paste("y=",B0,"+",B1,"*x")
#r_squared <- (syy-sse)/syy
#round(r_squared,3)
#paste("t=",round(t,2),";","p_value=",round(p_value,4))
# -

# (e)
# What is the value of total variation? (Enter your answer to five decimal places.)
# What percentage of it can be explained by the model relationship? (Enter your answer to one decimal place.) 

syy

r_squared

# + jupyter={"outputs_hidden": true}

