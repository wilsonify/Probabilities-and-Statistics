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

# # Homework 2

library('tidyverse')

# ## Problem 1
# Suppose that vehicles taking a particular freeway exit can turn right (R), turn left (L), or go straight (S). Consider observing the direction for each of three successive vehicles. (Enter your answers in set notation. Enter EMPTY or ∅ for the empty set.) 

options <- c('L','R','S')
all_possible <- expand.grid(options,options,options)
all_possible$joined  <- apply(all_possible, 1, paste, collapse="")

# (a) List all outcomes in the event A that all three vehicles go in the same direction.

all_same <- all_possible$Var1==all_possible$Var2 & all_possible$Var2==all_possible$Var3
A <- all_possible[all_same,'joined']
A

# (b) List all outcomes in the event B that all three vehicles take different directions. 

all_diff <- all_possible$Var1!=all_possible$Var2 &
            all_possible$Var1!=all_possible$Var3 &
            all_possible$Var2!=all_possible$Var3
B <- all_possible[all_diff,'joined']
B

# (c) List all outcomes in the event C that exactly two of the three vehicles turn right. 

two_rights <- as.integer(all_possible$Var1=='R')+
              as.integer(all_possible$Var2=='R')+
              as.integer(all_possible$Var3=='R') == 2
C <- all_possible[two_rights,'joined']
C

# (d) List all outcomes in the event D that exactly two vehicles go in the same direction.

two_same <- as.integer(all_possible$Var1==all_possible$Var2) +
            as.integer(all_possible$Var1==all_possible$Var3) +
            as.integer(all_possible$Var2==all_possible$Var3) == 1
D <- all_possible[two_same,'joined']
D

# (e) List outcomes in D'.

D_prime <- all_possible[!two_same,'joined']
D_prime

# List outcomes in C ∪ D.

union(C,D)

# List outcomes in C ∩ D.

intersect(C,D)

# ## Problem 2
#  Three components are connected to form a system as shown in the accompanying diagram. Because the components in the 2–3 subsystem are connected in parallel, that subsystem will function if at least one of the two individual components functions. For the entire system to function, component 1 must function and so must the 2–3 subsystem. 
#
# ![image.png](attachment:image.png)
#
# The experiment consists of determining the condition of each component [S (success) for a functioning component and F (failure) for a nonfunctioning component]. (Enter your answers in set notation. Enter EMPTY or ∅ for the empty set.) 

conditions <- c('S','F')
all_cond <- expand.grid(conditions,conditions,conditions)
all_cond$joined <- apply(all_cond, 1, paste, collapse="")
all_cond$joined

# (a) Which outcomes are contained in the event A that exactly two of the three components function? 

A <- all_cond[rowSums(all_cond=='S')==2,'joined']
A

# (b) Which outcomes are contained in the event B that at least two of the components function?

B <- all_cond[rowSums(all_cond=='S')>=2,'joined']
B

# (c) Which outcomes are contained in the event C that the system functions?

sys_functions <- all_cond$Var1=='S' & rowSums(all_cond=='S')>=2
C <- all_cond[sys_functions,'joined']
C

# (d) List outcomes in C'.

all_cond[!sys_functions,'joined']

# (e) List outcomes in A ∪ C.

union(A,C)

# (f) List outcomes in A ∩ C.

intersect(A,C)

# (g) List outcomes in B ∪ C.

union(B,C)

# (h) List outcomes in B ∩ C. 

intersect(B,C)

# ## Problem 3
# An engineering construction firm is currently working on power plants at three different sites. Let Ai denote the event that the plant at site i is completed by the contract date. Use the operations of union, intersection, and complementation to describe each of the following events in terms of A<sub>1</sub>, A<sub>2</sub>, and A<sub>3</sub>, draw a Venn diagram, and shade the region corresponding to each one. 
#
#

# (a) At least one plant is completed by the contract date. 
#
# A1 ∪ A2 ∪ A3
# ![image.png](attachment:image.png)

# (b) All plants are completed by the contract date.
#
# A1 ∩ A2 ∩ A3
#     
# ![image.png](attachment:image.png)

# (c) Only the plant at site 1 is completed by the contract date. 
#
# A1 ∩ A2' ∩ A3'
#
# ![image.png](attachment:image.png)

# (d) Exactly one plant is completed by the contract date. 
#
# (A1 ∩ A2' ∩ A3') ∪ (A1' ∩ A2 ∩ A3') ∪ (A1' ∩ A2' ∩ A3)
#
# ![image.png](attachment:image.png)

# (e) Either the plant at site 1 or both of the other plants are completed by the contract date.
#
# A1 ∪ (A2 ∩ A3)
#
# ![image.png](attachment:image.png)

# ## Problem 4
# A mutual fund company offers its customers a variety of funds: a money-market fund, three different bond funds (short, intermediate, and long-term), two stock funds (moderate and high-risk), and a balanced fund. Among customers who own shares in just one fund, the percentages of customers in the different funds are as follows. 

money_market        <- 0.24
short_bond          <- 0.14
intermediate_bond   <- 0.07 
long_bond           <- 0.05
high_risk_stock     <- 0.17
moderate_risk_stock <-0.25
balanced            <- 0.08

# (a) What is the probability that the selected individual owns shares in the balanced fund? 

balanced

# (b) What is the probability that the individual owns shares in a bond fund?

short_bond+intermediate_bond+long_bond

# (c) What is the probability that the selected individual does not own shares in a stock fund? 

1-(high_risk_stock+moderate_risk_stock)

# ## Problem 5
# Consider randomly selecting a student at a large university, and let A be the event that the selected student has a Visa card and B be the analogous event for MasterCard. Suppose that

P_A  <-  0.7
P_B  <-  0.4

# Could it be the case that
# P(A ∩ B) = 0.5?
# Why or why not? [Hint: For any two sets A and B if A is a subset of B then
# P(A) ≤ P(B).]
#
# No, this is not possible. Since A ∩ B is contained in the event B, it must be the case that P(A ∩ B) ≤ P(B). However 0.5 > 0.4 violates this requirement.    

#  (b)
# From now on, suppose that
# P(A ∩ B) = 0.3.
# What is the probability that the selected student has at least one of these two types of cards? 

P_AintersectB <- 0.3
P_AorB <- P_A+P_B-P_AintersectB
P_AorB

# (c) What is the probability that the selected student has neither type of card? 
#

1-(P_AorB)

# (d) Describe, in terms of A and B, the event that the selected student has a Visa card but not a MasterCard. 
#
#
# A ∩ B'
#  

# Calculate the probability of this event.

P_A-P_AintersectB

#  (e)
# Calculate the probability that the selected student has exactly one of the two types of cards. 

(P_A-P_AintersectB)+(P_B-P_AintersectB)

# ## Problem 6
# Suppose that 50% of all adults regularly consume coffee, 55% regularly consume carbonated soda, and 65% regularly consume at least one of these two products. 

P_coffee <- 0.5
P_soda <- 0.55
P_coffee_or_soda <- 0.65

# (a) What is the probability that a randomly selected adult regularly consumes both coffee and soda?

P_coffee_and_soda <- P_coffee+P_soda-P_coffee_or_soda
P_coffee_and_soda

# (b) What is the probability that a randomly selected adult doesn't regularly consume at least one of these two products?

1-P_coffee_or_soda

# ## Problem 7
# An insurance company offers four different deductible levels—none, low, medium, and high—for its homeowner's policyholders and three different levels—low, medium, and high—for its automobile policyholders. The accompanying table gives proportions for the various categories of policyholders who have both types of insurance. For example, the proportion of individuals with both low homeowner's deductible and low auto deductible is 0.07 (7% of all such individuals). 

L_Auto <-c(0.04,0.07,0.05,0.04)
M_Auto <-c(0.07,0.12,0.20,0.06)
H_Auto <-c(0.02,0.03,0.15,0.15)
insurance <- data.frame(rbind(L_Auto,M_Auto,H_Auto))
colnames(insurance) <- c('N_Home','L_Home','M_Home','H_Home')
insurance

# Suppose an individual having both types of policies is randomly selected.
# (a) What is the probability that the individual has a medium auto deductible and a high homeowner's deductible?

insurance['M_Auto','H_Home']

# (b) What is the probability that the individual has a low auto deductible? 
# A low homeowner's deductible?

P_L_Auto <- sum(insurance['L_Auto',])
P_L_Auto
P_L_Home <- sum(insurance[,'L_Home'])
P_L_Home

# (c) What is the probability that the individual is in the same category for both auto and homeowner's deductibles?

P_same <- insurance['L_Auto','L_Home']+
insurance['M_Auto','M_Home']+
insurance['H_Auto','H_Home']
P_same

# (d) Based on your answer in part (c), what is the probability that the two categories are different?

P_diff <- 1-P_same
P_diff

# (e) What is the probability that the individual has at least one low deductible level?

P_one_low <- sum(insurance['L_Auto',        ])+
             sum(insurance[        ,'L_Home'])-
                 insurance['L_Auto','L_Home']
P_one_low

# (f) Using the answer in part (e), what is the probability that neither deductible level is low?

1-P_one_low

# ## Problem 8
# Show that if one event A is contained in another event B (i.e., A is a subset of B), then
# P(A) ≤ P(B).
# [Hint: For such A and B, A and
# B ∩ A'
# are disjoint and
# B = A ∪ (B ∩ A'),
# as can be seen from a Venn diagram.] 
#
#
# Since A is contained in B, we may write B as 
# B=A ∪ (B ∩ A'), the union of two mutually exclusive events. 
#
# Then P(B) = P(A ∪ (B ∩ A'))
# and  P(B) = P(A) + P(B ∩ A') 
#
#
# Then, since P(B ∩ A')>=0
# it follows that P(B) ≥ P(A)+0. 
#
# This proves the statement. 

#  For general A and B, what does this imply about the relationship among
# P(A ∩ B),
# P(A)
# and
# P(A ∪ B)?
#
# P(A ∩ B) ≤ P(A) ≤ P(A ∪ B)
#

# ## Problem 9
# An academic department with five faculty members—Anderson, Box, Cox, Cramer, and Fisher—must select two of its members to serve on a personnel review committee. Because the work will be time-consuming, no one is anxious to serve, so it is decided that the representative will be selected by putting the names on identical pieces of paper and then randomly selecting two. 

members <- c('Anderson', 'Box', 'Cox', 'Cramer', 'Fisher')
initials <- c('A','B','C','C','F')
outcomes <- t(combn(members,m = 2, simplify = TRUE))
outcomes

# (a) What is the probability that both Anderson and Box will be selected? [Hint: List the equally likely outcomes.]

n_outcomes=dim(outcomes)[1]
1/n_outcomes

# (b) What is the probability that at least one of the two members whose name begins with C is selected?

init_outcomes <- t(combn(initials,m = 2, simplify = TRUE))
isC <- init_outcomes=='C'
sum(apply(isC,1,any))/n_outcomes

# (c) If the five faculty members have taught for 3, 6, 7, 10, and 14 years, respectively, at the university, what is the probability that the two chosen representatives have a total of at least 16 years teaching experience there?

years_taught <- c(3,6,7,10,14)
years_outcomes <- t(combn(years_taught,m = 2, simplify = TRUE))
sum(apply(years_outcomes,1,sum)>=16)/n_outcomes

# ## Problem 10
#  As of April 2006, roughly 50 million .com web domain names were registered (e.g., yahoo.com).

two_letter_domains <- expand.grid(letters,letters)

# (a)
# How many domain names consisting of just two letters in sequence can be formed?

dim(two_letter_domains)[1]
26*26

# How many domain names of length two are there if digits as well as letters are permitted as characters? [Note: A character length of three or more is now mandated.]

36*36

# (b)
# How many domain names are there consisting of three letters in sequence? [Note: All are currently taken.]

26*26*26

# How many of this length are there if either letters or digits are permitted?

36*36*36

# (c)
# How many domain names are there consisting of four letters in sequence?

26^4

# How many of this length are there if either letters or digits are permitted?

36^4

# (d)
# As of a certain date, 98,795 of the four-character sequences using either letters or digits had not yet been claimed. If a four-character name is randomly selected on that date, what is the probability that it is already owned? (Round your answer to four decimal places.) 

round(1-98795/(36^4),4)

# ## Problem 11
# Computer keyboard failures can be attributed to electrical defects or mechanical defects. A repair facility currently has 25 failed keyboards, 8 of which have electrical defects and 17 of which have mechanical defects. 

N <- 25
n <- 7
n_electrical <- 8
n_mechanical <- 17

# (a)
# How many ways are there to randomly select 7 of these keyboards for a thorough inspection (without regard to order)?

total  <- choose(N,n)
total

# (b)
# In how many ways can a sample of 7 keyboards be selected so that exactly two have an electrical defect?

choose(n_electrical,2)*choose(N-n_electrical,n-2)

# (c)
# If a sample of 7 keyboards is randomly selected, what is the probability that at least 6 of these will have a mechanical defect? (Round your answer to four decimal places.) 

case_6 <- choose(n_mechanical,6)*choose(N-n_mechanical,n-6)
case_7 <- choose(n_mechanical,7)*choose(N-n_mechanical,n-7)
round((case_6+case_7)/total,4)

# ## Problem 12
# The population of a particular country consists of three ethnic groups. Each individual belongs to one of the four major blood groups. The accompanying joint probability table gives the proportions of individuals in the various ethnic group-blood group combinations. 

eth_1 <- c(0.082,0.115,0.011,0.004)
eth_2 <- c(0.126,0.141,0.018,0.003)
eth_3 <- c(0.215,0.209,0.056,0.020)
blood_demo <- rbind(eth_1,eth_2,eth_3)
colnames(blood_demo) <- c('O','A','B','AB')
blood_demo

# Suppose that an individual is randomly selected from the population, and define events by A = {type A selected}, B = {type B selected}, and C = {ethnic group 3 selected}. 

P_A <- sum(blood_demo[,'A']) 
P_B <- sum(blood_demo[,'B'])
P_C <- sum(blood_demo['eth_3',])

# (a) Calculate P(A), P(C), and P(A ∩ C). (Enter your answers to three decimal places.) 

P_A
P_C
P_A_and_C <- blood_demo['eth_3','A']
P_A_and_C

# (b) Calculate both P(A | C) and P(C | A). (Round your answers to three decimal places.) 

P_A_and_C/P_C
round(P_A_and_C/P_A,3)

# Explain in context what each of these probabilities represents. (Select all that apply.) 
#
# If we know that the individual came from ethnic group 3, the probability that he has type A is given by P(A | C).
#
# If a person has type A blood, the probability that he is from ethnic group 3 is given by P(C | A).

#
# (c) If the selected individual does not have type B blood, what is the probability that he or she is from ethnic group 1? (Round your answer to three decimal places.)

P_notB <- 1-P_B
P_1_and_notB <- sum(blood_demo['eth_1',c('O','A','AB')])
round(P_1_and_notB/P_notB,3)

# ## Problem 13
#
#  A department store sells sport shirts in three sizes (small, medium, and large), three patterns (plaid, print, and stripe), and two sleeve lengths (long and short). The accompanying tables give the proportions of shirts sold in the various category combinations. 

# +
short_sleeve <- rbind(S=c(0.04,0.02,0.05)
                      ,M=c(0.08,0.08,0.12)
                      ,L=c(0.03,0.07,0.08))
colnames(short_sleeve) <-c('Pl','Pr','St')    
short_sleeve

long_sleeve <- rbind(S=c(0.03,0.02,0.03)
                     ,M=c(0.10,0.04,0.07)
                     ,L=c(0.04,0.02,0.08))
colnames(long_sleeve) <-c('Pl','Pr','St')    
long_sleeve

# -

# (a) What is the probability that the next shirt sold is a medium, long-sleeved, print shirt?

long_sleeve['M','Pr']

# (b) What is the probability that the next shirt sold is a medium print shirt?

short_sleeve['M','Pr']+long_sleeve['M','Pr']

# (c) What is the probability that the next shirt sold is a short-sleeved shirt? 
# A long-sleeved shirt?

sum(short_sleeve)
sum(long_sleeve)

# (d) What is the probability that the size of the next shirt sold is medium?

sum(short_sleeve['M',])+
sum(long_sleeve['M',])

# What is the probability that the pattern of the next shirt sold is a print?

sum(short_sleeve[,'Pr'])+
sum(long_sleeve[,'Pr'])

# (e) Given that the shirt just sold was a short-sleeved plaid, what is the probability that its size was medium? (Round your answer to three decimal places.)

round(short_sleeve['M','Pl']/sum(short_sleeve[,'Pl']),3)

# (f) Given that the shirt just sold was a medium plaid, what is the probability that it was short-sleeved? 
#
# Long-sleeved? (Round your answer to three decimal places.)

round(short_sleeve['M','Pl']/(short_sleeve['M','Pl']+long_sleeve['M','Pl']),3)
round(long_sleeve['M','Pl']/(short_sleeve['M','Pl']+long_sleeve['M','Pl']),3)

# ## Problem 14
# If P(B | A) > P(B), show that
# P(B' | A) < P(B').
# [Hint: Add
# P(B' | A)
# to both sides of the given inequality and then use the fact that
# P(A | B) + P(A' | B) = 1.]
#
#
# P(B | A) + P(B' | A) > P(B) + P(B' | A)
#
# 1                    > P(B) + P(B' | A) 
#
# 1-P(B)               > P(B' | A)
#
# 1-P(B)               > P(B' | A)
#
# P(B' | A)            < P(B') 	

# ## Problem 15
# Seventy-eight percent of the light aircraft that disappear while in flight in a certain country are subsequently discovered. Of the aircraft that are discovered, 68% have an emergency locator, whereas 90% of the aircraft not discovered do not have such a locator. Suppose a light aircraft has disappeared. (Round your answers to three decimal places.)

#A:discovered
#B:has locator
P_A <- 0.78 #given
P_B_given_A <- 0.68 #given
P_notB_given_notA <- 0.9 #given

# (a) If it has an emergency locator, what is the probability that it will not be discovered?
#
# given:
#
# $P(A)= 0.78$
# and
#
# $P(B|A) = 0.68$ 
# and
#
# $P(B'|A') = 0.9$ 
#
# definition of complement
#
# $P(A') = 1-P(A) = 1-0.78 = 0.22$ 
#
# complement of conditional probability
#
# $P(B|A')=1-P(B'|A')=1-0.9=0.1 $
#
# partition B
#
# $P(B)=P(A) \cdot P(B|A) + P(A') \cdot P(B|A')$
#
# $=0.78 \cdot 0.68   + 0.22 \cdot 0.1 = 0.5524$
#
# Apply Bayes Theorem
#
# $P(A'|B)=P(B|A') \cdot \frac{P(A')}{P(B)}$
#          
# $=0.1 \cdot \frac{0.22}{0.5524} = 0.0398$

P_notA <- 1-P_A #definition of complement
P_B_given_notA=1-P_notB_given_notA #complement of conditional probability
P_B=P_A*P_B_given_A+P_notA*P_B_given_notA # partition of B
P_notA_given_B=P_B_given_notA*P_notA/P_B # Bayes Theorem
round(P_notA_given_B,3)

# (b) If it does not have an emergency locator, what is the probability that it will be discovered?
#
# complement
#
# $P(B') = 1-P(B) = 1-0.5524 = 0.4476$
#
# complement of conditional prob
#
# $P(B'|A)=1-P(B|A) = 1 - 0.68 = 0.32$
#
# Bayes Theorm
#
# $P(A|B')=P(B'|A) \cdot \frac{P(A)}{P(B')}$
# $=0.32 \cdot \frac{0.78}{0.4476} = 0.558$

P_notB=1-P_B # complement
P_notB_given_A=1-P_B_given_A # complement of conditional prob
P_A_given_notB=P_notB_given_A*P_A/P_notB
round(P_A_given_notB,3)

# ## Problem 16
#  Blue Cab operates 15% of the taxis in a certain city, and Green Cab operates the other 85%. After a nighttime hit-and-run accident involving a taxi, an eyewitness said the vehicle was blue. Suppose, though, that under night vision conditions, only 80% of individuals can correctly distinguish between a blue and a green vehicle. What is the (posterior) probability that the taxi at fault was blue? [Hint: A tree diagram might help. Note: This is based on an actual incident.] (Round your answer to four decimal places.)
#  
# Define
#
# B: Blue at Fault
#
# G: Green at Fault
#
# IB: ID as Blue
#
# IG: ID at Green
#
# Given
#
# P(G)=0.85 and P(B)=0.15
#
# P(IB|B)=0.8 and P(IG|G)=0.8
#
# P(IB|G)=0.2 and P(IG|B)=0.2
#
# Partition IB 
#
# $P(IB)=P(G) \cdot P(IB|G)+P(B) \cdot P(IB|B)$
#
# $P(IB)=0.85 \cdot 0.2+0.15 \cdot 0.8=0.29$
#
# Bayes Theorm
#
# $P(BF|IB)=P(IB|B) \cdot \frac{P(B)}{P(IB)}$
#
# $=0.8 \cdot \frac{0.15}{0.29}=0.4138$

# +
#Given:
P_G=0.85
P_B=0.15
P_IB_given_B=0.8
P_IB_given_G=0.2

P_IB=P_G*P_IB_given_G+P_B*P_IB_given_B #Partition IB 
P_B_given_IB=P_IB_given_B*P_B/P_IB #Bayes Theorem
round(P_B_given_IB,4)
# -

# Indicate the probability rule you used. 
#
# Bayes Theorm

# ## Problem 17
# An oil exploration company currently has two active projects, one in Asia and the other in Europe. Let A be the event that the Asian project is successful and B be the event that the European project is successful. Suppose that A and B are independent events with P(A) = 0.2 and P(B) = 0.8.

P_A=0.2
P_B=0.8

# (a) If the Asian project is not successful, what is the probability that the European project is also not successful?

P_notA=1-P_A
P_notB=1-P_B
P_notB

# Explain your reasoning.
#
# Since the events are independent, then A' and B' are independent.     

# (b) What is the probability that at least one of the two projects will be successful?

P_A_and_B=P_A*P_B # intesection of independent events
P_A_or_B=P_A+P_B-P_A_and_B # sum rule
P_A_or_B

# (c) Given that at least one of the two projects is successful, what is the probability that only the Asian project is successful? (Round your answer to three decimal places.)
#
# $P(A \cap B') = P(A)-P(A \cup B)$

P_A_and_notB_given_A_or_B=(P_A-P_A_and_B)/P_A_or_B
round(P_A_and_notB_given_A_or_B,3)

# ## Problem 18
# Suppose that the proportions of blood phenotypes in a particular population are as follows:
#
#
# | A  | B  | AB | O  |
# |----|----|----|----|
# |0.48|0.13|0.05|0.34|
#
#
# Assuming that the phenotypes of two randomly selected individuals are independent of one another, what is the probability that both phenotypes are O? (Enter your answer to four decimal places.)

P_O=0.34
P_O^2

# What is the probability that the phenotypes of two randomly selected individuals match? (Enter your answer to four decimal places.)

# +
P_A=0.48
P_B=0.13
P_AB=0.05
P_O=0.34

P_A^2+P_B^2+P_AB^2+P_O^2
# -

# ## Problem 19
# Consider independently rolling two fair dice, one red and the other green. Let A be the event that the red die shows 3 dots, B be the event that the green die shows 4 dots, and C be the event that the total number of dots showing on the two dice is 7. Are these events pairwise independent (i.e., are A and B independent events, are A and C independent, and are B and C independent)? 
#
# Yes, the events are pairwise independent.

# Are the three events mutually independent? 
#
# No, the three events are not mutually independent.

# ## Problem 20
# The probability that an individual randomly selected from a particular population has a certain disease is 0.06. A diagnostic test correctly detects the presence of the disease 97% of the time and correctly detects the absence of the disease 99% of the time. If the test is applied twice, the two test results are independent, and both are positive, what is the (posterior) probability that the selected individual has the disease? [Hint: Tree diagram with first-generation branches corresponding to Disease and No Disease, and second- and third-generation branches corresponding to results of the two tests.] 

# D:Diseased
# H:Healthy
# P:Positive Test Result
# N:Negative Test Result

# +
P_D=0.06
P_H=1-P_D

P_D_given_P=0.97
P_H_given_P=1-P_D_given_P

P_H_given_N=0.99
P_D_given_N=1-P_H_given_N
# -

P_PP_given_D=P_D_given_P*P_D_given_P/P_D
P_PP_given_H=P_H_given_P*P_H_given_P/P_H
P_PP=P_PP_given_D+P_PP_given_H #Partition PP 
P_D_given_PP=P_PP_given_D*P_D/(P_PP)


