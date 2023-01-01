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

# # First axiom
# The probability of an outcome of an event is a non-negative real number:
#
# $P(E) >= 0 $

# # Second axiom
# The probability of at least one outcome out of all possible outcomes for an event is 1.
#
# $P(\Omega) = 1$

# # Third axiom
# The probability of the union of mutually exclusive events is the sum of the probabilities of each event.
#
# given A and B are mutually exclusive:
# $P(A \cup B) = P(A) + P(B)$

# # The probability of the empty set is 0 
#
# $P(\emptyset) = 0$

# # The probability of the union of events
#
# $ P(A \cup B) =  P(A) + P(B) - P(A \cap B)$

# # Definition of Complementary Probability
# The probability of not A is 1 minus probability of A
#
# $ P(A') = 1 - P(A) $

# # Definition of Conditional Probability
#
# The Probability of A given B is the Probability of A intersect B divided by the probability of B.
#
# $P(A|B) = \frac{P(A \cap B)}{P(B)}$
#

# # Definition of Independent Events
#
# A and B are independent if: $P(A|B) = P(A)$

# # Bayes Theorem
#
# $P(A|B) = \frac{P(B|A)P(A)}{P(B)}$

# # Partition
#
# $P(B) = P(B|A)P(A) + P(B|A')P(A')$

# # Shortcut Formula For Variance
#
# $  \sigma^2 = \frac{\sum{(y_i - \bar{y})^2}}{n-1} $ 
#
# $ = \frac{\sum{(y_i^2 - 2 \bar{y} y_i + \bar{y}^2)}}{n-1} $
#
# $ = \frac{\sum{y_i^2} - \sum{2 \bar{y} y_i} + \sum{\bar{y}^2}}{n-1} $
#
# $ = \frac{\sum{y_i^2} - 2 \bar{y} \sum{y_i} + \bar{y}^2 \sum{1}}{n-1} $
#
# $ = \frac{\sum{y_i^2} - 2 (\frac{\sum{y_i}}{n}) \sum{y_i} + (\frac{\sum{y_i}}{n})^2 \sum{1}}{n-1} $
#
# $ = \frac{\sum{y_i^2} - 2 (\frac{(\sum{y_i})^2}{n}) + \frac{(\sum{y_i})^2}{n} }{n-1} $
#
# $ = \frac{\sum{y^2_i} - \frac{(\sum{y_i})^2}{n}}{n-1} $


