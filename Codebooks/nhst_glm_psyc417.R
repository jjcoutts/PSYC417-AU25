# NHST and GLM code
# PSYC417 

# load the required packages
library(ggplot2) # visualizations
library(jtools) # APA graphics
library(dplyr) # data wrangling

teams <-read.csv("teams.csv")

# explore the data
summary(teams) # descriptive statistics
str(teams) # structure summary similar to global environment

# plot dysfunction by teams
ggplot(data = teams, aes(x = dysfunc, y = perform)) + 
  geom_point(color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "red") + # add line of best fit
  labs(x = "Team Dysfunction", y = "Team Performance") +
  jtools::theme_apa() 
# looks to be small positive relationship 

# fit simple regression model with perform regressed on dysfunction
simp_reg_model <- lm(perform ~ dysfunc, data = teams)
summary(simp_reg_model) # obtain model estimates
anova(simp_reg_model) # obtain SSE information (ancillary)
# generate CIs
confint(simp_reg_model)
# effect of dysfunction is statistically significant
# b0: when dysfunction was 0, performance was expected to be at -0.035. Since dysfunction does not have a meaningful 0, this isn't a meaningful interpretation.
# b1: for each one-unit increase in dysfunction increased performance by 0.110 units

# manually conduct hypothesis test
test_stat = 0.11025/0.18402;test_stat # generate test statistic 
p_val = (1-pt(test_stat, 58))*2;p_val # for two-tailed, remove *2 for one-tailed

##### check assumptions
# normality of residuals
hist(resid(simp_reg_model)) # looks normal, good! 
qqnorm(resid(simp_reg_model)) # looks like a straight enough line, good! 

# linearity
plot(simp_reg_model, 1) # somewhat concerning

# homogeneity of variance
plot(simp_reg_model, 3) # concerning

##### end check assumptions

# plot dysfunction by teams
ggplot(data = teams, aes(x = dysfunc, y = perform)) + 
  geom_point(color = "navy") + # make scatterplot
  geom_abline(intercept = -0.03549, slope = 0.11025) + # add line of best fit manually
  geom_vline(xintercept = 0.2, color = "firebrick", linetype = "dashed") + # one unit increase in x
  geom_vline(xintercept = 1.2, color = "firebrick", linetype = "dashed") + # one unit increase in x
  geom_hline(yintercept = -0.01254, color = "firebrick", linetype = "dashed") + # predicted value of y at x = .2
  geom_hline(yintercept = 0.09771, color = "firebrick", linetype = "dashed") + # predicted value of y at x = 1.2
  geom_hline(yintercept = -0.03549, color = "forestgreen") + # line for y-intercept
  geom_vline(xintercept = 0, color = "forestgreen") + # line for y-intercept
  xlim(c(0,2)) + # start graph at 0 
  jtools::theme_apa() # make APA format

# if X is dichotomous and coded as 0 and 1, the estimates have a nice interpretation
summary(glm(perform~eval, data = teams))

# show that all models are the same with the same data
set.seed(40000) # to reproduce results
x = c(rep(0,100), rep(1,100)) # x dichotomous
y = x*.3 + rnorm(200) # y correlated with x
data = data.frame(x = x, y =y) # put x and y into a data frame

# GLM models 
t.test(data$y ~ as.factor(x), var.equal = TRUE) # t test
summary(aov(data$y ~ as.factor(x))) # as an ANOVA
summary(glm(y ~ x, data = data)) # simple regression
summary(glm(scale(y) ~ scale(x), data = data)) # standardized simple regression
cor.test(data$y, data$x) # correlation model

mult_reg_model <- lm(perform ~ dysfunc + negtone, data = teams)
summary(simp_reg_model) # obtain model estimates
anova(mult_reg_model) # obtain SSE information (ancillary)
# generate CIs
confint(mult_reg_model)
# effect of dysfunction is no longer significant but the effect of negative tone is. 
# b0: when dysfunction and negative tone were 0, performance was expected to be at -0.022. This is not a meaningful interpretation.
# b1: for each one-unit increase in dysfunction, performance increased by 0.44 units assuming they had the same negative tone. 
# b2: For each one-unit increase in negative tone, performance decreased by 0.534 units holding dysfunction constant. 
# negtone was either confounding or masking the relationship between dysfunction and performance

# you can make these models quite complex
big_model <- glm(perform ~ dysfunc + negtone + negexp + division, data = teams)
summary(big_model)
anova(big_model)
# b0: expected value of Y when all predictors are 0
# b(k where k is kth predictor) - expected change in Y with a one-unit increase in x_k controlling for the other k-1 variables in the model



### end of script