# Following Guide on https://ase.tufts.edu/gsc/gradresources/guidetomixedmodelsinr/mixed%20model%20guide.html
# Following teacher value-added in https://www.nber.org/papers/w14607.pdf

# Load (possibly) required packages
require(lme4)
require(car)
require(MASS)
require(mlmRev)
require(ggplot2)
require(pscl)
require(fitdistrplus)
require(dplyr)
require(MCMCglmm)
require(glmmTMB)
require(DHARMa)

# No scientific notation (optional)
options(scipen=999)

# Load Data
model_data = read.csv("C:/Users/abeers/Documents/Projects/CampaignZero/CPZ/data/processed/nashville/nashville_model_formatted_citizen.csv")

# Change Data Types and Normalize
model_data$police_id = as.factor(model_data$police_id)
model_data$division = as.factor(model_data$division)
model_data$race = as.factor(model_data$race)
model_data$gender = as.factor(model_data$gender)
model_data$time_period = as.factor(model_data$time_period)
model_data$full_name = as.factor(model_data$full_name)
model_data$age = (model_data$age - mean(model_data$age)) / sd(model_data$age)
model_data$experience = (model_data$experience - mean(model_data$experience)) / sd(model_data$experience)
model_data$allegations.nonzero = model_data$allegations + 1
model_data = model_data[!(model_data$time_period == 19),] # Almost no data in this time period. Possible error in preprocessing.

# Check Data is loaded correctly.
head(model_data)

# Determine Distribution of Data.
fitn <- fitdist(model_data$allegations, "norm")
fitp <- fitdist(model_data$allegations, "pois")
fitnb <- fitdist(model_data$allegations, "nbinom")
denscomp(list(fitn, fitp, fitnb), main="Comparison of Distribution Fit for Allegations Data")
chosen_distribution = "negative_binomial"
  
# Visualize Data Histogram
ggplot(model_data) + geom_histogram(aes(allegations, y = ..density..), binwidth = 1)

# Compare Models by Distribution, No Controls
output_model_normal <- glmmTMB(allegations ~ (1 | police_id), data = model_data, family=gaussian(), verbose=2)
output_model_p <- glmmTMB(allegations ~ (1 | police_id), data = model_data, family=poisson(), ziformula=~1, verbose=2)
output_model_nb <- glmmTMB(allegations ~ (1 | police_id), data = model_data, family=nbinom1(), verbose=2)

# Compare Models by Controls
# We take the best model from the previous section. We don't exhaustively check every combination, but use some intuition to add them sequentially.
output_model <- glmmTMB(allegations ~ (1 | police_id), data = model_data, family=nbinom1(), verbose=2)
output_model_division <- glmmTMB(allegations ~ division + (1 | police_id), data = model_data, family=nbinom1(), verbose=2)
output_model_time_period <- glmmTMB(allegations ~ time_period + division + (1 | police_id), data = model_data, family=nbinom1(), verbose=2)
output_model_experience <- glmmTMB(allegations ~ experience + time_period + division + (1 | police_id), data = model_data, family=nbinom1(), verbose=2)
output_model_race <- glmmTMB(allegations ~ race + experience + time_period + division + (1 | police_id), data = model_data, family=nbinom1(), verbose=2)
output_model_gender <- glmmTMB(allegations ~ gender + race + experience + time_period + division + (1 | police_id), data = model_data, family=nbinom1(), verbose=2)
output_model_age <- glmmTMB(allegations ~ age + gender + race + experience + time_period + division + (1 | police_id), data = model_data, family=nbinom1(), verbose=2)

# My initial attempts at model comparisons via AIC indicate that division + time_period + experience is where model performance maxes out. The time_period parameter looks supicious though, because none of the fixed effects coefficients are significant, and the coefficients are very large. So, TBD! Someone with more stats experience can look into it.
  output_model_p <- glmmTMB(allegations ~ time_period + division + (1 | police_id), data = model_data, family=poisson(), ziformula=~1, verbose=2)
  output_model <- glmmTMB(allegations ~ division + experience + (1 | police_id), data = model_data, family=nbinom1(), verbose=2)
  output_model_normal <- glmmTMB(allegations ~ time_period + division + experience + race +(1 | police_id), data = model_data, family=gaussian(), verbose=2)

# View Model Summary
summary(output_model)

# P-Values if calculable.
Anova(output_model)

# Model Diagnostics. For more information, see https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html.
res <- simulateResiduals(output_model)
plotResiduals(model_data$allegations, res$scaledResiduals)
plot(res)
testResiduals(res)
testZeroInflation(res)

# Create results data frames.
# This is a little messy.
random_effects = data.frame(ranef(output_model))
random_effects$n = count(model_data, police_id)$n
time_period_level_data = data.frame("police_id" = model_data$police_id, "residual" = resid(output_model), "predictions"=predict(output_model), "allegations"=model_data$allegations)
time_period_level_data = merge(random_effects, time_period_level_data, by.x="grp", by.y="police_id")
time_period_level_data$residual_randomeffect = time_period_level_data$residual + exp(time_period_level_data$condval)
time_period_level_data$condsd_transformed = exp(time_period_level_data$condsd)
time_period_level_data$predictions_transformed = exp(time_period_level_data$predictions)
time_period_level_data$full_name = model_data$full_name
time_period_level_data$force_count = model_data$force_count
colnames(time_period_level_data)[1] = "police_id"

cop_level_data = aggregate(time_period_level_data[, 2:15], list(time_period_level_data$police_id), mean)
names = aggregate(full_name~police_id, data=time_period_level_data, paste, collapse = ",") # I don't know how to do this better. Something with dplyr would work.
residual_sd = aggregate(time_period_level_data$residual, list(time_period_level_data$police_id), sd)
cop_level_data = merge(cop_level_data, residual_sd, by.y="Group.1", by.x="Group.1")
cop_level_data = merge(cop_level_data, names, by.x="Group.1", by.y="police_id")
colnames(cop_level_data)[1] = "police_id"

# Empirical Bayes Estimation / "Shrinkage" is a work in progress. This is incorrect, but captures the spirit of the operation, which is to multiply random effect estimates by a value which represents their reliability, as captured by the number of instances in the dataset and the variance of the random effect estimate. 
# shrinkage_numerator = sqrt(exp(cop_level_data$condsd))
# shrinkage_denominator =  sqrt(cop_level_data$x)
# cop_level_data$reliability = shrinkage_numerator / (shrinkage_numerator + shrinkage_denominator)
# cop_level_data$reliability = sqrt(cop_level_data$condsd) / .662
cop_level_data$reliability = 1 / sqrt(cop_level_data$condsd)
cop_level_data$shrunken_metric = cop_level_data$condval * cop_level_data$reliability
cop_level_data$shrunken_metric2 = cop_level_data$residual_randomeffect * cop_level_data$reliability

# Scratch for Force Prediction, not finished.
cop_level_data$s1_time_adjusted = cop_level_data$shrunken_metric * cop_level_data$n
cop_level_data$s2_time_adjusted = cop_level_data$shrunken_metric2 * cop_level_data$n
cop_level_data$force_time_adjusted = cop_level_data$force_count / cop_level_data$n
cops_with_force = cop_level_data[cop_level_data$force_count > 0,]
force_model1 <- glmmTMB(force_count ~ s1_time_adjusted, data = cops_with_force, family=nbinom1(), verbose=2)
force_model2 <- glmmTMB(force_count ~ s2_time_adjusted, data = cops_with_force, family=nbinom1(), verbose=2)
cops_with_force$force_predictions = exp(predict(force_model1))
cops_with_force$force_predictions2 = exp(predict(force_model2))
ggplot(cops_with_force, aes(x=force_predictions, y=force_count)) + geom_point() + geom_abline()

# Save out results..
write.csv(time_period_level_data, file = "C:/Users/abeers/Documents/Projects/CampaignZero/CPZ/data/processed/nashville/nashville_time_period_results.csv")
write.csv(cop_level_data, file = "C:/Users/abeers/Documents/Projects/CampaignZero/CPZ/data/processed/nashville/nashville_police_id_results.csv")

# Scratch Code
ggplot(cop_level_data) + geom_histogram(aes(s2_time_adjusted))
ggplot(cop_level_data) + geom_histogram(aes(shrunken_metric))
ggplot(cop_level_data, aes(x=condval, y=shrunken_metric)) + geom_point() + geom_abline()
ggplot(time_period_level_data) + geom_histogram(aes(residual_randomeffect), binwidth=.1)
ggplot(random_effects) + geom_histogram(aes(condval))
qqp(police_residuals, "norm")
gamma <- fitdistr(police_residuals, "gamma")
qqp(police_residuals, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]])
# output_model <- glmer(allegations ~ time_period + division + age + experience + race + gender + (1 | police_id), data = model_data, family = poisson(link = "log"), nAGQ = 1, control=glmerControl(optimizer = "nloptwrap"), verbose=2)  # Set nAGQ to # of desired iterations
# output_model = MCMCglmm(allegations ~ time_period + division + age + experience + race + gender, random = ~police_id, data = model_data)