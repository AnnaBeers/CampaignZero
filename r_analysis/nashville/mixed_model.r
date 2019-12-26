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
model_data = read.csv("[users]/Documents/Projects/CampaignZero/CPZ/data/processed/nashville/nashville_model_formatted_citizen.csv")

# Change Data Types and Normalize
model_data$police_id = as.factor(model_data$police_id)
model_data$division = as.factor(model_data$division)
model_data$race = as.factor(model_data$race)
model_data$gender = as.factor(model_data$gender)
model_data$time_period = as.factor(model_data$time_period)
model_data$full_name = as.factor(model_data$full_name)
model_data$age = (model_data$age - mean(model_data$age)) / sd(model_data$age)
model_data$experience = (model_data$experience - mean(model_data$experience)) / sd(model_data$experience)
model_data = model_data[!(model_data$time_period == 19),]

# Check Data is loaded correctly.
head(model_data)

# Determine Distribution of Data. Not working right now, work in progress.
  # Make complaint data non-zero if necessary for distribution.
  model_data$complaints.nonzero = model_data$complaints + 1
  # Normal
  qqp(model_data$complaints, "norm")
  # Log Normal
  qqp(model_data$complaints, "lnorm")
  # Negative Binomial
  nbinom <- fitdistr(model_data$complaints, "Negative Binomial")
  qqp(model_data$complaints, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])
  # Poisson
  filtered_complaints = model_data[model_data$complaints <= 5, ]
  filtered_poisson = fitdistr(filtered_complaints$complaints, "Poisson")
  poisson_dist <- fitdistr(model_data$complaints, "Poisson")
  qqp(model_data$complaints, "pois", lambda=poisson_dist$estimate)
  # Gamma
  gamma <- fitdistr(model_data$complaints.nonzero, "gamma")
  qqp(model_data$complaints.nonzero, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]])
chosen_distribution = "negative_binomial"
  
# Visualize Data Histogram
ggplot(model_data) +
  geom_histogram(aes(complaints, y = ..density..), binwidth = 1)

# Run Model
if (chosen_distribution == "poisson"){
  output_model_p <- glmmTMB(complaints ~ time_period + division + (1 | police_id), data = model_data, family=poisson(), ziformula=~1, verbose=2)
  output_model = output_model_p
}
if (chosen_distribution == 'negative_binomial'){
  # My initial attempts at model comparisons via AIC indicate that division + time_period + experience is where model performance maxes out. The time_period parameter looks supicious though, because none of the fixed effects coefficients are significant, and the coefficients are very large. So, TBD! Someone with more stats experience can look into it.
  output_model <- glmmTMB(complaints ~ division + experience + (1 | police_id), data = model_data, family=nbinom1(), verbose=2)
  output_model_alternate <- glmmTMB(complaints ~ division + time_period + experience + (1 | police_id), data = model_data, family=nbinom1(), verbose=2)
}
if (chosen_distribution == "normal"){
  output_model_normal <- glmmTMB(complaints ~ time_period + division + experience + race +(1 | police_id), data = model_data, family=gaussian(), verbose=2)
  }

# View Model Summary
summary(output_model)

# P-Values if calculable.
Anova(output_model)

# Model Diagnostics. For more information, see https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html.
res <- simulateResiduals(output_model)
plotResiduals(model_data$complaints, res$scaledResiduals)
plot(res)
testResiduals(res)
testZeroInflation(res)

# Create results data frames.
# This is a little messy.
random_effects = data.frame(ranef(output_model))
random_effects$n = count(model_data, police_id)$n
time_period_level_data = data.frame("police_id" = model_data$police_id, "residual" = resid(output_model), "predictions"=predict(output_model), "complaints"=model_data$complaints)
time_period_level_data = merge(random_effects, time_period_level_data, by.x="grp", by.y="police_id")
time_period_level_data$residual_randomeffect = time_period_level_data$residual + exp(time_period_level_data$condval)
time_period_level_data$condsd_transformed = exp(time_period_level_data$condsd)
time_period_level_data$predictions_transformed = exp(time_period_level_data$predictions)
time_period_level_data$full_name = model_data$full_name
colnames(time_period_level_data)[1] = "police_id"

cop_level_data = aggregate(time_period_level_data[, 2:14], list(time_period_level_data$police_id), mean)
names = aggregate(full_name~police_id, data=time_period_level_data, paste) # I don't know how to do this better. Something with dplyr would work.
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

# Save out results..
write.csv(time_period_level_data, file = "[users]/Documents/Projects/CampaignZero/CPZ/data/processed/nashville/nashville_time_period_results.csv")
write.csv(cop_level_data, file = "[users]/Documents/Projects/CampaignZero/CPZ/data/processed/nashville/nashville_police_id_results.csv")

# Scratch Code
predictions = predict(output_model, se.fit=TRUE)
ggplot(cop_level_data) + geom_histogram(aes(shrunken_metric))
ggplot(cop_level_data) + geom_histogram(aes(shrunken_metric2))
ggplot(cop_level_data, aes(x=condval, y=shrunken_metric)) + geom_point() + geom_abline()
ggplot(time_period_level_data) + geom_histogram(aes(residual_randomeffect), binwidth=.1)
ggplot(random_effects) + geom_histogram(aes(condval))
qqp(police_residuals, "norm")
gamma <- fitdistr(police_residuals, "gamma")
qqp(police_residuals, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]])
# output_model <- glmer(complaints ~ time_period + division + age + experience + race + gender + (1 | police_id), data = model_data, family = poisson(link = "log"), nAGQ = 1, control=glmerControl(optimizer = "nloptwrap"), verbose=2)  # Set nAGQ to # of desired iterations
# output_model = MCMCglmm(complaints ~ time_period + division + age + experience + race + gender, random = ~police_id, data = model_data)