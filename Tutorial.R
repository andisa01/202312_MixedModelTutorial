
# The purpose of this script is to demonstrate the use of mixed effect models for partial pooling parameter estimates in linear models
# Author: A. Z. Andis Arietta
# Date: 2023 12 09

# Follow along at: https://github.com/andisa01/202312_MixedModelTutorial
# Read the associated blog post at azandisresearch.com/...

load.image("./MixedModel_Presentation/Slide1.JPG") %>% plot(axes = FALSE)


## Load libraries ====
library(tidyverse) # For data wrangling

####
# The commands in this section are only for live demos. You don't need to execute these to follow along.
library(imager) # I use this to display images during live demo.
source("https://raw.githubusercontent.com/andisa01/andis_utils/main/00_HelperFunctions.R") # Andis's helper functions and display settings
options(ggplot2.discrete.colour = c(RColorBrewer::brewer.pal(8, "Dark2"), RColorBrewer::brewer.pal(8, "Set1"))) # Set the default colors for ggplot
####


# Intro =====
# The focus of this talk will be on solving common problems in using regression for inference by using mixed effect models.

## Regression: fashionable and fabulous ====
# Hopefully everyone remembers the idea behind a basic regression
load.image("./MixedModel_Presentation/Slide3.JPG") %>% plot(axes = FALSE)

load.image("./MixedModel_Presentation/Slide4.JPG") %>% plot(axes = FALSE)
# Think about how your data conforms to this view of the world.


### Simulate dragon data ====
set.seed(8702) 

obs_per_species = 100
num_species = 12

dat_x <-
  # Set the range of the independent variables and the fixed effect parameters
  data.frame(
    peasants = round(rnorm(num_species * obs_per_species, 15, 7), 0),
    species = LETTERS[1:num_species],
    B0 = 20,
    B1 = 2,
    error = rnorm(num_species * obs_per_species, 0, 10)
  ) %>%
  # Constrain number of peasants consumed to positive values
  mutate(
    peasants = ifelse(peasants < 0, 0, peasants)
  )

dat_b <-
  # Set the random effect parameters for species
  data.frame(
    species = LETTERS[1:num_species],
    b0 = rnorm(num_species, 0, 10),
    b1 = rnorm(num_species, 0, 0.5)
  )

dat_sample_het <-
  # Set a random sample size
  data.frame(
    sample_size = round(runif(num_species, 1, 200)),
    species = LETTERS[1:num_species]
  ) %>%
  # Ensure that three species have 1, 2, and 3 observations, respectively for demoing small sample size issues
  mutate(
    sample_size = ifelse(species == LETTERS[num_species], 2, sample_size),
    sample_size = ifelse(species == LETTERS[num_species - 1], 1, sample_size),
    sample_size = ifelse(species == LETTERS[num_species - 2], 3, sample_size)
  )

dat_f <-
  # Combine the datasets
  left_join(
  dat_x,
  dat_b,
  by = "species"
) %>%
  # Calculate the true y values and the observed y values
  mutate(
    y = B0 + b0 + (B1 + b1)*peasants,
    dragon_length = y + error
  ) %>%
  # Constrain dragon length to be greater than zero
  filter(dragon_length > 0) %>%
  # Subsample to established sample size per species
  left_join(
    dat_sample_het,
    by = "species"
  ) %>%
  group_by(species) %>%
  mutate(id = sample(n())) %>%
  filter(id <= sample_size) %>%
  select(-id) %>%
  ungroup() %>%
  # Create a region variable that is correlated with the dependent variable
  mutate(
    region =
      case_when(
        y >= quantile(.$y, 0.8) ~ "Asgard",
        y < quantile(.$y, 0.8) & y >= quantile(.$y, 0.6) ~ "Belgium",
        y < quantile(.$y, 0.6) & y >= quantile(.$y, 0.4) ~ "Mordor",
        y < quantile(.$y, 0.4) & y >= quantile(.$y, 0.2) ~ "Rhode Island",
        y < quantile(.$y, 0.2) ~ "Texas"
      )
  )


## Visualize the simulated dataset ====
load.image("./MixedModel_Presentation/Slide1.JPG") %>% plot(axes = FALSE)


# In this example, we have 12 species of dragons 
# First, lets take at the relationship between dragon length and the number of attributed peasants consumed by each dragon

## No pooling model ====
dat_f %>%
  ggplot(aes(x = peasants, y = dragon_length, col = species)) +
  geom_point(size = 4, pch = 16, alpha = 0.5) +
  geom_smooth(method = 'lm', fullrange = TRUE) +
  coord_cartesian(xlim = c(0, 40), ylim = c(0, 130)) +
  facet_wrap(vars(species)) +
  theme(legend.position = "right")

lm(dragon_length ~ peasants * species, data = dat_f) %>% summary()
# In this case, we are basically fitting independent models to each of the species. Each has its own slope and intercept parameter.
# Almost every species has a positive slope. J's slope is similar to the rest, but we have no confidence in it. We can't even estimate a slope for K because we've a single observation. We can fit a line to L but we use all of our degrees of freedom and the line is severly negative.
# Should we believe the slope for L?
# Should we have more confidence in the slope for J?
# Could we make a reasonable guess of the slop of K?
# Have you run into similar issues of uneven sample sizes in your analyses?

## Complete pooling model ====
# If we are interested in describing the general relationship, a common method would be to fit a linear model through all of the points as a single group. This is a complete pooling of the species information.
dat_f %>%
  ggplot(aes(x = peasants, y = dragon_length)) +
  geom_point(size = 4, pch = 16, alpha = 0.5, col = "grey20") +
  geom_smooth(method = 'lm', col = "firebrick", se = FALSE, size = 2)

lm_dragons <- lm(dragon_length ~ peasants, data = dat_f)

summary(lm_dragons)

# We'll store predictions of the complete pooling model for plotting
dat_predictions <- 
  # Create a dataframe with all combinations of species, regions, and a range of peasant consumption
  expand.grid(
    species = LETTERS[1:num_species],
    region = unique(dat_f$region),
    peasants = seq(min(dat_f$peasants), max(dat_f$peasants), 1)
  ) %>%
  # Estimate the model fit
  mutate(
    lm_y_hat = predict(lm_dragons, newdata = .)
  )

# Here's how the no pooling model compares to the complete pooling model
dat_f %>%
  ggplot(aes(x = peasants, y = dragon_length)) +
  geom_point(aes(col = species), size = 4, pch = 16, alpha = 0.5) +
  geom_smooth(aes(col = species), method = 'lm', se = FALSE, fullrange = TRUE) +
  geom_smooth(data = dat_predictions, aes(y = lm_y_hat), method = 'lm', col = "firebrick", size = 2) +
  coord_cartesian(xlim = c(0, 40), ylim = c(0, 130)) +
  facet_wrap(vars(species)) +
  theme(legend.position = "right")
# The complete pooling line is pretty good. We get a much more reasonable lookign estimate for species L and we can make predictions for species K now. But we are consistently biasing estimates high for species A and E while consistently biasing low for sepecies D.

# Could we do even better by strategically sharing information about all observations together while still using the known species groups to inform the estimates? Yes, the answer is partial pooling.

## Partial pooling model ====

# We'll use the lme4 package to fit our mixed models (nlme is another popular package).
library(lme4)

# The arugments are very similar to the lm function, but we need to specify random effects in addition to our fixed effects.
# We basically have a few ways to think about how the information about species groups should inform our estimates.

## . Random intercept ====

# Do we think that the general relationship is the same across all species, but they simply differ in their initial length? If so, maybe we would want to fit a random intercept model
lmer_dragons_rifs_cond <- lmer(dragon_length ~ peasants + (1|species), data = dat_f)
summary(lmer_dragons_rifs_cond)

dat_predictions <- 
  dat_predictions %>%
  mutate(
    lmer_y_hat_rifs_cond = predict(lmer_dragons_rifs_cond, newdata = ., re.form = NULL)
  )

dat_f %>%
  ggplot(aes(x = peasants, y = dragon_length, col = species)) +
  geom_point(aes(col = species), size = 4, pch = 16, alpha = 0.3) +
  geom_smooth(data = dat_predictions, aes(col = species, y = lmer_y_hat_rifs_cond), se = FALSE) +
  theme(legend.position = "right") +
  labs(title = "Random intercept")
# Random intercept models are common when you are grouping repeated observations over time. For example, if you are recording the effects of a drug on weightloss, you would want to scale the intercept to the weight of each individual and treat the individual's ID as the grouping effect. Or measuring effects of fertilizer across plots over time where some plots might be more shaded and have higher production regardless. This is called a repeated measures model design.

dat_f %>%
  ggplot(aes(x = peasants, y = dragon_length, col = species)) +
  geom_point(aes(col = species), size = 4, pch = 16, alpha = 0.3) +
  geom_smooth(data = dat_predictions, aes(col = species, y = lmer_y_hat_rifs_cond), se = FALSE) +
  theme(legend.position = "right") +
  labs(title = "Random intercept") +
  facet_wrap(vars(species))
# Not bad, but the slope looks off for species C, F, and D.

## . Random slope ====
# Alternatively, we might think that all dragons hatch from eggs of the same size, so their initial length shoudl be the same, but growth rates by peasant intake might vary from that point. We could fit a random slope model. 
lmer_dragons_firs_cond <- lmer(dragon_length ~ peasants + (0 + peasants |species), data = dat_f)
summary(lmer_dragons_rifs_cond)

dat_predictions <- 
  dat_predictions %>%
  mutate(
    lmer_y_hat_firs_cond = predict(lmer_dragons_firs_cond, newdata = ., re.form = NULL)
  )

dat_f %>%
  ggplot(aes(x = peasants, y = dragon_length, col = species)) +
  geom_point(aes(col = species), size = 4, pch = 16, alpha = 0.3) +
  geom_smooth(data = dat_predictions, aes(col = species, y = lmer_y_hat_firs_cond), se = FALSE) +
  theme(legend.position = "right") +
  labs(title = "Random intercept")
# This form of model is commonly used when you are tracking things with a common starting point, like bank accounts that all start at zero, or size-constrained community assembly, or distance from a common point. Often an alternative specification for repeated measures.

dat_f %>%
  ggplot(aes(x = peasants, y = dragon_length, col = species)) +
  geom_point(aes(col = species), size = 4, pch = 16, alpha = 0.3) +
  geom_smooth(data = dat_predictions, aes(col = species, y = lmer_y_hat_firs_cond), se = FALSE) +
  theme(legend.position = "right") +
  labs(title = "Random intercept") +
  facet_wrap(vars(species))
# The slopes for C, F, and G look better, but the intercepts look totally wrong for A, C, and E.

## . Random slope and intercept ====

# If we think that every dragon species has its own starting point and intrinsic slope process, we could fit the slope and intercept as random effect parameters.
lmer_dragons_rirs_cond <- lmer(dragon_length ~ peasants + (peasants |species), data = dat_f)
summary(lmer_dragons_rirs_cond)

dat_predictions <- 
  dat_predictions %>%
  mutate(
    lmer_y_hat_rirs_cond = predict(lmer_dragons_rirs_cond, newdata = ., re.form = NULL)
  )

dat_f %>%
  ggplot(aes(x = peasants, y = dragon_length, col = species)) +
  geom_point(aes(col = species), size = 4, pch = 16, alpha = 0.3) +
  geom_smooth(data = dat_predictions, aes(col = species, y = lmer_y_hat_rirs_cond), se = FALSE) +
  theme(legend.position = "right") +
  labs(title = "Random intercept")
# This form of model is commonly used when you are tracking things with a common starting point, like bank accounts that all start at zero, or size-constrained community assembly, or distance from a common point. Often an alternative specification for repeated measures.

dat_f %>%
  ggplot(aes(x = peasants, y = dragon_length, col = species)) +
  geom_point(aes(col = species), size = 4, pch = 16, alpha = 0.3) +
  geom_smooth(data = dat_predictions, aes(col = species, y = lmer_y_hat_rirs_cond), se = FALSE) +
  theme(legend.position = "right") +
  labs(title = "Random intercept") +
  facet_wrap(vars(species))
# The slopes for C, F, and G look better, but the intercepts look totally wrong for A, C, and E.

## Comparison ====
# These look good! Let's compare the random slope and intercept partial pooling model to the complete and no pooling models
dat_f %>%
  ggplot(aes(x = peasants, y = dragon_length)) +
  geom_point(aes(col = species), size = 4, pch = 16, alpha = 0.5) +
  geom_smooth(aes(col = species), method = 'lm', se = FALSE, fullrange = TRUE, size = 2) +
  geom_smooth(data = dat_predictions, aes(y = lm_y_hat), method = 'lm', col = "firebrick", size = 2) +
  geom_smooth(data = dat_predictions, aes(y = lmer_y_hat_rirs_cond), method = 'lm', col = "green3", size = 2, lty = 2) +
  coord_cartesian(xlim = c(0, 40), ylim = c(0, 130)) +
  facet_wrap(vars(species)) +
  theme(legend.position = "right")

## Simpson's paradox ====
# We've seen how partial pooling can solve issues of uneven sampling and small sample sizes. But there is another phenomenon where these models come in very handy.

dat_shift <- 
  data.frame(
    x_shift = runif(num_species, 0, 100)
  ) %>%
  arrange(x_shift) %>%
  mutate(
    species = LETTERS[1:num_species]
  )

dat_f_simpsons <-
  dat_f %>%
  left_join(
    dat_shift
  ) %>%
  mutate(peasants = peasants + x_shift,
         dragon_length = dragon_length + 100 - x_shift)

dat_f_simpsons %>%  
  ggplot(aes(x = peasants, y = dragon_length)) +
  geom_point(aes(col = species), pch = 16, alpha = 0.7) +
  geom_smooth(aes(col = species), method = 'lm', se = FALSE) +
  theme(legend.position = "right")
# The points within each group (and therefore the slopes) are identical to the last exercise. The overall slopw is 2. We can clearly see that all of the slopes are basically positive. THe only difference is the poisition of the groups themselves. If we fit a complete pooling model here, the slope would be negative!

lm(dragon_length ~ peasants, data = dat_f_simpsons) %>% summary()

dat_f_simpsons %>%  
  ggplot(aes(x = peasants, y = dragon_length)) +
  geom_point(aes(col = species), pch = 16, alpha = 0.7) +
  geom_smooth(aes(col = species), method = 'lm', se = FALSE) +
  geom_smooth(method = 'lm', col = 1)

# This is called Simpson's paradox, or the Yule-Simpson effect.
# However, if we fit a mixed model, our slope will be correct.

lmer(dragon_length ~ peasants + (peasants|species), data = dat_f_simpsons) %>% summary()

## Multiple effects ====
# Finally, mixed models can be really helpful when we have multiple groupings with some known structure.

# In our dataset, we also know which region the observations were from.
dat_f %>%
  ggplot(aes(x = peasants, y = dragon_length, col = region)) +
  geom_point(pch = 16, alpha = 0.7, size = 4) +
  theme(legend.position = "right")

# We can draw some elipses to see that knowing the region tells us quite a bit about the size of dragons and the number of peasants consumed.
dat_f %>%
  ggplot(aes(x = peasants, y = dragon_length, col = region)) +
  geom_point(pch = 16, alpha = 0.7, size = 4) +
  theme(legend.position = "right") +
  ggforce::geom_mark_ellipse(size = 2)

# That relationship holds true for every species.
dat_f %>%
  ggplot(aes(x = peasants, y = dragon_length, col = region)) +
  geom_point(pch = 16, alpha = 0.7, size = 4) +
  theme(legend.position = "right") +
  facet_wrap(vars(species))

# For instance, is K a small dragon because K is a small species in general or because our only record of K is from Texas where dragons are always small?

# We can use heirarchical mixed models to simultaneously partially pool information about the species AND the region. But, to do so, we have to use background knowledge or intuition to determine the structre of our random effects. Or, in other words, we need to determine how region and species interact.

# If we think that the growth of dragons is mostly constrained by their species and environmental effects modulate that intrinsic rate in the same way across all species, we could visualize that effects structure like this:
load.image("./MixedModel_Presentation/Slide5.JPG") %>% plot(axes = FALSE)
# This is called a crossed effect design. We would be allowing both species and region to have it's own intercept and slope parameter.
lmer(dragon_length ~ peasants + (peasants|region) + (peasants|species), data = dat_f) %>% summary()

# If we though that every species has its own intrinsic rate of growth, but each reacts different to the environmental effect of the region, we might want to use a nested random effect structre that looks like this:
load.image("./MixedModel_Presentation/Slide6.JPG") %>% plot(axes = FALSE)
# In this case, we would be allowing each species to have its own intercept and slope and each region within a species to have its own variance from the overall species.
lmer(dragon_length ~ peasants + (peasants|species/region), data = dat_f) %>% summary()

# We may think that every species reacts completely differently in every region with no overall tren between species or regions. In that case, we would give each species/region combination its own intercept and slop varince from the fixed effect.
load.image("./MixedModel_Presentation/Slide7.JPG") %>% plot(axes = FALSE)
lmer(dragon_length ~ peasants + (peasants|species:region), data = dat_f) %>% summary()

# As before, we could also allow different levels of the hierarchy to vary in slope or intercept or both or neither. 
# Maybe we think that species have their instrinsic slope and intercept, but the region simply increases or decreases the hatching size, but doesnt't impact groeth rate. In that case, we'd fit region as a random intercept only crossed with random slope an intercept for species.
lmer(dragon_length ~ peasants + (1|region) + (peasants|species), data = dat_f) %>% summary()

# The more random variables, the more options for structure. You need to use your expert opinion and background knowledge to determine the proper structure.
# Technically, you can also fit a handful of models and then use likelihood ratio tests to compare different effect structures. This is kind of shaky ground philosophically beacuse you are using the same data to estimate the structre that you are using to estimate parameters, which is double-dipping on your inference.

## Estimating confidence and hypothesis testing. ====
# One thing you might have noticed from the output is that lme4 does not give you a p-value. The authors of the package do that intentionally. The reason is that it is really difficult to accurately estimate degress of freedom in partially pooled models because a single data point can actually share information with other points, therefore artificially inflating or diminishing the degress of freedom.
# There are methods to get around this. lmerTest package uses a method to calculate degrees of freedom and estimate confidence intervals parametrically.

# There are also semi-parametric methods that bootstrap the residuals.

# My preference is to use fully nonparametric methods to bootstrap all parameters. If you are insterested in this method, I have a tutorial on my blog: https://www.azandisresearch.com/2022/12/31/visualize-mixed-effect-regressions-in-r-with-ggplot2/
load.image("./MixedModel_Presentation/Slide8.JPG") %>% plot(axes = FALSE)


# Resources: ====
# For those inclined to the Bayesian persuation, TJ Mahr has an excellent section of his partial pooling tutorial showing an implementation in RStanARM. https://www.tjmahr.com/plotting-partial-pooling-in-mixed-effects-models/
# Full disclosure, Mahr's blog was a big inspiration for this blog and presentation.
load.image("./MixedModel_Presentation/Slide9.JPG") %>% plot(axes = FALSE)
