# Author: Heti Pisarev
# Date: 05.12.2020
# Comment: 
# Read the long-format datasets BPRSL and ratsl
# do some longitudinal data analysis


# Access the packages dplyr and tidyr
library(dplyr)
library(tidyr)
library(ggplot2)

## Analysis Chapter 8 + RATS data

ratsl = read.csv(file = (paste0(getwd(), "/data/ratsl.csv")), row.names=1)
ratsl$Group <- factor(ratsl$Group)
ratsl$ID <- factor(ratsl$ID)
str(ratsl)

head(ratsl, n=3)

# Draw the plot
ggplot(ratsl, aes(x = Time, y = weight, linetype = ID)) +
  geom_line() +
  scale_linetype_manual(values = rep(1:10, times=4)) +
  facet_grid(. ~ Group, labeller = label_both) +
  theme(legend.position = "none") + 
  scale_y_continuous(limits = c(min(ratsl$weight), max(ratsl$weight)))


# Standardise the variable bprs
ratsl <- ratsl %>%
  group_by(Time) %>%
  mutate(stdweight = scale(weight)) %>%
  ungroup()


str(ratsl)

ggplot(ratsl, aes(x = Time, y = stdweight, linetype = ID)) +
  geom_line() +
  scale_linetype_manual(values = rep(1:10, times=4)) +
  facet_grid(. ~ Group, labeller = label_both) +
  theme(legend.position = "none") + 
  scale_y_continuous(name="standardized weight", limits = c(min(ratsl$stdweight), max(ratsl$stdweight)))




n <- ratsl$Time %>% unique() %>% length()

# Summary data with mean and standard error of bprs by treatment and week 
ratss <- ratsl %>%
  group_by(Group, Time) %>%
  summarise( mean = mean(weight), se = sd(weight)/sqrt(n) ) %>%
  ungroup()

# Glimpse the data
glimpse(ratss)

# Plot the mean profiles
ggplot(ratss, aes(x = Time, y = mean, linetype = Group, shape = Group)) +
  geom_line() +
  scale_linetype_manual(values = c(1,2,3)) +
  geom_point(size=1) +
  scale_shape_manual(values = c(1,2,3))+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se, linetype="1"), width=0.3) +
  scale_y_continuous(name = "mean(weight) +/- se(weight)")


# Create a summary data by treatment and subject with mean as the summary variable (ignoring baseline week 0).
ratsl10 <- ratsl %>%
  filter(Time > 1) %>%
  group_by(Group, ID) %>%
  summarise( mean=mean(weight) ) %>%
  ungroup()

head(ratsl10, n=3)

# Glimpse the data
glimpse(ratsl10)

# Draw a boxplot of the mean versus treatment
ggplot(ratsl10, aes(x = Group, y = mean)) +
  geom_boxplot() +
  stat_summary(fun = "mean", geom = "point", shape=23, size=4, fill = "white") +
  scale_y_continuous(name = "mean(weight), Time 8-64")

# Create a new data by filtering the outlier and adjust the ggplot code the draw the plot again with the new data
ratsl10s <- ratsl10 %>%  filter(mean<550)

ggplot(ratsl10s, aes(x = Group, y = mean)) +
  geom_boxplot() +
  stat_summary(fun = "mean", geom = "point", shape=23, size=4, fill = "white") +
  scale_y_continuous(name = "mean(weight), Time 8-64")


# Equal variances or not?
tapply(ratsl10s$mean, ratsl10s$Group, sd)

# Perform a two-sample t-test
t1=t.test(mean[Group!=3] ~ Group[Group!=3], data = ratsl10s, var.equal = TRUE)
t2=t.test(mean[Group!=2] ~ Group[Group!=2], data = ratsl10s, var.equal = FALSE)
t3=t.test(mean[Group!=1] ~ Group[Group!=1], data = ratsl10s, var.equal = FALSE)

# significance level
0.05/3

dim(ratsl10)
dim(ratsl10s)

# Add the baseline from the original data as a new variable to the summary data
ratsl10s <- ratsl10 %>%
  mutate(baseline = rats$WD1)

str(ratsl10s)

# Fit the linear model with the mean as the response 
fit <- lm(mean ~ factor(Group) + baseline, data = ratsl10s)
summary(fit)
# Compute the analysis of variance table for the fitted model with anova()
anova(fit)


### Snslysis Chapter 9

BPRSL = read.csv(file = (paste0(getwd(), "/data/bprsl.csv")), row.names=1)
str(BPRSL)
BPRSL$treatment <- factor(BPRSL$treatment)
BPRSL$subject <- factor(BPRSL$subject)
str(BPRSL)


ggplot(BPRSL, aes(x = week, y = bprs, linetype = subject)) +
  geom_line() +
  scale_linetype_manual(values = rep(1:10, times=4)) +
  facet_grid(. ~ treatment, labeller = label_both) +
  theme(legend.position = "none") + 
  scale_y_continuous(name="standardized weight")


str(BPRSL)
hist(BPRSL$bprs)

# create a regression model RATS_reg
bprsl_reg <- lm(bprs~week + factor(treatment), data=BPRSL)

# print out a summary of the model
summary(bprsl_reg)



# access library lme4
library(lme4)

# Create a random intercept model
bprs_ref <- lmer(bprs~week + factor(treatment) + (1 | subject), data = BPRSL, REML = FALSE)

# Print the summary of the model

summary(bprs_ref)

# create a random intercept and random slope model
bprs_ref1 <- lmer(bprs~week + factor(treatment) + (week | subject), data = BPRSL, REML = FALSE)

# print a summary of the model
summary(bprs_ref1)

# perform an ANOVA test on the two models
anova(bprs_ref1, bprs_ref)

# create a random intercept and random slope model with the interaction
bprs_ref2 <- lmer(bprs ~ week + factor(treatment) + week*factor(treatment) + (week | subject), data = BPRSL, REML = FALSE)

# print a summary of the model
summary(bprs_ref2)

# perform an ANOVA test on the two models
anova(bprs_ref2, bprs_ref1)

# draw the plot of RATSL with the observed Weight values

ggplot(BPRSL, aes(x = week, y = bprs, linetype = subject)) +
  geom_line() +
  scale_linetype_manual(values = rep(1:10, times=4)) +
  facet_grid(. ~ treatment, labeller = label_both) +
  scale_x_continuous(name = "Time (weeks)") +
  scale_y_continuous(name = "bprs") +
  theme(legend.position = "none")


# Create a vector of the fitted values
Fitted <- fitted(bprs_ref2)

# Create a new column fitted to RATSL
BPRSL=BPRSL %>% mutate(Fitted=Fitted)

str(BPRSL)
# draw the plot of BPRSL with the Fitted values of weight

ggplot(BPRSL, aes(x = week, y = Fitted, linetype = subject)) +
  geom_line() +
  scale_linetype_manual(values = rep(1:10, times=4)) +
  facet_grid(. ~ treatment, labeller = label_both) +
  scale_x_continuous(name = "Time (weeks)") +
  scale_y_continuous(name = "Fitted bprs") +
  theme(legend.position = "none")

