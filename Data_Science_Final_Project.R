rm(list = ls())

# Install and load necessary packages
if (!require(pacman)) install.packages("pacman")
pacman::p_load(ggplot2, haven)

#setting the working directory
setwd("C:\\OLD Asus\\Cdoc\\Hs-Fresenius\\PDFs\\Third Semester\\Data Science\\Project\\142621-V1")


# first figure (employment rate(CZ Level))
# read the data
cz_adult_emp_pov <- read_dta("Replication_Data\\cz_adult_emp_pov.dta")


head(cz_adult_emp_pov)


model<-lm(emp2000 ~ pov_rate , data = cz_adult_emp_pov)
show(model)

#Normal plot
library("ggplot2")

ggplot(cz_adult_emp_pov, aes(x = pov_rate, y = emp2000)) +
  geom_point() +
  stat_smooth(formula = y ~ x, method = "lm", se = FALSE, colour = "red", linetype = 1)



#binscatterplot
if (!require(pacman)) install.packages("pacman")
pacman::p_load(ggplot2, binsreg, haven, dplyr)

#binscatter(emp2000 ~ pov_rate, data = cz_adult_emp_pov, nbins = 20)

##############################################################################################################

#https://lost-stats.github.io/Presentation/Figures/binscatter.html



library(dplyr)

# this will create 20 quantiles using y and assign the observations in each quantile to a separate bin
cz_adult_emp_pov = cz_adult_emp_pov %>% mutate(bin = ntile(pov_rate, n=20))

new_cz_adult_emp_pov = cz_adult_emp_pov %>% group_by(bin) %>% summarise(emp2000mean = mean(emp2000), pov_ratemean = mean(pov_rate)) #find the x and y mean of each bin

ggplot(new_cz_adult_emp_pov, aes(x = pov_ratemean, y = emp2000mean)) + 
  geom_point()+
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Poverty Rate Mean", y = "Employment 2000 Mean") +
  theme_minimal()




# second figure (life expectancy (CZ Level))
# read the data
cz_life_expect_pov <- read_dta("Replication_Data\\cz_life_expect_pov.dta")


head(cz_life_expect_pov)


model<-lm(le_agg ~ pov_rate , data = cz_life_expect_pov)
show(model)

#normal plot
if (!require(pacman)) install.packages("pacman")
pacman::p_load(ggplot2, binsreg, haven, dplyr)

ggplot(cz_life_expect_pov, aes(x = pov_rate, y = le_agg)) +
  geom_point() +
  stat_smooth(formula = y ~ x, method = "lm", se = FALSE, colour = "red", linetype = 1)


#binscatter plot
# 20 bin devision
# this will create 20 quantiles using y and assign the observations in each quantile to a separate bin
cz_life_expect_pov = cz_life_expect_pov %>% mutate(bin = ntile(pov_rate, n=20))

new_cz_life_expect_pov = cz_life_expect_pov %>% group_by(bin) %>% summarise(le_aggmean = mean(le_agg), pov_ratemean = mean(pov_rate)) #find the x and y mean of each bin

ggplot(new_cz_life_expect_pov, aes(x = pov_ratemean, y = le_aggmean)) + 
  geom_point()+
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Poverty Rate Mean", y = "Life Expectancy Mean") +
  theme_minimal()



# third figure (Upward mobility(CZ Level))
# read the data
cz_upward_mobility_pov <- read_dta("Replication_Data\\cz_upward_mobility_pov.dta")


head(cz_upward_mobility_pov)


model<-lm(kfr_pooled_p25_cz ~ pov_rate , data = cz_upward_mobility_pov)
show(model)

#normal plot
if (!require(pacman)) install.packages("pacman")
pacman::p_load(ggplot2, binsreg, haven, dplyr)

ggplot(cz_upward_mobility_pov, aes(x = pov_rate, y = kfr_pooled_p25_cz)) +
  geom_point() +
  stat_smooth(formula = y ~ x, method = "lm", se = FALSE, colour = "red", linetype = 1)


# 20 bin devision
# this will create 20 quantiles using y and assign the observations in each quantile to a separate bin
cz_upward_mobility_pov = cz_upward_mobility_pov %>% mutate(bin = ntile(pov_rate, n=20))

new_cz_upward_mobility_pov = cz_upward_mobility_pov %>% group_by(bin) %>% summarise(kfr_pooled_p25_czmean = mean(kfr_pooled_p25_cz), pov_ratemean = mean(pov_rate)) #find the x and y mean of each bin

ggplot(new_cz_upward_mobility_pov, aes(x = pov_ratemean, y = kfr_pooled_p25_czmean)) + 
  geom_point()+
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Poverty Rate Mean", y = "Life Expectancy Mean") +
  theme_minimal()


# fourth figure (mean test score(CZ Level))
# read the data
district_test_scores_pov <- read_dta("Replication_Data\\district_test_scores_pov.dta")


head(district_test_scores_pov)


model<-lm(mn_avg_ol ~ pov_rate , data = district_test_scores_pov)
show(model)

#normal plot
if (!require(pacman)) install.packages("pacman")
pacman::p_load(ggplot2, binsreg, haven, dplyr)

ggplot(district_test_scores_pov, aes(x = pov_rate, y = mn_avg_ol)) +
  geom_point() +
  stat_smooth(formula = y ~ x, method = "lm", se = FALSE, colour = "red", linetype = 1)


# 20 bin devision
# this will create 20 quantiles using y and assign the observations in each quantile to a separate bin
district_test_scores_pov = district_test_scores_pov %>% mutate(bin = ntile(pov_rate, n=20))

new_district_test_scores_pov = district_test_scores_pov %>% group_by(bin) %>% summarise(mn_avg_olmean = mean(mn_avg_ol), pov_ratemean = mean(pov_rate)) #find the x and y mean of each bin

ggplot(new_district_test_scores_pov, aes(x = pov_ratemean, y = mn_avg_olmean)) + 
  geom_point()+
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Poverty Rate Mean", y = "Mean Test Score Mean") +
  theme_minimal()



#Table 1 replication

# read the data
# fifth dataset(tract_adult_emp)
tract_adult_emp_pov <- read_dta("Replication_Data\\tract_adult_emp_pov.dta")


head(tract_adult_emp_pov)


model<-lm(emp2000 ~ pov_rate , data = tract_adult_emp_pov)
show(model)

#normal plot
if (!require(pacman)) install.packages("pacman")
pacman::p_load(ggplot2, binsreg, haven, dplyr)

ggplot(tract_adult_emp_pov, aes(x = pov_rate, y = emp2000)) +
  geom_point() +
  stat_smooth(formula = y ~ x, method = "lm", se = FALSE, colour = "red", linetype = 1)


# 20 bin devision
# this will create 20 quantiles using y and assign the observations in each quantile to a separate bin
tract_adult_emp_pov = tract_adult_emp_pov %>% mutate(bin = ntile(pov_rate, n=20))

new_tract_adult_emp_pov = tract_adult_emp_pov %>% group_by(bin) %>% summarise(emp2000mean = mean(emp2000), pov_ratemean = mean(pov_rate)) #find the x and y mean of each bin

ggplot(new_tract_adult_emp_pov, aes(x = pov_ratemean, y = emp2000mean)) + 
  geom_point()+
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Poverty Rate Mean", y = "Life Expectancy Mean") +
  theme_minimal()



#sixth dataset(tract_upward_mobility)
# read the data
tract_upward_mobility_pov <- read_dta("Replication_Data\\tract_upward_mobility_pov.dta")


head(tract_upward_mobility_pov)


model<-lm(kfr_pooled_p25 ~ pov_rate , data = tract_upward_mobility_pov)
show(model)

#normal plot
if (!require(pacman)) install.packages("pacman")
pacman::p_load(ggplot2, binsreg, haven, dplyr)

ggplot(tract_adult_emp_pov, aes(x = pov_rate, y = kfr_pooled_p25)) +
  geom_point() +
  stat_smooth(formula = y ~ x, method = "lm", se = FALSE, colour = "red", linetype = 1)


# 20 bin devision
# this will create 20 quantiles using y and assign the observations in each quantile to a separate bin
tract_upward_mobility_pov = tract_upward_mobility_pov %>% mutate(bin = ntile(pov_rate, n=20))

new_tract_upward_mobility_pov = tract_upward_mobility_pov %>% group_by(bin) %>% summarise(kfr_pooled_p25mean = mean(kfr_pooled_p25), pov_ratemean = mean(pov_rate)) #find the x and y mean of each bin

ggplot(new_tract_upward_mobility_pov, aes(x = pov_ratemean, y = kfr_pooled_p25mean)) + 
  geom_point()+
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Poverty Rate Mean", y = "Life Expectancy Mean") +
  theme_minimal()


#seventh datasets(district_test_scores)
# read the data
district_test_scores_pov <- read_dta("Replication_Data\\district_test_scores_pov.dta")


head(district_test_scores_pov)


model<-lm(mn_avg_ol ~ pov_rate , data = district_test_scores_pov)
show(model)

#normal plot
if (!require(pacman)) install.packages("pacman")
pacman::p_load(ggplot2, binsreg, haven, dplyr)

ggplot(district_test_scores_pov, aes(x = pov_rate, y = mn_avg_ol)) +
  geom_point() +
  stat_smooth(formula = y ~ x, method = "lm", se = FALSE, colour = "red", linetype = 1)


# 20 bin devision
# this will create 20 quantiles using y and assign the observations in each quantile to a separate bin
district_test_scores_pov = district_test_scores_pov %>% mutate(bin = ntile(pov_rate, n=20))

new_district_test_scores_pov = district_test_scores_pov %>% group_by(bin) %>% summarise(mn_avg_olmean = mean(mn_avg_ol), pov_ratemean = mean(pov_rate)) #find the x and y mean of each bin

ggplot(new_district_test_scores_pov, aes(x = pov_ratemean, y = mn_avg_olmean)) + 
  geom_point()+
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Poverty Rate Mean", y = "Mean Test Score Mean") +
  theme_minimal()



# Table 1, Column 1: Employment Rate (CZ)
model1 <- lm(emp2000 ~ pov_rate, data = cz_adult_emp_pov)
summary(model1)
mean_emp2000 <- mean(cz_adult_emp_pov$emp2000, na.rm = TRUE)

# Table 1, Column 2: Life Expectancy (CZ)
model2 <- lm(le_agg ~ pov_rate, data = cz_life_expect_pov)
summary(model2)
mean_le_agg <- mean(cz_life_expect_pov$le_agg, na.rm = TRUE)

# Table 1, Column 3: Upward Mobility (CZ)
model3 <- lm(kfr_pooled_p25_cz ~ pov_rate, data = cz_upward_mobility_pov)
summary(model3)
mean_kfr_pooled_p25_cz <- mean(cz_upward_mobility_pov$kfr_pooled_p25_cz, na.rm = TRUE)

# Table 1, Column 4: Test Scores (School Districts)
model4 <- lm(mn_avg_ol ~ pov_rate, data = district_test_scores_pov)
summary(model4)
mean_mn_avg_ol <- mean(district_test_scores_pov$mn_avg_ol, na.rm = TRUE)

# Table 1, Column 5: Employment Rate (Tract)
model5 <- lm(emp2000 ~ pov_rate, data = tract_adult_emp_pov)
summary(model5)
mean_tract_emp2000 <- mean(tract_adult_emp_pov$emp2000, na.rm = TRUE)

# Table 1, Column 6: Upward Mobility (Tract)
model6 <- lm(kfr_pooled_p25 ~ pov_rate, data = tract_upward_mobility_pov)
summary(model6)
mean_tract_kfr_pooled_p25 <- mean(tract_upward_mobility_pov$kfr_pooled_p25, na.rm = TRUE)







if (!require(stargazer)) {
  install.packages("stargazer")
}
library(stargazer)




# Create Table 1
stargazer(
  model1, model2, model3, model4, model5, model6,
  type = "text",
  title = "Table 1: Regression Results",
  dep.var.labels = c("Employment Rate", "Life Expectancy", "Upward Mobility", "Test Score", "Employment Rate", "Upward Mobility"),
  column.labels = c("CZ", "CZ", "CZ", "School District", "Tract", "Tract"),
  covariate.labels = c("Poverty Rate"),
  add.lines = list(
    c("Mean", round(mean_emp2000, 3), round(mean_le_agg, 3), round(mean_kfr_pooled_p25_cz, 3), round(mean_mn_avg_ol, 3), round(mean_tract_emp2000, 3), round(mean_tract_kfr_pooled_p25, 3))
  ),
  omit.stat = c("f", "ser", "adj.rsq"),
  no.space = TRUE
)
