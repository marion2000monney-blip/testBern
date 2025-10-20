# -----------------------------------------------------------------------------------------
# ASSIGNMENT - ENERGY ECONOMICS
# Marion Monney
# ----------------------------------------------------------------------------------------- 

# ------ pre-eliminary commands ------
# set working directory
getwd()
setwd("C:/Users/mario/OneDrive - Universitaet Bern/master/Semester II/Energy Economics/project/data")
# load packages
library(readxl)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(mlogit)
library(xtable)  # for LaTeX tables



# ------ TASK 1 ------

# clear workspace
rm(list = ls())
# import data
data1 <- read.csv("energy_cons.csv", header = TRUE, sep = ",")
# check data
head(data1)   
glimpse(data1)
dim(data1)
names(data1)
summary(data1)


# -- (i) --
# Get the number of unique countries
num_countries <- length(unique(data1$iso3))
num_countries


# -- (ii) --
# Check if the data is balanced = all countries have the same number of years
table_country_years <- table(data1$iso3)
length(unique(table_country_years)) == 1  
# TRUE: meaning that the dataset is balanced
data1 %>% 
  filter(if_all(where(is.numeric), is.na))
# there is no row where all values are missing, so each country has some information for each year
table_country_years
# but there is some NA values for energy consumption
summary(tapply(!is.na(data1$energy_cons), data1$iso3, sum))


# -- (iii) --
# group the relevant variables
variables <- c("gas_price", "energy_cons", "employment_share", "cdd", "hdd")
# summary statistics (rounding the results at 2 digits)
table_summary <- sapply(data1[variables], function(x) c(
  n = sum(!is.na(x)),
  mean = round(mean(x, na.rm = TRUE), 2),
  median = round(median(x, na.rm = TRUE), 2),
  sd = round(sd(x, na.rm = TRUE), 2),
  min = round(min(x, na.rm = TRUE), 2),
  max = round(max(x, na.rm = TRUE), 2)
))
table_summary
# description of the result in the pdf file
# create and save LaTeX table
table <- xtable(t(table_summary), 
                caption = "Summary Statistics",
                label = "tab:summary_stats",
                digits = 2)
print(table, file = "summary_statistics.tex", include.rownames = TRUE, floating = FALSE)


# -- (iv) --
# median value of GDP per capita in 2019
data1$gdp_per_capita <- data1$gdp / data1$pop
gdp2019_med <- median(data1$gdp_per_capita[data1$year == 2019], na.rm = TRUE)
gdp2019_med
# whether or not country's GDP per capita in 2019 is above the median
data1$gdp2019_above_med <- NA
data1$gdp2019_above_med[data1$year == 2019] <- data1$gdp_per_capita[data1$year == 2019] > gdp2019_med
# remove countries with missing data in 2014 and rename data: data2
countries_2014 <- data1$iso3[!is.na(data1$gdp_per_capita) & data1$year == 2014]
data2 <- data1[data1$iso3 %in% countries_2014, ]

# create the 2 groups based on GDP per capita in 2019
data2$group <- NA
data2$group[data2$year == 2019 & data2$gdp_per_capita > gdp2019_med] <- "Above Median"
data2$group[data2$year == 2019 & data2$gdp_per_capita <= gdp2019_med] <- "Below Median"
data2$group # there are NA values for all other years

# we have to make sure that each country stay in the same group for all years
data_2019 <- data2[data2$year == 2019, ]
group_2019 <- setNames(
  ifelse(data_2019$gdp_per_capita > gdp2019_med, "Above Median", "Below Median"),
  data_2019$iso3)
data2$group <- group_2019[data2$iso3] # now, each country has a group for all years
data2$group

# annual average energy consumption per capita in kWh for each group and each year
data2$energy_kWh_capita <- (data2$energy_cons * 1000) / data2$pop
energy_average <- data2 %>%
  group_by(year, group) %>%
  summarise(kWh_capita_average = mean(energy_kWh_capita, na.rm = TRUE)) %>%
  ungroup() # removes the grouping created by group_by
energy_average

# plot the evolution of the average energy consumption per capita over time
plot <- ggplot(energy_average, aes(x = year, y = kWh_capita_average, color = group)) +
  geom_line(size = 1.5) +
  labs(
    x = "Year",
    y = "Average Energy Consumption per Capita in kWh",
    color = "GDP per Capita Group",
    title = "Evolution of Average Energy Consumption"
  ) +
  theme_minimal(base_size = 20) +        # larger base font size
  theme(
    legend.position = "top",             # move legend to top
    plot.title = element_text(face = "bold", hjust = 0.5) # center and bold title
  )
ggsave("energy_consumption.png",
      plot = plot,
      path = "c:/Users/mario/OneDrive - Universitaet Bern/master/Semester II/Energy Economics/project/data/")


# -- (v) --
# countries having both electricity price + natural gas prices available for the entire time of observation
countries_all_years <- names(which(
  tapply(!is.na(data2$gas_price) & !is.na(data2$elec_price), data2$iso3, all)
))
length(countries_all_years)



# ------ TASK 2 ------

# same data as in TASK 1
# I use data1 instead of data2 (that removed missing values in 2014)


# --- (i) ---
# change variables to get kWh
data1$gas_price_kwh <- data1$gas_price / 1000
data1$energy_kWh_capita <- (data1$energy_cons * 1000) / data1$pop

# generate all relevant variables 
variables_log <- c("energy_kWh_capita", "gas_price_kwh", "gdp_per_capita", "hdd", "cdd", "elec_price")
names_log   <- c("energy_kWh_capita_log", "gas_price_log", "gdp_capita_log", "hdd_log", "cdd_log", "elec_price_log")
# check which variables contain values smaller than 0 (to avoid issues)
sapply(data1[variables_log], function(x) any(x <= 0, na.rm = TRUE))
# there is only 0 for hdd and cdd variables (we can also see this with the summary table of Task 1)
# log-transform only if values > 0, otherwise set NA
data1[names_log] <- lapply(data1[variables_log], function(x) {
  ifelse(x > 0, log(x), NA)
})


# --- (ii) ---
# estimate the model with OLS
# using gas price
model1 <- lm(energy_kWh_capita_log ~ gas_price_log + gdp_capita_log + employment_share + manufact, data = data1, na.action = na.exclude)
summary(model1)
# interpretation on the pdf file


# --- (iii) ---
# estimate the model with OLS
# using gas price
model2 <- lm(energy_kWh_capita_log ~ gas_price_log + hdd_log + cdd_log + gdp_capita_log + employment_share + manufact, 
            data = data1, na.action = na.exclude)
summary(model2)
# interpretation on the pdf file


# --- (iv) ---
# create lag variable for energy demand
data1 <- data1 %>%
  arrange(iso3, year) %>% # sort the data: years are ordered within each country
  group_by(iso3) %>% # perform mutate() separately for each country
  mutate(energy_kWh_capita_log_lag = lag(energy_kWh_capita_log)) %>%
  ungroup()

# estimate the model with OLS using natural gas price
model3 <- lm(energy_kWh_capita_log ~ gas_price_log + gdp_capita_log + employment_share + manufact + energy_kWh_capita_log_lag, 
            data = data1, na.action = na.exclude)
summary(model3)
# compare with model 1
summary(model1)
# interpretation on the pdf file


# --- (v) ---
# short term natural gas price elasticity
short_term_gas <- coef(model3)["gas_price_log"]
short_term_gas
# long term natural gas price elasticity
lambda_gas <- coef(model3)["energy_kWh_capita_log_lag"]
long_term_gas <- short_term_gas / (1 - lambda_gas)
long_term_gas
# interpretation on the pdf file


# --- (vi) ---
# estimate the model with OLS using natural electricity price
model4 <- lm(energy_kWh_capita_log ~ elec_price_log + gdp_capita_log + employment_share + manufact + energy_kWh_capita_log_lag
             + factor(iso3) # control for country fixed effects
             ,
             data = data1, na.action = na.exclude)
summary(model4)

# compare with other models
summary(model1)
summary(model3)
# short term natural electricity price elasticity
short_term_elec <- coef(model4)["elec_price_log"]
short_term_elec
# long term natural electricity price elasticity
lambda_elec <- coef(model4)["energy_kWh_capita_log_lag"]
long_term_elec <- short_term_elec / (1 - lambda_elec)
long_term_elec


# --- (vii) ---
# comparison
names <- c("Short-term gas", "Short-term electricity", "Long-term gas", "Long-term electricity")
values <- c(short_term_gas, short_term_elec, long_term_gas, long_term_elec)
names(values) <- names
values

# if we include country fixed effects in the model with gas price
model5 <- lm(energy_kWh_capita_log ~ gas_price_log + gdp_capita_log + employment_share + manufact + energy_kWh_capita_log_lag + 
            factor(iso3) # control for country fixed effects
            ,
            data = data1, na.action = na.exclude)
summary(model5)

# short term natural gas price elasticity WITH country fixed effects
short_term_gas <- coef(model5)["gas_price_log"]
short_term_gas
# long term natural gas price elasticity WITH country fixed effects
lambda_gas <- coef(model5)["energy_kWh_capita_log_lag"]
long_term_gas <- short_term_gas / (1 - lambda_gas)
long_term_gas
# interpretation on the pdf file


# --- (viii) ---
# split the dataset into the two sub-samples + run the models 
# create the 2 groups based on GDP per capita in 2019 like in Task 1
countries_2014 <- data1$iso3[!is.na(data1$gdp_per_capita) & data1$year == 2014]
data1 <- data1[data1$iso3 %in% countries_2014, ]
data1$group <- NA
data1$group[data1$year == 2019 & data1$gdp_per_capita > gdp2019_med] <- "Above Median"
data1$group[data1$year == 2019 & data1$gdp_per_capita <= gdp2019_med] <- "Below Median"
data_2019 <- data1[data1$year == 2019, ]
group_2019 <- setNames(
  ifelse(data_2019$gdp_per_capita > gdp2019_med, "Above Median", "Below Median"),
  data_2019$iso3)
data1$group <- group_2019[data1$iso3]

# group "Above Median"
model_above <- lm(energy_kWh_capita_log ~ elec_price_log + gdp_capita_log + employment_share + manufact + energy_kWh_capita_log_lag
               + factor(iso3),
             data = data1[data1$group == "Above Median", ])
summary(model_above)

# group "Below Median"
model_below <- lm(energy_kWh_capita_log ~ elec_price_log + gdp_capita_log + employment_share + manufact + energy_kWh_capita_log_lag
               + factor(iso3),
               data = data1[data1$group == "Below Median", ])
summary(model_below)

# short term electricity price elasticity of group above median
short_term_above <- coef(model_above)["elec_price_log"]
short_term_above
# long term electricity price elasticity of group above median
lambda_above <- coef(model_above)["energy_kWh_capita_log_lag"]
long_term_above <- short_term_above / (1 - lambda_above)
long_term_above
# short term electricity price elasticity of group below median
short_term_below <- coef(model_below)["elec_price_log"]
short_term_below
# long term electricity price elasticity of group below median
lambda_below <- coef(model_below)["energy_kWh_capita_log_lag"]
long_term_below <- short_term_below / (1 - lambda_below)
long_term_below


# --- (ix) ---
# comparison
names <- c("Short-term below median", "Short-term above median", "Long-term below median", "Long-term above median")
values <- c(short_term_below, short_term_above, long_term_below, long_term_above)
names(values) <- names
values
# interpretation on the pdf file



# ------ TASK 3 ------

# clear workspace
rm(list = ls())
# load data
data3 <- read.csv("choice_data.csv", header = TRUE, sep = ",")
# check data
glimpse(data3)
dim(data3)
summary(data3)


# --- (i) ---
names(data3)
head(data3)
# in the pdf file


# --- (ii) ---
# share of women who participated in the survey
number_case <- length(unique(data3$case))
number_wom <- length(unique(data3$case[data3$male==0]))
share_wom <- number_wom / number_case
share_wom


# --- (iii) ---
# check if the dataset is balanced: all individuals must have the same number of alternatives
number_alt <- table(data3$alt)
number_alt # not all individuals have the alternative "Car"
case_alt_table <- table(data3$case)
length(unique(case_alt_table)) == 1 # FALSE
summary(data3$noalt[data3$choice==1]) # other way to see it

# number of individuals that do not face 3 alternatives
table(case_alt_table)
sum(case_alt_table < 3)


# --- (iv) ---
# keep only a subsample with individuals facing 3 alternatives
subsample <- subset(data3, noalt==3)
dim(subsample) # 2490 + 2*81 = 2652
dim(data3)
head(subsample)
# chosen alternatives: where choice == 1
chosen_modes <- subsample$alt[subsample$choice == 1]

# calculate shares for each mode
mode_shares <- prop.table(table(chosen_modes)) * 100

# plot of the different travel modes chosen + save the chart
png("c:/Users/mario/OneDrive - Universitaet Bern/master/Semester II/Energy Economics/project/data/mode_shares.png", 
    width = 800, height = 600)
pie(mode_shares,
    labels = paste0(names(mode_shares), ": ", round(mode_shares, 1), "%"),
    cex = 2,
    main = "Share of users by travel mode", 
    cex.main = 3,
    col = c("lightblue", "lavender", "mintcream"))
dev.off()
# plot is on the pdf file


# --- (v) ---
# estimation of the choice models using SM as the base alternative
# multinomial logit model = considering both alternative and choice specific variables
# convert the subsample to a mlogit data: such that each row is an alternative 
mlogit_data <- mlogit.data(subsample, choice = "choice", shape = "long", 
                           alt.var = "alt", id.var = "case")
head(mlogit_data)

# -- a) --
# using all available control variables except male 
model8 <- mlogit(choice ~ traveltime + cost + freq | income + cost_coverage, 
                 data = mlogit_data, 
                 reflevel = "SM")
summary(model8)

# -- b) --
# using all available control variables
model9 <- mlogit(choice ~ traveltime + cost + freq | income + cost_coverage + male, 
         data = mlogit_data, 
  reflevel = "SM")
summary(model9)


# --- (vi) ---
# likelihood ratio test to compare the two models
lrtest(model8, model9)
# interpretation is on the pdf file


# --- (vii) ---
summary(model9)
# interpretation is on the pdf file


# --- (viii) ---
# calculate the marginal effect at means of an alternative's travel time
round(effects(model9,covariate="traveltime", type="aa"),digits=4)
# aa stands for absolute-absolute = interpretation in percentage points
# interpretation is on the pdf file



# ------ TASK 4 ------

# clear workspace
rm(list = ls())
i <- 0.03
Q <- 3500
tax <- 0.0406
T <- 25
p <- 0.32
Inv <- 20000
cvar <- 100/3500
PVF <- (1/i-(1/(i*((1+i)^T))))


# --- (i) ---
NPV1 <- -Inv + (p - cvar) * Q * PVF
NPV1


# --- (ii) ---
NPV2 <- -Inv + (p + tax - cvar) * Q * PVF
NPV2


# --- (iii) ---
Q2 <- 0.5 * 0.8 * 7000
cvar2 <- 100 / Q2
# NPV = -Inv + ((p + p_feed) - cvar2)*Q2*PVF = 0
p_feed <- (Inv / PVF - (p - cvar2) * Q2) / Q2
p_feed


# --- (v) ---
set.seed(123)
n <- 1000
p2 <- rnorm(n, 0.32, 0.025)
p2
NPV3 <- -Inv + (p2 + tax - cvar) * Q * PVF

# probability of having a positive or negative NPV
positive_NPV <- mean(NPV3 > 0)
positive_NPV

# plot the results: histogram and cumulative distribution function
png("NPV_distribution.png", width = 1200, height = 600)
par(mfrow=c(1,2))
hist_NPV3 <- hist(NPV3, main="NPV distribution with price uncertainty", col="lavender", 
                 xlab="NPV", ylab="Number occurences", breaks=20, cex.lab = 1.5, cex.main = 2)
abline(v = 0, col="red", lwd="2")
cdf_NPV3 <- plot(ecdf(NPV3), main = "CDF of NPV with price uncertainty", xlab="NPV", ylab="Cumulative probability", 
                 cex.lab = 1.5, cex.main = 2)
abline(v = 0, col="red", lwd="2")
abline(h = 1 - positive_NPV, col="green", lwd="2")
dev.off()

