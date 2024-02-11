library(dplyr)
library(expss)
immigration_data <- read.csv(file="immigration.csv")
head(immigration_data)

## Question 1

# making 'econact' a factor
class(immigration_data$econact)
immigration_data <- apply_labels(immigration_data, econact=c("Employed"= 1, "Education"= 2, "Unemployed"= 3, "OutLabourMarket"= 4))
immigration_data$econact <- as.factor(immigration_data$econact)
class(immigration_data$econact)

# making 'female' a factor
class(immigration_data$female)
immigration_data$female <- as.factor(immigration_data$female)
class(immigration_data$female)

# making 'partner' a factor
class(immigration_data$partner)
immigration_data$partner <- as.factor(immigration_data$partner)
class(immigration_data$partner)

nrow(immigration_data) # there are 33706 observations
length(unique(immigration_data$country)) # there are 27 countries

# new dataset for all countries
immigration_data.c <- immigration_data %>%
  group_by(country) %>%
  summarise(country = first(country), nind = n(), mean.zimmatt = mean(zimmatt), c_zgni = first(c_zgni), c_ltunemp = first(c_ltunemp), c_znetmig = first(c_znetmig))
immigration_data.c$mean.zimmatt <- round(immigration_data.c$mean.zimmatt, digits = 2)
print(immigration_data.c, n = 27)

# histograms for zimmatt
hist(immigration_data$zimmatt)
hist(immigration_data.c$mean.zimmatt)

# labour market country breakdown ('econact')
countries_labour_market <- immigration_data %>%
  group_by(country) %>%
  summarise(Employed_percent = round(sum(econact == "Employed", na.rm = TRUE) / n() * 100, 1),
    Education_percent = round(sum(econact == "Education", na.rm = TRUE) / n() * 100, 1),
    Unemployed_percent = round(sum(econact == "Unemployed", na.rm = TRUE) / n() * 100, 1),
    OutLabourMarket_percent = round(sum(econact == "OutLabourMarket", na.rm = TRUE) / n() * 100, 1), .groups = 'drop')
countries_labour_market





