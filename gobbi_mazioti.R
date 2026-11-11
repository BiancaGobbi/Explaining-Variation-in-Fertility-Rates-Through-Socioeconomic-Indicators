# Fertility Rates Statistics Project 2026| Gobbi and Mazioti 
# ----------------------------
# Initialization 
# ----------------------------

#import libraries 
library(tidyverse)
library(plotly)
library(car)
library(lmtest)
library(sandwich)

# ============================================================
# Read raw datasets 
fertility     <- read.csv("fertility.csv", stringsAsFactors = FALSE)
edu           <- read.csv("education.csv", stringsAsFactors = FALSE)
gdp           <- read.csv("gdp.csv", stringsAsFactors = FALSE)
femlabor      <- read.csv("femlabor.csv", stringsAsFactors = FALSE)
femlifeexp    <- read.csv("femlifeexp.csv", stringsAsFactors = FALSE)
healthcare    <- read.csv("healthcare.csv", stringsAsFactors = FALSE)
urbanization  <- read.csv("urbanization.csv", stringsAsFactors = FALSE)
fempolpart    <- read.csv("fempolpart.csv", stringsAsFactors = FALSE)
crime         <- read.csv("crime.csv", stringsAsFactors = FALSE)

# ============================================================
#Filter each dataset to the year we use in the analysis
fertility_2023 <- subset(fertility, Year == 2023)
edu_2023 <- subset(edu, Year == 2023)
gdp_2023 <- subset(gdp, Year == 2023)
femlabor_2023 <- subset(femlabor, Year == 2023)
femlifeexp_2023 <- subset(femlifeexp, Year == 2023)
healthcare_2023 <- subset(healthcare, Year == 2023)
urbanization_2023 <- subset(urbanization, Year == 2023)
fempolpart_2023 <- subset(fempolpart, Year == 2023)

# ============================================================
# Rename columns 
colnames(fertility_2023)[colnames(fertility_2023) == "Entity"] <- "Country"
colnames(fertility_2023)[colnames(fertility_2023) == "Fertility.rate...Sex..all...Age..all...Variant..estimates"] <- "Fertility_rate"
colnames(edu_2023)[colnames(edu_2023) == "Entity"] <- "Country"
colnames(edu_2023)[colnames(edu_2023) == "Average.years.of.schooling.among.women"] <- "Education"   
colnames(gdp_2023)[colnames(gdp_2023) == "Entity"] <- "Country"
colnames(gdp_2023)[colnames(gdp_2023) == "GDP..output..multiple.price.benchmarks."] <- "GDP"   
colnames(femlabor_2023)[colnames(femlabor_2023) == "Entity"] <- "Country"
colnames(femlabor_2023)[colnames(femlabor_2023) == "Labor.force.participation.rate..female....of.female.population.ages.15....modeled.ILO.estimate."] <- "Female_labor_participation"
colnames(femlifeexp_2023)[colnames(femlifeexp_2023) == "Entity"] <- "Country"
colnames(femlifeexp_2023)[colnames(femlifeexp_2023) == "Life.expectancy...Sex..female...Age..0...Variant..estimates"] <- "Female_life_expectancy"
colnames(healthcare_2023)[colnames(healthcare_2023) == "Entity"] <- "Country"
colnames(healthcare_2023)[colnames(healthcare_2023) == "Current.health.expenditure..CHE..as.percentage.of.gross.domestic.product..GDP....."] <- "Healthcare"
colnames(urbanization_2023)[colnames(urbanization_2023) == "Entity"] <- "Country"
colnames(urbanization_2023)[colnames(urbanization_2023) == "Urban.population"] <- "Urbanization"
colnames(fempolpart_2023)[colnames(fempolpart_2023) == "Entity"] <- "Country"
colnames(fempolpart_2023)[colnames(fempolpart_2023) == "Women.s.political.empowerment.index..central.estimate."] <- "Female_political_participation"
colnames(crime)[colnames(crime) == "country"] <- "Country"
colnames(crime)[colnames(crime) == "crimeIndex"] <- "Crime_Rate"
# ============================================================
# Drop columns we do not need 
fertility_2023$Code <- NULL
fertility_2023$Year <- NULL
edu_2023$Code <- NULL
edu_2023$Year <- NULL
gdp_2023$Code <- NULL
gdp_2023$Year <- NULL
femlabor_2023$Code <- NULL
femlabor_2023$Year <- NULL
femlifeexp_2023$Code <- NULL
femlifeexp_2023$Year <- NULL
healthcare_2023$Code <- NULL
healthcare_2023$Year <- NULL
urbanization_2023$Code <- NULL
urbanization_2023$Year <- NULL
urbanization_2023$Rural.population <- NULL
fempolpart_2023$Code <- NULL
fempolpart_2023$Year <- NULL
fempolpart_2023$World.regions.according.to.OWID <- NULL
crime$rank <- NULL
crime$pop2023 <- NULL

# ============================================================
# Merge datasets
merged <- merge(fertility_2023, edu_2023, by = "Country", all = FALSE)
merged <- merge(merged, gdp_2023, by = "Country", all = FALSE)
merged <- merge(merged, femlabor_2023, by = "Country", all = FALSE)
merged <- merge(merged, femlifeexp_2023, by = "Country", all = FALSE)
merged <- merge(merged, healthcare_2023, by = "Country", all = FALSE)
merged <- merge(merged, urbanization_2023, by = "Country", all = FALSE)
merged <- merge(merged, fempolpart_2023, by = "Country", all = FALSE)
merged <- merge(merged, crime, by = "Country", all = FALSE)
merged$Country <- trimws(merged$Country)
#write.csv(merged, "merged_fertility_project_2023.csv", row.names = FALSE)
# ============================================================
#Initial plots
plot(merged$GDP/ 1e12, merged$Fertility_rate,
     xlab = "GDP (trillions USD)",
     ylab = "Fertility (number of births)",
     main = "Fertility vs GDP")
plot(merged$Education, merged$Fertility_rate,
     xlab = "Education (years)",
     ylab = "Fertility (number of births)",
     main = "Fertility vs Education")

plot(merged$Female_labor_participation, merged$Fertility_rate,
     xlab = "Female labor participation (%)",
     ylab = "Fertility (number of births)",
     main = "Fertility vs Female Labor Force Participation")

plot(merged$Female_life_expectancy, merged$Fertility_rate,
     xlab = "Female life expectancy (years)",
     ylab = "Fertility (number of births)",
     main = "Fertility vs Female Life Expectancy")

plot(merged$Healthcare, merged$Fertility_rate,
     xlab = "Healthcare expenditure (%)",
     ylab = "Fertility (number of births)",
     main = "Fertility vs Healthcare Expenditure")

plot(merged$Urbanization/ 1e6, merged$Fertility_rate,
     xlab = "Urban population (millions)",
     ylab = "Fertility (number of births)",
     main = "Fertility vs Urbanization")

plot(merged$Female_political_participation*100, merged$Fertility_rate,
     xlab = "Female political participation (%)",
     ylab = "Fertility (number of births)",
     main = "Fertility vs Female Political Participation ")

plot(merged$Crime_Rate, merged$Fertility_rate,
     xlab = "Crime Rate (%)",
     ylab = "Fertility(number of births)",
     main = "Fertility vs Crime Rate")

# ============================================================
# Interactive plots 
#GDP: 
plot_ly(
  data = merged,
  x = ~GDP,
  y = ~Fertility_rate,
  type = "scatter",
  mode = "markers",
  text = ~paste0(
    "Country: ", Country,
    "<br>GDP: ", round(GDP / 1e12, 2), " trillion USD",
    "<br>Fertility: ", round(Fertility_rate, 2)
  ),
  hoverinfo = "text"
) %>%
  layout(
    title = "Fertility vs GDP",
    xaxis = list(title = "GDP"),
    yaxis = list(title = "Fertility rate (births per woman)")
  )

#Education: 
plot_ly(
  data = merged,
  x = ~Education,
  y = ~Fertility_rate,
  type = "scatter",
  mode = "markers",
  text = ~paste0(
    "Country: ", Country,
    "<br>Education: ", round(Education, 2),
    "<br>Fertility: ", round(Fertility_rate, 2)
  ),
  hoverinfo = "text"
) %>%
  layout(
    title = "Fertility vs Education",
    xaxis = list(title = "Education (years)"),
    yaxis = list(title = "Fertility rate (births per woman)")
  )

#Female Labor Participation: 
plot_ly(
  data = merged,
  x = ~Female_labor_participation,
  y = ~Fertility_rate,
  type = "scatter",
  mode = "markers",
  text = ~paste0(
    "Country: ", Country,
    "<br>Female labor participation: ", round(Female_labor_participation, 1), "%",
    "<br>Fertility: ", round(Fertility_rate, 2)
  ),
  hoverinfo = "text"
)
#fem pol part: 
plot_ly(
  data = merged,
  x = ~(Female_political_participation * 100),
  y = ~Fertility_rate,
  type = "scatter",
  mode = "markers",
  text = ~paste0(
    "Country: ", Country,
    "<br>Female political participation: ", round(Female_political_participation * 100, 2), "%",
    "<br>Fertility rate: ", round(Fertility_rate, 2)
  ),
  hoverinfo = "text"
) %>%
  layout(
    title = "Fertility vs Female Political Participation",
    xaxis = list(title = "Female political participation (%)"),
    yaxis = list(title = "Fertility rate (births per woman)")
  )

#fem life exp: 
plot_ly(
  data = merged,
  x = ~Female_life_expectancy,
  y = ~Fertility_rate,
  type = "scatter",
  mode = "markers",
  text = ~paste0(
    "Country: ", Country,
    "<br>Female life expectancy: ", round(Female_life_expectancy, 2),
    "<br>Fertility rate: ", round(Fertility_rate, 2)
  ),
  hoverinfo = "text"
) %>%
  layout(
    title = "Fertility vs Female Life Expectancy",
    xaxis = list(title = "Female life expectancy (years)"),
    yaxis = list(title = "Fertility rate (births per woman)")
  )
#healthcare
plot_ly(
  data = merged,
  x = ~Healthcare,
  y = ~Fertility_rate,
  type = "scatter",
  mode = "markers",
  text = ~paste0(
    "Country: ", Country,
    "<br>Healthcare expenditure: ", round(Healthcare, 2), "% of GDP",
    "<br>Fertility rate: ", round(Fertility_rate, 2)
  ),
  hoverinfo = "text"
) %>%
  layout(
    title = "Fertility vs Healthcare Expenditure",
    xaxis = list(title = "Healthcare expenditure (% of GDP)"),
    yaxis = list(title = "Fertility rate (births per woman)")
  )
#urbanization
plot_ly(
  data = merged,
  x = ~(Urbanization / 1e6),
  y = ~Fertility_rate,
  type = "scatter",
  mode = "markers",
  text = ~paste0(
    "Country: ", Country,
    "<br>Urban population: ", round(Urbanization / 1e6, 2), " million",
    "<br>Fertility rate: ", round(Fertility_rate, 2)
  ),
  hoverinfo = "text"
) %>%
  layout(
    title = "Fertility vs Urbanization",
    xaxis = list(title = "Urban population (millions)"),
    yaxis = list(title = "Fertility rate (births per woman)")
  )

#Crime rate: 
plot_ly(
  data = merged,
  x = ~Crime_Rate,
  y = ~Fertility_rate,
  type = "scatter",
  mode = "markers",
  text = ~paste0(
    "Country: ", Country,
    "<br>Crime index: ", round(Crime_Rate, 1),
    "<br>Fertility: ", round(Fertility_rate, 2)
  ),
  hoverinfo = "text"
)

plot_ly(merged, x = ~Fertility_rate, type = "histogram") %>%
  layout(title = "Distribution of Fertility Rate", xaxis = list(title="Fertility rate (number of births)"))

# ============================================================
# Removal of outliers 
merged$Country <- trimws(merged$Country)
colnames(merged) <- c(
  "Country",
  "Fertility_rate",
  "Education",
  "GDP",
  "Female_labor_participation",
  "Female_life_expectancy",
  "Healthcare",
  "Urbanization",
  "Female_political_participation",
  "Crime_Rate"
)
outliers <- c("United States", "China", "India")  
final_clean <- merged[!(trimws(merged$Country) %in% outliers), ]
#write.csv(final_clean, "merged_fertility_project_2023_final.csv", row.names = FALSE)
# ============================================================
# Regression model (multiple linear regression)
final_clean$Country <- trimws(final_clean$Country)
final_clean$log_GDP <- log(final_clean$GDP)
final_clean$log_Urbanization <- log(final_clean$Urbanization)
vars_needed <- c(
  "Fertility_rate", "Education", "log_GDP", "Female_labor_participation",
  "Female_life_expectancy", "Healthcare", "log_Urbanization",
  "Female_political_participation", "Crime_Rate"
)

df_cc <- final_clean[complete.cases(final_clean[vars_needed]), vars_needed]
model_all <- lm(
  Fertility_rate ~ Education + log_GDP + Female_labor_participation +
    Female_life_expectancy + Healthcare + log_Urbanization +
    Female_political_participation + Crime_Rate,
  data = df_cc
)
summary(model_all)

# ============================================================
# Nested model comparison
model_restricted_cc <- lm(
  Fertility_rate ~ log_GDP + Female_labor_participation +
    Female_life_expectancy + log_Urbanization + Crime_Rate,
  data = df_cc
)
anova(model_restricted_cc, model_all)

# ============================================================
# Model selection using AIC (backwards)
model_aic <- step(model_all, direction = "backward", trace = FALSE)
summary(model_aic)

# ============================================================
#  Diagnostics for FINAL model (AIC-selected) 
final_model <- model_aic   # this is the final selected model

pred_final <- fitted(final_model)
res_final  <- resid(final_model)

# Actual vs Predicted
plot(pred_final, final_model$model$Fertility_rate,
     xlab = "Predicted fertility",
     ylab = "Actual fertility",
     main = "Actual vs Predicted (Final model)")
abline(0, 1, col = "red", lwd = 2)

# Residuals vs Fitted
plot(pred_final, res_final,
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "Residuals vs Fitted (Final model)")
abline(h = 0, col = "red", lwd = 2)

# Q-Q plot
qqnorm(res_final, main = "Q–Q Plot of Residuals (Final model)")
qqline(res_final, col = "red", lwd = 2)

# Histogram of residuals + normal curve
hist(res_final,
     breaks = 20,
     freq = FALSE,
     main = "Histogram of Residuals (Final model)",
     xlab = "Residuals",
     col = "lightgray",
     border = "white")

x <- seq(min(res_final), max(res_final), length.out = 200)
lines(x, dnorm(x, mean(res_final), sd(res_final)), col = "red", lwd = 2)


# ============================================================
# normality check
shapiro.test(res_final)

# ============================================================
#  Hypothesis testing: fertility differs by education group
test_df <- final_clean[!is.na(final_clean$Education) & !is.na(final_clean$Fertility_rate), ]
med_edu <- median(test_df$Education)
test_df$Edu_group <- factor(ifelse(test_df$Education >= med_edu,"High education", "Low education"))

boxplot(Fertility_rate ~ Edu_group, data = test_df,
        main = "Fertility rate by Education group",
        xlab = "Education group", ylab = "Fertility rate (births per woman)")

# Two-sample t-test
t_res <- t.test(Fertility_rate ~ Edu_group, data = test_df)
t_res
# Wilcoxon rank-sum test
w_res <- wilcox.test(Fertility_rate ~ Edu_group, data = test_df)
w_res


