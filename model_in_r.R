# Load necessary libraries
library(readxl)
library(dplyr)
library(MASS)
library(ordinal)

# Load the data
data_01 <- read_excel("pyday/data/data_final.xlsx", sheet = "data")

# Check data types
str(data_01)

# Summary statistics
summary_data_01 <- summary(data_01)

# Transformations
data_01 <- data_01 %>%
  mutate(tipo_cliente = case_when(
    NPS >= 9 ~ "Promotor",
    NPS >= 7 & NPS < 9 ~ "Neutro",
    TRUE ~ "Detractor"
  ))

# Count by tipo_cliente
table(data_01$tipo_cliente)

# Convert 'tipo_cliente' to ordered factor
data_01$tipo_cliente <- factor(data_01$tipo_cliente, levels = c("Detractor", "Neutro", "Promotor"), ordered = TRUE)

# Convert 'NPS' to ordered factor
data_01$NPS <- factor(data_01$NPS, levels = 0:10, ordered = TRUE)

# Count by NPS
table(data_01$NPS)

# Count by Market
table(data_01$Market)

# Convert 'Market' to a factor
data_01$Market <- as.factor(data_01$Market)

# Create dummy variables for 'Market'
market_dummies <- model.matrix(~ Market - 1, data_01)

# Convert the dummy matrix to a data frame
market_dummies_df <- as.data.frame(market_dummies)

# Combine the original data with the dummy variables
data_01 <- cbind(data_01, market_dummies_df)

# Model fitting using ordered logit (proportional odds model)
mod_log_01 <- clm(NPS ~ `Driver 1` + `Driver 2` + `Driver 3`, data = data_01, link = "logit")

# Model summary
summary(mod_log_01)


## Modelo 02
mod_log_02 <- clm(tipo_cliente ~ `Driver 1` + `Driver 2` + `Driver 3`, 
                  data = data_01, link = "logit")

# Model 02 summary
summary(mod_log_02)

## Modelo 03
mod_log_03 <- clm(tipo_cliente ~ `Driver 1` + `Driver 2` + `Driver 3` + Edad + MarketMEX, 
                  data = data_01, link = "logit")

# Model 03 summary
summary(mod_log_03)

# Modelo por ubicación (for US market)
data_us <- data_01[data_01$MarketUS == 1, ]

mod_log_04 <- clm(tipo_cliente ~ `Driver 1` + `Driver 2` + `Driver 3` + Edad, 
                  data = data_us, link = "logit")

# Model 04 summary
summary(mod_log_04)

### Cálculo de la probabilidad

# Extract coefficients from Model 04
data_reg_04 <- coef(mod_log_04)

# Probability calculation using a specific coefficient (assuming position 3 corresponds to 'Edad')
# Example 1:
prob_1 <- 1 / (1 + exp(-0.003110))

# Example 2: Using the 'Edad' coefficient from Model 04
prob_edad <- 1 / (1 + exp(-mod_log_04[["beta"]][["Edad"]]))

# Print the probabilities
prob_1
prob_edad
