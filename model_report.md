Model Performance Report
================

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax
for authoring HTML, PDF, and MS Word documents. For more details on
using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that
includes both content as well as the output of any embedded R code
chunks within the document. You can embed an R code chunk like this:

``` r
# Load necessary libraries
library(readxl)
library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 4.3.3

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(MASS)
```

    ## 
    ## Attaching package: 'MASS'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     select

``` r
library(ordinal)
```

    ## Warning: package 'ordinal' was built under R version 4.3.3

    ## 
    ## Attaching package: 'ordinal'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     slice

``` r
# Load the data
data_01 <- read_excel("pyday/data/data_final.xlsx", sheet = "data")

# Check data types
str(data_01)
```

    ## tibble [5,000 × 9] (S3: tbl_df/tbl/data.frame)
    ##  $ ID           : num [1:5000] 1000 1001 1002 1003 1004 ...
    ##  $ Market       : chr [1:5000] "US" "MEX" "UK" "UK" ...
    ##  $ Survey date  : chr [1:5000] "09/01/2021" "11/07/2021" "25/12/2021" "10/01/2021" ...
    ##  $ Customer Name: chr [1:5000] "Krista Richards" "Monica King" "Ricky Armstrong" "Andrea Foley" ...
    ##  $ NPS          : num [1:5000] 10 9 0 10 8 10 10 4 8 10 ...
    ##  $ Driver 1     : num [1:5000] 9 7 2 10 7 8 8 1 9 8 ...
    ##  $ Driver 2     : num [1:5000] 5 1 2 7 5 6 9 5 6 1 ...
    ##  $ Driver 3     : num [1:5000] 10 5 8 7 10 7 6 8 9 5 ...
    ##  $ Edad         : num [1:5000] 69 28 51 27 78 73 39 71 62 34 ...

``` r
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
```

    ## 
    ## Detractor    Neutro  Promotor 
    ##      1719       972      2309

``` r
# Convert 'tipo_cliente' to ordered factor
data_01$tipo_cliente <- factor(data_01$tipo_cliente, levels = c("Detractor", "Neutro", "Promotor"), ordered = TRUE)

# Convert 'NPS' to ordered factor
data_01$NPS <- factor(data_01$NPS, levels = 0:10, ordered = TRUE)

# Count by NPS
table(data_01$NPS)
```

    ## 
    ##    0    1    2    3    4    5    6    7    8    9   10 
    ##  565  172  154  181  120  321  206  292  680  712 1597

``` r
# Count by Market
table(data_01$Market)
```

    ## 
    ##  MEX   UK   US 
    ## 1649 1720 1631

``` r
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
```

    ## formula: NPS ~ `Driver 1` + `Driver 2` + `Driver 3`
    ## data:    data_01
    ## 
    ##  link  threshold nobs logLik   AIC      niter max.grad cond.H 
    ##  logit flexible  5000 -8450.85 16927.70 6(1)  3.73e-09 2.6e+04
    ## 
    ## Coefficients:
    ##            Estimate Std. Error z value Pr(>|z|)    
    ## `Driver 1` 0.634897   0.012984  48.898   <2e-16 ***
    ## `Driver 2` 0.015937   0.009124   1.747   0.0807 .  
    ## `Driver 3` 0.170846   0.011877  14.385   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Threshold coefficients:
    ##      Estimate Std. Error z value
    ## 0|1    1.9707     0.1114   17.69
    ## 1|2    2.3557     0.1109   21.24
    ## 2|3    2.6604     0.1112   23.93
    ## 3|4    2.9947     0.1122   26.69
    ## 4|5    3.2105     0.1133   28.35
    ## 5|6    3.7983     0.1178   32.25
    ## 6|7    4.1983     0.1220   34.42
    ## 7|8    4.8479     0.1307   37.10
    ## 8|9    6.0302     0.1421   42.45
    ## 9|10   6.9042     0.1462   47.24

``` r
## Modelo 02
mod_log_02 <- clm(tipo_cliente ~ `Driver 1` + `Driver 2` + `Driver 3`, 
                  data = data_01, link = "logit")

# Model 02 summary
summary(mod_log_02)
```

    ## formula: tipo_cliente ~ `Driver 1` + `Driver 2` + `Driver 3`
    ## data:    data_01
    ## 
    ##  link  threshold nobs logLik   AIC     niter max.grad cond.H 
    ##  logit flexible  5000 -3254.26 6518.51 5(0)  8.79e-08 7.9e+03
    ## 
    ## Coefficients:
    ##            Estimate Std. Error z value Pr(>|z|)    
    ## `Driver 1`  0.78054    0.01810  43.134   <2e-16 ***
    ## `Driver 2`  0.01799    0.01172   1.535    0.125    
    ## `Driver 3`  0.21822    0.01609  13.561   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Threshold coefficients:
    ##                  Estimate Std. Error z value
    ## Detractor|Neutro   5.5931     0.1736   32.21
    ## Neutro|Promotor    7.3916     0.1945   38.00

## Including Plots

You can also embed plots, for example:

    ## formula: tipo_cliente ~ `Driver 1` + `Driver 2` + `Driver 3` + Edad + MarketMEX
    ## data:    data_01
    ## 
    ##  link  threshold nobs logLik   AIC     niter max.grad cond.H 
    ##  logit flexible  5000 -3254.21 6522.43 5(0)  8.79e-08 2.3e+05
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)    
    ## `Driver 1`  0.7807672  0.0181153  43.100   <2e-16 ***
    ## `Driver 2`  0.0180973  0.0117271   1.543    0.123    
    ## `Driver 3`  0.2181088  0.0160969  13.550   <2e-16 ***
    ## Edad       -0.0001868  0.0018773  -0.099    0.921    
    ## MarketMEX  -0.0196749  0.0711171  -0.277    0.782    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Threshold coefficients:
    ##                  Estimate Std. Error z value
    ## Detractor|Neutro   5.5786     0.1995   27.96
    ## Neutro|Promotor    7.3771     0.2178   33.87

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.

``` r
# Extract some model metrics, e.g., coefficients, log-likelihood, AIC, etc.
coef(mod_log_03)  # Coefficients
```

    ## Detractor|Neutro  Neutro|Promotor       `Driver 1`       `Driver 2` 
    ##     5.5785721663     7.3770751597     0.7807671817     0.0180972972 
    ##       `Driver 3`             Edad        MarketMEX 
    ##     0.2181088069    -0.0001867579    -0.0196748834

``` r
logLik(mod_log_03)  # Log-likelihood
```

    ## 'log Lik.' -3254.213 (df=7)

``` r
AIC(mod_log_03)  # AIC
```

    ## [1] 6522.427

``` r
# Example predictions
predictions <- predict(mod_log_03, newdata = data_01)
```
