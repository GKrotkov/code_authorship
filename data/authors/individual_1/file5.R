## ----setup, echo = FALSE---------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = FALSE, message = FALSE, 
                      warning = FALSE, error = FALSE)


## --------------------------------------------------------------------------------------------------------
load("data.rda")
library(tidyverse)
library(hexbin)
library(scales)
library(factoextra)
library(tidyquant)


## ----n_postings distribution-----------------------------------------------------------------------------
ggplot(companies[companies$n_postings > 10, ], 
       aes(x = n_postings, color = company_size)) +
    geom_histogram(bins = 35, fill = "deepskyblue1", color = "black") + 
    labs(title = "Honeywell has a high volume of job postings", 
         subtitle = "Even among the top 5% of LinkedIn posters",
         x = "Number of Postings", 
         y = "Frequency", 
         tag = "Figure 1") + 
    geom_vline(xintercept = 46, color = "red") + 
    annotate("text", x = 42, y = 20, label="Honeywell", angle = 90) + 
    theme_bw() + 
    theme(panel.grid = element_blank())



## ----salary----------------------------------------------------------------------------------------------
ggplot(companies[companies$company_size %in% 7, ], 
       aes(x = ifelse(mean_salary_min < 1000, NA,
                      mean_salary_min))) + 
    geom_histogram(bins = 20, fill = "deepskyblue1", color = "black") + 
    geom_vline(xintercept = 124268.2, color = "red") + 
    annotate("text", x = 120000, y = 25, label="Honeywell", angle = 90) + 
    labs(title = "Honeywell's Salaries are Reflective of White Collar Work", 
         subtitle = "Honeywell in red",
         tag = "Figure 2",
         x = "Minimum Salary (dollars)", y = "Count") + 
    scale_x_continuous(labels = label_dollar()) + 
    theme_bw() + 
    theme(panel.grid = element_blank())


## ----stock price plot------------------------------------------------------------------------------------
# Stock Price for Booz Allen
booz_allen <- tq_get(c("BAH"), get  = "stock.prices",
                     from = "2015-01-01", to = "2023-10-01")

# Stock Price for Verizon
verizon <- tq_get(c("VZ"), get  = "stock.prices",
                  from = "2015-01-01", to = "2023-10-01")


# Stock price for Honeywell
honeywell <- tq_get(c("HON"), get  = "stock.prices",
                    from = "2015-01-01", to = "2023-10-01")


# Stock Price for Apex Systems
apex_systems <- tq_get(c("ASGN"), get  = "stock.prices",
                       from = "2015-01-01", to = "2023-10-01")

# Stock price for IBM
ibm <- tq_get(c("IBM"), get  = "stock.prices",
              from = "2015-01-01", to = "2023-10-01")

# Stock Price for Oracle
oracle <- tq_get(c("ORCL"), get  = "stock.prices",
                 from = "2015-01-01", to = "2023-10-01")

# Combine all Company Stock Prices into 1 data frame
combine_stocks <- rbind(booz_allen, verizon, honeywell, 
                        apex_systems, ibm, oracle)

grouped_stocks <- combine_stocks %>%
  filter(symbol != "HON")

# Plot the daily closing stock price from Jan 1, 2015 to October 1, 2023
combine_stocks %>%
  filter(symbol != "HON") %>%
  ggplot() +
  geom_line(aes(x = date, y = close, group = symbol), 
            alpha = 0.5, color = "gray") +
  geom_line(data = filter(combine_stocks, symbol == "HON"), 
            aes(x = date, y = close, color = symbol), alpha = 1) +
  labs(x = "Date",
       y = "Close Price of Stock",
       title = "Honeywell Stock Outperforms Competitors from 2015-2023",
       color = "Ticker Symbol",
       tag = "Figure 3",
       subtitle = "Includes Verizon, Oracle, 
       Booz Allen Hamilton, IBM, Apex Systems") +
  theme_bw() +
  theme(panel.grid = element_blank())


## ----size and influence----------------------------------------------------------------------------------
ggplot(companies, aes(y = log(employees), 
                      x = log(followers), 
                      color = factor(company_id == 1344), 
                      alpha = factor(company_id == 1344), 
                      size = factor(company_id == 1344))) + 
           geom_point() + 
    labs(title = "Honeywell meets our size and influence expectations", 
         subtitle = "Honeywell in red",
         tag = "Figure 4",
         x = "Log Employees", y = "Log LinkedIn Followers", 
         color = "Company:", alpha = "Company:", size = "Company:") + 
    scale_color_manual(values = c("grey", "red"), 
                       labels = c("Other", "Honeywell")) + 
    scale_alpha_manual(values = c(0.4, 1), 
                       labels = c("Other", "Honeywell")) + 
    scale_size_manual(values = c(1, 3), 
                      labels = c("Other", "Honeywell")) + 
    theme_bw() + 
    theme(panel.grid = element_blank())



## ----followers boxplots----------------------------------------------------------------------------------
# Followers Boxplot
ggplot(companies[!is.na(companies$company_size), ], 
       aes(x = factor(company_size), y = log(followers))) + 
    geom_violin(fill = "deepskyblue1", scale = "width") +
    geom_boxplot(fill = "deepskyblue4") + 
    geom_hline(yintercept = 14.75, color = "red") +
    annotate("text", x = 2, y = 14.25, label="Honeywell", 
             angle = 90, size = 4) +
    coord_flip() +
    labs(x = "Company Size Category", y = "Log Linkedin Followers",
         title = "Honeywell Has a Strong LinkedIn Presence",
         subtitle = "Even Accounting for Size", tag = "Figure 5") +
    theme_bw() + 
    theme(panel.grid = element_blank())


## ---- pca------------------------------------------------------------------------------------------------
companies_quant <- companies[, c("employees", "followers", "n_postings", 
                                 "mean_salary_max", "mean_salary_min", 
                                 "fulltime_pct", "parttime_pct")]

companies_pca <- prcomp(na.omit(companies_quant), 
                        center = TRUE, scale = TRUE)

fviz_pca_var(companies_pca, label = "var", labelsize = 4,
             alpha.var = 0.5, col.var = "darkblue", repel = TRUE) + 
    theme_bw() + 
    labs(title = "Employees and Followers Vary with Postings", 
         subtitle = "Other variables are nearly uncorrelated",
         tag = "Figure 6") + 
    theme(panel.grid = element_blank())

