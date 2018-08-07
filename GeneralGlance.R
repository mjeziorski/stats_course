# Copied from R-Bloggers 
# General data description
library(funModeling) 
library(tidyverse) 
library(Hmisc)
# For a generic data glance.
# Run all the functions with the following function:
basic_eda <- function(My_data)
  {
    glimpse(My_data)
    df_status(My_data)
    freq(My_data) 
    profiling_num(My_data)
    plot_num(My_data)
    describe(My_data)
}

data=heart_disease %>% select(age, max_heart_rate, thal, has_heart_disease)
basic_eda(data)
