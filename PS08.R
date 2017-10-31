library(tidyverse)
library(caret)

# Package for easy timing in R
library(tictoc)




# Demo of timer function --------------------------------------------------
# Run the next 5 lines at once
tic()
Sys.sleep(3)
timer_info <- toc()
runtime <- timer_info$toc - timer_info$tic
runtime



# Get data ----------------------------------------------------------------
# Accelerometer Biometric Competition Kaggle competition data
# https://www.kaggle.com/c/accelerometer-biometric-competition/data

tic()
train <- read_csv("~/Downloads/train.csv")
timer <- toc() 


# YOOGE!
dim(train)
head(train)



# knn modeling ------------------------------------------------------------
 
# Values to use: setting n to number from 0 to 10000 with 500 increments, and setting k to numbers from 0 to 10 with 1 incremenents  
n_values <- c(seq(0, 1000000, 500))
k_values <- c(seq(0, 5, 1))
runtime_dataframe <- expand.grid(n_values, k_values) %>%
  as_tibble() %>%
  rename(n=Var1, k=Var2) %>%
  mutate(runtime = n*k)
runtime_dataframe


# Time knn here -----------------------------------------------------------

for (i in 1:length(k_values)) {
  #go through the each of the k_values 
  for ( j in 1:length(n_values)) {
    #go through the each of the n_values
    #slice the train set at the jth value of n_value we are on 
    trainingset <- slice(train, 1:n_values[j])
    a <- 0
    for (d in 1:(length(n_values)*length(k_values))) {
      tic()
      model_formula <- as.formula(Device ~ X + Y + Z  )
      model_knn <- caret::knn3(model_formula, data=trainingset , k =  k_values[i])
      timer <- toc()
      x <- timer$toc - timer$tic
      a[d] <- x
    }
  }
}

runtime_dataframe[,3] <- a 
runtime_dataframe

# Plot your results ---------------------------------------------------------
# Think of creative ways to improve this barebones plot. Note: you don't have to
# necessarily use geom_point
runtime_plot <- ggplot(runtime_dataframe, aes(x=n, y=k, col=runtime))  +  geom_point()

runtime_plot
ggsave(filename="meron_gedrago.png", width=16, height = 9)




# Runtime complexity ------------------------------------------------------
# Can you write out the rough Big-O runtime algorithmic complexity as a function
# of:
# -n: number of points in training set
# -k: number of neighbors to consider
# -p: number of predictors used? In this case p is fixed at 3

#Big-O runtime complexity : O(n*k)




