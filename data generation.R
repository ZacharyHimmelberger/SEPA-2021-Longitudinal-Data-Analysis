#
# SEPA 2021 Longitudinal Analysis Workshop
# Data Generation
#

# load libraries
library(tidyverse)

# set random seed
set.seed(17032021) # 17-March-2021

# participant ID
id <- 1:300

# gender
gender <- sample(c("female", "male"), replace=TRUE, size=300, prob=c(.6, .4))

# create a dataframe
df <- data.frame(id, gender)

# peer.support
df$peer.support[df$gender == "female"] <- rnorm(n=nrow(df[df$gender == "female", ]), mean=5, sd=1)
df$peer.support[df$gender == "male"] <- rnorm(n=nrow(df[df$gender == "male", ]), mean=4, sd=1)

# self-esteem TIME 1
df$self.esteem.1[df$gender == "female"] <- 10 - 2.5 + 1.5*(df[df$gender == "female", "peer.support"]) + rnorm(n=nrow(df[df$gender == "female", ]), mean=0, sd=3)
df$self.esteem.1[df$gender == "male"] <- 10 + 1.5*(df[df$gender == "male", "peer.support"]) + rnorm(n=nrow(df[df$gender == "male", ]), mean=0, sd=3)

# self-esteem TIME 2
df$self.esteem.2[df$gender == "female"] <- df$self.esteem.1[df$gender == "female"] - 1.50 + rnorm(n=nrow(df[df$gender == "female", ]), mean=0, sd=1)
df$self.esteem.2[df$gender == "male"] <- df$self.esteem.1[df$gender == "male"] - 1 + rnorm(n=nrow(df[df$gender == "male", ]), mean=0, sd=1)

# self-esteem TIME 3
df$self.esteem.3[df$gender == "female"] <- df$self.esteem.2[df$gender == "female"] - 1.25 + rnorm(n=nrow(df[df$gender == "female", ]), mean=0, sd=1)
df$self.esteem.3[df$gender == "male"] <- df$self.esteem.2[df$gender == "male"] - 1 + rnorm(n=nrow(df[df$gender == "male", ]), mean=0, sd=1)

# self-esteem TIME 4
df$self.esteem.4[df$gender == "female"] <- df$self.esteem.3[df$gender == "female"] - 1 + rnorm(n=nrow(df[df$gender == "female", ]), mean=0, sd=1)
df$self.esteem.4[df$gender == "male"] <- df$self.esteem.3[df$gender == "male"] - 1 + rnorm(n=nrow(df[df$gender == "male", ]), mean=0, sd=1)

# convert to long data format, round values, and keep only a random 
# subset of the observations to mimic missing data 
df.long <- 
  df %>%
  pivot_longer(
    cols=c(self.esteem.1, self.esteem.2, self.esteem.3, self.esteem.4),
    names_to="temp",
    values_to="self.esteem") %>%
  mutate_if(is.numeric, round) %>%
  slice_sample(prop=.90) # keeps 90% of the observations

# recode grade
df.long$grade <- recode(df.long$temp,
                        "self.esteem.1"=6, 
                        "self.esteem.2"=7,
                        "self.esteem.3"=8,
                        "self.esteem.4"=9)

# center grade so that our first time point equals zero and mean center peer-support
df.long$grade.c <- df.long$grade - 6
df.long$peer.support.c <- df.long$peer.support - mean(df.long$peer.support)

# create a wide version of the dataframe
# this is necessary because we deleted some observations in the long format
df.wide <- 
  df.long %>%
  pivot_wider(
    id_cols=c(id, gender, peer.support),
    names_from=grade,
    names_prefix="grade.",
    values_from=self.esteem)

# mean center peer-support in the wide format data
df.wide$peer.support.c <- df.wide$peer.support - mean(df.wide$peer.support)


