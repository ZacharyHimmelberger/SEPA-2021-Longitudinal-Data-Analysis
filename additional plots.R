#
# SEPA 2021 Longitudinal Analysis Workshop
# Additional Plots
#

# call data generation script
source("/Users/zach.himmelberger/OneDrive - Maryville College/Professional Development/SEPA Longitudinal Presentation/data generation.R")

# load packages
# tidyverse already loaded in previous script
library(ggpubr)

#
# different growth trends
#

# create data.frame with different trends
x <- seq(0, 100)
y <- x 
y2 <- x + x^2 + x^3 + x^4 
y3 <- log(x)
plot.df <- data.frame(x, y, y2, y3)

one <- ggplot(data = plot.df) +
  geom_line(aes(x = x, y = y)) +
  scale_y_continuous(limits = c(0, 150)) +
  labs(title = "Linear") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, vjust = -2),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

two <- ggplot(data = plot.df) +
  geom_line(aes(x = x, y = y)) +
  scale_y_continuous(limits = c(0, 500)) +
  labs(title = "Linear") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, vjust = -2),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

three <- ggplot(data = plot.df) +
  geom_line(aes(x = x, y = y2)) +
  labs(title = "Exponential") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, vjust = -2),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

four <- ggplot(data = plot.df) +
  geom_line(aes(x = x, y = y3)) +
  labs(title = "Log") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, vjust = -2),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

growth.trends.plot <- ggarrange(one, two, three, four, nrow = 2, ncol = 2)

# ggsave("/Users/zach.himmelberger/OneDrive - Maryville College/Professional Development/SEPA Longitudinal Presentation/Beamer Presentation/growth.trends.plot.jpeg",
#       plot = growth.trends.plot,
#       units="in", 
#       width=5, 
#       height=5, 
#       dpi=300)

#
# graph showing sensitivity to noise in data with many measurements
#

#simulate data
sim.data <- data.frame(id = 1:15)

set.seed(17032021) # 17-March-2021

sim.data$t1 <- rnorm(n=nrow(sim.data), mean=10, sd=2)
sim.data$t2 <- sim.data$t1 + rnorm(n=nrow(sim.data), mean=1, sd=2)
sim.data$t3 <- sim.data$t2 + rnorm(n=nrow(sim.data), mean=.5, sd=2)
sim.data$t4 <- sim.data$t3 + rnorm(n=nrow(sim.data), mean=-1, sd=2)
sim.data$t5 <- sim.data$t4 + rnorm(n=nrow(sim.data), mean=1, sd=2)
sim.data$t6 <- sim.data$t5 + rnorm(n=nrow(sim.data), mean=1, sd=2)
sim.data$t7 <- sim.data$t6 + rnorm(n=nrow(sim.data), mean=1, sd=2)
sim.data$t8 <- sim.data$t7 + rnorm(n=nrow(sim.data), mean=0, sd=2)
sim.data$t9 <- sim.data$t8 + rnorm(n=nrow(sim.data), mean=1, sd=2)
sim.data$t10 <- sim.data$t9 + rnorm(n=nrow(sim.data), mean=1, sd=2)

# convert to long format
sim.data.long <- 
  sim.data %>%
  pivot_longer(
    cols=c(t1:t10),
    names_to="time",
    values_to="DV")

# recode time variable
sim.data.long$time <- recode(sim.data.long$time, 
                             "t1" = 1,
                             "t2" = 2,
                             "t3" = 3,
                             "t4" = 4,
                             "t5" = 5,
                             "t6" = 6,
                             "t7" = 7,
                             "t8" = 8,
                             "t9" = 9,
                             "t10" = 10)

# plot results with all ten time points
one <- ggplot(data=sim.data.long, aes(x=time, y=DV)) +
  stat_summary(fun = "mean", geom = "line") +
  scale_x_continuous(name="Time", breaks = c(1:10), labels = c(1:10)) +
  scale_y_continuous(name="Outcome Variable") +
  theme_classic() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# plot results with five time points
two <- sim.data.long %>%
  filter(time %% 2 == 0) %>%
  ggplot(aes(x=time, y=DV)) +
  stat_summary(fun = "mean", geom = "line") +
  scale_x_continuous(name="Time", breaks = seq(2, 10, by = 2), labels = seq(2, 10, by = 2)) +
  scale_y_continuous(name="Outcome Variable") +
  theme_classic() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

num.obs.sensivity.plot <- ggarrange(one, two, ncol = 2)

# ggsave("/Users/zach.himmelberger/OneDrive - Maryville College/Professional Development/SEPA Longitudinal Presentation/Beamer Presentation/num.obs.sensivity.plot.jpeg",
#       plot = num.obs.sensivity.plot,
#       units="in", 
#       width=5, 
#       height=5, 
#       dpi=300)
