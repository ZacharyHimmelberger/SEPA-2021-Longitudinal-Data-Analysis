#
# SEPA 2021 Longitudinal Analysis Workshop
# OLS Regression
#

# call data generation script
source("/Users/zach.himmelberger/OneDrive - Maryville College/Professional Development/SEPA Longitudinal Presentation/data generation.R")

# load packages
# tidyverse already loaded in previous script, no other packages needed

#
# cross-sectional approach
# 

# regress 6th grade self-esteem on gender and peer support
ols.fit <- lm(grade.6 ~ gender*peer.support.c, data=df.wide)

# summarize the model fit
summary(ols.fit)
summary(ols.fit)$sigma^2
deviance(ols.fit)

# plot the regression line
regression.line.plot <- ggplot(data=na.omit(df.wide[c("peer.support", "gender", "grade.6")])) +
  geom_point(aes(x=peer.support, y=grade.6, color=gender),
             size=.25) +
  geom_line(aes(x=peer.support, y=predict(ols.fit), color=gender)) + 
  scale_y_continuous(name="Self-Esteem", limits=c(0, 25)) +
  scale_x_continuous(name="Peer Support", limits=c(0, 8)) +
  scale_color_manual(name="Gender", labels=c("Female", "Male"), 
                     values=c("darkorange2", "grey23")) +
  theme_classic() +
  theme(legend.position="right")
regression.line.plot

# ggsave("/Users/zach.himmelberger/OneDrive - Maryville College/Professional Development/SEPA Longitudinal Presentation/Beamer Presentation/regression.line.plot.jpeg",
#       plot = regression.line.plot,
#       units="in", 
#       width=5, 
#       height=5, 
#       dpi=300)

# plot the residuals
ols.fit.df <- fortify(ols.fit) # saves output of fit as a data.frame

ols.residual.plot <- ggplot(data=ols.fit.df, aes(x = .fitted, y = .resid)) + 
  geom_point(size=.75) +
  geom_hline(yintercept=0, linetype="dashed") +
  annotate("segment", alpha=0.2, color="darkred", size=3,
           x = 13.665, xend = 13.7, 
           y = -10, yend = 10) +
  scale_x_continuous(name="Fitted Value") +
  scale_y_continuous(name="Residual") + 
  theme_classic()

# ggsave("/Users/zach.himmelberger/OneDrive - Maryville College/Professional Development/SEPA Longitudinal Presentation/Beamer Presentation/ols.residual.plot.jpeg",
#       plot = ols.residual.plot,
#       units="in", 
#       width=5, 
#       height=5, 
#       dpi=300)
  
#
# traditional approach - complete pooling
# we aggregate across people (in other words, we ignore that the data comes from different people) 
#

# regress self-esteem on grade 
complete.pooling.fit <- lm(self.esteem ~ grade.c, data=df.long)

# summarize the model fit
summary(complete.pooling.fit)

# add the predicted value to the data.frame
df.long$complete.pooling.prediction <- predict(complete.pooling.fit)

# plot the regression line
complete.pooling.plot <- ggplot(data=df.long) +
  geom_point(aes(x=grade.c, y=self.esteem), size=.25) +
  geom_line(aes(x=grade.c, y=complete.pooling.prediction)) + 
  scale_x_continuous(name="Grade", labels=c("6", "7", "8", "9")) +
  scale_y_continuous(name="Predicted Self-Esteem") + 
  theme_classic() +
  theme(legend.position="right")

# ggsave("/Users/zach.himmelberger/OneDrive - Maryville College/Professional Development/SEPA Longitudinal Presentation/Beamer Presentation/complete.pooling.plot.jpeg",
#       plot = complete.pooling.plot,
#       units="in", 
#       width=5, 
#       height=5, 
#       dpi=300)

#
# traditional approach - no pooling example
# each participant gets there own intercept and slope (a separate regression for each person)
#

no.pool.models <- df.long %>% 
  group_by(id) %>% 
  summarise(mod = fitted(lm(self.esteem ~ grade.c)))

# plot the regression line
no.pooling.plot <- df.long %>%
  group_by(id) %>%
  summarise(grade.c = grade.c, 
            fitted = fitted(lm(self.esteem ~ grade.c))) %>%
  filter(id %in% sample(unique(.$id), 300*.05)) %>%
  ggplot(aes(x = grade.c, y = fitted)) +
  geom_line(aes(group = id), alpha = .8) +
  scale_x_continuous(name="Grade", labels=c("6", "7", "8", "9")) +
  scale_y_continuous(name="Predicted Self-Esteem") + 
  theme_classic() +
  theme(legend.position="right")

# ggsave("/Users/zach.himmelberger/OneDrive - Maryville College/Professional Development/SEPA Longitudinal Presentation/Beamer Presentation/no.pooling.plot.jpeg",
#       plot = no.pooling.plot,
#       units="in", 
#       width=5, 
#       height=5, 
#       dpi=300)





