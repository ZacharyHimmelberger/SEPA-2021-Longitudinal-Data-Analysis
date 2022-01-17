#
# SEPA 2021 Longitudinal Analysis Workshop
# Multilevel Models
#

# call data generation script
source("https://raw.githubusercontent.com/ZacharyHimmelberger/SEPA-2021-Longitudinal-Data-Analysis/main/data%20generation.R")

# load libraries
# tidyverse already imported in previous script
library(lme4)

# unconditional means model
# 

# create and summarize model
unconditional.means.model <- lmer(self.esteem ~ 1 +
                                    (1 | id), data=df.long, REML=FALSE)
summary(unconditional.means.model)
deviance(unconditional.means.model)

# add the predicted value to the data.frame
df.long$unconditional.means.fitted <- fitted(unconditional.means.model)

# plot results
unconditional.means.plot <- ggplot(data=df.long, aes(x=grade.c, y=unconditional.means.fitted, 
                          color=factor(id))) +
  geom_line(size=.75) + 
  geom_hline(yintercept=fixef(unconditional.means.model)[1],
             size = 1.25) +
  scale_x_continuous(name="Grade", labels=c("6", "7", "8", "9")) +
  scale_y_continuous(name="Predicted Self-Esteem", limits = c(0, 20)) +
  scale_color_brewer(palette="Dark2") +
  theme_classic() +
  theme(legend.position="none")

ggsave("/Users/zach.himmelberger/OneDrive - Maryville College/Professional Development/SEPA Longitudinal Presentation/Beamer Presentation/unconditional.means.plot.jpeg",
       plot = unconditional.means.plot,
       units="in", 
       width=5, 
       height=5, 
       dpi=300)

# ICC = var(U_oj)/(var(U_0j)+(\pi^2/3))
(11.179)/(11.179+3.206) # ICC
# this indicates that ~78% of the self-esteem is explained by individual differences 

# 
# unconditional growth model
# 

# create and summarize model
unconditional.growth.model <- lmer(self.esteem ~ 1 + grade.c +
                                    (1 + grade.c | id), data=df.long, REML=FALSE)
summary(unconditional.growth.model)
# there is a small correlation (r=.15) between the intercept and slope
# the variance values provide a baseline for future models
# var(intercept)=10.33; var(grade)=0.31; var(Residual)=0.45

# add the predicted value to the data.frame
df.long$unconditional.growth.fitted <- fitted(unconditional.growth.model)

# plot results 
unconditional.growth.plot <- ggplot(data = df.long, 
                                    aes(x=grade.c, 
                                        y=unconditional.growth.fitted, 
                                        color=factor(id))) +
  geom_line(size = .75) + 
  geom_abline(intercept=fixef(unconditional.growth.model)[1],
             slope=fixef(unconditional.growth.model)[2],
             size = 1.25) +
  scale_x_continuous(name="Grade", labels=c("6", "7", "8", "9")) +
  scale_y_continuous(name="Predicted Self-Esteem", limits = c(0, 20)) + 
  scale_color_brewer(palette="Dark2") +
  theme_classic() +
  theme(legend.position="none")
# we can see lots of variability in the intercepts and some variability in the slopes

# ggsave("/Users/zach.himmelberger/OneDrive - Maryville College/Professional Development/SEPA Longitudinal Presentation/Beamer Presentation/unconditional.growth.plot.jpeg",
#       plot = unconditional.growth.plot,
#       units="in", 
#       width=5, 
#       height=5, 
#       dpi=300)

# 
# conditional growth model
# 

# create and summarize model
conditional.growth.model <- lmer(self.esteem ~ gender*grade.c + peer.support.c*grade.c + 
                            (1 + grade.c | id), data = df.long, REML = FALSE)

summary(conditional.growth.model)
# there is still only a small correlation (r=.18) between the intercept and slope
# the variance values can be compared to the unconditional growth model 

# unconditional growth model: var(intercept)=10.33; var(grade)=0.31; var(Residual)=0.45
# conditional growth model: var(intercept)=8.40; var(grade)=0.29; var(Residual)=0.45

#
# model comparison
# 

anova(unconditional.means.model, unconditional.growth.model, conditional.growth.model)

# add the predicted value to the data.frame
df.long$conditional.growth.model.fitted <- fitted(conditional.growth.model)

# female with 1 sd below than mean on peer support
14.26369403 + 1.23408810*(-1.148893) # intercept
-1.25436 + (-0.06132)*(-1.148893) # slope

# female with 1 sd above than mean on peer support
14.26369403 + 1.23408810*(1.148893) # intercept
-1.25436 + (-0.06132)*(1.148893) # slope

# male with 1 sd lower than mean on peer support
14.26369403 + 1.23408810*(-1.148893) + 2.30871*(1) # intercept
-1.25436 + (-0.06132)*(-1.148893) +  0.19761*(1) # slope

# male with 1 sd above than mean on peer support
14.26369403 + 1.23408810*(1.148893) + 2.30871*(1) # intercept
-1.25436 + (-0.06132)*(1.148893) + 0.19761*(1) # slope

example.cases.plot <- ggplot(data=df.long) +
  geom_point(aes(x=grade.c, y=self.esteem), alpha = 0) + 
  geom_abline(aes(intercept = 14.26369403 + 1.23408810*(-1.148893),
              slope = -1.25436 + (-0.06132)*(-1.148893),
              color = "female", linetype = "high")) +
  geom_abline(aes(intercept = 14.26369403 + 1.23408810*(1.148893),
              slope = -1.25436 + (-0.06132)*(1.148893),
              color = "female", linetype = "low")) +
  geom_abline(aes(intercept = 14.26369403 + 1.23408810*(-1.148893) + 2.30871*(1),
              slope = -1.25436 + (-0.06132)*(-1.148893) +  0.19761*(1),
              color = "male", linetype = "high")) +
  geom_abline(aes(intercept = 14.26369403 + 1.23408810*(1.148893) + 2.30871*(1),
              slope = -1.25436 + (-0.06132)*(1.148893) + 0.19761*(1),
              color = "male", linetype = "low")) +
  scale_x_continuous(name="Grade", labels=c("6", "7", "8", "9")) +
  scale_y_continuous(name="Predicted Self-Esteem") +
  scale_color_manual(name="Gender", values=c("female" = "darkorange2", "male" = "grey23")) +
  scale_linetype_manual(name="Peer Support", values=c("high" = "solid", "low" = "dashed")) +
  theme_classic()

# ggsave("/Users/zach.himmelberger/OneDrive - Maryville College/Professional Development/SEPA Longitudinal Presentation/Beamer Presentation/example.cases.plot.jpeg",
#       plot = example.cases.plot,
#       units="in", 
#       width=5, 
#       height=5, 
#       dpi=300)

conditional.growth.plot <- ggplot(data = df.long,
            aes(x=grade.c,
                y=conditional.growth.model.fitted,
                color=factor(id))) +
  geom_line(size = .75) +
  geom_abline(intercept = 14.26369403,
              slope = -1.25436,
              linetype = "dashed",
              size = 1.25) +
  geom_abline(intercept = 14.26369403 + 2.30871*(1),
              slope = -1.25436 +  0.19761*(1),
              linetype = "solid",
              size = 1.25) +
  scale_x_continuous(name="Grade", labels=c("6", "7", "8", "9")) +
  scale_y_continuous(name="Predicted Self-Esteem", limits = c(0, 20)) + 
  scale_color_brewer(palette="Dark2") +
  theme_classic() +
  theme(legend.position="none")

# ggsave("/Users/zach.himmelberger/OneDrive - Maryville College/Professional Development/SEPA Longitudinal Presentation/Beamer Presentation/conditional.growth.plot.jpeg",
#       plot = conditional.growth.plot,
#       units="in", 
#       width=5, 
#       height=5, 
#       dpi=300)


#
## Not in Presentation
#



# randomly sample 20 participants
set.seed(022721)
random.subset.participants <- sample(nrow(df.wide), 20)
df.subset <- df.long[df.long$id %in% random.subset.participants, ]

# plot data from 20 participants (not currently in presentation)
unconditional.growth.subset.plot <- ggplot(data=df.subset, aes(x=grade.c, y=unconditional.growth.fitted, group=id, color=factor(id))) +
  geom_line(size=.25) +
  geom_abline(intercept=fixef(unconditional.growth.model)[1],
              slope=fixef(unconditional.growth.model)[2]) +
  scale_x_continuous(name="Grade", labels=c("6", "7", "8", "9")) +
  scale_y_continuous(name="Predicted Self-Esteem") +
  theme(legend.position="none")

# alternative plot (not currently in presentation)
ggplot(data=df.subset, aes(x=grade.c, y=unconditional.growth.fitted)) +
  geom_line(aes(color=factor(id)), size=.25) +
  geom_abline(intercept=fixef(unconditional.growth.model)[1],
              slope=fixef(unconditional.growth.model)[2]) +
  scale_x_continuous(name="Grade", labels=c("6", "7", "8", "9")) +
  scale_y_continuous(name="Predicted Self-Esteem") +
  facet_wrap(~factor(id)) +
  theme(legend.position="none")




