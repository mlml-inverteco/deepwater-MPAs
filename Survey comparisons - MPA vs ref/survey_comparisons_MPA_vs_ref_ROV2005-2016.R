# Title: "Substrate ratios from ROV surveys of MPA and reference transects"
library(tidyverse)
library(ggpubr)

subtype <- c('Hard')

substrate_type_comparisons <- read.csv('substrate_type_comparisons.csv',header=TRUE)
substrate_type_comparisons <- dplyr::filter(substrate_type_comparisons,Substrate==subtype)
# subset <- substrate_type_comparisons$Proportion[substrate_type_comparisons$Substrate==subtype]

attach(substrate_type_comparisons)
  substrate_type_comparisons$Designation <- as.factor(Designation)
  substrate_type_comparisons$MPA <- as.factor(MPA)

#Paired t-test to look at whether proportion of a substrate type is equal between MPA and reference site.
  #If the sites are similar, then look for p > 0.05
#First split MPA vs ref sites out
  MPA <- dplyr::filter(substrate_type_comparisons,Designation=="MPA")
  ref <- dplyr::filter(substrate_type_comparisons,(Designation=="Reference"))
  t.test(MPA$Proportion, ref$Proportion, paired=TRUE,alternative="two.sided")

  prop.test()
  
# model1 <- glm(segelav ~ treat*seg, poisson, data=data) #best LM
# model2 <- glm(segelav ~ treat, poisson, data=data)
# model3 <- glm(segelav ~ seg, poisson, data=data) #best GLM
# model4 <- glm(segelav ~ treat+seg, poisson, data=data)
# model5 <- glm(segelav ~ treat*ptlength, poisson, data=data)
# model6 <- glm(segelav ~ ptlength, poisson, data=data)
# 
# Result= compareGLM(model1, model2, model3, model4, model5, model6)
# plot(Result$Fit.criteria$AICc,
#      xlab = "Model number",
#      ylab = "AICc")
# Result$Fit.criteria$AICc
# 
# summary(model3)

  # install.packages("rcompanion")
  library(rcompanion)
  
  
#Plot the mean of Y for different factor levels
plot.design(Proportion ~ MPA, data=substrate_type_comparisons)
plot.design(Proportion ~ Designation, data=hard_only)
plot.design(Proportion ~ MPA + Designation, data=hard_only)
#Plot the mean of Y for two-way combinations of factors
interaction.plot(hard_only$MPA,hard_only$Designation,hard_only$Proportion)
#Generate the full ANOVA model with all interactions
fullmodel <- 'Proportion ~ MPA + Designation'
raccoon_model <- aov(writeLines(fullmodel),data=hard_only)
raccoon_model
#Present ANOVA summary table
summary(raccoon_model)
#Residual plots
plot(raccoon_model)
plot(raccoon_model$residuals)
hist(hard_only$Proportion)
