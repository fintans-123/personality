

library(tidyverse)
library(sjlabelled)
library(psych)
library(lavaan)
library(semPlot)
library(ggplot2)
library(semPlot)
library(cowplot)

setwd("C:/Users/fintan.smith/Desktop/Personality/Personality")

df <- read_spss("personality.sav")


df <- select(df,id, contains("IPIP"), contains("big_five"))


df <- lapply(df, as.numeric)

df<- as.data.frame(df)

df <- df%>%
  mutate(IPIP_mini_6 = 6 - IPIP_mini_6)%>%
  mutate(IPIP_mini_16 = 6 - IPIP_mini_16)%>%
  mutate(IPIP_mini_7 = 6 - IPIP_mini_7)%>%
  mutate(IPIP_mini_17 = 6 - IPIP_mini_17)%>%
  mutate(IPIP_mini_8 = 6 - IPIP_mini_8)%>%
  mutate(IPIP_mini_18 = 6 - IPIP_mini_18)%>%
  mutate(IPIP_mini_9 = 6 - IPIP_mini_9)%>%
  mutate(IPIP_mini_19 = 6 - IPIP_mini_19)%>%
  mutate(IPIP_mini_10 = 6 - IPIP_mini_10)%>%
  mutate(IPIP_mini_20 = 6 - IPIP_mini_20)%>%
  mutate(IPIP_mini_15= 6 - IPIP_mini_15)



# Correlations ------------------------------------------------------------



cor.test(df$IPIP_mini_18, df$big_five_neuroticism)


for_cor <- select(df,contains("IPIP"), -IPIP_mini)

cor_mat<- cor(for_cor)

#Keiser myer olkin  >= .60
KMO(cor_mat)

cortest.bartlett(cor_mat, n=2116)

#Correlation matrix plot
cor.plot(cor_mat)


# EFA ---------------------------------------------------------------------



#EFA 
fafitfree <- fa(for_cor,nfactors = 20, rotate = "varimax")

summary(fafitfree)

#EFA scree plot - eigenvalues
n_factors <- length(fafitfree$e.values)
scree     <- data.frame(
  Factor_n =  as.factor(1:n_factors), 
  Eigenvalue = fafitfree$e.values)
ggplot(scree, aes(x = Factor_n, y = Eigenvalue, group = 1)) + 
  geom_point() + geom_line() +
  xlab("Number of factors") +
  ylab("Eigenvalue") +
  labs( title = "Scree Plot")


parallel <- fa.parallel(for_cor)

fafive <- fa(for_cor,nfactors = 5, rotate = "varimax")

summary(fafive)


# CFA whole model ---------------------------------------------------------



CFA_model <- '
extraversion =~ IPIP_mini_1 + IPIP_mini_11 + IPIP_mini_6 + IPIP_mini_16
agreeableness =~ IPIP_mini_2 + IPIP_mini_12 + IPIP_mini_7 + IPIP_mini_17
conscientiousness =~ IPIP_mini_8 + IPIP_mini_18 + IPIP_mini_3 + IPIP_mini_13
neuroticism =~ IPIP_mini_4 + IPIP_mini_14 + IPIP_mini_9 + IPIP_mini_19
openness=~ IPIP_mini_5 + IPIP_mini_15 + IPIP_mini_10 + IPIP_mini_20'
  
CFA <- lavaan::cfa(CFA_model, data=df)

summary(CFA, fit.measures=T, standardized=TRUE)

semPaths(CFA,residuals=F,sizeMan=7,"std",
         posCol=c("red"),
         edge.label.cex=1.2,layout="circle")



# CFA individual scales ---------------------------------------------------



#EXTRAVERSION

CFA_extraversion_model <-
  'extraversion=~ IPIP_mini_1 + IPIP_mini_11 + IPIP_mini_6 + IPIP_mini_16'

CFA_extraversion <- lavaan::cfa(CFA_extraversion_model, data=df)

summary(CFA_extraversion, fit.measures=T, standardized=TRUE)

#AGREEABLENESS

CFA_agreeableness_model <- 'agreeableness =~ IPIP_mini_2 + IPIP_mini_12 + IPIP_mini_7 + IPIP_mini_17'

CFA_agreeableness <- lavaan::cfa(CFA_agreeableness_model, data=df)

summary(CFA_agreeableness, fit.measures=T, standardized=TRUE)


#INTELLECT / OPENNESS

CFA_open_model <- 'openness=~ IPIP_mini_5 + IPIP_mini_15 + IPIP_mini_10 + IPIP_mini_20'

CFA_open <- lavaan::cfa(CFA_open_model, data=df)

summary(CFA_open, fit.measures=T, standardized=TRUE)


#Conscientiousness

CFA_conscientious_model <- 'conscientiousness =~ IPIP_mini_8 + IPIP_mini_18 + IPIP_mini_3 + IPIP_mini_13'

CFA_conscientious <- lavaan::cfa(CFA_conscientious_model, data=df)

summary(CFA_conscientious, fit.measures=T, standardized=TRUE)


#NEUROTICISM

CFA_neuroticism_model <- 'neuroticism =~ IPIP_mini_4 + IPIP_mini_14 + IPIP_mini_9 + IPIP_mini_19'

CFA_neuroticism <- lavaan::cfa(CFA_neuroticism_model, data=df)

summary(CFA_neuroticism, fit.measures=T, standardized=TRUE)





# Scoring -----------------------------------------------------------------


#End with dataframe with average score, 1/2 sd above mean and 1/2 sd below mean

describe(df)

low_extraversion <- mean(df$big_five_extraversion) - (sd(df$big_five_extraversion) /2)
high_extraversion <- mean(df$big_five_extraversion) + (sd(df$big_five_extraversion) /2)
low_neuroticism <- mean(df$big_five_neuroticism) - (sd(df$big_five_neuroticism) /2)
high_neuroticism <- mean(df$big_five_neuroticism) + (sd(df$big_five_neuroticism) /2)
low_conscientiousness <- mean(df$big_five_conscientiousness) - (sd(df$big_five_conscientiousness) /2)
high_conscientiousness <- mean(df$big_five_conscientiousness) + (sd(df$big_five_conscientiousness) /2)
low_intellect <- mean(df$big_five_intellect) - (sd(df$big_five_intellect) /2)
high_intellect <- mean(df$big_five_intellect) + (sd(df$big_five_intellect) /2)
low_agreeableness <- mean(df$big_five_agreeableness) - (sd(df$big_five_agreeableness) /2)
high_agreeableness <- mean(df$big_five_agreeableness) + (sd(df$big_five_agreeableness) /2)




trait <- c("extraversion", "agreeableness", "neuroticism", "conscientiousness", "intellect")
average <- c(mean(df$big_five_extraversion),mean(df$big_five_agreeableness), mean(df$big_five_neuroticism), mean(df$big_five_conscientiousness), mean(df$big_five_intellect))
low <- c(low_extraversion, low_agreeableness,low_neuroticism,low_conscientiousness,low_intellect)
high <- c(high_extraversion, high_agreeableness,high_neuroticism,high_conscientiousness,high_intellect)

average_values <- data.frame(trait= trait,average,low, high)


ggplot(data=df, aes(big_five_neuroticism)) + geom_histogram(binwidth = 1, fill="white", color="black") +
  geom_vline(aes(xintercept=mean(big_five_neuroticism)), color="orange", linetype="dashed", size=1)+ xlab("Neuroticism score")+ ylab("Count") + theme_cowplot()

#Alternative quantile scoring method

quantile(df$big_five_extraversion)
quantile(df$big_five_agreeableness)
quantile(df$big_five_conscientiousness)
quantile(df$big_five_intellect)
quantile(df$big_five_neuroticism)
