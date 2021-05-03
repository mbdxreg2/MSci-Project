library(tidyverse)
library(Hmisc)
library(ggplot2)

my_data <- read_csv("Survey Data.csv")


##DEMOGRAPHICS:

#Number of male and female Ps (where 1 = F, 2 = M, 3 = not given)

count(my_data, sex)


#Mean and SD age

mean(my_data$age, na.rm = TRUE)  

sd(my_data$age, na.rm = TRUE)


#Checking that Ps answered attention check questions correctly

count(my_data, spq_a)

count(my_data, shai_a)

count(my_data, phq15_a)


# Checking temperature answers

count(my_data, temp_rating)
count(my_data, temp)



##CREATING NEW COlUMNS


#Mean SPS (overall) intensity


my_data$mean_intensity = (my_data$sps1_i + my_data$sps2_i) / 2


#Mental health questionnaire scores


#NB//Order is SHAI, STAI (state), STAI (trait), STAI (total), PHQ-15, PHQ-9, SHS and SPQ


my_data$SHAI_score = my_data$shai_1 + my_data$shai_2 + my_data$shai_3 + my_data$shai_4 + my_data$shai_5 + my_data$shai_6 + my_data$shai_7 + my_data$shai_8 + my_data$shai_9 + my_data$shai_10 + my_data$shai_11 + my_data$shai_12 + my_data$shai_13 + my_data$shai_14 + my_data$shai_15 + my_data$shai_16 + my_data$shai_17 + my_data$shai_18 - 18

my_data$STAI_score = my_data$stai_s1 + my_data$stai_s2 + my_data$stai_s3 + my_data$stai_s4 + my_data$stai_s5 + my_data$stai_t1 + my_data$stai_t2 + my_data$stai_t1 + my_data$stai_t4 + my_data$stai_t5

my_data$STAI_s_score = my_data$stai_s1 + my_data$stai_s2 + my_data$stai_s3 + my_data$stai_s4 + my_data$stai_s5 

my_data$STAI_t_score = my_data$stai_t1 + my_data$stai_t2 + my_data$stai_t1 + my_data$stai_t4 + my_data$stai_t5

my_data$PHQ15_score = my_data$phq15_1 + my_data$phq15_2 + my_data$phq15_3 + my_data$phq15_5 + my_data$phq15_6 + my_data$phq15_7 + my_data$phq15_8 + my_data$phq15_9 + my_data$phq15_10 + my_data$phq15_11 + my_data$phq15_12 + my_data$phq15_13 + my_data$phq15_14 + my_data$phq15_15

#Note that phq15_4 omitted, since that was a female-only question, and if kept in scores would not be comparable between males and females

my_data$PHQ9_score = my_data$phq9_1 + my_data$phq9_2 + my_data$phq9_3 + my_data$phq9_4 + my_data$phq9_5 + my_data$phq9_6 + my_data$phq9_7 + my_data$phq9_8 + my_data$phq9_9

my_data$SHS_score = my_data$shs_1 + my_data$shs_2 + my_data$shs_3 + my_data$shs_4 + my_data$shs_5 + (6 - my_data$shs_6) + (6 - my_data$shs_7) + my_data$shs_8 + my_data$shs_9 + (6 - my_data$shs_10)

my_data$SPQ_score = (5 - my_data$spq_1) + my_data$spq_2 + my_data$spq_3 + my_data$spq_4 + my_data$spq_5 + my_data$spq_6 + my_data$spq_7 + my_data$spq_8 + my_data$spq_9 + my_data$spq_10 + (5 - my_data$spq_11) + (5 - my_data$spq_12) + (5 - my_data$spq_13)


#Creating a column for max intensity (via creating a new table)

my_data_int <- my_data[, c("sps1_i1", "sps1_i2","sps1_i3","sps1_i4","sps1_i5","sps1_i6","sps1_i7","sps1_i8","sps1_i9","sps1_i10","sps1_i11","sps1_i12", "sps2_i1", "sps2_i2", "sps2_i3","sps2_i4","sps2_i5","sps2_i6","sps2_i7","sps2_i8","sps2_i9","sps2_i10","sps2_i11","sps2_i12")]

my_data_int[is.na(my_data_int)] = 0

my_data$Max_int <- apply(my_data_int, 1, max, na.rm = TRUE)


##DESCRIPTIVE STATISTICS


#Mean SPS intensity

mean(my_data$mean_intensity)
sd(my_data$mean_intensity)

me <- qt(.975,104)*sd(my_data$mean_intensity)/sqrt(104)

mean(my_data$mean_intensity) - me
mean(my_data$mean_intensity) + me


#SHAI score

mean(my_data$SHAI_score)
sd(my_data$SHAI_score)

me <- qt(.975,104)*sd(my_data$SHAI_score)/sqrt(104)

mean(my_data$SHAI_score) - me
mean(my_data$SHAI_score) + me


#STAI score

mean(my_data$STAI_score)
sd(my_data$STAI_score)

me <- qt(.975,104)*sd(my_data$STAI_score)/sqrt(104)

mean(my_data$STAI_score) - me
mean(my_data$STAI_score) + me


#PHQ-15 score

mean(my_data$PHQ15_score)
sd(my_data$PHQ15_score)

me <- qt(.975,104)*sd(my_data$PHQ15_score)/sqrt(104)

mean(my_data$PHQ15_score) - me
mean(my_data$PHQ15_score) + me

#Mean SPS amount

mean(my_data$Mean_SPS_Amount)
sd(my_data$Mean_SPS_Amount)

me <- qt(.975,104)*sd(my_data$Mean_SPS_Amount)/sqrt(104)

mean(my_data$Mean_SPS_Amount) - me
mean(my_data$Mean_SPS_Amount) + me

#Maximum SPS intensity

mean(my_data$Max_int)
sd(my_data$Max_int)

me <- qt(.975,104)*sd(my_data$Max_int)/sqrt(104)

mean(my_data$Max_int) - me
mean(my_data$Max_int) + me


#Total SPS amount

mean(my_data$Total_SPS_Amount)
sd(my_data$Total_SPS_Amount)

me <- qt(.975,104)*sd(my_data$Total_SPS_Amount)/sqrt(104)

mean(my_data$Total_SPS_Amount) - me
mean(my_data$Total_SPS_Amount) + me


#STAI-s score

mean(my_data$STAI_s_score)
sd(my_data$STAI_s_score)

me <- qt(.975,104)*sd(my_data$STAI_s_score)/sqrt(104)

mean(my_data$STAI_s_score) - me
mean(my_data$STAI_s_score) + me


#STAI-t score

mean(my_data$STAI_t_score)
sd(my_data$STAI_t_score)

me <- qt(.975,104)*sd(my_data$STAI_t_score)/sqrt(104)

mean(my_data$STAI_t_score) - me
mean(my_data$STAI_t_score) + me

#SPQ score


mean(my_data$SPQ_score)
sd(my_data$SPQ_score)

me <- qt(.975,104)*sd(my_data$SPQ_score)/sqrt(104)

mean(my_data$SPQ_score) - me
mean(my_data$SPQ_score) + me

#SHS score

mean(my_data$SHS_score)
sd(my_data$SHS_score)

me <- qt(.975,104)*sd(my_data$SHS_score)/sqrt(104)

mean(my_data$SHS_score) - me
mean(my_data$SHS_score) + me


###PRIMARY ANALYSES###


#Linear regression for SHAI score and mean SPS overall intensity

ggplot(my_data, aes(x = SHAI_score, y = mean_intensity)) + geom_point()

ggplot(my_data, aes(x = SHAI_score, y = mean_intensity)) + geom_point() +
  geom_hline(yintercept = mean(my_data$mean_intensity), colour = "blue") +
  geom_smooth(method = "lm", se = FALSE)

rcorr(my_data$SHAI_score, my_data$mean_intensity)

model_mi0 <- lm (mean_intensity ~ 1, data = my_data)
model_mi1 <- lm (mean_intensity ~ SHAI_score, data = my_data)

anova(model_mi0, model_mi1)

summary(model_mi1)

confint(model_mi1, 'SHAI_score', level=0.95)


#Linear regression for STAI total score and mean SPS overall intensity

ggplot(my_data, aes(x = STAI_score, y = mean_intensity)) + geom_point()

ggplot(my_data, aes(x = STAI_score, y = mean_intensity)) + geom_point() +
  geom_hline(yintercept = mean(my_data$mean_intensity), colour = "blue") +
  geom_smooth(method = "lm", se = FALSE)

rcorr(my_data$STAI_score, my_data$mean_intensity)

model_mi2 <- lm (mean_intensity ~ 1, data = my_data)
model_mi3 <- lm (mean_intensity ~ STAI_score, data = my_data)

anova(model_mi2, model_mi3)

summary(model_mi3)

confint(model_mi3, 'STAI_score', level=0.95)


#Linear regression for mean SPS overall intensity and PHQ-15 score

ggplot(my_data, aes(x = mean_intensity, y = PHQ15_score)) + geom_point()

ggplot(my_data, aes(x = mean_intensity, y = PHQ15_score)) + geom_point() +
  geom_hline(yintercept = mean(my_data$PHQ15_score), colour = "blue") +
  geom_smooth(method = "lm", se = FALSE)

rcorr(my_data$mean_intensity, my_data$PHQ15_score)

model_mi4 <- lm (PHQ15_score ~ 1, data = my_data)
model_mi5 <- lm (PHQ15_score ~ mean_intensity, data = my_data)

anova(model_mi4, model_mi5)

summary(model_mi5)

confint(model_mi5, 'mean_intensity', level=0.95)


###SECONDARY ANALYSES###


#Linear regression for SHAI score and mean SPS amount

ggplot(my_data, aes(x = SHAI_score, y = Mean_SPS_Amount)) + geom_point()

ggplot(my_data, aes(x = SHAI_score, y = Mean_SPS_Amount)) + geom_point() +
  geom_hline(yintercept = mean(my_data$Mean_SPS_Amount), colour = "blue") +
  geom_smooth(method = "lm", se = FALSE)

rcorr(my_data$SHAI_score, my_data$Mean_SPS_Amount)

model_ma0 <- lm (Mean_SPS_Amount ~ 1, data = my_data)
model_ma1 <- lm (Mean_SPS_Amount ~ SHAI_score, data = my_data)

anova(model_ma0, model_ma1)

summary(model_ma1)

confint(model_ma1, 'SHAI_score', level=0.95)


#Linear regression for STAI total score and mean SPS amount

ggplot(my_data, aes(x = STAI_score, y = Mean_SPS_Amount)) + geom_point()

ggplot(my_data, aes(x = STAI_score, y = Mean_SPS_Amount)) + geom_point() +
  geom_hline(yintercept = mean(my_data$Mean_SPS_Amount), colour = "blue") +
  geom_smooth(method = "lm", se = FALSE)

rcorr(my_data$STAI_score, my_data$Mean_SPS_Amount)

model_ma2 <- lm (Mean_SPS_Amount ~ 1, data = my_data)
model_ma3 <- lm (Mean_SPS_Amount ~ STAI_score, data = my_data)

anova(model_ma2, model_ma3)

summary(model_ma3)

confint(model_ma3, 'STAI_score', level=0.95)


#Linear regression for mean SPS amount and PHQ-15 score

ggplot(my_data, aes(x = Mean_SPS_Amount, y = PHQ15_score)) + geom_point()

ggplot(my_data, aes(x = Mean_SPS_Amount, y = PHQ15_score)) + geom_point() +
  geom_hline(yintercept = mean(my_data$PHQ15_score), colour = "blue") +
  geom_smooth(method = "lm", se = FALSE)

rcorr(my_data$Mean_SPS_Amount, my_data$PHQ15_score)

model_ma4 <- lm (PHQ15_score ~ 1, data = my_data)
model_ma5 <- lm (PHQ15_score ~ Mean_SPS_Amount, data = my_data)

anova(model_ma4, model_ma5)

summary(model_ma5)

confint(model_ma5, 'Mean_SPS_Amount', level=0.95)


###EXPLORATORY ANALYSES###


##CHECKING SEX DIFFERENCES

wc1 <- wilcox.test(mean_intensity ~ sex, data = my_data,
                   exact = FALSE)
wc1


wc2 <- wilcox.test(Mean_SPS_Amount ~ sex, data = my_data,
                   exact = FALSE)
wc2

wc3 <- wilcox.test(SHAI_score ~ sex, data = my_data,
                   exact = FALSE)
wc3

wc4 <- wilcox.test(STAI_score ~ sex, data = my_data,
                   exact = FALSE)
wc4

wc5 <- wilcox.test(PHQ15_score ~ sex, data = my_data,
                   exact = FALSE)
wc5


##OTHER SPS DATA


#SPS TYPES DATA

my_data$sps1.1_total = my_data$sps1_1 + my_data$sps2_1

count(my_data, sps1.1_total)


my_data$sps1.2_total = my_data$sps1_2 + my_data$sps2_2

count(my_data, sps1.2_total)


my_data$sps1.3_total = my_data$sps1_3 + my_data$sps2_3

count(my_data, sps1.3_total)


my_data$sps1.4_total = my_data$sps1_4 + my_data$sps2_4

count(my_data, sps1.4_total)


my_data$sps1.5_total = my_data$sps1_5 + my_data$sps2_5

count(my_data, sps1.5_total)


my_data$sps1.6_total = my_data$sps1_6 + my_data$sps2_6

count(my_data, sps1.6_total)


my_data$sps1.7_total = my_data$sps1_7 + my_data$sps2_7

count(my_data, sps1.7_total)


my_data$sps1.8_total = my_data$sps1_8 + my_data$sps2_8

count(my_data, sps1.8_total)


my_data$sps1.9_total = my_data$sps1_9 + my_data$sps2_9

count(my_data, sps1.9_total)


my_data$sps1.10_total = my_data$sps1_10 + my_data$sps2_10

count(my_data, sps1.10_total)


my_data$sps1.11_total = my_data$sps1_11 + my_data$sps2_11

count(my_data, sps1.11_total)

#CHECKING FOR DIFFERENCES IN DOMINANT VS NON-DOMINANT HAND

wc6 <- wilcox.test(my_data$Left_SPS_Amount, my_data$Right_SPS_Amount)
wc6

mean(my_data$Left_SPS_Amount)
sd(my_data$Left_SPS_Amount)


mean(my_data$Right_SPS_Amount)
sd(my_data$Right_SPS_Amount)

#CHECKING FOR DIFFERENCES IN RIGHT VS LEFT HAND

wc7 <- wilcox.test(my_data$sps1_i, my_data$sps2_i)
wc7


mean(my_data$sps1_i)
sd(my_data$sps1_i)


mean(my_data$sps2_i)
sd(my_data$sps2_i)


##FURTHER ANALYSES OF QUESTIONNAIRE DATA


#SPEARMAN'S RHO TESTS FOR MENTAL HEALTH QUESTIONNAIRES

#Depression and PHQ-15

cor.test( ~ PHQ9_score + PHQ15_score,
          data=my_data,
          method = "spearman",
          continuity = FALSE,
          conf.level = 0.95)

#Depression and SHAI

cor.test( ~ PHQ9_score + SHAI_score,
          data=my_data,
          method = "spearman",
          continuity = FALSE,
          conf.level = 0.95)

#Depression and STAI

cor.test( ~ PHQ9_score + STAI_score,
          data=my_data,
          method = "spearman",
          continuity = FALSE,
          conf.level = 0.95)

#PHQ15 and SHAI

cor.test( ~ PHQ15_score + SHAI_score,
          data=my_data,
          method = "spearman",
          continuity = FALSE,
          conf.level = 0.95)

#PHQ15 and STAI

cor.test( ~ PHQ15_score + STAI_score,
          data=my_data,
          method = "spearman",
          continuity = FALSE,
          conf.level = 0.95)

#SHAI and STAI

cor.test( ~ STAI_score + SHAI_score,
          data=my_data,
          method = "spearman",
          continuity = FALSE,
          conf.level = 0.95)


#SPEARMAN'S RHO TESTS FOR SHS and SPQ


#SHS and Depression

cor.test( ~ SHS_score + PHQ9_score,
          data=my_data,
          method = "spearman",
          continuity = FALSE,
          conf.level = 0.95)

#SHS and PHQ-15


cor.test( ~ SHS_score + PHQ15_score,
          data=my_data,
          method = "spearman",
          continuity = FALSE,
          conf.level = 0.95)

#SHS and SHAI

cor.test( ~ SHS_score + SHAI_score,
          data=my_data,
          method = "spearman",
          continuity = FALSE,
          conf.level = 0.95)

#SHS and STAI

cor.test( ~ SHS_score + STAI_score,
          data=my_data,
          method = "spearman",
          continuity = FALSE,
          conf.level = 0.95)


#SPQ and Depression

cor.test( ~ SPQ_score + PHQ9_score,
          data=my_data,
          method = "spearman",
          continuity = FALSE,
          conf.level = 0.95)


#SPQ and PHQ-15

cor.test( ~ SPQ_score + PHQ15_score,
          data=my_data,
          method = "spearman",
          continuity = FALSE,
          conf.level = 0.95)

#SPQ and SHAI

cor.test( ~ SPQ_score + SHAI_score,
          data=my_data,
          method = "spearman",
          continuity = FALSE,
          conf.level = 0.95)

#SPQ and STAI

cor.test( ~ SPQ_score + STAI_score,
          data=my_data,
          method = "spearman",
          continuity = FALSE,
          conf.level = 0.95)

#SHS and SPQ

cor.test( ~ SPQ_score + SHS_score,
          data=my_data,
          method = "spearman",
          continuity = FALSE,
          conf.level = 0.95)


##FURTHER ANALYSES OF SPS INTENSITY AND AMOUNT DATA


#MAX SPS INTENSITY LINEAR REGRESSIONS


#Linear regression between SHAI score and max SPS intensity

ggplot(my_data, aes(x = SHAI_score, y = Max_int)) + geom_point()

ggplot(my_data, aes(x = SHAI_score, y = Max_int)) + geom_point() +
  geom_hline(yintercept = mean(my_data$Max_int), colour = "blue") +
  geom_smooth(method = "lm", se = FALSE)

rcorr(my_data$SHAI_score, my_data$Max_int)

model_max0 <- lm (Max_int ~ 1, data = my_data)
model_max1 <- lm (Max_int ~ SHAI_score, data = my_data)

anova(model_max0, model_max1)

summary(model_max1)

confint(model_max1, 'SHAI_score', level=0.95)


#Linear regression between STAI score and max SPS intensity

ggplot(my_data, aes(x = STAI_score, y = Max_int)) + geom_point()

ggplot(my_data, aes(x = STAI_score, y = Max_int)) + geom_point() +
  geom_hline(yintercept = mean(my_data$Max_int), colour = "blue") +
  geom_smooth(method = "lm", se = FALSE)

rcorr(my_data$STAI_score, my_data$Max_int)

model_max2 <- lm (Max_int ~ 1, data = my_data)
model_max3 <- lm (Max_int ~ STAI_score, data = my_data)

anova(model_max2, model_max3)

summary(model_max3)

confint(model_max3, 'STAI_score', level=0.95)


#Linear regression between max SPS intensity and PHQ-15 score

ggplot(my_data, aes(x = Max_int, y = PHQ15_score)) + geom_point()

ggplot(my_data, aes(x = Max_int, y = PHQ15_score)) + geom_point() +
  geom_hline(yintercept = mean(my_data$PHQ15_score), colour = "blue") +
  geom_smooth(method = "lm", se = FALSE)

rcorr(my_data$Max_int, my_data$PHQ15_score)

model_max4 <- lm (PHQ15_score ~ 1, data = my_data)
model_max5 <- lm (PHQ15_score ~ Max_int, data = my_data)

anova(model_max4, model_max5)

summary(model_max5)

confint(model_max5, 'Max_int', level=0.95)


##TOTAL SPS AMOUNT LINEAR REGRESSIONS


#Linear regression for SHAI score and total SPS amount

ggplot(my_data, aes(x = SHAI_score, y = Total_SPS_Amount)) + geom_point()

ggplot(my_data, aes(x = SHAI_score, y = Total_SPS_Amount)) + geom_point() +
  geom_hline(yintercept = mean(my_data$Total_SPS_Amount), colour = "blue") +
  geom_smooth(method = "lm", se = FALSE)

rcorr(my_data$SHAI_score, my_data$Total_SPS_Amount)

model_ma0 <- lm (Total_SPS_Amount ~ 1, data = my_data)
model_ma1 <- lm (Total_SPS_Amount ~ SHAI_score, data = my_data)

anova(model_ma0, model_ma1)

summary(model_ma1)

confint(model_ma1, 'SHAI_score', level=0.95)


#Linear regression for STAI total score and total SPS amount

ggplot(my_data, aes(x = STAI_score, y = Total_SPS_Amount)) + geom_point()

ggplot(my_data, aes(x = STAI_score, y = Total_SPS_Amount)) + geom_point() +
  geom_hline(yintercept = mean(my_data$Total_SPS_Amount), colour = "blue") +
  geom_smooth(method = "lm", se = FALSE)

rcorr(my_data$STAI_score, my_data$Total_SPS_Amount)

model_ma2 <- lm (Total_SPS_Amount ~ 1, data = my_data)
model_ma3 <- lm (Total_SPS_Amount ~ STAI_score, data = my_data)

anova(model_ma2, model_ma3)

summary(model_ma3)

confint(model_ma3, 'STAI_score', level=0.95)


#Linear regression for total SPS amount and PHQ-15 score

ggplot(my_data, aes(x = Total_SPS_Amount, y = PHQ15_score)) + geom_point()

ggplot(my_data, aes(x = Total_SPS_Amount, y = PHQ15_score)) + geom_point() +
  geom_hline(yintercept = mean(my_data$PHQ15_score), colour = "blue") +
  geom_smooth(method = "lm", se = FALSE)

rcorr(my_data$Total_SPS_Amount, my_data$PHQ15_score)

model_ma4 <- lm (PHQ15_score ~ 1, data = my_data)
model_ma5 <- lm (PHQ15_score ~ Total_SPS_Amount, data = my_data)

anova(model_ma4, model_ma5)

summary(model_ma5)

confint(model_ma5, 'Total_SPS_Amount', level=0.95)


##STAI SUBSCORE LINEAR REGRESSIONS


#Linear regression between STAI-s score and mean SPS overall intensity

ggplot(my_data, aes(x = STAI_s_score, y = mean_intensity)) + geom_point()

ggplot(my_data, aes(x = STAI_s_score, y = mean_intensity)) + geom_point() +
  geom_hline(yintercept = mean(my_data$mean_intensity), colour = "blue") +
  geom_smooth(method = "lm", se = FALSE)

rcorr(my_data$STAI_s_score, my_data$mean_intensity)

model_s0 <- lm (mean_intensity ~ 1, data = my_data)
model_s1 <- lm (mean_intensity ~ STAI_s_score, data = my_data)

anova(model_s0, model_s1)

summary(model_s1)

confint(model_s1, 'STAI_s_score', level=0.95)


#Linear regression for STAI-s score and mean SPS amount

ggplot(my_data, aes(x = STAI_s_score, y = Mean_SPS_Amount)) + geom_point()

ggplot(my_data, aes(x = STAI_s_score, y = Mean_SPS_Amount)) + geom_point() +
  geom_hline(yintercept = mean(my_data$Mean_SPS_Amount), colour = "blue") +
  geom_smooth(method = "lm", se = FALSE)

rcorr(my_data$STAI_s_score, my_data$Mean_SPS_Amount)

model_s2 <- lm (Mean_SPS_Amount ~ 1, data = my_data)
model_s3 <- lm (Mean_SPS_Amount ~ STAI_s_score, data = my_data)

anova(model_s2, model_s3)

summary(model_s3)

confint(model_s3, 'STAI_s_score', level=0.95)


#Linear regression between STAI-t score and mean SPS overall intensity

ggplot(my_data, aes(x = STAI_t_score, y = mean_intensity)) + geom_point()

ggplot(my_data, aes(x = STAI_t_score, y = mean_intensity)) + geom_point() +
  geom_hline(yintercept = mean(my_data$mean_intensity), colour = "blue") +
  geom_smooth(method = "lm", se = FALSE)

rcorr(my_data$STAI_t_score, my_data$mean_intensity)

model_t0 <- lm (mean_intensity ~ 1, data = my_data)
model_t1 <- lm (mean_intensity ~ STAI_t_score, data = my_data)

anova(model_t0, model_t1)

summary(model_t1)

confint(model_t1, 'STAI_t_score', level=0.95)


#Linear regression for STAI-t score and mean SPS amount

ggplot(my_data, aes(x = STAI_t_score, y = Mean_SPS_Amount)) + geom_point()

ggplot(my_data, aes(x = STAI_t_score, y = Mean_SPS_Amount)) + geom_point() +
  geom_hline(yintercept = mean(my_data$Mean_SPS_Amount), colour = "blue") +
  geom_smooth(method = "lm", se = FALSE)

rcorr(my_data$STAI_t_score, my_data$Mean_SPS_Amount)

model_t2 <- lm (Mean_SPS_Amount ~ 1, data = my_data)
model_t3 <- lm (Mean_SPS_Amount ~ STAI_t_score, data = my_data)

anova(model_t2, model_t3)

summary(model_t3)

confint(model_t3, 'STAI_t_score', level=0.95)


##SHS AND SPQ CORRELATIONS


#Correlation between SHS score and mean SPS overall intensity

ggplot(my_data, aes(x = SHS_score, y = mean_intensity)) + geom_point() + geom_smooth(method = "lm", se = FALSE)

rcorr(my_data$SHS_score, my_data$mean_intensity)


#Correlation between SHS score and mean SPS amount

ggplot(my_data, aes(x = SHS_score, y = Mean_SPS_Amount)) + geom_point() + geom_smooth(method = "lm", se = FALSE)

rcorr(my_data$SHS_score, my_data$Mean_SPS_Amount)



#Correlation between SPQ score and mean SPS overall intensity

ggplot(my_data, aes(x = SPQ_score, y = mean_intensity)) + geom_point() + geom_smooth(method = "lm", se = FALSE)

rcorr(my_data$SPQ_score, my_data$mean_intensity)


#Correlation between SPQ score and mean SPS amount

ggplot(my_data, aes(x = SPQ_score, y = Mean_SPS_Amount)) + geom_point() + geom_smooth(method = "lm", se = FALSE)

rcorr(my_data$SPQ_score, my_data$Mean_SPS_Amount)