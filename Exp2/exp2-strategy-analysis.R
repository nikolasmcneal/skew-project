library(plyr)
library(tidyverse)
library(ggpubr)
library(broom)
library(lmerTest)
library(ggeffects)
library(cetcolor)
library(patchwork)


path = "C:/Users/blair/Documents/Research/Nick Project/Data/Exp-2/"

acc <- read.csv(paste0(path,"Accuracy-Condition-Data_Filtered_3.3.2023.csv"))
acc$cond = "acc"
val <- read.csv(paste0(path,"Value-Condition-Data_Filtered_3.3.2023.csv"))
val$cond = "val"
exp2 <- rbind(acc,val)

exp2 <- exp2 %>%
  filter(FullColorVision!="No",)

# Some details about vals
exp2 <- exp2 %>%
  mutate(L1 = read.table(text=arrL)$V1,R1 = read.table(text=arrR)$V1,
         L2 = read.table(text=arrL)$V2,R2 = read.table(text=arrR)$V2,
         L3 = read.table(text=arrL)$V3,R3 = read.table(text=arrR)$V3,
         L4 = read.table(text=arrL)$V4,R4 = read.table(text=arrR)$V4,
         L5 = read.table(text=arrL)$V5,R5 = read.table(text=arrR)$V5,
         L6 = read.table(text=arrL)$V6,R6 = read.table(text=arrR)$V6,
  ) %>%
  group_by(subject,Trial) %>%
  mutate(minL = min(c(L1,L2,L3,L4,L5,L6)),
         minR = min(c(R1,R2,R3,R4,R5,R6)),
         maxR = max(c(R1,R2,R3,R4,R5,R6)),
         maxL = max(c(L1,L2,L3,L4,L5,L6))
  ) %>%
ungroup()

# Code in hypothetical accuracy of choosing by Max Color



exp2 <- exp2 %>%
  mutate(conexp2 = factor(cond, levels = c("acc","val"),
                        labels = c("Accuracy-Based","Value-Based")),
         logRT = log(rt),
         OV = valSum/1000,
          OV_Cat2 = ifelse(valSum>=100,"H",
                          ifelse(valSum<=65,"L","M")),
         OV_Cat2 = factor(OV_Cat2, levels = c("L","M","H"),
                          labels = c("Low","Middle","High")),
         OV_Cat = ntile(OV,3),
         OV_Cat = factor(OV_Cat, labels = c("L","M","H"),
                         levels = c(2,1,3) )
         )


exp2 %>%
  group_by(subject,Trial) %>%
  # Is the highest-value color in the better option?
  mutate(HVC = ifelse( (VD>0 & maxL > maxR) | (VD<0 & maxR > maxL),
                       "Best Color in Better Option",
                       ifelse(maxL == maxR,"Shared Best Color",
                              "Best Color in Worse Option"))
         ) %>%
  ungroup() %>%
  group_by(subject,rainbow,conexp2,aVD,OV_Cat2,HVC) %>%
  summarise(a = mean(accuracy)) %>%
  group_by(subject,rainbow,conexp2,OV_Cat2,HVC) %>%
  summarise(ma = mean(a)) %>%
  ggplot(aes(x = OV_Cat2, y = ma, color = conexp2, shape = conexp2,
             group = conexp2))+
  theme_pubr() +
  facet_wrap(~HVC) +
  stat_summary(position = position_dodge(width=.3)) +
  stat_summary(position = position_dodge(width=.3),
               geom = "line") +
  scale_color_viridis_d(end = .8,option = "C"
  ) +
  labs(title = "Experiment 2",
        y = "Mean Acccuracy",
       x = "Value Category",
       color = "Reward",
       shape = "Reward")

exp2 %>%
  group_by(subject,Trial) %>%
  # Is the highest-value color in the better option?
  mutate(HVC = ifelse( (VD>0 & maxL > maxR) | (VD<0 & maxR > maxL),
                       "Best Color in Better Option",
                       ifelse(maxL == maxR,"Shared Best Color",
                              "Best Color in Worse Option"))
  ) %>%
  ungroup() %>%
  group_by(subject,conexp2,aVD,OV_Cat2,HVC) %>%
  summarise(a = mean(rt/1000)) %>%
  group_by(subject,conexp2,OV_Cat2,HVC) %>%
  summarise(ma = mean(a)) %>%
  ggplot(aes(x = OV_Cat2, y = ma, color = conexp2, shape = conexp2,
             group = conexp2))+
  theme_pubr() +
  facet_wrap(~HVC) +
  stat_summary(position = position_dodge(width=.3)) +
  stat_summary(position = position_dodge(width=.3),
               geom = "line") +
  scale_color_viridis_d(end = .8,option = "C"
  ) +
  labs(y = "Mean Response Time (sec)",
       x = "Value Category",
       color = "Reward",
       shape = "Reward")

# Worst color effects?
exp2 %>%
  group_by(subject,Trial) %>%
  # Is the highest-value color in the better option?
  mutate(HVC = ifelse( (VD>0 & minL < minR) | (VD<0 & minR < minL),
                       "Worst Color in Better Option",
                       ifelse(minL == minR,"Shared Worst Color",
                              "Worst Color in Worse Option"))
  ) %>%
  ungroup() %>%
  group_by(subject,rainbow,conexp2,aVD,OV_Cat2,HVC) %>%
  summarise(a = mean(accuracy)) %>%
  group_by(subject,rainbow,conexp2,OV_Cat2,HVC) %>%
  summarise(ma = mean(a)) %>%
  ggplot(aes(x = OV_Cat2, y = ma, color = conexp2, shape = conexp2,
             group = conexp2))+
  theme_pubr() +
  facet_wrap(~HVC) +
  stat_summary(position = position_dodge(width=.3)) +
  stat_summary(position = position_dodge(width=.3),
               geom = "line") +
  scale_color_viridis_d(end = .8,option = "C"
  ) +
  labs(title = "Experiment 2",
       y = "Mean Acccuracy",
       x = "Value Category",
       color = "Reward",
       shape = "Reward")

# Both types
strat.df <- exp2 %>%
  group_by(subject,Trial) %>%
  # Is the highest-value color in the better option?
  mutate(HVC = factor(ifelse( ((VD<0 & minL < minR) | (VD>0 & minR < minL)) & (maxR==maxL),
                       "Worst Color in Worst Option",
                       ifelse( ((VD>0 & maxL > maxR) | (VD<0 & maxR > maxL)) & (minR==minL), 
                               "Best Color in Best Option",
                                              ifelse(((VD>0 & minL < minR) | (VD>0 & minR < minL)) & (maxR==maxL),
                                                     "Worst Color in Best Option",
                                                     ifelse(((VD<0 & maxL > maxR) | (VD>0 & maxR > maxL)) & (minR==minL),
                                                            "Best Color in Worst Option",
                                                                           ifelse( (minL == minR) & (maxR == maxL), 
                                                                                   "Best+Worst Color in Both Options",
                                                            "Other"
                                                            ))))),
                      levels = c("Best Color in Best Option",
                                 "Worst Color in Best Option",
                                 "Best+Worst Color in Both Options",
                                 "Best Color in Worst Option",
                                 "Worst Color in Worst Option",
                                 "Other")
  ),
# TODO make graph so best/worst color in same facet
                                      
  ) %>%
  ungroup() 

strat.df %>%
  group_by(subject,rainbow,conexp2,aVD,OV_Cat2,HVC) %>%
  summarise(a = mean(accuracy)) %>%
  group_by(subject,rainbow,conexp2,OV_Cat2,HVC) %>%
  summarise(ma = mean(a)) %>%
  ggplot(aes(x = OV_Cat2, y = ma, color = conexp2, shape = conexp2,
             group = conexp2))+
  theme_pubr() +
  facet_wrap(~HVC, nrow=2) +
  stat_summary(position = position_dodge(width=.3)) +
  stat_summary(position = position_dodge(width=.3),
               geom = "line") +
  scale_color_viridis_d(end = .8,option = "C"
  ) +
  labs(title = "Experiment 2",
       y = "Mean Acccuracy",
       x = "Value Category",
       color = "Reward",
       shape = "Reward")

# Model-based analyses

glm <- glmer(data=exp2,
             formula = accuracy ~ aVD + OV * conexp2 +
               (1|subject) +
               (0+aVD|subject) +
               (0+OV|subject),
             family = binomial(link="logit"),
             control = glmerControl(optimizer = "bobyqa"))
summary(glm)


glm2 <- glmer(data=exp2,
              formula = LR ~ VD * OV * conexp2 +
                (1|subject) +
                (0+VD|subject) +
                (0+OV|subject) +
                (0+ VD * OV|subject),
              family = binomial(link="logit"),
              control = glmerControl(optimizer = "bobyqa"))
summary(glm2)

glm3 <- glmer(data=exp2,
              formula = accuracy ~ aVD + OV_Cat * conexp2 +
                (1|subject) +
                (0+aVD|subject) +
                (0+OV_Cat|subject),
              family = binomial(link="logit"),
              control = glmerControl(optimizer = "bobyqa"))
summary(glm3)

ggpredict(glm3,
          terms = c("aVD","OV_Cat","conexp2")) %>%
  plot()+
  theme_pubr()

exp2$valSum2 <- exp2$valSum/1000

lm <- lmer(data=exp2,
           formula = logRT ~ aVD + valSum2 * conexp2 +
             (1|subject) +
             (0+aVD|subject) +
             (0+valSum2|subject),
           control = lmerControl(optimizer = "bobyqa"))
summary(lm)

lm2 <- lmer(data=exp2,
           formula = logRT ~ aVD + OV_Cat * conexp2 +
             (1|subject) +
             (0+aVD|subject) +
             (0+OV_Cat|subject),
           control = lmerControl(optimizer = "bobyqa"))
summary(lm2)

ggpredict(lm,
          terms = c("aVD","OV_Cat","conexp2")) %>%
  plot()+
  theme_pubr()


# Can we get a metric for highest-value colors

exp2 <- exp2 %>%
  group_by(subject,Trial) %>%
  mutate(LeftLV = length(c(L1[L1<4],L2[L2<4],L3[L3<4],
                           L4[L4<4],L5[L5<4],L6[L6<4])),
         RightLV = length(c(R1[R1<4],R2[R2<4],R3[R3<4],
                           R4[L4<4],L5[R5<4],R6[R6<4])),
         LeftHV = length(c(L1[L1>8],L2[L2>8],L3[L3>8],
                           L4[L4>8],L5[L5>8],L6[L6>8])),
         RightHV = length(c(R1[R1>8],R2[R2>8],R3[R3>8],
                            R4[L4>8],L5[R5>8],R6[R6>8])),
         LV_Dif = LeftLV - RightLV,
         HV_Dif = LeftHV - RightHV,
         aLVD = abs(LV_Dif),
         aHVD = abs(HV_Dif),
         LeftOthV = sum(c(L1,L2,L3,L4,L5,L6)) - maxL,
         RightOthV = sum(c(R1,R2,R3,R4,R5,R6)) - maxR,
         maxVD = maxL - maxR,
         othVD = LeftOthV - RightOthV,
         avgL = (sum(c(L1,L2,L3,L4,L5,L6)) - maxL)/5,
         avgR = (sum(c(R1,R2,R3,R4,R5,R6)) - maxR)/5,
         avgVD = avgL-avgR,
         minVD = minL - minR,
         avgL2 = (sum(c(L1,L2,L3,L4,L5,L6)) - maxL - minL)/4,
         avgR2 = (sum(c(R1,R2,R3,R4,R5,R6)) - maxR - minR)/4,
         avgVD2 = avgL2-avgR2
  ) %>%
  ungroup()

exp2 %>%
  group_by(subject,conexp2,aVD,aHVD) %>%
  summarise(a = mean(accuracy)) %>%
  group_by(subject,conexp2,aHVD) %>%
  summarise(ma = mean(a)) %>%
  ggplot(aes(x = aHVD, y = ma, color = conexp2, group = conexp2)) +
  theme_pubr() +
  geom_smooth()

# No real difference in accuracy as a funct
exp2 %>%
  group_by(subject,conexp2,aVD,aLVD) %>%
  summarise(a = mean(accuracy)) %>%
  group_by(subject,conexp2,aLVD) %>%
  summarise(ma = mean(a)) %>%
  ggplot(aes(x = aLVD, y = ma, color = conexp2, group = conexp2)) +
  theme_pubr() +
  geom_smooth()


# Regression of maxVD + othVD

glm4 <- glmer(data=exp2,
              formula = LR ~ maxVD + othVD +
              (1|subject) +
              (0+maxVD|subject) +
              (0+othVD|subject),
              family=binomial(link="logit")
              )
summary(glm4)
# MaxVD = 0.24, p < .001
# othVD = 0.38, p < .001

# Regression of maxVD and avgVD
glm5 <- glmer(data=exp2,
              formula = LR ~ maxVD + avgVD +
                (1|subject) +
                (0+maxVD|subject) +
                (0+avgVD|subject),
              family=binomial(link="logit")
)
summary(glm5)

# Regression of maxVD and minVD avgVD
glm6 <- glmer(data=exp2,
              formula = LR ~ maxVD + minVD + avgVD2 +
                (1|subject) +
                (0+maxVD|subject) +
                (0+minVD|subject) +
                (0+avgVD2|subject),
              family=binomial(link="logit")
)
summary(glm6)

# Regression with ranking
exp2 <- exp2 %>%
  group_by(subject,Trial) %>%
  mutate(rankL1 = sort(c(L1,L2,L3,L4,L5,L6),decreasing=T)[1],
         rankL2 = sort(c(L1,L2,L3,L4,L5,L6),decreasing=T)[2],
         rankL3 = sort(c(L1,L2,L3,L4,L5,L6),decreasing=T)[3],
         rankL4 = sort(c(L1,L2,L3,L4,L5,L6),decreasing=T)[4],
         rankL5 = sort(c(L1,L2,L3,L4,L5,L6),decreasing=T)[5],
         rankL6 = sort(c(L1,L2,L3,L4,L5,L6),decreasing=T)[6],
         rankR1 = sort(c(R1,R2,R3,R4,R5,R6),decreasing=T)[1],
         rankR2 = sort(c(R1,R2,R3,R4,R5,R6),decreasing=T)[2],
         rankR3 = sort(c(R1,R2,R3,R4,R5,R6),decreasing=T)[3],
         rankR4 = sort(c(R1,R2,R3,R4,R5,R6),decreasing=T)[4],
         rankR5 = sort(c(R1,R2,R3,R4,R5,R6),decreasing=T)[5],
         rankR6 = sort(c(R1,R2,R3,R4,R5,R6),decreasing=T)[6],             
         ) %>%
  ungroup()



beta.list <- lapply(unique(exp2$subject),function(x){
  tmp.d = exp2 %>%
    filter(subject == x)
  tmp.m = glm(data = tmp.d,
      formula = LR ~ rankL1 + rankR1 + rankL2 + rankR2 + 
        rankL3 + rankR3 +
        rankL4 + rankR4 + rankL5 + rankR5 + rankL6 + rankR6,
      family=binomial(link="logit"))
  tmp.c = data.frame(coeff = coefficients(tmp.m),
                     s = x)
  tmp.c$name = rownames(tmp.c)
  return(tmp.c)
}) 
beta.df <- rbind.fill(beta.list)

summary(aov(data = beta.df[beta.df$name!="(Intercept)",],
      coeff ~ name))
plot(TukeyHSD(aov(data = beta.df[beta.df$name!="(Intercept)",],
             coeff ~ name)))

summary(glm(data = exp2[exp2$OV_Cat2 == "Low",],
                      formula = LR ~ rankL1 + rankR1 + rankL2 + rankR2 + 
                        rankL3 + rankR3 +
                        rankL4 + rankR4 + rankL5 + rankR5 + rankL6 + rankR6,
                      family=binomial(link="logit")))


# Assess individual variability in VD and OV effects on choice/RT

indv.df <- data.frame(model = character(),
                      coefficient = character(),
                      value = numeric(),
                      subject = character())

for (s in unique(exp2$subject)) {
  tmp.df = exp2 %>% filter(subject == s)
  tmp.glm = glm(data=tmp.df,
                formula = accuracy ~ aVD + valSum,
                family=binomial(link="logit"))
  tmp.lm = lm(data=tmp.df,
               formula = log(rt) ~ aVD + valSum)
  
  s.df = data.frame(model = c(rep("Choice Model",3),rep("RT Model",3)),
                    coefficient = rep(c("Intercept","VD","OV"),2),
                    values = c(as.numeric(coefficients(tmp.glm)),
                               as.numeric(coefficients(tmp.lm))),
                    pvals = c(as.numeric(coef(summary(tmp.glm))[,'Pr(>|z|)']),
                              as.numeric(coef(summary(tmp.lm))[,'Pr(>|t|)'])),
                    subject = rep(s,6)
                    )
  
  indv.df = rbind(indv.df, s.df)
  
}

indv.df <- indv.df %>%
  mutate(psig = factor(ifelse(pvals < .05, 1, 0),
                      levels = c(0,1),
                      labels = c("Non-Sig","Sig"))
  )

ggplot(indv.df,
       aes(x = coefficient, y = values)) +
  theme_pubr() +
  geom_point(aes(color = psig),
             position = position_jitter()) +
  facet_wrap(~model + coefficient, scales = "free",
             ncol = 3) +
  scale_color_viridis_d(begin=.1,
                        end=.9) +
  labs(x = element_blank(),
       y = "Coeff. Estimate",
       color = element_blank()) +
  theme(axis.text.x=element_blank())
  

# Color repetition by OV Cat
rep.df <- exp2 %>%
  mutate(OV_Cat = ifelse(valSum <= quantile(valSum, probs = .25), "1",
                         ifelse(valSum >= quantile(valSum, probs = .75),
                                "2","3")),
         OV_Cat2 = factor(OV_Cat, levels = c("1","2","3"),
                          labels = c("Low","Middle","High"))
  ) %>%
  group_by(subject,Trial,OV_Cat2) %>%
  summarise(f1L = 1 * (1 %in% c(L1,L2,L3,L4,L5,L6) ),
            f2L = 2 * (2 %in% c(L1,L2,L3,L4,L5,L6) ),
            f3L = 3 * (3 %in% c(L1,L2,L3,L4,L5,L6) ),
            f4L = 4 * (4 %in% c(L1,L2,L3,L4,L5,L6) ),
            f5L = 5 * (5 %in% c(L1,L2,L3,L4,L5,L6) ),
            f6L = 6 * (6 %in% c(L1,L2,L3,L4,L5,L6) ),
            f7L = 7 * (7 %in% c(L1,L2,L3,L4,L5,L6) ),
            f8L = 8 * (8 %in% c(L1,L2,L3,L4,L5,L6) ),
            f9L = 9 * (9 %in% c(L1,L2,L3,L4,L5,L6) ),
            f10L = 10 * (10 %in% c(L1,L2,L3,L4,L5,L6) ),
            f11L = 1 * (11 %in% c(L1,L2,L3,L4,L5,L6) ),
            f12L = 1 * (12 %in% c(L1,L2,L3,L4,L5,L6) ),
            f1R = 1 * (1 %in% c(R1,R2,R3,R4,R5,R6) ),
            f2R = 1 * (2 %in% c(R1,R2,R3,R4,R5,R6) ),
            f3R = 1 * (3 %in% c(R1,R2,R3,R4,R5,R6) ),
            f4R = 1 * (4 %in% c(R1,R2,R3,R4,R5,R6) ),
            f5R = 1 * (5 %in% c(R1,R2,R3,R4,R5,R6) ),
            f6R = 1 * (6 %in% c(R1,R2,R3,R4,R5,R6) ),
            f7R = 1 * (7 %in% c(R1,R2,R3,R4,R5,R6) ),
            f8R = 1 * (8 %in% c(R1,R2,R3,R4,R5,R6) ),
            f9R = 1 * (9 %in% c(R1,R2,R3,R4,R5,R6) ),
            f10R = 1 * (10 %in% c(R1,R2,R3,R4,R5,R6) ),
            f11R = 1 * (11 %in% c(R1,R2,R3,R4,R5,R6) ),
            f12R = 1 * (12 %in% c(R1,R2,R3,R4,R5,R6) )) %>%
  group_by(subject,OV_Cat2) %>%
  summarise(one = sum(f1L,f1R),two = sum(f2L,f2R),
            three = sum(f3L,f3R),four = sum(f4L,f4R),
            five = sum(f5L,f5R),six = sum(f6L,f6R),
            seven = sum(f7L,f7R),eight = sum(f8L,f8R),
            nine = sum(f9L,f9R),ten = sum(f10L,f10R),
            eleven = sum(f11L,f11R),twelve = sum(f12L,f12R)) %>%
  pivot_longer(cols = c(one,two,three,four,five,six,seven,eight,nine,ten,eleven,twelve)) %>%
  mutate(name = factor(name,
                       levels = c("one","two","three","four",
                                  "five","six","seven","eight",
                                  "nine","ten","eleven","twelve")))
  
rep.df %>%
  filter(subject == unique(rep.df$subject)[1]) %>%
  ggplot(aes(x = name, y = value, fill = OV_Cat2, group = OV_Cat2)) +
  theme_pubr() +
  geom_bar(stat = "identity",position="dodge") +
  labs(y = "Frequency",
       x = "Value")
  
  
# Look at pattern by OV cat
exp2 %>%
  mutate(OV_Cat = ifelse(valSum <= quantile(valSum, probs = .25), "1",
                         ifelse(valSum >= quantile(valSum, probs = .75),
                                "2","3")),
         OV_Cat2 = factor(OV_Cat, levels = c("1","2","3"),
                          labels = c("Low","Middle","High"))
  ) %>%
  group_by(subject,Trial,OV_Cat2) %>%
  mutate(bestCol = ifelse( (maxL>maxR & VD>0)| (maxL<maxR & VD<0)  ,1,0),
         worstCol = ifelse( (minR>minL & VD<0)| (minR<minL & VD>0) ,1,0),
         bwCol = ifelse( (bestCol > worstCol) | (worstCol > bestCol),1,0)
  ) %>%
  ungroup() %>%
  #group_by(subject,OV_Cat2) %>%
  #summarise(BC = sum(bestCol)/length(bestCol),
  #          WC = sum(worstCol)/length(worstCol),
  #          bwCol = sum(bwCol)/length(bwCol))
  
  #
  summarise(BC = sum(bestCol[maxR!=maxL])/length(bestCol[maxR!=maxL]),
            WC = sum(worstCol[minR!=minL])/length(worstCol[minR!=minL]),
            BW = sum(bwCol[maxR!=maxL | minR!=minL])/length(bwCol[maxR!=maxL | minR!=minL])) #%>%
  #group_by(OV_Cat2) %>%
  #summarise(pBC = mean(BC), pWC = mean(WC),pBW = mean(BW)
  #)
# Pick best color helps Low > High, but only by ~.04 points
# Avoid worst color helps High > Low, but only by ~.09 points
# Using both would help with High by ~.02 points

# Can we characterize how often people use this strategy?  
  
strat.df <- exp2 %>%
  mutate(OV_Cat = ifelse(valSum <= quantile(valSum, probs = .25), "1",
                                        ifelse(valSum >= quantile(valSum, probs = .75),
                                               "2","3")),
                        OV_Cat2 = factor(OV_Cat, levels = c("1","2","3"),
                                         labels = c("Low","Middle","High"))
  ) %>%
  group_by(subject,Trial,OV_Cat2) %>%
  mutate(bestColChoice =  ifelse( (maxL>maxR & LR==1)| (maxL<maxR & LR==0) ,1,0),
         worstColChoice =  ifelse( (minL>minR & LR==1)| (minL<minR & LR==0) ,1,0),
         bwChoice = ifelse( (maxL>maxR & LR==1)| (maxL<maxR & LR==0) | (minL>minR & LR==1)| (minL<minR & LR==0),1,0)
  ) %>% ungroup() 


strat.df %>%
  ggplot(aes(x = aVD, y = bwChoice, color = OV_Cat2, group = OV_Cat2)) +
  theme_pubr() +
  stat_summary(position = position_dodge2(width = 0.4)) +
  geom_smooth(method=glm,) +
  labs(x = "Relative Value Difference",
       y = "P(Color Heuristic)",
       color = "Value Category")

strat.df %>%
  mutate(bw = factor(bwChoice,
                     levels = c(0,1),
                    labels= c("Non-Heuristic","Heuristic")),
         correct = ifelse(accuracy==1,"Correct","Error")) %>%
  ggplot(aes(x = aVD, y = rt/1000, color = bw, group = bw)) +
  theme_pubr() +
  facet_wrap(~correct) +
  scale_color_viridis_d(end = .75,option = "A") +
  stat_summary(position = position_dodge2(width = 0.4)) +
  labs(x = "Relative Value Difference",
       y = "Response Times (ec)",
       color = "Choice")


# Does this strategy use vary by LR
exp2 %>%
  mutate(OV_Cat = ifelse(valSum <= quantile(valSum, probs = .25), "1",
                         ifelse(valSum >= quantile(valSum, probs = .75),
                                "2","3")),
         OV_Cat2 = factor(OV_Cat, levels = c("1","2","3"),
                          labels = c("Low","Middle","High"))
  ) %>%
  group_by(subject,Trial,OV_Cat2) %>%
  mutate(bestColL = ifelse( (maxL>maxR & VD>0) ,1,0),
         bestColR = ifelse((maxL<maxR & VD<0), 1, 0),
         worstColL = ifelse( (minR>minL & VD<0) ,1,0),
         worstColR = ifelse((minR<minL & VD>0), 1, 0),
         bwColL = ifelse( bestColL ==1 | worstColL==1 ,1,0),
         bwColR = ifelse( bestColR ==1 | worstColR==1,1,0),
  ) %>%
  group_by(subject,OV_Cat2) %>%
  summarise(BCL = sum(bestColL)/length(bestColL),
            BCR = sum(bestColR)/length(bestColR),
            WCL = sum(worstColL)/length(worstColL),
            WCR = sum(worstColR)/length(worstColR),
            BWL = sum(bwColL)/length(bwColL),
            BWR = sum(bwColR)/length(bwColR)
            ) %>%
  group_by(OV_Cat2) %>%
  summarise(pBC_L = mean(BCL),pBC_R = mean(BCR),
            pWC_L = mean(WCL),pWC_R = mean(WCR),
            pBW_L = mean(BWL),pBW_R = mean(BWR)
  )

## No,


# Plot behavior again, but filter out trials where there are 
# bestcolor or worst color ties

strat.df <- strat.df %>%
  group_by(subject,Trial) %>%
  mutate(eq = ifelse( (maxL == maxR) & (minR == minL),"Equal","Non-Equal") ) %>%
  ungroup()



eq.choice <- strat.df %>%
  filter(eq != "Equal")  %>%
  ggplot(aes(x = aVD, y = bwChoice, color = OV_Cat2, OV_Cat2)) +
  theme_pubr() +
  stat_summary(position = position_dodge2(width = 0.4)) +
  geom_smooth(method=glm,se=F) +
  labs(x = "Relative Value Difference",
       y = "P(Color Heuristic)",
       color = "Value Category") +
  guides(color = "none")

eq.rt <- strat.df %>%
  filter(eq != "Equal")  %>%
  mutate(bw = factor(bwChoice,
                     levels = c(0,1),
                     labels= c("Non-Heuristic","Heuristic"))
  ) %>%
  ggplot(aes(x = aVD, y = rt/1000, color = bw, group = bw)) +
  theme_pubr() +
  geom_smooth(method=glm,se=F) +
  scale_color_viridis_d(end = .75,option = "A") +
  stat_summary(position = position_dodge2(width = 0.4)) +
  labs(x = "Relative Value Difference",
       y = "Response Times (sec)",
       color = "Choice")

eq.rt2 <- strat.df %>%
  filter(eq != "Equal")  %>%
  mutate(bw = factor(bwChoice,
                     levels = c(0,1),
                     labels= c("Non-Heuristic","Heuristic"))
  ) %>%
  ggplot(aes(x = aVD, y = rt/1000, color = OV_Cat2, shape = bw)) +
  theme_pubr() +
  geom_smooth(method=glm,se=F,aes(linetype = bw)) +
  scale_shape(solid=F) +
  stat_summary(position = position_dodge2(width = 0.4)) +
  labs(x = "Relative Value Difference",
       y = "Response Times (sec)",
       color = "Value Category",
       shape = "Choice",
       linetype="Choice") + 
  guides(
         color = guide_legend(order = 1), 
         shape = guide_legend(order = 2),
         linetype = guide_legend(order = 3))
      
eq.choice + eq.rt2 +  plot_layout(guides = "collect") & 
  theme(legend.position = 'top')













