# Script for looking at how the value of the information (AOI)
# Subjects attend to is affected by the OV of the trial's options
# Thus, we can detect if people's attention is influence by extreme
# and/or discriminating information

# Blair R K Shevlin, May 2023

library(tidyverse)
library(ggpubr)


#Load data
data_directory <- "~/Research/Nick Project/skew-project/Prague/"
load(paste0(data_directory,"optOut_combined_fixLevel_filtered.RData"))


fcData <- allDat4_filtered %>%
  filter(Phase==2)
rm(allDat4_filtered)

fcData.fixval <- fcData %>%
  mutate(roi_val = ifelse(ROI2 == "RsquareBL",val2BL,
                          ifelse(ROI2 == "LsquareBL",val1BL,
                                 ifelse(ROI2 == "RsquareBM",val2BM,
                                        ifelse(ROI2 == "LsquareBM",val1BM,
                                               ifelse(ROI2 == "RsquareBR",val2BR,
                                                      ifelse(ROI2 == "LsquareBR",val1BR,
                                                             ifelse(ROI2 == "RsquareTL",val2TL,
                                                                    ifelse(ROI2 == "LsquareTL",val2TL,
                                                                           ifelse(ROI2 == "RsquareTM",val2TM,
                                                                                  ifelse(ROI2 == "LsquareTM",val1TM,
                                                                                         ifelse(ROI2 == "RsquareTR",val2TR,
                                                                                                ifelse(ROI2=="LsquareTR",val1TR,NA))))))
                                                             ) 
                                                      )
                                               )
                                        )
                                 )
                          )
         ) %>%
  filter(roi_val > 0)

# Function for determine the relative rank of a value being
# attended to in the context of the values of the trial
val2rank <- function(vals,ROI) {
  locations <- c("LsquareTL","LsquareTM","LsquareTR",
                 "LsquareBL","LsquareBM","LsquareBR",
                 "RsquareTL","RsquareTM","RsquareTR",
                 "RsquareBL","RsquareBM","RsquareBR")
  val_df <- data.frame(locations = locations,
                       val = vals)
  
  # Lower rank = lower val
  val_df$ranks = rank(val_df$val) #,
                      #ties.method = c("min"))
  
  ROI_filt = ROI[ROI %in% locations]
  
  
  val_df$ROI_rank = ifelse(val_df$locations == ROI_filt,
                            val_df$ranks[val_df$locations == ROI_filt],
                           0
  )
  
  return(val_df$ROI_rank[val_df$ROI_rank>0])
}


val2z <- function(vals,ROI) {
  locations <- c("LsquareTL","LsquareTM","LsquareTR",
                 "LsquareBL","LsquareBM","LsquareBR",
                 "RsquareTL","RsquareTM","RsquareTR",
                 "RsquareBL","RsquareBM","RsquareBR")
  val_df <- data.frame(locations = locations,
                       val = vals)
  
  # Lower rank = lower val
  val_df$zscore = scale(val_df$val) 
  
  ROI_filt = ROI[ROI %in% locations]
  
  
  val_df$zscore = ifelse(val_df$locations == ROI_filt,
                           val_df$zscore[val_df$locations == ROI_filt],
                           NA
  )
  
  return(val_df$zscore[is.na(val_df$zscore)==F])
}

fcData.rankval <- fcData.fixval %>%
  group_by(SubjectNumber,Trial,fixnum) %>%
  mutate(roi_rank = val2rank(c(val1TL,val1TM,val1TR,
                               val1BL,val1BM,val1BR,
                               val2TL,val2TM,val2TR,
                               val2BL,val2BM,val2BR),
                             ROI2),
         roi_z = val2z(c(val1TL,val1TM,val1TR,
                            val1BL,val1BM,val1BR,
                            val2TL,val2TM,val2TR,
                            val2BL,val2BM,val2BR),
                          ROI2),
  )

fcData.rankval <- fcData.rankval %>%
  mutate(trialValence = factor(trialValence,
                               levels = c("Negative",
                                          "Mixed",
                                          "Positive")))

ggplot(fcData.rankval,
       aes(x = OV, y = roi_rank, color = trialValence)) +
  theme_pubr(base_size = 14) +
  scale_color_manual(values = RColorBrewer::brewer.pal(3, "Set1")) +
  stat_summary() +
  labs(x = "Overall Value",
       y = "Relative Rank of AOI\nLower rank denotes lower value",
       color = "Trial Valence")

# version with z-score instead 
ggplot(fcData.rankval,
       aes(x = OV, y = roi_z, color = trialValence)) +
  theme_pubr(base_size = 14) +
  scale_color_manual(values = RColorBrewer::brewer.pal(3, "Set1")) +
  stat_summary() +
  labs(x = "Overall Value",
       y = "Z Score of AOI",
       color = "Trial Valence")

# Add skew information

skew.df <- read.csv(paste0(data_directory,"Prague_exp_data_skew.csv")) %>%
  select(c(SubjectNumber,skew)) %>% distinct() %>%
  mutate(skew_z = scale(skew))

fcData.skew <- merge(fcData.rankval, skew.df, by = "SubjectNumber")

# Make sure no missing skew data
fcData.skew[is.na(fcData.skew$skew),] # This should be empty


fcData.skew <- fcData.skew %>%
  mutate(skew_bin = ifelse(skew <= quantile(skew,probs = .25), "Low Skew",
                           ifelse(skew >= quantile(skew,probs = .75),
                                  "High Skew", "Moderate Skew")))


m0 <- lmer(data = fcData.skew,
           formula = roi_rank ~ OV * skew + abs(OV) +
             (1+OV+abs(OV)|SubjectNumber),
           control = lmerControl(optimizer='bobyqa'))
summary(m0)

m1 <- lmer(data = fcData.skew,
           formula = roi_rank ~ OV * skew * abs(OV) +
             (1|SubjectNumber) + (0+OV|SubjectNumber) + (0+abs(OV)|SubjectNumber),
           control = lmerControl(optimizer='bobyqa'))
summary(m1)


# Let's try with z-scored skew
m0z <- lmer(data = fcData.skew,
           formula = roi_rank ~ OV * skew_z + abs(OV) +
             (1+OV+abs(OV)|SubjectNumber),
           control = lmerControl(optimizer='bobyqa'))
summary(m0z)

m1z <- lmer(data = fcData.skew,
           formula = roi_rank ~ OV * skew_z * abs(OV) +
             (1|SubjectNumber) + (0+OV|SubjectNumber) + (0+abs(OV)|SubjectNumber),
           control = lmerControl(optimizer='bobyqa'))
summary(m1z)
