###########
# R code for ASA K-12 Poster Competition entry:
# "The Lost Art of the Double Steal?"
# Ray Levine
# July 21, 2021
###########

# R Packages used
library(tidyverse)
library(gridExtra) # for ggplot subplots
library(data.table) # to summarize double steals by STATEs

# Jason Osborne's function to set up the bases/outs coding
# Note: make sure to include na = character() when reading in the 
# data csv file.  Otherwise blank cells will be NAs and have to 
# use is.na functionality to create the BASES and STATES
addstatevar <- function(sdata){
  # this function creates STATE and NEW.STATE variables for before and after the event
  # in each record in a retrosheet play-by-play dataset
  sdata %>% mutate(#BASES = 
    # paste(ifelse(is.na(BASE1_RUN_ID) == FALSE, 1, 0),
    #       ifelse(is.na(BASE2_RUN_ID) == FALSE, 1, 0),
    #       ifelse(is.na(BASE3_RUN_ID) == FALSE, 1, 0), sep = ""),
    BASES = 
      paste(ifelse(BASE1_RUN_ID > '', 1, 0),
            ifelse(BASE2_RUN_ID > '', 1, 0),
            ifelse(BASE3_RUN_ID > '', 1, 0),sep=""),
    STATE = paste(BASES , OUTS_CT),
    NRUNNER1=as.numeric(RUN1_DEST_ID ==1|BAT_DEST_ID==1),
    NRUNNER2=as.numeric(RUN1_DEST_ID ==2|RUN2_DEST_ID ==2|BAT_DEST_ID==2),
    NRUNNER3=as.numeric(RUN1_DEST_ID ==3|RUN2_DEST_ID ==3|RUN3_DEST_ID ==3|BAT_DEST_ID==3),
    NOUTS=OUTS_CT + EVENT_OUTS_CT,
    NEW.BASES=paste(NRUNNER1,NRUNNER2,NRUNNER3,sep=""),
    NEW.STATE=paste(NEW.BASES , NOUTS)) -> sdata
  sdata}

# Initialize storage matrices
Runs_Values_Outs_dblst = matrix(0, nrow = 1, ncol = 3)
Runs_Values_Outs_faildblst = matrix(0, nrow = 1, ncol = 3)
Runs_Values_Outs_nodblst = matrix(0, nrow = 1, ncol = 3)
Run_Expectancy_dblst = matrix(0, nrow = 1, ncol = 3)
Run_Expectancy_faildblst = matrix(0, nrow = 1, ncol = 3)
Run_Expectancy_nodblst = matrix(0, nrow = 1, ncol = 3)
RV_dblstl = matrix(0, nrow = 1, ncol = 2)
RV_faildblstl = matrix(0, nrow = 1, ncol = 2)
ndblstl = 0
nfaildblstl = 0
nnodblstl = 0

# read in "boxscore" data obtained from retrosheet
headers <- read_csv("fields.csv")
for(season in 1980:2019){
  print(season)
  data.file <- paste("Data/", "all", season, ".csv", sep="")
  pbpyear <- read_csv(data.file , 
                    col_names = pull(headers , Header),
                    na = character())

# Richard is a poopy head: computing runs for the rest of the inning
# Code based on Chapter 5 of Marchi et al. Analyzing Baseball Data with R, Second Edition
pbpyear %>% 
  mutate(RUNS = AWAY_SCORE_CT + HOME_SCORE_CT,
         HALF.INNING = paste(GAME_ID, INN_CT, BAT_HOME_ID),
         RUNS.SCORED = 
           (BAT_DEST_ID > 3) + (RUN1_DEST_ID > 3) + 
           (RUN2_DEST_ID > 3) + (RUN3_DEST_ID > 3)) ->
  pbpyear

pbpyear %>%
  group_by(HALF.INNING) %>%
  summarize(Outs.Inning = sum(EVENT_OUTS_CT), 
            Runs.Inning = sum(RUNS.SCORED),
            Runs.Start = first(RUNS),
            MAX.RUNS = Runs.Inning + Runs.Start) ->
  half_innings

pbpyear %>%
  inner_join(half_innings, by = "HALF.INNING") %>%
  mutate(RUNS.ROI = MAX.RUNS - RUNS) ->
  pbpyear

pbpyear %>% addstatevar -> pbpyear

pbpyear %>% 
  filter((STATE != NEW.STATE) | (RUNS.SCORED > 0)) ->
  pbpyear

#Richard is a poopy head: To eliminate half innings and partial half innings at the end of a game
pbpyear %>%
  filter(Outs.Inning == 3) -> pbpyearC

# Computing run values
pbpyearC %>% 
  group_by(STATE) %>%
  summarize(Mean = mean(RUNS.ROI)) %>%
  mutate(Outs = substr(STATE, 5, 5)) %>%
  arrange(Outs) -> RUNS

pbpyearC %>%
  left_join(select(RUNS, -Outs), by = "STATE") %>%
  rename(Runs.State = Mean) %>%
  left_join(select(RUNS, -Outs), 
            by = c("NEW.STATE" = "STATE")) %>%
  rename(Runs.New.State = Mean) %>%
  replace_na(list(Runs.New.State = 0)) %>%
  mutate(run_value = Runs.New.State - Runs.State +
           RUNS.SCORED) -> pbpyearC

#Subset the data: double steal
pbpyearC %>% filter((STATE == "110 0" | STATE == "110 1" | STATE == "110 2") &
                      ((RUN1_SB_FL==TRUE) | (RUN1_CS_FL==TRUE)) &  
                      ((RUN2_SB_FL==TRUE) | (RUN2_CS_FL==TRUE))) -> dblstyear

#Subset the data: failed double steal
pbpyearC %>% filter((EVENT_CD == 6 | EVENT_CD == 3) & 
                      (STATE == "110 0" | STATE == "110 1" | STATE == "110 2") &
                      ((RUN1_CS_FL == TRUE) | (RUN2_CS_FL == TRUE))) -> faildblstyear

#Subset the data: no double steal
pbpyearC %>% filter(STATE == "110 0" | STATE == "110 1" | STATE == "110 2") -> nodblstyear
nodblstyear %>% filter(((RUN1_SB_FL==FALSE) & (RUN1_CS_FL==FALSE)) &  
                         ((RUN2_SB_FL==FALSE) & (RUN2_CS_FL==FALSE))) -> nodblstyear

#Storing the run expectancy for each data subset:
# double steals, failed double steals, no double steals
# Used in scatterplot of RE by year (3x3 graphic)
dblstyear %>% 
  group_by(STATE) %>%
  summarize(Mean = mean(RUNS.ROI)) %>%
  mutate(Outs = substr(STATE, 5, 5)) %>%
  arrange(Outs) -> RUNS

RUNS_out <- matrix(RUNS$Mean, 1, 3)
dimnames(RUNS_out)[[2]] <- c("0 outs", "1 out", "2 outs")
dimnames(RUNS_out)[[1]] <- c("110")

Run_Expectancy_dblst <- rbind(Run_Expectancy_dblst , RUNS_out)
###

nodblstyear %>% 
  group_by(STATE) %>%
  summarize(Mean = mean(RUNS.ROI)) %>%
  mutate(Outs = substr(STATE, 5, 5)) %>%
  arrange(Outs) -> RUNS

RUNS_out <- matrix(RUNS$Mean, 1, 3)
dimnames(RUNS_out)[[2]] <- c("0 outs", "1 out", "2 outs")
dimnames(RUNS_out)[[1]] <- c("110")

Run_Expectancy_nodblst <- rbind(Run_Expectancy_nodblst , RUNS_out)
###

faildblstyear %>% 
  group_by(STATE) %>%
  summarize(Mean = mean(RUNS.ROI)) %>%
  mutate(Outs = substr(STATE, 5, 5)) %>%
  arrange(Outs) -> RUNS

RUNS_out <- matrix(RUNS$Mean, 1, 3)
dimnames(RUNS_out)[[2]] <- c("0 outs", "1 out", "2 outs")
dimnames(RUNS_out)[[1]] <- c("110")

Run_Expectancy_faildblst <- rbind(Run_Expectancy_faildblst , RUNS_out)
###

#Storing the run values for each data subset:
# double steals, failed double steals, no double steals
# Used in run value bar chart, color coded by outs
dblstyear %>% group_by(STATE) %>% summarize(Mean = mean(run_value)) -> RVs
Runs_Values_Outs = matrix(RVs$Mean , 1 , 3)
colnames(Runs_Values_Outs) = c("0 outs", "1 out", "2 outs")
row.names(Runs_Values_Outs) = c("110")
Runs_Values_Outs_dblst <- rbind(Runs_Values_Outs_dblst , Runs_Values_Outs)
###
faildblstyear %>% group_by(STATE) %>% summarize(Mean = mean(run_value)) -> RVs
Runs_Values_Outs = matrix(RVs$Mean , 1 , 3)
colnames(Runs_Values_Outs) = c("0 outs", "1 out", "2 outs")
row.names(Runs_Values_Outs) = c("110")
Runs_Values_Outs_faildblst <- rbind(Runs_Values_Outs_faildblst , Runs_Values_Outs)
###
nodblstyear %>% group_by(STATE) %>% summarize(Mean = mean(run_value)) -> RVs
Runs_Values_Outs = matrix(RVs$Mean , 1 , 3)
colnames(Runs_Values_Outs) = c("0 outs", "1 out", "2 outs")
row.names(Runs_Values_Outs) = c("110")
Runs_Values_Outs_nodblst <- rbind(Runs_Values_Outs_nodblst , Runs_Values_Outs)
###

# Store number of double steals, failed double steals, and no double steals
# Used to report number of each event in text
dblstyear %>% group_by(OUTS_CT) %>% summarize(N=n()) %>% select(N) -> N
ndblstl = rbind(ndblstl, t(N))
faildblstyear %>% group_by(OUTS_CT) %>% summarize(N=n()) %>% select(N) -> N
nfaildblstl = rbind(nfaildblstl, t(N))
nodblstyear %>% group_by(OUTS_CT) %>% summarize(N=n()) %>% select(N) -> N
nnodblstl = rbind(nnodblstl, t(N))


# Store table of counts from STATE to NEW.STATE for 
# double steals, failed double steals, and no double steals
# Used to create frequency table of counts by STATE and NEW STATE
dblstyear %>% select(STATE,NEW.STATE) %>% table -> freq
# Create tabulation of number of double steals in a table by STATE and NEW STATE
if(season==1980){table_dblstl = freq} else
  {table_dblstl = rbindlist(list(data.table(freq), 
                  data.table(table_dblstl)))[, sum(N), by = STATE:NEW.STATE]}
# Create tabulation of number of failed double steals in a table by STATE and NEW STATE
faildblstyear %>% select(STATE,NEW.STATE) %>% table -> freq
if(season==1980){table_faildblstl = freq} else
{table_faildblstl = rbindlist(list(data.table(freq), 
                               data.table(table_faildblstl)))[, sum(N), by = STATE:NEW.STATE]}


# Store every run value and out count for each successful double steal
# Used to compute the steal success probability cut point from equation
# (avg success RV)*P + (avg fail RV)*(1-P) = 0
RV_dblstl = rbind(RV_dblstl, cbind(dblstyear$run_value, dblstyear$OUTS_CT))
RV_faildblstl = rbind(RV_faildblstl, cbind(faildblstyear$run_value, faildblstyear$OUTS_CT))
}
#################

## RESULTS

# Remove initial values
Runs_Values_Outs_dblst = Runs_Values_Outs_dblst[-1,]
Runs_Values_Outs_faildblst = Runs_Values_Outs_faildblst[-1,]
Runs_Values_Outs_nodblst = Runs_Values_Outs_nodblst[-1,]
Run_Expectancy_dblst = Run_Expectancy_dblst[-1,]
Run_Expectancy_faildblst= Run_Expectancy_faildblst[-1,]
Run_Expectancy_nodblst = Run_Expectancy_nodblst[-1,]
RV_dblstl = RV_dblstl[-1,]
RV_faildblstl = RV_faildblstl[-1,]
ndblstl = ndblstl[-1,]
nfaildblstl = nfaildblstl[-1,]
nnodblstl = nnodblstl[-1,]

# Summary tables for Run Expectancies: means and std devs by outs over seasons
REresults_means = rbind(colMeans(Run_Expectancy_dblst), 
      colMeans(Run_Expectancy_faildblst),
      colMeans(Run_Expectancy_nodblst))
row.names(REresults_means) = c("Double Steal", "Fail", "None")
REresults_means
#               0 outs     1 out      2 outs
#Double Steal 1.9348789 1.2282417 0.549116425
#Fail         0.6177299 0.1741127 0.004166667
#None         1.4896565 0.9169395 0.437917600

REresults_sd = rbind(apply(Run_Expectancy_dblst, 2, sd),
                     apply(Run_Expectancy_faildblst, 2, sd),
                     apply(Run_Expectancy_nodblst, 2, sd))
row.names(REresults_sd) = c("Double Steal", "Fail", "None")
REresults_sd
#               0 outs      1 out     2 outs
#Double Steal 0.43118637 0.18478099 0.19351914
#Fail         0.38852126 0.08402420 0.02635231
#None         0.06519501 0.04560294 0.02575425

# Look at frequency tables of double steals and failed double steals
# by STATE and NEW STATE
table_dblstl
table_faildblstl

# Scatterplot smooths of run expectancies over seasons
# 3x3: Outs by double steal (success, fail, none)
# Note that there seems to be only one year (2002) that had RE>0 for failed double steals and 2 outs
plotframe = data.frame(Year=1980:2019, RE = Run_Expectancy_dblst[,1])
p01 = ggplot(plotframe, aes(Year, RE)) +
  #geom_point(colour="Turquoise4") +
  geom_point(aes(size = ndblstl[,1]), colour="Turquoise4") +
  scale_size_continuous(range = c(1, 5)) +
  geom_smooth(colour="Pink") +
  labs(x="Year", y="Run Expectancy") +
  ggtitle("Double Steal, 0 outs") +
  theme(legend.position = "none")
plotframe = data.frame(Year=1980:2019, RE = Run_Expectancy_faildblst[,1])
p02 = ggplot(plotframe, aes(Year, RE)) +
  #geom_point(colour="Turquoise4") +
  geom_point(aes(size = nfaildblstl[,1]), colour="Turquoise4") +
  scale_size_continuous(range = c(1, 5)) +
  geom_smooth(colour="Pink") +
  labs(x="Year", y="Run Expectancy") +
  ggtitle("Failed Double Steal, 0 outs") +
  theme(legend.position = "none")
plotframe = data.frame(Year=1980:2019, RE = Run_Expectancy_nodblst[,1])
p03 = ggplot(plotframe, aes(Year, RE)) +
  #geom_point(colour="Turquoise4") +
  geom_point(aes(size = nnodblstl[,1]), colour="Turquoise4") +
  scale_size_continuous(range = c(1, 5)) +
  geom_smooth(colour="Pink") +
  labs(x="Year", y="Run Expectancy") +
  ggtitle("No Double Steal, 0 outs") +
  theme(legend.position = "none")
#grid.arrange(p01, p02, p03, ncol = 1, nrow = 3)
plotframe = data.frame(Year=1980:2019, RE = Run_Expectancy_dblst[,2])
p11 = ggplot(plotframe, aes(Year, RE)) +
  #geom_point(colour="Turquoise4") +
  geom_point(aes(size = ndblstl[,2]), colour="Turquoise4") +
  scale_size_continuous(range = c(1, 5)) +
  geom_smooth(colour="Pink") +
  labs(x="Year", y="Run Expectancy") +
  ggtitle("Double Steal, 1 out") +
  theme(legend.position = "none")
plotframe = data.frame(Year=1980:2019, RE = Run_Expectancy_faildblst[,2])
p12 = ggplot(plotframe, aes(Year, RE)) +
  #geom_point(colour="Turquoise4") +
  geom_point(aes(size = nfaildblstl[,2]), colour="Turquoise4") +
  scale_size_continuous(range = c(1, 5)) +
  geom_smooth(colour="Pink") +
  labs(x="Year", y="Run Expectancy") +
  ggtitle("Failed Double Steal, 1 out") +
  theme(legend.position = "none")
plotframe = data.frame(Year=1980:2019, RE = Run_Expectancy_nodblst[,2])
p13 = ggplot(plotframe, aes(Year, RE)) +
  #geom_point(colour="Turquoise4") +
  geom_point(aes(size = nnodblstl[,2]), colour="Turquoise4") +
  scale_size_continuous(range = c(1, 5)) +
  geom_smooth(colour="Pink") +
  labs(x="Year", y="Run Expectancy") +
  ggtitle("No Double Steal, 1 out") +
  theme(legend.position = "none")
plotframe = data.frame(Year=1980:2019, RE = Run_Expectancy_dblst[,3])
p21 = ggplot(plotframe, aes(Year, RE)) +
  #geom_point(colour="Turquoise4") +
  geom_point(aes(size = ndblstl[,3]), colour="Turquoise4") +
  scale_size_continuous(range = c(1, 5)) +
  geom_smooth(colour="Pink") +
  labs(x="Year", y="Run Expectancy") +
  ggtitle("Double Steal, 2 outs") +
  theme(legend.position = "none")
plotframe = data.frame(Year=1980:2019, RE = Run_Expectancy_faildblst[,3])
p22 = ggplot(plotframe, aes(Year, RE)) +
  #geom_point(colour="Turquoise4") +
  geom_point(aes(size = nfaildblstl[,3]), colour="Turquoise4") +
  scale_size_continuous(range = c(1, 5)) +
  geom_smooth(colour="Pink") +
  labs(x="Year", y="Run Expectancy") +
  ggtitle("Failed Double Steal, 2 outs") +
  theme(legend.position = "none")
plotframe = data.frame(Year=1980:2019, RE = Run_Expectancy_nodblst[,3])
p23 = ggplot(plotframe, aes(Year, RE)) +
  #geom_point(colour="Turquoise4") +
  geom_point(aes(size = nnodblstl[,3]), colour="Turquoise4") +
  scale_size_continuous(range = c(1, 5)) +
  geom_smooth(colour="Pink") +
  labs(x="Year", y="Run Expectancy") +
  ggtitle("No Double Steal, 2 outs") +
  theme(legend.position = "none")
grid.arrange(p01, p02, p03, 
             p11, p12, p13, 
             p21, p22, p23,
             ncol = 3, nrow = 3)



# Bar chart of run values for all seasons
# Color coded by success/fail (two colors)
ndblstl = length(dblstyear$run_value)
nfaildblstl = length(faildblstyear$run_value)
#rbind(cbind(dblstyear$run_value, rep(1, ndblstl)),
#      cbind(faildblstyear$run_value, rep(2, nfaildblstl)))
RV = c(dblstyear$run_value, faildblstyear$run_value)
successind = c(rep(1, ndblstl), rep(2, nfaildblstl))
plotframeRV = data.frame(RV, successind)
ggplot(plotframeRV, aes(RV, fill=factor(successind))) +
  geom_histogram() +
  scale_fill_manual(name = "Double Steal",
                    values = c("Turquoise4", "gray70"),
                    labels = c("Success", "Fail"))

# Color coded by success/fail and outs (six colors)
RV = c(RV_dblstl[,1], RV_faildblstl[,1])
successoutsind = c(RV_dblstl[,2]+1 , RV_faildblstl[,2]+4)
plotframeRV = data.frame(RV, successoutsind)
ggplot(plotframeRV, aes(RV, fill=factor(successoutsind))) +
  geom_histogram() +
  labs(x="Run Values, 1980-2019", y="Count") +
  scale_fill_manual(name = "Double Steal",
                    values = c("Turquoise1", "Turquoise4", "blue",
                               "gray70", "gray40", "black"),
                    labels = c("Success, 0 outs", "Success, 1 out", "Success, 2 outs",
                               "Fail, 0 outs", "Fail, 1 out", "Fail, 2 outs"))
