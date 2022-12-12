plays <- read.csv("~/Downloads/plays_v10.csv")
attach(plays)

library(randomForest)
library(tree)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(GGally)
library(ggfortify)

#### DATASET DESCRIPTION ####

plays=plays[,c(5,9,11,18,19,20,21,22,23,24,27,28)]
attach(plays)

# yardline_100
summary(yardline_100)
hist(yardline_100) 
boxplot(yardline_100)

# game_seconds_remaining 
summary(game_seconds_remaining)
hist(game_seconds_remaining) 
boxplot(game_seconds_remaining)

# down
plays$down=as.factor(plays$down)
summary(down)
barplot(table(plays$down))
downs=table(plays$down)
downs[1]/(downs[1]+downs[2]+downs[3])
downs[2]/(downs[1]+downs[2]+downs[3])
downs[3]/(downs[1]+downs[2]+downs[3])
boxplot(down)

# shotgun 
plays$shotgun=as.factor(plays$shotgun)
summary(shotgun)
barplot(table(plays$shotgun))
shotguns=table(plays$shotgun)
shotguns[1]/(shotguns[1]+shotguns[2])
shotguns[2]/(shotguns[1]+shotguns[2])
boxplot(shotgun)

# ydstogo
summary(ydstogo)
sd(ydstogo)
hist(ydstogo, breaks=35, col="lavenderblush3") 
boxplot(ydstogo)

# posteam 
plays$posteam=as.factor(plays$posteam)
summary(posteam)
barplot(table(plays$posteam), ylim=c(0,1200), col="lavenderblush3")

# qtr 
plays$qtr=as.factor(plays$qtr)
summary(qtr)
barplot(table(plays$qtr))
qtrs=table(plays$qtr)
qtrs[1]/(qtrs[1]+qtrs[2]+qtrs[3]+qtrs[4])
qtrs[2]/(qtrs[1]+qtrs[2]+qtrs[3]+qtrs[4])
qtrs[3]/(qtrs[1]+qtrs[2]+qtrs[3]+qtrs[4])
qtrs[4]/(qtrs[1]+qtrs[2]+qtrs[3]+qtrs[4])
boxplot(qtr)

# posteam_timeouts 
plays$posteam_timeouts_remaining=as.factor(plays$posteam_timeouts_remaining)
summary(posteam_timeouts_remaining)
barplot(table(plays$posteam_timeouts_remaining))
posteam_timeouts_remainings=table(plays$posteam_timeouts_remaining)
posteam_timeouts_remainings[1]/(posteam_timeouts_remainings[1]+posteam_timeouts_remainings[2]+posteam_timeouts_remainings[3]+posteam_timeouts_remainings[4])
posteam_timeouts_remainings[2]/(posteam_timeouts_remainings[1]+posteam_timeouts_remainings[2]+posteam_timeouts_remainings[3]+posteam_timeouts_remainings[4])
posteam_timeouts_remainings[3]/(posteam_timeouts_remainings[1]+posteam_timeouts_remainings[2]+posteam_timeouts_remainings[3]+posteam_timeouts_remainings[4])
posteam_timeouts_remainings[4]/(posteam_timeouts_remainings[1]+posteam_timeouts_remainings[2]+posteam_timeouts_remainings[3]+posteam_timeouts_remainings[4])
boxplot(posteam_timeouts_remaining)

# defteam_timeouts  
plays$defteam_timeouts_remaining=as.factor(plays$defteam_timeouts_remaining)
summary(defteam_timeouts_remaining)
barplot(table(plays$defteam_timeouts_remaining))
defteam_timeouts_remainings=table(plays$defteam_timeouts_remaining)
defteam_timeouts_remainings[1]/(defteam_timeouts_remainings[1]+defteam_timeouts_remainings[2]+defteam_timeouts_remainings[3]+defteam_timeouts_remainings[4])
defteam_timeouts_remainings[2]/(defteam_timeouts_remainings[1]+defteam_timeouts_remainings[2]+defteam_timeouts_remainings[3]+defteam_timeouts_remainings[4])
defteam_timeouts_remainings[3]/(defteam_timeouts_remainings[1]+defteam_timeouts_remainings[2]+defteam_timeouts_remainings[3]+defteam_timeouts_remainings[4])
defteam_timeouts_remainings[4]/(defteam_timeouts_remainings[1]+defteam_timeouts_remainings[2]+defteam_timeouts_remainings[3]+defteam_timeouts_remainings[4])
boxplot(defteam_timeouts_remaining)

# score_differential
summary(score_differential)
hist(score_differential, bins=5) 
boxplot(score_differential)

##### VERIABLE SELECTION #####

plays <- read.csv("~/Downloads/plays_v10.csv")
attach(plays)

### Classification Tree ###
# Model with CP=0.0000001
classified_tree=rpart(play_type~posteam+posteam_type+defteam+yardline_100+quarter_seconds_remaining+half_seconds_remaining+game_seconds_remaining+game_half+quarter_end+drive+qtr+down+ydstogo+shotgun+no_huddle+posteam_timeouts_remaining+defteam_timeouts_remaining+posteam_score+defteam_score+score_differential,control=rpart.control(cp=0.0000001), na.action=na.omit)
# Find optimal CP
opt_cp=classified_tree$cptable[which.min(classified_tree$cptable[,"xerror"]),"CP"]

### Classification Forest ###
# Model with optimal CP
optimal_classified_forest=randomForest(play_type~posteam+posteam_type+defteam+yardline_100+quarter_seconds_remaining+half_seconds_remaining+game_seconds_remaining+game_half+quarter_end+drive+qtr+down+ydstogo+shotgun+no_huddle+posteam_timeouts_remaining+defteam_timeouts_remaining+posteam_score+defteam_score+score_differential,cp=opt_cp, na.action=na.omit, ntree=1000, data=plays, importance=TRUE, do.trace=50)
importance(optimal_classified_forest)
varImpPlot(optimal_classified_forest)

vars <- c(
  'shotgun',
  'down',
  'ydstogo',
  'game_seconds_remaining',
  'half_seconds_remaining',
  'score_differential',
  'quarter_seconds_remaining',
  'drive' ,
  'defteam_score',
  'posteam_score',
  'posteam_timeouts_remaining',  
  'qtr', 
  'yardline_100',
  'game_half', 
  'defteam_timeouts_remaining',
  'posteam',  
  'quarter_end',
  'defteam',
  'no_huddle',  
  'posteam_type')

mse<-c(importance(optimal_classified_forest)[14],
       importance(optimal_classified_forest)[12],
       importance(optimal_classified_forest)[13],
       importance(optimal_classified_forest)[7],
       importance(optimal_classified_forest)[6],
       importance(optimal_classified_forest)[20],
       importance(optimal_classified_forest)[5],
       importance(optimal_classified_forest)[10],
       importance(optimal_classified_forest)[19],
       importance(optimal_classified_forest)[18],
       importance(optimal_classified_forest)[16],
       importance(optimal_classified_forest)[11],
       importance(optimal_classified_forest)[4],
       importance(optimal_classified_forest)[8],
       importance(optimal_classified_forest)[17],
       importance(optimal_classified_forest)[1],
       importance(optimal_classified_forest)[9],
       importance(optimal_classified_forest)[3],
       importance(optimal_classified_forest)[15],
       importance(optimal_classified_forest)[2])

np<-c(importance(optimal_classified_forest)[34],
      importance(optimal_classified_forest)[32],
      importance(optimal_classified_forest)[33],
      importance(optimal_classified_forest)[27],
      importance(optimal_classified_forest)[26],
      importance(optimal_classified_forest)[40],
      importance(optimal_classified_forest)[25],
      importance(optimal_classified_forest)[30],
      importance(optimal_classified_forest)[39],
      importance(optimal_classified_forest)[38],
      importance(optimal_classified_forest)[36],
      importance(optimal_classified_forest)[31],
      importance(optimal_classified_forest)[24],
      importance(optimal_classified_forest)[28],
      importance(optimal_classified_forest)[37],
      importance(optimal_classified_forest)[21],
      importance(optimal_classified_forest)[29],
      importance(optimal_classified_forest)[23],
      importance(optimal_classified_forest)[35],
      importance(optimal_classified_forest)[22])

vars <- rev(vars)
mse <- rev(mse)
np <- rev(np)
imp <- data.frame(IncMSE=mse,IncNodePurity=np)
imp <- as.matrix(imp, decreasing = FALSE)
imp <- t(imp)
colnames(imp) <- vars
color.names=c("lavenderblush4","lavenderblush2")

bpimp <- barplot(heigh=imp, 
                 width=2, 
                 beside=TRUE, 
                 xlim=c(0,250),
                 xlab="Variable",
                 #ylab="Importance",
                 col=color.names,
                 legend.text=TRUE,
                 horiz=TRUE,
                 las=1)

plays <- read.csv("~/Downloads/plays_v10.csv")
attach(plays)

### Collinearity and Principal Component Analysis ###
quant_vars=plays[,c(9,11,12,13,14,16,18,19,20,21,22,23,24,27)] 
ggpairs(quant_vars) 
pca=prcomp(quant_vars, scale=TRUE)
autoplot(pca, data=quant_vars, colour="lavenderblush3", loadings=TRUE, loadings.label=TRUE,  loadings.colour = "lavenderblush4", loadings.label.colour="lavenderblush4")


##### BOOSTING MODEL #####

plays <- read.csv("~/Downloads/plays_v10.csv")
attach(plays)

plays$posteam=as.factor(plays$posteam)
attach(plays)

total_accuracies <- list()
pass_accuracies <- list()
run_accuracies <- list()

teams_list <- list("BAL","CIN","CLE","KC","TB","ATL","CAR","JAX","DEN","LAC","OAK","NE","NO")

### Model ###
library(gbm)
set.seed (1)
boosted=gbm(play_type~posteam+yardline_100+quarter_seconds_remaining+down+qtr+ydstogo+shotgun+no_huddle+posteam_timeouts_remaining+defteam_timeouts_remaining+score_differential,data=plays, distribution="bernoulli", n.trees=10000, interaction.depth=4)
summary(boosted) 

### Prediction Accuracy ###

# Load 2018 DataFrame
plays_2018 <- read.csv("~/Downloads/plays.csv")
plays_2018 = plays_2018[c(8547:9459),c(5,9,11,18,19,20,21,22,23,24,27,28)]

# Predict probabilities of 2018 data
predicted=predict(boosted, plays_2018, n.trees=10000, type="response")


team_total_accuracies_0.5 <- list()
team_pass_accuracies_0.5 <- list()
team_run_accuracies_0.5 <- list()

# Loop: If P>0.5; pass, if P<0.5; run.
playtypes <- rep()
for (i in 1:length(predicted)){
  playtype=ifelse(predicted[i]>0.5,5,10)
  playtypes <- append(playtypes, playtype, after=length(playtypes))
}
plays_2018["Predboost"]=playtypes
attach(plays_2018)

# Compare predictions with observed play type
diffs <- rep()
for (i in 1:length(predicted)){
  diff=ifelse(Predboost[i]-play_type[i]==10,"correct run",
              ifelse(Predboost[i]-play_type[i]==4,"correct pass",
                     ifelse(Predboost[i]-play_type[i]==9,"incorrect run",
                            ifelse(Predboost[i]-play_type[i]==5,"incorrect pass"))))
  diffs <- append(diffs, diff, after=length(diffs))
}
plays_2018["Predboost_acc"]=diffs
attach(plays_2018)

# Measure accuracy; percent of play types predicted correctly
plays_2018$Predboost_acc=as.factor(plays_2018$Predboost_acc)
PFA=table(Predboost_acc)

total_accuracy=(PFA[1]+PFA[2])/(PFA[1]+PFA[2]+PFA[3]+PFA[4])
pass_accuracy=PFA[1]/(PFA[1]+PFA[3])
run_accuracy=PFA[2]/(PFA[2]+PFA[4])

total_accuracy=as.numeric(total_accuracy)
pass_accuracy=as.numeric(pass_accuracy)
run_accuracy=as.numeric(run_accuracy)

total_accuracies<-append(total_accuracies, total_accuracy, after=length(total_accuracies))
pass_accuracies<-append(pass_accuracies, pass_accuracy, after=length(pass_accuracies))
run_accuracies<-append(run_accuracies, run_accuracy, after=length(run_accuracies))

# Team accuracy 
teams <- rep()
for (i in 1:length(teams_list)){
  for (j in 1:length(posteam)){
    team=ifelse(teams_list[i]==posteam[j],j,NA)
    teams <- append(teams, team, after=length(teams))}
  
  plays_2018_2 = plays_2018[c(na.omit(teams)),]
  
  plays_2018_2$Predboost_acc=as.factor(plays_2018_2$Predboost_acc)
  PFA=table(plays_2018_2$Predboost_acc)
  
  total_accuracy=(PFA[1]+PFA[2])/(PFA[1]+PFA[2]+PFA[3]+PFA[4])
  pass_accuracy=PFA[1]/(PFA[1]+PFA[3])
  run_accuracy=PFA[2]/(PFA[2]+PFA[4])
  
  total_accuracy=as.numeric(total_accuracy)
  pass_accuracy=as.numeric(pass_accuracy)
  run_accuracy=as.numeric(run_accuracy)
  
  team_total_accuracies_0.5<-append(team_total_accuracies_0.5, total_accuracy, after=length(team_total_accuracies_0.5))
  team_pass_accuracies_0.5<-append(team_pass_accuracies_0.5, pass_accuracy, after=length(team_pass_accuracies_0.5))
  team_run_accuracies_0.5<-append(team_run_accuracies_0.5, run_accuracy, after=length(team_run_accuracies_0.5))
}
# Load 2018 DataFrame
plays_2018 <- read.csv("~/Downloads/plays.csv")
plays_2018 = plays_2018[c(8547:9459),c(5,9,11,18,19,20,21,22,23,24,27,28)]
attach(plays_2018)

team_total_accuracies_0.6 <- list()
team_pass_accuracies_0.6 <- list()
team_run_accuracies_0.6 <- list()

# Loop: If P>0.6; pass, if P<0.4; run.
playtypes <- rep()
for (i in 1:length(predicted)){
  playtype=ifelse(predicted[i]>0.6,5,
                  ifelse(predicted[i]<0.4,10,2))
  playtypes <- append(playtypes, playtype, after=length(playtypes))}
plays_2018["Predboost"]=playtypes
attach(plays_2018)

# Eliminate rows that have a value of 2 in Predboost
twos <- rep()
for (i in 1:length(predicted)){
  two=ifelse(Predboost[i]==2,i,NA)
  twos <- append(twos, two, after=length(twos))}
# Drop all NA values from list
twos
na.omit(twos)
# Drop all row indexes from DataFrame
plays_2018 = plays_2018[-c(na.omit(twos)),]
attach(plays_2018)

# Compare predictions with observed play type
diffs <- rep()
for (i in 1:length(play_type)){
  diff=ifelse(Predboost[i]-play_type[i]==10,"correct run",
              ifelse(Predboost[i]-play_type[i]==4,"correct pass",
                     ifelse(Predboost[i]-play_type[i]==9,"incorrect run",
                            ifelse(Predboost[i]-play_type[i]==5,"incorrect pass"))))
  diffs <- append(diffs, diff, after=length(diffs))
}
plays_2018["Predboost_acc"]=diffs
attach(plays_2018)
View(plays_2018)

# Measure accuracy; percent of play types predicted correctly
plays_2018$Predboost_acc=as.factor(plays_2018$Predboost_acc)
PFA=table(Predboost_acc)

total_accuracy=(PFA[1]+PFA[2])/(PFA[1]+PFA[2]+PFA[3]+PFA[4])
pass_accuracy=PFA[1]/(PFA[1]+PFA[3])
run_accuracy=PFA[2]/(PFA[2]+PFA[4])

total_accuracy=as.numeric(total_accuracy)
pass_accuracy=as.numeric(pass_accuracy)
run_accuracy=as.numeric(run_accuracy)

total_accuracies<-append(total_accuracies, total_accuracy, after=length(total_accuracies))
pass_accuracies<-append(pass_accuracies, pass_accuracy, after=length(pass_accuracies))
run_accuracies<-append(run_accuracies, run_accuracy, after=length(run_accuracies))

# Team accuracy 
teams <- rep()
for (i in 1:length(teams_list)){
  for (j in 1:length(posteam)){
    team=ifelse(teams_list[i]==posteam[j],j,NA)
    teams <- append(teams, team, after=length(teams))}
  
  plays_2018_2 = plays_2018[c(na.omit(teams)),]
  
  plays_2018_2$Predboost_acc=as.factor(plays_2018_2$Predboost_acc)
  PFA=table(plays_2018_2$Predboost_acc)
  
  total_accuracy=(PFA[1]+PFA[2])/(PFA[1]+PFA[2]+PFA[3]+PFA[4])
  pass_accuracy=PFA[1]/(PFA[1]+PFA[3])
  run_accuracy=PFA[2]/(PFA[2]+PFA[4])
  
  total_accuracy=as.numeric(total_accuracy)
  pass_accuracy=as.numeric(pass_accuracy)
  run_accuracy=as.numeric(run_accuracy)
  
  team_total_accuracies_0.6<-append(team_total_accuracies_0.6, total_accuracy, after=length(team_total_accuracies_0.6))
  team_pass_accuracies_0.6<-append(team_pass_accuracies_0.6, pass_accuracy, after=length(team_pass_accuracies_0.6))
  team_run_accuracies_0.6<-append(team_run_accuracies_0.6, run_accuracy, after=length(team_run_accuracies_0.6))
}

# Load 2018 DataFrame
plays_2018 <- read.csv("~/Downloads/plays.csv")
plays_2018 = plays_2018[c(8547:9459),c(5,9,11,18,19,20,21,22,23,24,27,28)]
attach(plays_2018)

team_total_accuracies_0.7 <- list()
team_pass_accuracies_0.7 <- list()
team_run_accuracies_0.7 <- list()

# Loop: If P>0.7; pass, if P<0.3; run.
playtypes <- rep()
for (i in 1:length(predicted)){
  playtype=ifelse(predicted[i]>0.7,5,
                  ifelse(predicted[i]<0.3,10,2))
  playtypes <- append(playtypes, playtype, after=length(playtypes))}
plays_2018["Predboost"]=playtypes
attach(plays_2018)

# Elimate rows that have a value of 2 in Predboost
twos <- rep()
for (i in 1:length(predicted)){
  two=ifelse(Predboost[i]==2,i,NA)
  twos <- append(twos, two, after=length(twos))}
# Drop all NA values from list
twos
na.omit(twos)
# Drop all row indexes from DataFrame
plays_2018 = plays_2018[-c(na.omit(twos)),]
attach(plays_2018)

# Compare predictions with observed play type
diffs <- rep()
for (i in 1:length(play_type)){
  diff=ifelse(Predboost[i]-play_type[i]==10,"correct run",
              ifelse(Predboost[i]-play_type[i]==4,"correct pass",
                     ifelse(Predboost[i]-play_type[i]==9,"incorrect run",
                            ifelse(Predboost[i]-play_type[i]==5,"incorrect pass"))))
  diffs <- append(diffs, diff, after=length(diffs))
}
plays_2018["Predboost_acc"]=diffs
attach(plays_2018)

# Measure accuracy; percent of play types predicted correctly
plays_2018$Predboost_acc=as.factor(plays_2018$Predboost_acc)
PFA=table(Predboost_acc)

total_accuracy=(PFA[1]+PFA[2])/(PFA[1]+PFA[2]+PFA[3]+PFA[4])
pass_accuracy=PFA[1]/(PFA[1]+PFA[3])
run_accuracy=PFA[2]/(PFA[2]+PFA[4])

total_accuracy=as.numeric(total_accuracy)
pass_accuracy=as.numeric(pass_accuracy)
run_accuracy=as.numeric(run_accuracy)

total_accuracies<-append(total_accuracies, total_accuracy, after=length(total_accuracies))
pass_accuracies<-append(pass_accuracies, pass_accuracy, after=length(pass_accuracies))
run_accuracies<-append(run_accuracies, run_accuracy, after=length(run_accuracies))

# Team accuracy 
teams <- rep()
for (i in 1:length(teams_list)){
  for (j in 1:length(posteam)){
    team=ifelse(teams_list[i]==posteam[j],j,NA)
    teams <- append(teams, team, after=length(teams))}
  
  plays_2018_2 = plays_2018[c(na.omit(teams)),]
  
  plays_2018_2$Predboost_acc=as.factor(plays_2018_2$Predboost_acc)
  PFA=table(plays_2018_2$Predboost_acc)
  
  total_accuracy=(PFA[1]+PFA[2])/(PFA[1]+PFA[2]+PFA[3]+PFA[4])
  pass_accuracy=PFA[1]/(PFA[1]+PFA[3])
  run_accuracy=PFA[2]/(PFA[2]+PFA[4])
  
  total_accuracy=as.numeric(total_accuracy)
  pass_accuracy=as.numeric(pass_accuracy)
  run_accuracy=as.numeric(run_accuracy)
  
  team_total_accuracies_0.7<-append(team_total_accuracies_0.7, total_accuracy, after=length(team_total_accuracies_0.7))
  team_pass_accuracies_0.7<-append(team_pass_accuracies_0.7, pass_accuracy, after=length(team_pass_accuracies_0.7))
  team_run_accuracies_0.7<-append(team_run_accuracies_0.7, run_accuracy, after=length(team_run_accuracies_0.7))
}

# Load 2018 DataFrame
plays_2018 <- read.csv("~/Downloads/plays.csv")
plays_2018 = plays_2018[c(8547:9459),c(5,9,11,18,19,20,21,22,23,24,27,28)]
attach(plays_2018)

team_total_accuracies_0.8 <- list()
team_pass_accuracies_0.8 <- list()
team_run_accuracies_0.8 <- list()

# Loop: If P>0.8; pass, if P<0.2; run.
playtypes <- rep()
for (i in 1:length(predicted)){
  playtype=ifelse(predicted[i]>0.8,5,
                  ifelse(predicted[i]<0.2,10,2))
  playtypes <- append(playtypes, playtype, after=length(playtypes))}
plays_2018["Predboost"]=playtypes
attach(plays_2018)

# Elimate rows that have a value of 2 in Predboost
twos <- rep()
for (i in 1:length(predicted)){
  two=ifelse(Predboost[i]==2,i,NA)
  twos <- append(twos, two, after=length(twos))}
# Drop all NA values from list
twos
na.omit(twos)
# Drop all row indexes from DataFrame
plays_2018 = plays_2018[-c(na.omit(twos)),]
attach(plays_2018)

# Compare predictions with observed play type
diffs <- rep()
for (i in 1:length(play_type)){
  diff=ifelse(Predboost[i]-play_type[i]==10,"correct run",
              ifelse(Predboost[i]-play_type[i]==4,"correct pass",
                     ifelse(Predboost[i]-play_type[i]==9,"incorrect run",
                            ifelse(Predboost[i]-play_type[i]==5,"incorrect pass"))))
  diffs <- append(diffs, diff, after=length(diffs))
}
plays_2018["Predboost_acc"]=diffs
attach(plays_2018)

# Measure accuracy; percent of play types predicted correctly
plays_2018$Predboost_acc=as.factor(plays_2018$Predboost_acc)
PFA=table(Predboost_acc)

total_accuracy=(PFA[1]+PFA[2])/(PFA[1]+PFA[2]+PFA[3]+PFA[4])
pass_accuracy=PFA[1]/(PFA[1]+PFA[3])
run_accuracy=PFA[2]/(PFA[2]+PFA[4])

total_accuracy=as.numeric(total_accuracy)
pass_accuracy=as.numeric(pass_accuracy)
run_accuracy=as.numeric(run_accuracy)

total_accuracies<-append(total_accuracies, total_accuracy, after=length(total_accuracies))
pass_accuracies<-append(pass_accuracies, pass_accuracy, after=length(pass_accuracies))
run_accuracies<-append(run_accuracies, run_accuracy, after=length(run_accuracies))

# Team accuracy 
teams <- rep()
for (i in 1:length(teams_list)){
  for (j in 1:length(posteam)){
    team=ifelse(teams_list[i]==posteam[j],j,NA)
    teams <- append(teams, team, after=length(teams))}
  
  plays_2018_2 = plays_2018[c(na.omit(teams)),]
  
  plays_2018_2$Predboost_acc=as.factor(plays_2018_2$Predboost_acc)
  PFA=table(plays_2018_2$Predboost_acc)
  
  total_accuracy=(PFA[1]+PFA[2])/(PFA[1]+PFA[2]+PFA[3]+PFA[4])
  pass_accuracy=PFA[1]/(PFA[1]+PFA[3])
  run_accuracy=PFA[2]/(PFA[2]+PFA[4])
  
  total_accuracy=as.numeric(total_accuracy)
  pass_accuracy=as.numeric(pass_accuracy)
  run_accuracy=as.numeric(run_accuracy)
  
  team_total_accuracies_0.8<-append(team_total_accuracies_0.8, total_accuracy, after=length(team_total_accuracies_0.8))
  team_pass_accuracies_0.8<-append(team_pass_accuracies_0.8, pass_accuracy, after=length(team_pass_accuracies_0.8))
  team_run_accuracies_0.8<-append(team_run_accuracies_0.8, run_accuracy, after=length(team_run_accuracies_0.8))
}

# Load 2018 DataFrame
plays_2018 <- read.csv("~/Downloads/plays.csv")
plays_2018 = plays_2018[c(8547:9459),c(5,9,11,18,19,20,21,22,23,24,27,28)]
attach(plays_2018)

team_total_accuracies_0.9 <- list()
team_pass_accuracies_0.9 <- list()
team_run_accuracies_0.9 <- list()

# Loop: If P>0.9; pass, if P<0.1; run.
playtypes <- rep()
for (i in 1:length(predicted)){
  playtype=ifelse(predicted[i]>0.9,5,
                  ifelse(predicted[i]<0.1,10,2))
  playtypes <- append(playtypes, playtype, after=length(playtypes))}
plays_2018["Predboost"]=playtypes
attach(plays_2018)

# Elimate rows that have a value of 2 in Predboost
twos <- rep()
for (i in 1:length(predicted)){
  two=ifelse(Predboost[i]==2,i,NA)
  twos <- append(twos, two, after=length(twos))}
# Drop all NA values from list
twos
na.omit(twos)
# Drop all row indexes from DataFrame
plays_2018 = plays_2018[-c(na.omit(twos)),]
attach(plays_2018)

# Compare predictions with observed play type
diffs <- rep()
for (i in 1:length(play_type)){
  diff=ifelse(Predboost[i]-play_type[i]==10,"correct run",
              ifelse(Predboost[i]-play_type[i]==4,"correct pass",
                     ifelse(Predboost[i]-play_type[i]==9,"incorrect run",
                            ifelse(Predboost[i]-play_type[i]==5,"incorrect pass"))))
  diffs <- append(diffs, diff, after=length(diffs))
}
plays_2018["Predboost_acc"]=diffs
attach(plays_2018)

# Measure accuracy; percent of play types predicted correctly
plays_2018$Predboost_acc=as.factor(plays_2018$Predboost_acc)
PFA=table(Predboost_acc)

total_accuracy=(PFA[1]+PFA[2])/(PFA[1]+PFA[2]+PFA[3]+PFA[4])
pass_accuracy=PFA[1]/(PFA[1]+PFA[3])
run_accuracy=PFA[2]/(PFA[2]+PFA[4])

total_accuracy=as.numeric(total_accuracy)
pass_accuracy=as.numeric(pass_accuracy)
run_accuracy=as.numeric(run_accuracy)

total_accuracies<-append(total_accuracies, total_accuracy, after=length(total_accuracies))
pass_accuracies<-append(pass_accuracies, pass_accuracy, after=length(pass_accuracies))
run_accuracies<-append(run_accuracies, run_accuracy, after=length(run_accuracies))

# Team accuracy 
teams <- rep()
for (i in 1:length(teams_list)){
  for (j in 1:length(posteam)){
    team=ifelse(teams_list[i]==posteam[j],j,NA)
    teams <- append(teams, team, after=length(teams))}
  
  plays_2018_2 = plays_2018[c(na.omit(teams)),]
  
  plays_2018_2$Predboost_acc=as.factor(plays_2018_2$Predboost_acc)
  PFA=table(plays_2018_2$Predboost_acc)
  
  total_accuracy=(PFA[1]+PFA[2])/(PFA[1]+PFA[2]+PFA[3]+PFA[4])
  pass_accuracy=PFA[1]/(PFA[1]+PFA[3])
  run_accuracy=PFA[2]/(PFA[2]+PFA[4])
  
  total_accuracy=as.numeric(total_accuracy)
  pass_accuracy=as.numeric(pass_accuracy)
  run_accuracy=as.numeric(run_accuracy)
  
  team_total_accuracies_0.9<-append(team_total_accuracies_0.9, total_accuracy, after=length(team_total_accuracies_0.9))
  team_pass_accuracies_0.9<-append(team_pass_accuracies_0.9, pass_accuracy, after=length(team_pass_accuracies_0.9))
  team_run_accuracies_0.9<-append(team_run_accuracies_0.9, run_accuracy, after=length(team_run_accuracies_0.9))
}

# Load 2018 DataFrame
plays_2018 <- read.csv("~/Downloads/plays.csv")
plays_2018 = plays_2018[c(8547:9459),c(5,9,11,18,19,20,21,22,23,24,27,28)]
attach(plays_2018)

### Shotgun ###

# Loop: If P>0.5; pass, if P<0.5; run.
playtypes <- rep()
for (i in 1:length(predicted)){
  playtype=ifelse(predicted[i]>0.5,5,10)
  playtypes <- append(playtypes, playtype, after=length(playtypes))
}
plays_2018["Predboost"]=playtypes
attach(plays_2018)

# Compare predictions with observed play type
diffs <- rep()
for (i in 1:length(predicted)){
  diff=ifelse(Predboost[i]-play_type[i]==10,"correct run",
              ifelse(Predboost[i]-play_type[i]==4,"correct pass",
                     ifelse(Predboost[i]-play_type[i]==9,"incorrect run",
                            ifelse(Predboost[i]-play_type[i]==5,"incorrect pass"))))
  diffs <- append(diffs, diff, after=length(diffs))
}
plays_2018["Predboost_acc"]=diffs
attach(plays_2018)

guns <- rep()
for (i in 1:length(shotgun)){
  gun=ifelse(shotgun[i]==1,i,NA)
  guns <- append(guns, gun, after=length(guns))
}
plays_2018 = plays_2018[c(na.omit(guns)),]
attach(plays_2018)

# Measure accuracy; percent of play types predicted correctly
plays_2018$Predboost_acc=as.factor(plays_2018$Predboost_acc)
PFA=table(Predboost_acc)

total_accuracy=(PFA[1]+PFA[2])/(PFA[1]+PFA[2]+PFA[3]+PFA[4])
pass_accuracy=PFA[1]/(PFA[1]+PFA[3])
run_accuracy=PFA[2]/(PFA[2]+PFA[4])

percent_pass_shotgun=(PFA[1]+PFA[3])/(PFA[1]+PFA[2]+PFA[3]+PFA[4])


##### RESULTS GRAPHS #####
par(mfrow=c(1,1))
label = c("Pass>0.5, Run<0.5","Pass>0.6, Run<0.4","Pass>0.7, Run<0.3","Pass>0.8, Run<0.2","Pass>0.9, Run<0.1")
acc <- data.frame(Total=as.numeric(total_accuracies),
                  Pass=as.numeric(pass_accuracies),
                  Run=as.numeric(run_accuracies))
View(acc)
attach(acc)
acc <- as.matrix(acc)

color.names = c("lavenderblush4","lavenderblush3","lavenderblush2","lavenderblush1","lavenderblush")

barplot(height = acc,
        width=2, 
        beside=TRUE, 
        ylim=c(0,1),  
        xlab="Play Type",
        ylab="Accuracy (%)",
        col=color.names,
        legend.text = label)


par(mfrow=c(3,2))

teams_list <- c("BAL","CIN","CLE","KC","TB","ATL","CAR","JAX","DEN","LAC","OAK","NE","NO")
acc <- data.frame(total=as.numeric(team_total_accuracies_0.5),
                  pass=as.numeric(team_pass_accuracies_0.5),
                  run=as.numeric(team_run_accuracies_0.5))
View(acc)
acc <- as.matrix(acc)
acc <- t(acc)
colnames(acc) <- teams_list

acc <- as.data.frame(acc)
afcn <- acc[,c(1:3)]
afc <- acc[,c(4,8:12)]
nfc <- acc[,c(5:7,13)]

rm_afcn <- rowMeans(afcn)
rm_afcn <- (as.numeric(rm_afcn))

rm_afc <- rowMeans(afc)
rm_afc <- (as.numeric(rm_afc))

rm_nfc <- rowMeans(nfc)
rm_nfc <- (as.numeric(rm_nfc))

teams_list <- c("Total","Pass","Run")
acc <- data.frame(DIV=rm_afcn,
                  AFC=rm_afc,
                  NFC=rm_nfc)

View(acc)
acc <- as.matrix(acc)
acc <- t(acc)
colnames(acc) <- teams_list

color.names = c("lavenderblush4","lavenderblush3","lavenderblush1")

# percentage of plays predicted correctly
bp0.5<- barplot(heigh=acc, 
                xName=teams_list, 
                width=2, 
                beside=TRUE, 
                ylim=c(0,1),  
                main="Pass>0.5, Run<0.5",
                xlab="Play Type",
                ylab="Accuracy (%)",
                col=color.names)



teams_list <- c("BAL","CIN","CLE","KC","TB","ATL","CAR","JAX","DEN","LAC","OAK","NE","NO")
acc <- data.frame(total=as.numeric(team_total_accuracies_0.6),
                  pass=as.numeric(team_pass_accuracies_0.6),
                  run=as.numeric(team_run_accuracies_0.6))
View(acc)
acc <- as.matrix(acc)
acc <- t(acc)
colnames(acc) <- teams_list

acc <- as.data.frame(acc)
afcn <- acc[,c(1:3)]
afc <- acc[,c(4,8:12)]
nfc <- acc[,c(5:7,13)]

rm_afcn <- rowMeans(afcn)
rm_afcn <- (as.numeric(rm_afcn))

rm_afc <- rowMeans(afc)
rm_afc <- (as.numeric(rm_afc))

rm_nfc <- rowMeans(nfc)
rm_nfc <- (as.numeric(rm_nfc))

teams_list <- c("Total","Pass","Run")
acc <- data.frame(DIV=rm_afcn,
                  AFC=rm_afc,
                  NFC=rm_nfc)

View(acc)
acc <- as.matrix(acc)
acc <- t(acc)
colnames(acc) <- teams_list

color.names = c("lavenderblush4","lavenderblush3","lavenderblush1")

# percentage of plays predicted correctly
bp0.6 <- barplot(heigh=acc, 
                 xName=teams_list, 
                 width=2, 
                 beside=TRUE, 
                 ylim=c(0,1),  
                 main="Pass>0.6, Run<0.4",
                 xlab="Play Type",
                 ylab="Accuracy (%)",
                 col=color.names)



teams_list <- c("BAL","CIN","CLE","KC","TB","ATL","CAR","JAX","DEN","LAC","OAK","NE","NO")
acc <- data.frame(total=as.numeric(team_total_accuracies_0.7),
                  pass=as.numeric(team_pass_accuracies_0.7),
                  run=as.numeric(team_run_accuracies_0.7))
View(acc)
acc <- as.matrix(acc)
acc <- t(acc)
colnames(acc) <- teams_list

acc <- as.data.frame(acc)
afcn <- acc[,c(1:3)]
afc <- acc[,c(4,8:12)]
nfc <- acc[,c(5:7,13)]

rm_afcn <- rowMeans(afcn)
rm_afcn <- (as.numeric(rm_afcn))

rm_afc <- rowMeans(afc)
rm_afc <- (as.numeric(rm_afc))

rm_nfc <- rowMeans(nfc)
rm_nfc <- (as.numeric(rm_nfc))

teams_list <- c("Total","Pass","Run")
acc <- data.frame(DIV=rm_afcn,
                  AFC=rm_afc,
                  NFC=rm_nfc)

View(acc)
acc <- as.matrix(acc)
acc <- t(acc)
colnames(acc) <- teams_list

color.names = c("lavenderblush4","lavenderblush3","lavenderblush1")

bp0.7 <- barplot(heigh=acc, 
                 xName=teams_list, 
                 width=2, 
                 beside=TRUE, 
                 ylim=c(0,1),  
                 main="Pass>0.7, Run<0.3",
                 xlab="Play Type",
                 ylab="Accuracy (%)",
                 col=color.names)



teams_list <- c("BAL","CIN","CLE","KC","TB","ATL","CAR","JAX","DEN","LAC","OAK","NE","NO")
acc <- data.frame(total=as.numeric(team_total_accuracies_0.8),
                  pass=as.numeric(team_pass_accuracies_0.8),
                  run=as.numeric(team_run_accuracies_0.8))
View(acc)
acc <- as.matrix(acc)
acc <- t(acc)
colnames(acc) <- teams_list

acc <- as.data.frame(acc)
afcn <- acc[,c(1:3)]
afc <- acc[,c(4,8:12)]
nfc <- acc[,c(5:7,13)]

rm_afcn <- rowMeans(afcn)
rm_afcn <- (as.numeric(rm_afcn))

rm_afc <- rowMeans(afc)
rm_afc <- (as.numeric(rm_afc))

rm_nfc <- rowMeans(nfc)
rm_nfc <- (as.numeric(rm_nfc))

teams_list <- c("Total","Pass","Run")
acc <- data.frame(DIV=rm_afcn,
                  AFC=rm_afc,
                  NFC=rm_nfc)

View(acc)
acc <- as.matrix(acc)
acc <- t(acc)
colnames(acc) <- teams_list

color.names = c("lavenderblush4","lavenderblush3","lavenderblush1")

bp0.8 <- barplot(heigh=acc, 
                 xName=teams_list, 
                 width=2, 
                 beside=TRUE, 
                 ylim=c(0,1),  
                 main="Pass>0.8, Run<0.2",
                 xlab="Play Type",
                 ylab="Accuracy (%)",
                 col=color.names)




teams_list <- c("BAL","CIN","CLE","KC","TB","ATL","CAR","JAX","DEN","LAC","OAK","NE","NO")
acc <- data.frame(total=as.numeric(team_total_accuracies_0.9),
                  pass=as.numeric(team_pass_accuracies_0.9),
                  run=as.numeric(team_run_accuracies_0.9))
View(acc)
acc <- as.matrix(acc)
acc <- t(acc)
colnames(acc) <- teams_list

acc <- as.data.frame(acc)
afcn <- acc[,c(1:3)]
afc <- acc[,c(4,8:12)]
nfc <- acc[,c(5:7,13)]

rm_afcn <- rowMeans(afcn)
rm_afcn <- (as.numeric(rm_afcn))

rm_afc <- rowMeans(afc)
rm_afc <- (as.numeric(rm_afc))

rm_nfc <- rowMeans(nfc)
rm_nfc <- (as.numeric(rm_nfc))

teams_list <- c("Total","Pass","Run")
acc <- data.frame(DIV=rm_afcn,
                  AFC=rm_afc,
                  NFC=rm_nfc)

View(acc)
acc <- as.matrix(acc)
acc <- t(acc)
colnames(acc) <- teams_list

color.names = c("lavenderblush4","lavenderblush3","lavenderblush1")

bp0.9 <- barplot(heigh=acc, 
                 xName=teams_list, 
                 width=2, 
                 beside=TRUE, 
                 ylim=c(0,1),
                 main="Pass>0.9, Run<0.1",
                 xlab="Play Type",
                 ylab="Accuracy (%)",
                 col=color.names)

names <- c('DIV', 'AFC', 'NFC') 
clrs <- c("lavenderblush4","lavenderblush3","lavenderblush1")
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("topleft", title="Opponent Type", legend = names, lty=1, lwd=10, cex=1.25,
       bty='n', col = clrs)



