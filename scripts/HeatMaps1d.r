setwd("~/workspace/ultimate/")

library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(lattice)
library(class)


playsPrefix = "data/mens"
# playsPrefix = "data/womens"
# playsPrefix = "data/nexgen"


#####functions#####
getGrid <- function(xseq, yseq) {
  g <- list()
  g$grid <- as.matrix(expand.grid(xseq,yseq))
  g$rows <- length(xseq)
  g$cols <- length(yseq)
  g
}
jcwKNN <- function(xymatrix, outcomes, rowsubset, grid=ugrid, myk=100) {
  knn(xymatrix[rowsubset,],grid$grid, as.numeric(outcomes[rowsubset]), prob=T, k=myk, use.all=F)
}
class2prob <- function(knn) { #prob is for most likely class, not just one in particular
  prob <- attributes(knn)$prob
  flippers <- which(knn==levels(knn)[1])
  if(levels(knn)[1]!="0") print("warning, levels not {0,1}?")
  prob[flippers] <- 1-prob[flippers]
  prob
}

#####Parameters#####
#Field
source("scripts/util/field.r")
field = get_field("ultiapps")
# field = get_field("nexgen")
attach(field)
xseq <- seq(endzone_left_back,endzone_right_front,x_step)
yseq <- seq(ob_top,ob_bottom,y_step)
xeline <- (endzone_left_front-endzone_left_back)/x_step
ugrid <- getGrid(xseq,yseq)
detach(field)

#Function requiring parameters
plotKNN <- function(jcwknn, grid=ugrid, endzoneline=xeline) {
  levelplot(matrix(class2prob(jcwknn),grid$rows,grid$cols),at=seq(0,1,0.05),col.regions = terrain.colors(100),
            contour=F,pretty=T, cuts=10, xlab="Sideline", ylab="Own endzone",
            scales=list(alternating=0,tck=0),
            panel = function(...){
              panel.levelplot(...)
              panel.abline(v = xeline)
            }
  )
}

###Data table: dat###
#Load plays
playsFile <- paste(playsPrefix,"2013.csv",sep="")
playersFile <- paste(playsPrefix,"Players2013.csv",sep="")
plays <- read.table(playsFile, 
                    header = TRUE, sep = ",", strip.white=T, comment.char="$") %>%
  as_tibble()
players <- read.table(playersFile,
                      header = TRUE, sep = ",", strip.white=T, comment.char="$") %>%
  as_tibble()
#Sort plays (1) by game (2) then by point (3) then by possession (4) then by play
# attach(plays)
#Create Scoring.Possession
# temp <- aggregate(Score.=="true", by=list(Possession.Id), sum) #key-value:pid,score
# plays$Scoring.Possession <- as.numeric(temp[match(Possession.Id,temp[,1]),2]>0)
plays =
  plays %>%
  group_by(Possession.Id) %>% mutate(Scoring.Possession = max(Score.=="true")) %>% ungroup()

#Create Home.Scores.Point
# temp <- aggregate(Score.=="true" &
#                     ( (Home.Attacking.Right.=="false" & X.Position<=field$endzone_right_front)
#                       | (Home.Attacking.Right.=="true" & X.Position>=field$endzone_left_front)
#                     ),
#                   by=list(Point.Id), sum) #key-value:pid,score
# plays$Home.Scores.Point <- as.numeric(temp[match(Point.Id,temp[,1]),2]>0)
plays =
  plays %>%
  group_by(Point.Id) %>% mutate(Home.Scores.Point = max(Score.=="true" &
                                  ( (Home.Attacking.Right.=="false" & X.Position<=field$endzone_right_front)
                                    | (Home.Attacking.Right.=="true" & X.Position>=field$endzone_left_front)
                                  )
                                )) %>% ungroup()

#Create (X/Y).Throw.Distance
# E <- dim(plays)[1]
# SamePoint <- which(plays$Point.Id[-E]==plays$Point.Id[-1])
# plays$X.Throw.Distance <- rep(NA,E)
# plays$X.Throw.Distance[SamePoint] <- (X.Position[-1]-X.Position[-E])[SamePoint]
# plays$Y.Throw.Distance <- rep(NA,E)
# plays$Y.Throw.Distance[SamePoint] <- (Y.Position[-1]-Y.Position[-E])[SamePoint]
# rm(E,SamePoint)

#Now, orient all plays so Home attacks right
har <- plays$Home.Attacking.Right.=="true"
#hhd <- plays$Thrower.id!=0
hhd <- plays$Home.on.Offense=="true"
#note X.position is the receiver position
#and  Last.X is the thrower position
# detach(plays)


matrix <- cbind(plays$X.Position, plays$Y.Position)

#Plot the LOESS predictions
plot_loess <- function(loess, field, add=F, ...) {
  steps <- seq(field$endzone_left_back, field$endzone_right_front,field$x_step)
  par(mgp=c(2,0.5,0))
  if(add==F) {
    plot(steps, predict(loess,steps),
         type='l', ylim=c(0,1), axes=F, ylab="P(score)", xlab="", ...)
    axis(2, tck=0.02)
    axis(1, at=c(field$endzone_left_back,
                 field$endzone_left_front,
                 field$endzone_right_front,
                 field$endzone_right_back),
         labels=rep("^",4), tck=0, pos =0)
    
  } else {
    lines(steps, predict(loess,steps),type='l',...)
  }
}

condition <- har & hhd
rplays <- plays[condition,]
condition <- !har & hhd
lplays <- plays[condition,]
#Create p(score_point) and p(score_possession) as function of X.position
rscorepos <- subset(rplays, select=c(Scoring.Possession, X.Position))
lscorepos <- subset(lplays, select=c(Scoring.Possession, X.Position))
lscorepos[,2] <- field$endzone_right_back-lscorepos[,2]
scorepos <- rbind(rscorepos, lscorepos)
lo_scorepos <- loess(scorepos$Scoring.Possession ~ scorepos$X.Position)
plot_loess(lo_scorepos, field, lty=2, xlim=c(field$endzone_left_back,field$endzone_right_back+1000))

rscorepoint <- subset(rplays, select=c(Home.Scores.Point, X.Position))
lscorepoint <- subset(lplays, select=c(Home.Scores.Point, X.Position))
#(only works for home team -- need to reverse for opps)
lscorepoint[,2] <- field$endzone_right_back-lscorepoint[,2]
scorepoint <- rbind(rscorepoint, lscorepoint)
lo_scorepoint <- loess(scorepoint$Home.Scores.Point ~ scorepos$X.Position)
plot_loess(lo_scorepoint, field, add=T)

#Debug
# plot(scorepos[scorepos[,2]<2000,2],scorepos[scorepos[,2]<2000,1])
# lines(rscorepos[rscorepos[,2]<2000,2],rscorepos[rscorepos[,2]<2000,1],col=2, type="p", pch=17)
#There should be no score possessions that are not score points (UCF data fails, NYU data passes)
# plot(scorepos[scorepos[,2]<2000,2],scorepos[scorepos[,2]<2000,1])
# lines(scorepoint[scorepoint[,2]<2000,2],scorepoint[scorepoint[,2]<2000,1],col=2, type="p", pch=17)

#Opps
condition <- !har & !hhd
rplays_opp <- plays[condition,]
condition <- har & !hhd
lplays_opp <- plays[condition,]
#Create p(score_point) and p(score_possession) as function of X.position
rscorepos_opp <- subset(rplays_opp, select=c(Scoring.Possession, X.Position))
lscorepos_opp <- subset(lplays_opp, select=c(Scoring.Possession, X.Position))
lscorepos_opp[,2] <- field$endzone_right_back-lscorepos_opp[,2]
scorepos_opp <- rbind(rscorepos_opp, lscorepos_opp)
lo_scorepos_opp <- loess(scorepos_opp$Scoring.Possession ~ scorepos_opp$X.Position)
plot_loess(lo_scorepos_opp, field,add=T,lty=2,col=2)

rscorepoint_opp <- subset(rplays_opp, select=c(Home.Scores.Point, X.Position))
lscorepoint_opp <- subset(lplays_opp, select=c(Home.Scores.Point, X.Position))
#(only works for home team -- need to reverse for opps)
lscorepoint_opp[,2] <- field$endzone_right_back-lscorepoint_opp[,2]
scorepoint_opp <- rbind(rscorepoint_opp, lscorepoint_opp)
scorepoint_opp[,1] <- 1 - scorepoint_opp[,1] #reverses here
lo_scorepoint_opp <- loess(scorepoint_opp$Home.Scores.Point ~ scorepos_opp$X.Position)
plot_loess(lo_scorepoint_opp, field,add=T,col=2)

######
#Score individual contributions (given LOESS) for throwers, receivers, and defenders
######
fromto <- subset(plays, select=c(Thrower.id, Reciever.Id, Defender.Id, Last.X, X.Position))
fromto[!har&hhd,c("Last.X","X.Position")] <-
  field$endzone_right_back-fromto[!har&hhd,c("Last.X","X.Position")] #mirror left-going possessions
fromto[har&!hhd,c("Last.X","X.Position")] <- 
  field$endzone_right_back-fromto[har&!hhd,c("Last.X","X.Position")] #mirror left-going possessions

#Receiver evaluation
fromto$rec_diff <- rep(0, dim(fromto)[1])
rec_preds <- data.frame(sapply(fromto[hhd,-(1:3)], predict, object=lo_scorepoint)) #get predictions
rec_preds$X.Position[plays$Drop.[hhd]=="true"] <- 
  1 - sapply(fromto$X.Position[hhd & plays$Drop.=="true"], predict, object=lo_scorepoint_opp) #get predictions if turnover
rec_preds$X.Position[plays$Score.[hhd]=="true"] <- 1
fromto$rec_diff[hhd] <- rec_preds$X.Position - rec_preds$Last.X #TODO what about turnovers?
rec_preds_opps <- data.frame(sapply(fromto[!hhd,-(1:3)], predict, object=lo_scorepoint_opp))
rec_preds_opps$X.Position[plays$Drop.[!hhd]=="true"] <- 
  1 - sapply(fromto$X.Position[!hhd & plays$Drop.=="true"], predict, object=lo_scorepoint) #get predictions if turnover
rec_preds_opps$X.Position[plays$Score.[!hhd]=="true"] <- 1
fromto$rec_diff[!hhd] <- rec_preds_opps$X.Position - rec_preds_opps$Last.X
#if turnover to opp team, ... as a receiver, count drops
#if turnover to your team, ... as a receiver, ignore (counts for defense)
# fromto$diff[plays$Reciever.Id!=0 & plays$Thrower.id==0] <- 0
# fromto$diff[plays$Reciever.Id==0 & plays$Thrower.id!=0] <- 0
#Receivers score values
rec_score_map <- aggregate(. ~ fromto$Reciever.Id, data=fromto, sum)[,c("fromto$Reciever.Id","rec_diff")]
rec_score_map[,1] <- players$Player.Name[match(rec_score_map[,1], players$Player.Id)]

#Thrower evaluation
fromto$thr_diff <- rep(0, dim(fromto)[1])
thr_preds <- data.frame(sapply(fromto[hhd,-1], predict, object=lo_scorepoint)) #get predictions
thrower_faulted <- plays$Throwaway=="true" | plays$Intercept.=="true" | plays$Block.=="true"
thr_preds$X.Position[thrower_faulted[hhd]] <- 
  1 - sapply(fromto$X.Position[hhd & thrower_faulted], predict, object=lo_scorepoint_opp) #get predictions if turnover
thr_preds$X.Position[plays$Score.[hhd]=="true"] <- 1
fromto$thr_diff[hhd] <- thr_preds$X.Position - thr_preds$Last.X #TODO what about turnovers?
thr_preds_opps <- data.frame(sapply(fromto[!hhd,-1], predict, object=lo_scorepoint_opp))
thr_preds_opps$X.Position[thrower_faulted[!hhd]] <- 
  1 - sapply(fromto$X.Position[!hhd & thrower_faulted], predict, object=lo_scorepoint) #get predictions if turnover
thr_preds_opps$X.Position[plays$Score.[!hhd]=="true"] <- 1
fromto$thr_diff[!hhd] <- thr_preds_opps$X.Position - thr_preds_opps$Last.X
#if turnover to opp team, ... as a thrower, count throwaways, intercepts, and blocks
#if turnover to your team, ... as a thrower, ignore (counts for defense)
#Throwers score values
thr_score_map <- aggregate(. ~ fromto$Thrower.id, data=fromto, sum)[,c("fromto$Thrower.id","thr_diff")]
thr_score_map[,1] <- players$Player.Name[match(thr_score_map[,1], players$Player.Id)]

#Defender evaluation
fromto$def_diff <- rep(0, dim(fromto)[1])
def_preds <- 1 - data.frame(sapply(fromto[!hhd,-1], predict, object=lo_scorepoint_opp)) #get predictions
def_attred <- plays$Intercept.=="true" | plays$Block.=="true"
def_preds$X.Position[def_attred[!hhd]] <- 
  sapply(fromto$X.Position[!hhd & def_attred], predict, object=lo_scorepoint) #get predictions if turnover
def_preds$X.Position[plays$Score.[!hhd]=="true"] <- 0 #player let a score occur
fromto$def_diff[!hhd] <- def_preds$X.Position - def_preds$Last.X #TODO what about turnovers?
def_preds_opps <- 1 - data.frame(sapply(fromto[hhd,-1], predict, object=lo_scorepoint))
def_preds_opps$X.Position[def_attred[hhd]] <- 
  sapply(fromto$X.Position[hhd & def_attred], predict, object=lo_scorepoint_opp) #get predictions if turnover
def_preds_opps$X.Position[plays$Score.[hhd]=="true"] <- 0 #player let a score occur
fromto$def_diff[hhd] <- def_preds_opps$X.Position - def_preds_opps$Last.X
#if turnover to opp team, ... as a defender, ignore
#if turnover to your team, ... as a defender, count intercepts and blocks
#Throwers score values
def_score_map <- aggregate(. ~ fromto$Defender.Id, data=fromto, sum)[,c("fromto$Defender.Id","def_diff")]
def_score_map[,1] <- players$Player.Name[match(def_score_map[,1], players$Player.Id)]

all_score_map <- cbind(thr_score_map, rec_score_map$rec_diff, def_score_map$def_diff)
all_score_map$totals <- rowSums(all_score_map[,-1])
names(all_score_map) <- c("Player", "Throw", "Reception", "Defense", "Total")

#sean code

options(scipen=6)
pt_adjusted_scores <- subset(all_score_map, all_score_map$Player != "<NA>")

activePlayers <-subset(players, players$Attempted.Passes > 0 & players$Receptions > 0)

pt_adjusted_scores$ThrowPerOPoss <- pt_adjusted_scores$Throw/activePlayers$O.Possessions.Played
pt_adjusted_scores$ReceptionPerOPoss <- pt_adjusted_scores$Reception/activePlayers$O.Possessions.Played
pt_adjusted_scores$DefensePerDPoss <- pt_adjusted_scores$Defense/activePlayers$D.Possessions.Played
pt_adjusted_scores$TotalPerPoint <- pt_adjusted_scores$Total/activePlayers$Total.Points.Played
pt_adjusted_scores <- subset(pt_adjusted_scores, select = -c(Throw, Reception, Defense, Total))



#Note these are sum(expected value changes) from the previous throw, not the "alternative outcome", whatever that is.