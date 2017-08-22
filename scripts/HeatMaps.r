setwd("~/workspace/ultimate/")
source("scripts/util/field.r")

prefix = "data/mens"
outfile = "heatMaps.pdf"

library(lattice)
library(class)

#functions
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

#Parameters
field = get_field("ultiapps") # choices: "ultiapps" or "nexgen"
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

##Create Scoring.Possession and Home.Scores
dat <- read.table(paste(prefix,"2013.csv",sep=""), header = TRUE, sep = ",", strip.white=T, comment.char="$")
attach(dat)
temp <- aggregate(Score.=="true", by=list(Possession.Id), sum) #key-value:pid,score
dat$Scoring.Possession <- as.numeric(temp[match(Possession.Id,temp[,1]),2]>0)

temp <- aggregate(Score.=="true" &
                    ( (Home.Attacking.Right.=="false" & X.Position<=field$endzone_right_front)
                      | (Home.Attacking.Right.=="true" & X.Position>=field$endzone_left_front)
                    ),
                  by=list(Point.Id), sum) #key-value:pid,score
dat$Home.Scores <- as.numeric(temp[match(Point.Id,temp[,1]),2]>0)
detach(dat)

home_attack_right <- dat$Home.Attacking.Right.=="true"
home_attack_left <- dat$Home.Attacking.Right.=="false"
home_disc <- dat$Home.on.Offense == "true"
opp_disc <- dat$Home.on.Offense == "false"
#home_disc <- dat$Thrower.id!=0
#opp_disc <- dat$Thrower.id==0

matrix <- cbind(dat$X.Position, dat$Y.Position)

pdf(outfile)
# #anybody attacking left
# condition <- (home_attack_left & home_disc) | (home_attack_right & opp_disc)
# plotKNN(jcwKNN(matrix,as.numeric(dat$Scoring.Possession), condition))
# 
# #anybody attacking right
# condition <- (home_attack_left & opp_disc) | (home_attack_right & home_disc)
# plotKNN(jcwKNN(matrix,as.numeric(dat$Scoring.Possession), condition))
# 
# #Home attacking left
# condition <- home_attack_left & home_disc
# plotKNN(jcwKNN(matrix,as.numeric(dat$Scoring.Possession), condition))
# #plotKNN(jcwKNN(matrix,as.numeric(dat$Home.Scores), condition))
# 
# #Home attacking right
# condition <- home_attack_right & home_disc
# plotKNN(jcwKNN(matrix,as.numeric(dat$Scoring.Possession), condition))
# #plotKNN(jcwKNN(matrix,as.numeric(dat$Home.Scores), condition))
# 
# #opp attacking left
# condition <- home_attack_right & opp_disc
# plotKNN(jcwKNN(matrix,as.numeric(dat$Scoring.Possession), condition))
# #plotKNN(jcwKNN(matrix,as.numeric(1-dat$Home.Scores), condition))
# 
# #opp attacking right
# condition <- home_attack_left & opp_disc
# plotKNN(jcwKNN(matrix,as.numeric(dat$Scoring.Possession), condition))
# #plotKNN(jcwKNN(matrix,as.numeric(1-dat$Home.Scores), condition))

#Home directionally combined
condition <- home_attack_right & home_disc
home_combined_Matrix <- matrix[condition,]
home_combined_Outcome <- as.numeric(dat$Scoring.Possession)[condition]
condition <- home_attack_left & home_disc
rotmatrix <- matrix[condition,]
rotmatrix[,1] <- field$x_mid + (field$x_mid - rotmatrix[,1])
rotmatrix[,2] <- field$y_mid + (field$y_mid - rotmatrix[,2])
home_combined_Matrix <- rbind(home_combined_Matrix,rotmatrix)
home_combined_Outcome <- append(home_combined_Outcome, as.numeric(dat$Scoring.Possession)[condition])
home_combined_Poss <- jcwKNN(home_combined_Matrix,home_combined_Outcome, myk=100)
plotKNN(home_combined_Poss)
panel.text(50, 50, "p(home_scores), possession", cex = 1.2, font = 1, adj = c(0,0)) 

#opponents directionally combined
condition <- home_attack_left & opp_disc
opp_combined_Matrix <- matrix[condition,]
opp_combined_Outcome <- as.numeric(dat$Scoring.Possession)[condition]
condition <- home_attack_right & opp_disc
rotmatrix <- matrix[condition,]
rotmatrix[,1] <- field$x_mid + (field$x_mid - rotmatrix[,1])
rotmatrix[,2] <- field$y_mid + (field$y_mid - rotmatrix[,2])
opp_combined_Matrix <- rbind(opp_combined_Matrix,rotmatrix)
opp_combined_Outcome <- append(opp_combined_Outcome, as.numeric(dat$Scoring.Possession)[condition])
opp_combined_Poss <- jcwKNN(opp_combined_Matrix,opp_combined_Outcome, myk=100)
plotKNN(opp_combined_Poss)
panel.text(50, 50, "p(opp_scores), possession", cex = 1.2, font = 1, adj = c(0,0)) 

#Home/opp knn differential map
levelplot(matrix(class2prob(home_combined_Poss)-class2prob(opp_combined_Poss),ugrid$rows,ugrid$cols),
          at=seq(-0.35,0.35,0.05),col.regions = cm.colors(20),contour=F,pretty=T,
          cuts=10, xlab="Sideline", ylab="Own endzone",
          scales=list(alternating=0,tck=0),
          panel = function(...){
            panel.levelplot(...)
            panel.abline(v = xeline)
          }
          )
panel.text(50, 50, "home minus opp p(scores), possession", cex = 1.2, font = 1, adj = c(0,0)) 


### Scoring point, not scoring possession ###
#Home directionally combined
condition <- home_attack_right & home_disc
home_combined_Matrix <- matrix[condition,]
home_combined_Outcome <- as.numeric(dat$Home.Scores)[condition]
condition <- home_attack_left & home_disc
rotmatrix <- matrix[condition,]
rotmatrix[,1] <- field$x_mid + (field$x_mid - rotmatrix[,1])
rotmatrix[,2] <- field$y_mid + (field$y_mid - rotmatrix[,2])
home_combined_Matrix <- rbind(home_combined_Matrix,rotmatrix)
home_combined_Outcome <- append(home_combined_Outcome, as.numeric(dat$Home.Scores)[condition])
home_combined_Point <- jcwKNN(home_combined_Matrix,home_combined_Outcome, myk=100)
plotKNN(home_combined_Point)
panel.text(50, 50, "p(home_scores), point", cex = 1.2, font = 1, adj = c(0,0)) 

#opponents directionally combined
condition <- home_attack_left & opp_disc
opp_combined_Matrix <- matrix[condition,]
opp_combined_Outcome <- as.numeric(1-dat$Home.Scores)[condition]
condition <- home_attack_right & opp_disc
rotmatrix <- matrix[condition,]
rotmatrix[,1] <- field$x_mid + (field$x_mid - rotmatrix[,1])
rotmatrix[,2] <- field$y_mid + (field$y_mid - rotmatrix[,2])
opp_combined_Matrix <- rbind(opp_combined_Matrix,rotmatrix)
opp_combined_Outcome <- append(opp_combined_Outcome, as.numeric(1-dat$Home.Scores)[condition])
opp_combined_Point <- jcwKNN(opp_combined_Matrix,opp_combined_Outcome, myk=100)
plotKNN(opp_combined_Point)
panel.text(50, 50, "p(opp_scores), point", cex = 1.2, font = 1, adj = c(0,0)) 

#Home/opp knn differential map
levelplot(matrix(class2prob(home_combined_Point)-class2prob(opp_combined_Point),ugrid$rows,ugrid$cols),
          at=seq(-0.35,0.35,0.05),col.regions = cm.colors(20),contour=F,pretty=T,
          cuts=10, xlab="Sideline", ylab="Own endzone",
          scales=list(alternating=0,tck=0),
          panel = function(...){
            panel.levelplot(...)
            panel.abline(v = xeline)
          }
)
panel.text(50, 50, "home minus opp p(scores), point", cex = 1.2, font = 1, adj = c(0,0)) 

dev.off()

# #DEBUG
# levelplot(matrix(class2prob(opp_combined_Point)-class2prob(opp_combined_Poss)<0,79,36),
#           at=seq(-0.3,0.3,0.05),col.regions = cm.colors(20),contour=F,pretty=T,
#           cuts=10, xlab="Sideline", ylab="Own endzone",
#           scales=list(alternating=0,tck=0),
#           panel = function(...){
#             panel.levelplot(...)
#             panel.abline(v = 18)
#           }
# )
# 
# #opponents directionally combined
# condition <- home_attack_left & opp_disc
# opp_combined_PossMatrix <- matrix[condition,]
# opp_combined_PossOutcome <- as.numeric(dat$Scoring.Possession)[condition]
# condition <- home_attack_right & opp_disc
# rotmatrix <- matrix[condition,]
#rotmatrix[,1] <- x_mid + (x_mid - rotmatrix[,1])
#rotmatrix[,2] <- y_mid + (y_mid - rotmatrix[,2])
# opp_combined_PossMatrix <- rbind(opp_combined_PossMatrix,rotmatrix)
# opp_combined_PossOutcome <- append(opp_combined_PossOutcome, as.numeric(dat$Scoring.Possession)[condition])
# opp_combined_PossKNN <- jcwKNN(opp_combined_PossMatrix,opp_combined_PossOutcome, myk=100)
# plotKNN(opp_combined_PossKNN)
# 
# #opponents directionally combined
# condition <- home_attack_left & opp_disc
# opp_combined_PointMatrix <- matrix[condition,]
# opp_combined_PointOutcome <- as.numeric(1-dat$Home.Scores)[condition]
# condition <- home_attack_right & opp_disc
# rotmatrix <- matrix[condition,]
# rotmatrix[,1] <- x_mid + (x_mid - rotmatrix[,1])
# rotmatrix[,2] <- y_mid + (y_mid - rotmatrix[,2])
# opp_combined_PointMatrix <- rbind(opp_combined_PointMatrix,rotmatrix)
# opp_combined_PointOutcome <- append(opp_combined_PointOutcome, as.numeric(1-dat$Home.Scores)[condition])
# opp_combined_PointKNN <- jcwKNN(opp_combined_PointMatrix,opp_combined_PointOutcome, myk=100)
# plotKNN(opp_combined_PointKNN)
# 
# levelplot(matrix(class2prob(opp_combined_PointKNN)-class2prob(opp_combined_PossKNN)<0,79,36),
#           col.regions = cm.colors(20),contour=F,pretty=T,
#           cuts=10, xlab="Sideline", ylab="Own endzone",
#           scales=list(alternating=0,tck=0),
#           panel = function(...){
#             panel.levelplot(...)
#             panel.abline(v = 18)
#           }
# )
# 
# sum(abs(opp_combined_PossMatrix-opp_combined_PointMatrix))
# opp_combined_PossMatrix[which(opp_combined_PossOutcome-opp_combined_PointOutcome>0),]
# plot(class2prob(opp_combined_PointKNN)-class2prob(opp_combined_PossKNN))
# sum((class2prob(opp_combined_PointKNN)-class2prob(opp_combined_PossKNN))[which(class2prob(opp_combined_PointKNN)-class2prob(opp_combined_PossKNN)!=0)]<0)
# 
# # # which(home_disc & dat$Scoring.Possession & !dat$Home.Scores)
# # # which(opp_disc & dat$Scoring.Possession & dat$Home.Scores)
# # 
# # # #(DEBUG) badones
# badones <- matrix(class2prob(opp_combined_PointKNN)-class2prob(opp_combined_PossKNN)<0,79,36)
# for(i in 1:dim(badones)[1]) {
#   for(j in 1:dim(badones)[2]) {
#     if(badones[i,j])
#       print(paste(i*10,j*10))
#   }
# }
# levelplot(matrix(class2prob(opp_combined_PointKNN)-class2prob(opp_combined_PossKNN)>=0,79,36),
#           col.regions = cm.colors(20),contour=F,pretty=T,
#           cuts=10, xlab="Sideline", ylab="Own endzone",
#           scales=list(alternating=0,tck=0),
#           panel = function(...){
#             panel.levelplot(...)
#             panel.abline(v = 18)
#           }
# )

#Next, draw maps for being in specific sectors of the map
#e.g. stuck on the line around midfield

#Graph for hot/cold connections

