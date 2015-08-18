library(dplyr)
library(mosaic)
library(ggplot2)
library(fImport)
library(foreach)

Georgia = read.csv("Github/STA380/data/georgia2000.csv")
summary(Georgia)

Georgia$undercount = abs(Georgia$votes - Georgia$ballots)

UndercountByEquip = group_by(Georgia, equip)
UndercountByEquip = summarise(UndercountByEquip, AvgUndercount = mean(Georgia$undercount), SumBallots = sum(Georgia$ballots), SumVotes = sum(Georgia$votes), BallotConversionRate = sum(Georgia$votes)/sum(Georgia$ballots))

UndercountByEquip
barplot(UndercountByEquip$AvgUndercount, names = UndercountByEquip$equip, ylab="Avg Undercount", xlab="Voting Equipment Type", col=4, main="Average Undercount by Voting Mechanism")
barplot((1-UndercountByEquip$BallotConversionRate)*100, names = UndercountByEquip$equip, ylab="Ballot Failure Rate", xlab="Voting Equipment Type", col=4, main="Percentage of Ballots which Don't Become Votes")

PoorVsEquip = xtabs(~poor + equip, data=Georgia)
PoorVsEquip
PoorVsEquipProp = prop.table(PoorVsEquip, margin=2)
PoorVsEquipProp

ggplot(Georgia, aes(x=perAA, y=undercount, color = equip, shape = factor(poor))) + geom_point (size=4)


MyExchangeTradedFunds = c("SPY", "TLT", "LQD", "EEM", "VNQ")
ETFPrices = yahooSeries(MyExchangeTradedFunds, from='2010-08-01', to='2015-07-31')

head(ETFPrices)

YahooPricesToReturns = function(series) {
  mycols = grep('Adj.Close', colnames(series))
  closingprice = series[,mycols]
  N = nrow(closingprice)
  percentreturn = as.data.frame(closingprice[2:N,]) / as.data.frame(closingprice[1:(N-1),]) - 1
  mynames = strsplit(colnames(percentreturn), '.', fixed=TRUE)
  mynames = lapply(mynames, function(x) return(paste0(x[1], ".PctReturn")))
  colnames(percentreturn) = mynames
  as.matrix(na.omit(percentreturn))
}

ETFReturns = YahooPricesToReturns(ETFPrices)
pairs(ETFReturns)
summary(ETFReturns)

par(mfrow=c(3,2))
plot(ETFReturns[,1], type='l', ylim=c(-.05, .05))
plot(ETFReturns[,2], type='l', ylim=c(-.05, .05))
plot(ETFReturns[,3], type='l', ylim=c(-.05, .05))
plot(ETFReturns[,4], type='l', ylim=c(-.05, .05))
plot(ETFReturns[,5], type='l', ylim=c(-.05, .05))

par(mfrow=c(3,2))
hist(ETFReturns[,1], 25, xlim=c(-.10, .10), xlab="SPY Returns")
hist(ETFReturns[,2], 25, xlim=c(-.10, .10), xlab="TLT Returns")
hist(ETFReturns[,3], 25, xlim=c(-.10, .10), xlab="LQD Returns")
hist(ETFReturns[,4], 25, xlim=c(-.10, .10), xlab="EEM Returns")
hist(ETFReturns[,5], 50, xlim=c(-.10, .10), xlab="VNQ Returns")

ReturnSDs = rep(0,5)

for (i in 1:length(ReturnSDs)){
  ReturnSDs[i] = sd(ETFReturns[,i])
}

ReturnIQRs = rep(0,5)

for (i in 1:length(ReturnIQRs)){
  ReturnIQRs[i] = quantile(ETFReturns[,i], .75) - quantile(ETFReturns[,i], .25)
}

ReturnSDs
ReturnIQRs

EvenSplitSimulation = foreach(i=1:10000, .combine='rbind') %do% {
  totalwealth = 100000
  weights = c(0.2, 0.2, 0.2, 0.2, 0.2)
  holdings = weights * totalwealth
  n_days = 20
  for(today in 1:n_days) {
    return.today = resample(ETFReturns, 1, orig.ids=FALSE)
    holdings = holdings + holdings*return.today
    totalwealth = sum(holdings)
    holdings = weights*totalwealth
  }
  totalwealth
}

summary(EvenSplitSimulation)
par(mfrow=c(1,1))
hist(EvenSplitSimulation[,1], 25)
sd(EvenSplitSimulation)


AlphaLevels = rep(0,3)
AlphaLevels[1] = quantile(EvenSplitSimulation[,n_days],0.05) - 100000


set.seed(722)
SafeSimulation = foreach(i=1:10000, .combine='rbind') %do% {
  totalwealth = 100000
  weights = c(0.1, 0.1, 0.8)
  holdings = weights * totalwealth
  n_days = 20
  wealthtracker = rep(0, n_days) # Set up a placeholder to track total wealth
  for(today in 1:n_days) {
    return.today = resample(ETFReturns[,c(1,2,3)], 1, orig.ids=FALSE)
    holdings = holdings + holdings*return.today
    totalwealth = sum(holdings)
    wealthtracker[today] = totalwealth
    holdings = weights*totalwealth
  }
  totalwealth
}

summary(SafeSimulation)
par(mfrow=c(1,1))
hist(SafeSimulation[,1], 25)
sd(SafeSimulation)

AlphaLevels[2] = quantile(SafeSimulation,0.05) - 100000
AlphaLevels[2]

set.seed(722)
PossibleWeights = seq(0, 1, length = 11)
ReturnValues = rep(0, 10000)
RiskySimulation = foreach(i=1:11, .combine = 'rbind') %do% {
  for(j in 1:10000) {
    totalwealth = 100000
    weights = c(PossibleWeights[i], 1-PossibleWeights[i])
    holdings = weights * totalwealth
    n_days = 20
    wealthtracker = rep(0, n_days) # Set up a placeholder to track total wealth
    for(today in 1:n_days) {
      return.today = resample(ETFReturns[,c(4,5)], 1, orig.ids=FALSE)
      holdings = holdings + holdings*return.today
      totalwealth = sum(holdings)
      wealthtracker[today] = totalwealth
      holdings = weights*totalwealth
    }
    ReturnValues[j] = totalwealth
  }
  ReturnValues
}



par(mfrow=c(3, 4))
hist(RiskySimulation[1,], 25)
hist(RiskySimulation[2,], 25)
hist(RiskySimulation[3,], 25)
hist(RiskySimulation[4,], 25)
hist(RiskySimulation[5,], 25)
hist(RiskySimulation[6,], 25)
hist(RiskySimulation[7,], 25)
hist(RiskySimulation[8,], 25)
hist(RiskySimulation[9,], 25)
hist(RiskySimulation[10,], 25)
hist(RiskySimulation[11,], 25)

AlphaVsOneMinusAlpha = matrix(0, nrow=11, ncol=2)

for (i in 1:length(AlphaVsOneMinusAlpha[,1])) {
  AlphaVsOneMinusAlpha[i,1] = quantile(RiskySimulation[i,],0.05) - 100000
}

for (i in 1:length(AlphaVsOneMinusAlpha[,2])) {
  AlphaVsOneMinusAlpha[i,2] = quantile(RiskySimulation[i,],0.95) - 100000
}

AlphaVsOneMinusAlpha
par(mfrow=c(1, 1))
plot(AlphaVsOneMinusAlpha, main="Returns at Alpha Value .05 vs Returns at Alpha Value .95", xlab="Return at Alpha = .05", ylab = "Return at Alpha = .95")


AlphaLevels[3] = quantile(RiskySimulation[10,],0.05) - 100000

AlphaLevels


Wine = read.csv("data/wine.csv")
head(Wine)
summary(Wine)
dim(Wine)
pairs(Wine)
WineScaled = scale(Wine[,-(c(12,13))], center=TRUE, scale=TRUE)

mu = attr(WineScaled,"scaled:center")
sigma = attr(WineScaled,"scaled:scale")

WineDistanceMatrix = dist(WineScaled, method="euclidean")
HierarchicalWineClustering = hclust(WineDistanceMatrix, method="single")
par(mfrow=c(1,1))
plot(HierarchicalWineClustering, cex=0.8)

WineClusters = cutree(HierarchicalWineClustering, h=4)
summary(factor(WineClusters))

PossibleKs = matrix(0, nrow=19, ncol=2)
PossibleKs[,1] = 2:20

set.seed(722)
for(i in 1:19){
  WineKMeanClusters = kmeans(WineScaled, i, nstart=50)
  CHk = ((WineKMeanClusters$betweenss/(i-1))/(WineKMeanClusters$tot.withinss/(nrow(WineScaled)-i)))
  PossibleKs[i,2] = CHk 
}
plot(PossibleKs, xlab="# of Clusters", Ylab="CH(k)", main="Choosing K to Maximize CH(k)")

set.seed(722)
WineKMeanClusters = kmeans(WineScaled, 4, nstart=50)
Wine$cluster = factor(WineKMeanClusters$cluster)
head(Wine)
summary(factor(WineKMeanClusters$cluster))

qplot(fixed.acidity, alcohol, data = Wine, color=factor(WineKMeanClusters$cluster), shape=Wine$color, size=2)
qplot(chlorides, citric.acid, data = Wine, color=factor(WineKMeanClusters$cluster), shape=Wine$color, size=2)

ClusterVsColor = xtabs(~cluster + color, data=Wine)
ClusterVsColor
ClusterVsColorProp = prop.table(ClusterVsColor, margin=1)
ClusterVsColorProp

WinePrincipalComponent = prcomp(WineScaled)
loadings = WinePrincipalComponent$rotation
scores = WinePrincipalComponent$x
loadings[,1:2]
head(scores[,1:2])

qplot(scores[,1], scores[,2], color=Wine$color, xlab='Component 1', ylab='Component 2')
qplot(scores[,1], scores[,2], color=Wine$quality, xlab='Component 1', ylab='Component 2', size=1) + scale_color_gradient(low="blue", high="orange")


#Market Segmentation
##Using Social Media Data to Find Similar Customers


SocialMedia = read.csv("data/social_marketing.csv", header=TRUE, row.names=1)

# Normalize phrase counts to phrase frequencies
SocialMediaFrequencies = SocialMedia/rowSums(SocialMedia)

# PCA
pc2 = prcomp(SocialMediaFrequencies, scale=TRUE)
SMLoadings = pc2$rotation
SMScores = pc2$x

qplot(SMScores[,1], SMScores[,2], xlab='Component 1', ylab='Component 2')

# The top categories associated with each component
Component1Ordered = order(SMLoadings[,1])
colnames(SocialMediaFrequencies)[head(o1,25)]
colnames(SocialMediaFrequencies)[tail(o1,25)]

o2 = order(loadings[,2])
colnames(Z)[head(o2,25)]
colnames(Z)[tail(o2,25)]

PossibleKsSocial = matrix(0, nrow=19, ncol=2)
PossibleKsSocial[,1] = 2:20
set.seed(722)
for(i in 1:19){
  SocialKMeanClusters = kmeans(SocialMediaFrequencies, i, nstart=50)
  CHk = ((SocialKMeanClusters$betweenss/(i-1))/(SocialKMeanClusters$tot.withinss/(nrow(SocialMediaFrequencies)-i)))
  PossibleKsSocial[i,2] = CHk 
}
plot(PossibleKsSocial, xlab="# of Clusters", ylab="CH(k)", main="Choosing K to Maximize CH(k)")

SocialKMeanClusters = kmeans(SocialMediaFrequencies, 4, nstart=50)
SocialMedia$cluster = factor(SocialKMeanClusters$cluster)

summary(factor(SocialKMeanClusters$cluster))

sort(SocialKMeanClusters$centers[1,], decreasing=TRUE)[1:5]
sort(SocialKMeanClusters$centers[2,], decreasing=TRUE)[1:5]
sort(SocialKMeanClusters$centers[3,], decreasing=TRUE)[1:5]
sort(SocialKMeanClusters$centers[4,], decreasing=TRUE)[1:5]
