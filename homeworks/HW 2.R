library(RCurl)
library(ggplot2)
library(reshape)
library(plyr)
library(tm)
library(caret)
library(kknn)
library(e1071)
library(randomForest)
library(arules)

AirportURLString = getURL("https://raw.githubusercontent.com/jacebarton/STA380/master/data/ABIA.csv", ssl.verifypeer=0L, followlocation = 1L)
Airport = read.csv(text=AirportURLString)
summary(Airport)

DepartureDelay <- data.frame(carrier=Airport$UniqueCarrier, delay=Airport$DepDelay, leaving=Airport$Origin)
head(DepartureDelay)
DepartureDelay = DepartureDelay[DepartureDelay$leaving == "AUS", ]
DepartureDelay = na.omit(DepartureDelay)
summary(DepartureDelay)
CarrierDepartureDelays = ddply(DepartureDelay, ~carrier, summarise, mean=mean(delay), sd=sd(delay))

ArrivalDelay <- data.frame(carrier=Airport$UniqueCarrier, delay=Airport$ArrDelay, arriving=Airport$Dest)
head(DepartureDelay)
ArrivalDelay = ArrivalDelay[ArrivalDelay$arriving == "AUS", ]
ArrivalDelay = na.omit(ArrivalDelay)
summary(ArrivalDelay)
CarrierArrivalDelays = ddply(ArrivalDelay, ~carrier, summarise, mean=mean(delay), sd=sd(delay))

CarrierDelays = merge(CarrierArrivalDelays, CarrierDepartureDelays, by="carrier")
CarrierDelays$CarrierNames = c("Pinnacle", "American", "JetBlue", "Continental", "Delta", "AtlanticSE", "Frontier", "Envoy", "Northwest", "Comair", "SkyWest", "United", "US", "Southwest", "ExpressJet", "Mesa")

CarrierDelays$Count = summary(Airport$UniqueCarrier)
colnames(CarrierDelays) = c("CarrierCode", "MeanDepartureDelay", "SDDepartureDelay", "MeanArrivalDelay", "SDArrivalDelay", "CarrierNames", "Count")
CarrierDelays

ggplot(CarrierDelays, aes(x=MeanArrivalDelay, y=MeanDepartureDelay, label=CarrierNames)) + annotate("rect", xmin = -Inf, xmax = mean(CarrierDelays$MeanArrivalDelay), ymin = -Inf, ymax = mean(CarrierDelays$MeanDepartureDelay), fill= "green")  + 
  annotate("rect", xmin = -Inf, xmax = mean(CarrierDelays$MeanArrivalDelay), ymin = mean(CarrierDelays$MeanDepartureDelay), ymax = Inf , fill= "yellow") + 
  annotate("rect", xmin = mean(CarrierDelays$MeanArrivalDelay), xmax = Inf, ymin = -Inf, ymax = mean(CarrierDelays$MeanDepartureDelay), fill= "yellow") + 
  annotate("rect", xmin = mean(CarrierDelays$MeanArrivalDelay), xmax = Inf, ymin = mean(CarrierDelays$MeanDepartureDelay), ymax = Inf, fill= "red") + 
  geom_point(aes(size=CarrierDelays$Count)) + geom_vline(xintercept=mean(CarrierDelays$MeanArrivalDelay)) + geom_hline(yintercept=mean(CarrierDelays$MeanDepartureDelay)) + scale_size_continuous(range=c(3,15)) + geom_text(size=3, hjust=1)

ggplot(CarrierDelays, aes(x=MeanArrDelay, y=MeanDepDelay, color=CarrierNames, label=CarrierNames)) + geom_point(aes(size=CarrierDelays$Count)) + geom_vline(xintercept=mean(CarrierDelays$MeanArrDelay)) + geom_hline(yintercept=mean(CarrierDelays$MeanDepDelay)) + scale_size_continuous(range=c(3,15)) + geom_text(size=3, hjust=1) +
  annotate("text", x = 13, y = 14, label="Delayed Both Ways")


readerPlain = function(fname){
  readPlain(elem=list(content=readLines(fname)), 
            id=fname, language='en') }

## Rolling two directories together into a single corpus
#Get all files
train_author_dirs = Sys.glob('../data/ReutersC50/C50train/*')
train_file_list = NULL
train_labels = NULL
#build a single corpus
for(author in train_author_dirs) {
  author_name = substring(author, first=29)
  files_to_add = Sys.glob(paste0(author, '/*.txt'))
  train_file_list = append(train_file_list, files_to_add)
  train_labels = append(train_labels, rep(author_name, length(files_to_add)))
}

# Need a more clever regex to get better names here
train_all_docs = lapply(train_file_list, readerPlain) 
names(train_all_docs) = sub('.txt', '', names(train_all_docs))

train_corpus = Corpus(VectorSource(train_all_docs))
names(train_corpus) = train_file_list

# Clean up tokens in corpus
train_corpus = tm_map(train_corpus, tolower) # make everything lowercase
train_corpus = tm_map(train_corpus, removeNumbers) # remove numbers
train_corpus = tm_map(train_corpus, removePunctuation) # remove punctuation
train_corpus = tm_map(train_corpus, stripWhitespace) ## remove excess white-space
train_corpus = tm_map(train_corpus, removeWords, stopwords("SMART"))

Train_Document_Term_Matrix = DocumentTermMatrix(train_corpus)
Train_Document_Term_Matrix # some basic summary statistics
Train_Document_Term_Matrix = removeSparseTerms(Train_Document_Term_Matrix, 0.975)
inspect(Train_Document_Term_Matrix[1:10,1:5])
# Now a dense matrix
Train_Matrix = as.matrix(Train_Document_Term_Matrix)

## Rolling two directories together into a single corpus
#get all files
test_author_dirs = Sys.glob('../data/ReutersC50/C50test/*')
test_file_list = NULL
test_labels = NULL
#build a single corpus
for(author in test_author_dirs) {
  author_name = substring(author, first=28)
  files_to_add = Sys.glob(paste0(author, '/*.txt'))
  test_file_list = append(test_file_list, files_to_add)
  test_labels = append(test_labels, rep(author_name, length(files_to_add)))
}

# Need a more clever regex to get better names here
test_all_docs = lapply(test_file_list, readerPlain) 
names(test_all_docs) = sub('.txt', '', names(test_all_docs))

test_corpus = Corpus(VectorSource(test_all_docs))
names(test_corpus) = test_file_list

# Clean up tokens in corpus
test_corpus = tm_map(test_corpus, tolower) # make everything lowercase
test_corpus = tm_map(test_corpus, removeNumbers) # remove numbers
test_corpus = tm_map(test_corpus, removePunctuation) # remove punctuation
test_corpus = tm_map(test_corpus, stripWhitespace) ## remove excess white-space
test_corpus = tm_map(test_corpus, removeWords, stopwords("SMART"))

Test_Document_Term_Matrix = DocumentTermMatrix(test_corpus, control = list(dictionary=Terms(Train_Document_Term_Matrix)) )
Test_Document_Term_Matrix # some basic summary statistics
Test_Document_Term_Matrix = removeSparseTerms(Test_Document_Term_Matrix, 0.975)
inspect(Test_Document_Term_Matrix[1:10,1:5])
# Now a dense matrix
Test_Matrix = as.matrix(Test_Document_Term_Matrix)


NaiveBayesModel = naiveBayes(Train_Matrix, as.factor(train_labels), laplace=1)

NaiveBayesPredict = predict(object=NaiveBayesModel,newdata = Test_Matrix)


NaiveBayesResults = as.data.frame(table(NaiveBayesPredict,test_labels))
NaiveBayesResultsTable = table(NaiveBayesPredict, test_labels)
total_correct_vector = rep(0, 50)
predicted_per_author = rep(0,50)
for (i in 1:length(NaiveBayesResultsTable[1,])){
  total_correct_vector[i] = NaiveBayesResultsTable[i, i]
  predicted_per_author[i] = sum(NaiveBayesResultsTable[i,])
}

CorrectByAuthor = data.frame(row.names(NaiveBayesResultsTable), total_correct_vector)
CorrectByAuthor

PredictedForAuthor = data.frame(row.names(NaiveBayesResultsTable), predicted_per_author)
SortedPredictedForAuthor = PredictedForAuthor[order(-PredictedForAuthor$predicted_per_author),]
SortedPredictedForAuthor

OverallClassificationRate = sum(total_correct_vector)/2500
OverallClassificationRate

ClassificationRateByAuthor = data.frame(row.names(NaiveBayesResultsTable), total_correct_vector/predicted_per_author)
ClassificationRateByAuthor

TrainDataFrame = as.data.frame(Train_Matrix)
TestDataFrame = as.data.frame(Test_Matrix)




#KNearestModel = kknn(PredictAuthor~.,TrainDataFrame,TestDataFrame,k=5,kernel="rectangular")

#AuthorBoost = maboost(PredictAuthor~.,data=TrainDataFrame,iter=5000, nu=0.2)

AuthorRandomForest = randomForest(x=TrainDataFrame, y=as.factor(train_labels), ntree=50, mtry=30)

PredictedAuthor = predict(AuthorRandomForest, newdata = TestDataFrame)

RandomForestResults = as.data.frame(table(PredictedAuthor,test_labels))
RandomForestResultsTable = table(PredictedAuthor, test_labels)
RF_total_correct_vector = rep(0, 50)
RF_predicted_per_author = rep(0,50)
for (i in 1:length(RandomForestResultsTable[1,])){
  RF_total_correct_vector[i] = RandomForestResultsTable[i, i]
  RF_predicted_per_author[i] = sum(RandomForestResultsTable[i,])
}

RFCorrectByAuthor = data.frame(row.names(RandomForestResultsTable),RF_total_correct_vector)
RFCorrectByAuthor

RFPredictedForAuthor = data.frame(row.names(RandomForestResultsTable), RF_predicted_per_author)
RFSortedPredictedForAuthor = RFPredictedForAuthor[order(-RFPredictedForAuthor$RF_predicted_per_author),]
RFSortedPredictedForAuthor

RFOverallClassificationRate = sum(RF_total_correct_vector)/2500
RFOverallClassificationRate

RFImportance = as.data.frame(importance(AuthorRandomForest))
RFImportanceDataFrame = data.frame(row.names(RFImportance), RFImportance)
SortRFImportance = RFImportanceDataFrame[order(-RFImportanceDataFrame$MeanDecreaseGini),]
head(SortRFImportance)


Groceries = read.transactions("../data/groceries.txt", format = "basket", sep=",")
summary(Groceries)

GroceriesRules <- apriori(Groceries, parameter=list(support=.005, confidence=.5, maxlen=4))
inspect(GroceriesRules)

inspect(subset(GroceriesRules, subset=lift > 3 & support > .01))
inspect(subset(GroceriesRules, subset=support > 0.015))
inspect(subset(GroceriesRules, subset=support > .02 & confidence > 0.6))