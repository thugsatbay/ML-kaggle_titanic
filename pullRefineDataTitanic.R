#Loading all data
train<-read.csv("train.csv",header = TRUE)
test<-read.csv("test.csv",header=TRUE)
test$Survived<-NA

#Adding all data to make it in standard form
#train<-train[,c(1,seq(3,12))]
train<-rbind(train,test)

#Gets the Prefix Of Name that helps to tell the status of person
train$prefix<-sub('(.*, )','',train$Name) 
train$prefix<-sub('(\\..*)','',train$prefix)

#Correecting Prefix Data
train$prefix[train$prefix=="Mlle"]<-'Miss'
train$prefix[train$prefix=="Ms"]<-'Miss'
train$prefix[train$prefix=="Mme"]<-'Mr'

#The new parameter created count
summaryOfPrefix<-summary(factor(train$prefix))
summaryOfPrefix
rarePrefix<-names(summaryOfPrefix[summaryOfPrefix<10])
train$prefix[train$prefix %in% rarePrefix]<-'RarePrefix'
#Check The New Prefix Column Data
summary(factor(train$prefix))

#Family Parameter
train$sizeFamily<-train$Parch+train$SibSp+1

#
##
###Graphs Visualization Learning
##
#
#A graph To demonstrate the Relation between survival and your status as a male, female
ggplot(train[1:891,], aes(x = prefix,fill=factor(Survived))) +
  geom_bar(stat='count', position='dodge',color="green",alpha=.9) +
  labs(x = 'Family Size')
#A pie chart about the ratio of married to unmarried women
femalePrefixses<-table(train[train$Sex=="female",13])
femalePrefixsesCount<-as.vector(femalePrefixses)
femalePrefixsesTitle<-names(femalePrefixses)
pctF<-round((femalePrefixsesCount/sum(femalePrefixsesCount))*100)
pctF<-paste(femalePrefixsesTitle,pctF,sep=" ")
pctF<-paste(pctF,"%",sep="")
pie(femalePrefixsesCount, labels = pctF, col=rainbow(length(pctF)), main="Pie Chart of Female Married, Younger/Older Distribution")
#A pie chart about the ratio of men to children
malePrefixses<-table(train[train$Sex=="male",13])
malePrefixsesCount<-as.vector(malePrefixses)
malePrefixsesTitle<-names(malePrefixses)
Mpct<-round((malePrefixsesCount/sum(malePrefixsesCount))*100)
Mpct<-paste(malePrefixsesTitle,Mpct,sep=" ")
Mpct<-paste(Mpct,"%",sep="")
pie(malePrefixsesCount, labels = Mpct, col=rainbow(length(Mpct)), main="Pie Chart of Male and Boys Distribution")
#A graph To demonstrate the Relation between survival and group size
ggplot(train[1:891,], aes(x = sizeFamily,fill=factor(Survived))) +
  geom_bar(stat='count', position='stack',color="green",alpha=.9) +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size')
# Show family size by survival using a mosaic plot
train$familySizeDimension[train$sizeFamily == 1] <- 'alone'
train$familySizeDimension[train$sizeFamily < 5 & train$sizeFamil > 1] <- 'small'
train$familySizeDimension[train$sizeFamily > 4] <- 'big'
mosaicplot(table(train$familySizeDimension, train$Survived), main='Family Size by Survival', shade=TRUE)

#Finding the deck information that is necessary as data is incomplete we will be only be going with deck and not room numbers
train$Deck<-factor(sapply(train$Cabin, function(x) strsplit(as.vector(x),NULL)[[1]][1]))

#For 1044 no fare is given so we estimate the Price
summary(train$Fare[train$Embarked=="S" & train$Pclass=="3" & train$PassengerId!=1044 & train$prefix=="Mr"])
train$Fare[1044]<-mean(8.050,7.925)

#
##
###Estimated Deck Using Fare Pclass and Embarked, should also have used family size
##
#
tempMin<-max(as.vector(train$Fare),na.rm=TRUE)
embarkedMapping_Info<-data.frame(Embarked=c("C","Q","S"),Mapping=c(1,25,49))
pClassMapping_Info<-data.frame(PClass=c("1","2","3"),Mapping=c(0,8,16))
pDeckMapping_Info<-data.frame(Deck=seq(8),Mapping=c("A","B","C","D","E","F","G","T"))
for(y in seq(length(train$PassengerId))){
print (y)
  if(is.na(train$Deck[y])){
  store_info<-sapply(c("C","Q","S"), function(x) (sapply(c("1","2","3"), function(y) (sapply(c("A","B","C","D","E","F","G","T"), function (z) train$Fare[train$Deck==z & (!is.na(train$Deck)) & train$Pclass==y & train$Embarked==x])))))
  valMod<-embarkedMapping_Info$Mapping[embarkedMapping_Info$Embarked==as.vector(train$Embarked[y])]+pClassMapping_Info$Mapping[pClassMapping_Info$PClass==as.vector(train$Pclass[y])]
  meanPossibleCostDeck<-sapply(seq(8),function(x) mean(store_info[valMod:(valMod+7)][[x]]))
  if(!is.na(train$Fare[y])){
  for (x in seq(8)){
    if(!is.nan(meanPossibleCostDeck[x])){
      if(abs(meanPossibleCostDeck[x]-as.vector(train$Fare[y]))<tempMin){
          train$Deck[y]<-pDeckMapping_Info$Mapping[pDeckMapping_Info$Deck==x]
        }
      }
    }}
}
}