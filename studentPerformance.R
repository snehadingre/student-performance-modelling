library(plyr)
library(ggplot2)
library(moments)
library(cluster)

library(randomForest)

edu <- read.csv(file = "/Path/to/dataset/xAPI-Edu-Data.csv", stringsAsFactors = FALSE)
#Dataset can be downloaded from https://www.kaggle.com/aljarah/xAPI-Edu-Data
gender <- edu$gender
nationality <- edu$NationalITy
hands <- edu$raisedhands
birthplace <- edu$PlaceofBirth

#bar chart for gender
ggplot(data=edu,aes(x=gender)) + geom_bar() + 
  geom_text(stat='count',aes(label=..count..),vjust=-1)

###3. Visualizing student information for Fall and Spring semesters
ggplot(data=edu,aes(x=Semester)) + geom_bar() + 
  geom_text(stat='count',aes(label=..count..),vjust=-1)


###4. Bar chart for topic

ggplot(data=edu,aes(x=Topic)) + geom_bar() + 
  geom_text(stat='count',aes(label=..count..),vjust=-1)


#Parental Information
#renaming mum to Mother in Relation for more consistent naming.
edu$Relation[(edu$Relation) == "Mum" ] <- "Mother"

pResp <- edu$Relation
pSurvey <-edu$ParentAnsweringSurvey
pSat <- edu$ParentschoolSatisfaction

###5. Bar chart for parent responsible for student

ggplot(data=edu,aes(x=pResp)) + geom_bar() + 
  geom_text(stat='count',aes(label=..count..),vjust=-1)


###6. Bar chart for parent answering survery

ggplot(data=edu,aes(x=pSurvey)) + geom_bar() + 
  geom_text(stat='count',aes(label=..count..),vjust=-1)


###7.Bar chart for parent's satisfaction with school

ggplot(data=edu,aes(x=pSat)) + geom_bar() + 
  geom_text(stat='count',aes(label=..count..),vjust=-1)

###Binning/ Discretizing _raisedHands_ into low, moderate and high:


rhand <- edu$raisedhands
rhand.cluster<-kmeans(rhand, 3, nstart=20)
#storing the result of clustering
rhand.cluster<- rhand.cluster$cluster
#changing the column values to the cluster
edu$raisedhandsBinned <- rhand.cluster

min_clust1=min(edu$raisedhands[(edu$raisedhandsBinned)=="1"])
min_clust1
min_clust2=min(edu$raisedhands[(edu$raisedhandsBinned)=="2"])
min_clust2
min_clust3=min(edu$raisedhands[(edu$raisedhandsBinned)=="3"])
min_clust3


if(min_clust1<min_clust2 && min_clust1<min_clust3){
  minvalue=1
} else if(min_clust2<min_clust1 && min_clust2<min_clust3){
  minvalue=2
} else {
  minvalue=3
}
edu$raisedhandsBinned[(edu$raisedhandsBinned) == minvalue ] <- "Low"

if(min_clust1>min_clust2 && min_clust1>min_clust3){
  maxvalue=1
} else if(min_clust2>min_clust1 && min_clust2>min_clust3){
  maxvalue=2
} else {
  maxvalue=3
}
edu$raisedhandsBinned[(edu$raisedhandsBinned) == maxvalue ] <- "High"


val<-which((edu$raisedhandsBinned=="1"))
edu$raisedhandsBinned[val] <- "Moderate"
val<-which((edu$raisedhandsBinned=="2"))
edu$raisedhandsBinned[val] <- "Moderate"
val<-which((edu$raisedhandsBinned=="3"))
edu$raisedhandsBinned[val] <- "Moderate"

head(edu)

###Binning/ Discretizing _VisITedResources_ into low, moderate and high:

vRes <- edu$VisITedResources
#Discretization of Visited Resources using k means clustering
vRes.cluster<-kmeans(vRes, 3, nstart=20)
vRes.cluster
#changing the column values to the cluster
edu$visitedBinned <- vRes.cluster$cluster
min_clust1=min(edu$VisITedResources[(edu$visitedBinned)=="1"])
min_clust1
min_clust2=min(edu$VisITedResources[(edu$visitedBinned)=="2"])
min_clust2
min_clust3=min(edu$VisITedResources[(edu$visitedBinned)=="3"])
min_clust3


if(min_clust1<min_clust2 && min_clust1<min_clust3){
  minvalue=1
} else if(min_clust2<min_clust1 && min_clust2<min_clust3){
  minvalue=2
} else {
  minvalue=3
}
edu$visitedBinned[(edu$visitedBinned) == minvalue ] <- "Low"

if(min_clust1>min_clust2 && min_clust1>min_clust3){
  maxvalue=1
} else if(min_clust2>min_clust1 && min_clust2>min_clust3){
  maxvalue=2
} else {
  maxvalue=3
}
edu$visitedBinned[(edu$visitedBinned) == maxvalue ] <- "High"


val<-which((edu$visitedBinned=="1"))
edu$visitedBinned[val] <- "Moderate"
val<-which((edu$visitedBinned=="2"))
edu$visitedBinned[val] <- "Moderate"
val<-which((edu$visitedBinned=="3"))
edu$visitedBinned[val] <- "Moderate"

head(edu)

###Binning/ Discretizing _AnnouncementsView_ into low, moderate and high:

vAnn <- edu$AnnouncementsView
vAnn.cluster<-kmeans(vAnn, 3, nstart=20)
vAnn.cluster
#changing the column values to the cluster
edu$announcementsBinned <- vAnn.cluster$cluster
min_clust1=min(edu$AnnouncementsView[(edu$announcementsBinned)=="1"])
min_clust1
min_clust2=min(edu$AnnouncementsView[(edu$announcementsBinned)=="2"])
min_clust2
min_clust3=min(edu$AnnouncementsView[(edu$announcementsBinned)=="3"])
min_clust3


if(min_clust1<min_clust2 && min_clust1<min_clust3){
  minvalue=1
} else if(min_clust2<min_clust1 && min_clust2<min_clust3){
  minvalue=2
} else {
  minvalue=3
}
edu$announcementsBinned[(edu$announcementsBinned) == minvalue ] <- "Low"

if(min_clust1>min_clust2 && min_clust1>min_clust3){
  maxvalue=1
} else if(min_clust2>min_clust1 && min_clust2>min_clust3){
  maxvalue=2
} else {
  maxvalue=3
}
edu$announcementsBinned[(edu$announcementsBinned) == maxvalue ] <- "High"


val<-which((edu$announcementsBinned=="1"))
edu$announcementsBinned[val] <- "Moderate"
val<-which((edu$announcementsBinned=="2"))
edu$announcementsBinned[val] <- "Moderate"
val<-which((edu$announcementsBinned=="3"))
edu$announcementsBinned[val] <- "Moderate"

head(edu)

###Binning/ Discretizing _Discussion_ into low, moderate and high:

disc <- edu$Discussion
#Discretization of Discussion using k means clustering
disc.cluster<-kmeans(disc, 3, nstart=20)
disc.cluster
#changing the column values to the cluster
edu$DiscussBinned <- disc.cluster$cluster

min_clust1=min(edu$Discussion[(edu$DiscussBinned)=="1"])
min_clust1
min_clust2=min(edu$Discussion[(edu$DiscussBinned)=="2"])
min_clust2
min_clust3=min(edu$Discussion[(edu$DiscussBinned)=="3"])
min_clust3


if(min_clust1<min_clust2 && min_clust1<min_clust3){
  minvalue=1
} else if(min_clust2<min_clust1 && min_clust2<min_clust3){
  minvalue=2
} else {
  minvalue=3
}
edu$DiscussBinned[(edu$DiscussBinned) == minvalue ] <- "Low"

if(min_clust1>min_clust2 && min_clust1>min_clust3){
  maxvalue=1
} else if(min_clust2>min_clust1 && min_clust2>min_clust3){
  maxvalue=2
} else {
  maxvalue=3
}
edu$DiscussBinned[(edu$DiscussBinned) == maxvalue ] <- "High"


val<-which((edu$DiscussBinned=="1"))
edu$DiscussBinned[val] <- "Moderate"
val<-which((edu$DiscussBinned=="2"))
edu$DiscussBinned[val] <- "Moderate"
val<-which((edu$DiscussBinned=="3"))
edu$DiscussBinned[val] <- "Moderate"
head(edu)

##Exploring relationships

###RaisedHands vs FinalGrade

#clustered bar chart of final grade by raised hands

#table of counts for raised hands and final grade
counts <- table(edu$Class, edu$raisedhandsBinned,
                dnn=c("Final Grade", "Raised Hands"))
#creating a sumtable
sumtable <- addmargins(counts, FUN = sum)
#proportion over rows
row.margin <- round(prop.table(counts, margin = 1),4)*100
#proportion over columns
col.margin <- round(prop.table(counts,margin = 2), 4)*100

barplot(counts,
        col = c("blue", "red", "green"),
        ylim = c(0, 150),
        ylab = "Count",
        xlab = "Raised Hands",
        main = "Final grade by Raised hands",
        beside = TRUE)
legend("topright",
       c(rownames(counts)),
       col = c("blue", "red", "green"),
       pch = 15,
       title = "Final Grade")
box(which = "plot",
    lty = "solid",
    col="black")

###Discussion vs Final grade of students

counts <- table(edu$Class, edu$DiscussBinned,
                dnn=c("Final Grade", "Discussion"))

#proportion over rows
row.margin <- round(prop.table(counts, margin = 1),4)*100

#proportion over columns
col.margin <- round(prop.table(counts,margin = 2), 4)*100

#We notice that students with high performance tend to have low discussion, but we need 
# further analysis to explore this better.The overlay bar graph shows similar results.

#overlay bar chart of grades by discussion
ggplot() +
  geom_bar(data=edu,
           aes(x = factor(edu$Class),
               fill = factor(edu$DiscussBinned)),
           position = "fill") +
  scale_x_discrete("Final Grade") +
  scale_y_continuous("Percent") +
  guides(fill=guide_legend(title="Discussion")) +
  scale_fill_manual(values=c("blue", "red", "green"))

###Announcements viewing vs Final grade of students

#Table of counts for Viewing Announcements and final grade
counts <- table(edu$Class, edu$announcementsBinned,
                dnn=c("Final Grade", "Viewing Announcements"))
#proportion over rows
row.margin <- round(prop.table(counts, margin = 1),4)*100
#proportion over columns
col.margin <- round(prop.table(counts,margin = 2), 4)*100

#clustered bar chart of Viewing Announcements by Final grade
barplot(t(counts),
        col = c("blue", "red", "green"),
        ylim = c(0, 150),
        ylab = "Counts",
        xlab = "Final Grades",
        main = "Viewing Announcements Count by Final Grade",
        beside = TRUE)
legend("topright",
       c(colnames(counts)),
       col = c("blue", "red", "green"),
       pch = 15,
       title = "Viewing Announcements")
box(which = "plot",
    lty = "solid",
    col="black")

###Resources Visted by students vs Final grade of students


counts <- table(edu$Class, edu$visitedBinned,
                dnn=c("Final Grade", "Visiting Resources"))
#proportion over rows
row.margin <- round(prop.table(counts, margin = 1),4)*100
#proportion over columns
col.margin <- round(prop.table(counts,margin = 2), 4)*100

#overlay bar chart of grades by viewing announcements
ggplot() +
  geom_bar(data=edu,
           aes(x = factor(edu$Class),
               fill = factor(edu$visitedBinned)),
           position = "fill") +
  scale_x_discrete("Final Grade") +
  scale_y_continuous("Percent") +
  guides(fill=guide_legend(title="Visiting Resources")) +
  scale_fill_manual(values=c("blue", "red", "green"))

#It is made very obvious by this chart that students with low grades did not 
# visit the resources frequently.
#Students with high performance visited moderately.

###Absence from classes vs Final grade of students

#Table of counts for Absence and final grade
counts <- table(edu$Class, edu$StudentAbsenceDays,
                dnn=c("Final Grade", "Absence"))
#proportion over rows
row.margin <- round(prop.table(counts, margin = 1),4)*100
#proportion over columns
col.margin <- round(prop.table(counts,margin = 2), 4)*100
#clustered bar chart of final grade by Absence
barplot(counts,
        col = c("blue", "red", "green"),
        ylim = c(0, 150),
        ylab = "Count",
        xlab = "Absence",
        main = "Final grade by Absence",
        beside = TRUE)
legend("topright",
       c(rownames(counts)),
       col = c("blue", "red", "green"),
       pch = 15,
       title = "Final Grade")
box(which = "plot",
    lty = "solid",
    col="black")

#It is made very obvious by this chart that absence above 7 days leads to low 
#performance.
#Students with high performance have very low absence days.

##Partitioning dataset into test and train dataset

#Partition the data into 75% training data and 25% testing data
edu$part <- runif(length(edu$Class),
                  min = 0,
                  max = 1)

training_edu <- edu[edu$part <= 0.75,]
testing_edu <- edu[edu$part > 0.75,]

#remove partition variable from training dataset: $part (22nd column)
training_edu<-training_edu[,-22]

#remove partition variable from testing dataset: $part (22nd column)
testing_edu<-testing_edu[,-22]
###Check validilty of partition for academic information:

testing_acad<-table(testing_edu$Semester)
training_acad<-table(training_edu$Semester)

table_academic <- as.table(rbind(training_acad,
                                 testing_acad))
dimnames(table_academic) <- list(Data.Set =
                                   c("Training Set", "Test Set"),
                                 Semester = c("Fall","Spring"))
Xsq_data <- chisq.test(table_academic)
# Show the test statistic,p-value, expected frequencies

Xsq_data$statistic  #1.013271

Xsq_data$p.value #0.3141205

Xsq_data$expected

###Check validilty of partition for behavioral information:


testing_behav<-table(testing_edu$DiscussBinned)
training_behav<-table(training_edu$DiscussBinned)

table_behavioral <- as.table(rbind(training_behav,
                                   testing_behav))
dimnames(table_behavioral) <- list(Data.Set =
                                     c("Training Set", "Test Set"),
                                   Discussion = c("High","Low","Moderate"))

Xsq_data <- chisq.test(table_behavioral)
# Show the test statistic,
# p-value, expected frequencies
Xsq_data$statistic  #1.770226
Xsq_data$p.value #0.4126674
Xsq_data$expected 
#Discussion
#Data.Set            High       Low   Moderate
#Training Set    125.59792 121.08542 114.31667
#Test Set       41.40208     39.91458     37.68333

#We can see that p value is greater than 0.05.
#Therefore, we accept the null hypothesis.Hence, the partition is valid.There is no
#significant difference in the proportions.

###Random Forest

str(training_edu)

#Change char datatype to factor for the following columns
training_edu$NationalITy<-as.factor(training_edu$NationalITy)
training_edu$Relation<-as.factor(training_edu$Relation)
training_edu$Topic<-as.factor(training_edu$Topic)
training_edu$StudentAbsenceDays<-as.factor(training_edu$StudentAbsenceDays)
training_edu$Class<-as.factor(training_edu$Class)
training_edu$Semester<-as.factor(training_edu$Semester)

set.seed(123)
modelRandom<-randomForest(Class~VisITedResources+AnnouncementsView+Relation+Topic+raisedhands+Discussion+StudentAbsenceDays,data=training_edu,mtry=2,ntree=450,importance=TRUE)

#Plot varibale importance
varImpPlot(modelRandom,type = 2)

#OOB estimate of error: 23.8%
modelRandom

