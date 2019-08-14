dat = read.csv(file="./Desktop/STAT844/project/FiveCitiePMData/raw/GuangzhouPM20100101_20151231.csv",header = TRUE,sep=",",row.names=NULL  )
colnames(dat)
# rownames(sh_data) <- c()
# rownames(bj_data) <- c()
# rownames(cd_data) <- c()
# rownames(gz_data) <- c()
# rownames(sy_data) <- c()



# dimensions are not identical
mydata =  dat #rbind(sh_data,sy_data,bj_data,gz_data,cd_data)
mydata$No=NULL
# "year","month","day","hour","season","DEWP","HUMI","PRES","TEMP","cbwd","Iws","precipitation","Iprec","city"))
#colnames(mydata)
#head(mydata)

#sum(is.na(mydata$PM_US.Post))


na.omit(mydata)
# list types for each attribute
#sapply(updated_myData, class)
#levels(updated_myData$cbwd)
#levels(updated_myData$city)

levels(df$cbwd)[1] <- "SW"
# sort it to NE, NW, SE, SW
df$cbwd <- factor(df$cbwd, levels = c("NE", "NW", "SE", "SW"))
summary(df$cbwd)


library(ggplot2)
library(lubridate)
#random forest 
require(randomForest)
require(MASS)
require(caTools)
require(dplyr)
require(ggplot2)
require(gam)
require(randomForest)
require(caret)
require(e1071)
library(caret)
library(e1071)
set.seed(101) 
df <- data.frame(mydata)
colnames(df)[which(names(df) == "PM_US.Post")] <- "pm2.5"

###
summary(df)
library(summarytools)
view(dfSummary(df, graph.magnif=0.5,isplay.labels=FALSE,max.distinct.values = 5))

library(psych)
describe.by(df)

library(Hmisc)
describe(df)
####


sample = sample.split(df$pm2.5, SplitRatio = .75)
train = subset(df, sample == TRUE)
test  = subset(df, sample == FALSE)


trControl = trainControl(
  method = "cv", 
  number = 5,
  verboseIter = TRUE
)

rf=randomForest(formula = train$pm2.5 ~ . , data = train)
print(rf)
rf_default <- train(train$pm2.5~.,
                    data = train,
                    method = "rf",
                    trControl = trControl)
# Print the results
print(rf_default)


model <- lm(pm2.5 ~., data = train)
# Make predictions and compute the R2, RMSE and MAE
predictions <- model %>% predict(test)
data.frame( R2 = R2(predictions, test$pm2.5),
            RMSE = RMSE(predictions, test$pm2.5),
            MAE = MAE(predictions, test$pm2.5))

RMSE(predictions, test$pm2.5)/mean(test$pm2.5)



# Define training control
train.control <- trainControl(method = "LOOCV")
trControl = trainControl(
  method = "cv", 
  number = 5,
  verboseIter = TRUE
)
# Train the model
model.rf <- train(pm2.5 ~., data = train, method = "rf",
               trControl = trControl)
# Summarize the results

model <- train(pm2.5 ~., data = train, method = "rlm",
                   trControl = train.control)


print(model)

score = list()

LOOCV_function = function(x,label){
  for(i in 1:nrow(x)){
    training = x[-i,]
    model = train(pm2.5~., data=train, trControl=train_control, method="nb")
      validation = x[i,]
    pred = predict(model, validation[,setdiff(names(validation),label)])
    score[[i]] = rmse(pred, validation[[label]]) # score/error of ith fold
  }
  return(unlist(score)) # returns a vector
}


# Fit Naive Bayes Model


model.lm<- train(
  train$pm2.5~ ., 
  train, # <- new!
  method = "lm",
  trControl = trainControl(
    method = "cv", 
    number = 5,
    verboseIter = TRUE
  )
)

print(model.lm)

##smnooth spline
set.seed(1234)

rows = which(df["Iprec"]>200)
sh=0
count = 0
for (row in rows){
  print(df[row,])
  count=count+1
  if df[row,"city"] == "shanghai":
     sh=sh+1
}


rf.mdl <- randomForest(y=train$pm2.5, x=train[,1:13])
( rf.cv <- rf.crossValidation(rf.mdl, train[,1:13], p=0.10, n=99, ntree=501) )
par(mfrow=c(2,2))
plot(rf.cv)  
plot(rf.cv, stat = "mse")
plot(rf.cv, stat = "var.exp")
plot(rf.cv, stat = "mae")

mod <- lm(df$pm2.5~ ., data=df)
cooksd <- cooks.distance(mod)

plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels

influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])  # influential row numbers
head(df[influential, ]) 

summary(df$DEWP)

table(df[influential, ]$year,df[influential, ]$city)
table(df$year,df$city)

### remove ouliners

out_humi = filter(df, df$HUMI <= -100)
out_humi
out_DEWP = filter(df, df$DEWP <= -100)
out_DEWP 
df = filter(df,df$HUMI >= -100 & df$DEWP>=-100)
colnames(df)

ggplot(df, aes(df[,6], pm2.5)) +
  geom_point(colour="firebrick", alpha ="0.5") +
  xlab("DEWP")

ggplot(df, aes(df[,7], pm2.5)) +
  geom_point(colour="firebrick", alpha ="0.5") +
  xlab("HUMI")


ggplot(df, aes(df[,10], pm2.5)) +
  geom_point() +
  xlab("cbwd")


df1 <- df %>%
  mutate(date = make_date(year, month, day),
         datetime = make_datetime(year, month, day, hour)) %>%
  arrange(datetime)


which(colnames(df) == "PM_US.Post")
which(colnames(df) == "Ir")
colnames(df)

par(mfrow=c(2,2))

for(i in 6:) {
  if(is.factor(df1[,i])) {
    print(ggplot(df1, aes(df1[, i])) +
            geom_histogram(stat = "count") +
            xlab(colnames(df1)[i]))
  }
  else {
    print(ggplot(df1, aes(df1[, i])) +
            geom_histogram(binwidth = 2) +
            xlab(colnames(df1)[i]))
  }
}


par(mfrow=c(2,2))
for(i in 1:13) {
  if(!is.factor(df[,i])) {
    print(paste("pm2.5 & ", colnames(df)[i], sep = ""))
    print(cor(df$pm2.5, df[, i], use = "complete.obs"))
    print(ggplot(df, aes(df[,i], pm2.5)) +
            geom_point() +
            xlab(colnames(df)[i]))
  }
  else {
    print(ggplot(df, aes(df[,i], pm2.5)) +
            geom_boxplot() +
            xlab(colnames(df)[i]))
 
  }
}


City = factor(df$city)
colnames(df)
p1 = ggplot(df, aes(df[,1], pm2.5)) + geom_point(aes(colour = City),position = "jitter") + xlab("year")


h1 = ggplot(df, aes(df[, 1])) +
  geom_histogram(binwidth = 0.5) +
  xlab("year")

p2 = ggplot(df, aes(df[,2], pm2.5)) +
  geom_point(aes(colour = City,alpha = 0.5) + xlab("month")


h2 = ggplot(df, aes(df[, 2])) +
  geom_histogram(binwidth = 0.5) + xlab("month")


p3 = ggplot(df, aes(df[,3], pm2.5)) +
  geom_point() + xlab("day")


h3 = ggplot(df, aes(df[, 3])) +
  geom_histogram(binwidth = 0.5) + xlab("day")



p4 = ggplot(df, aes(df[,4], pm2.5)) +
  geom_point() + xlab("hour")+geom_jitter()

h4 =ggplot(df, aes(df[, 4])) +
  geom_histogram(binwidth = 0.5) + xlab("hour")



p5 = ggplot(df, aes(df[,5], pm2.5)) +
  geom_point() + xlab("season")+geom_jitter()


p6 = ggplot(df, aes(df[,6], pm2.5)) +
  geom_point() + xlab("DEWP")+geom_jitter()

p7 = ggplot(df, aes(df[,7], pm2.5)) +
  geom_point() + xlab("HUMI")+geom_jitter()


p8 = ggplot(df, aes(df[,8], pm2.5)) +
  geom_point() + xlab("PRES")+geom_jitter()


p9 = ggplot(df, aes(df[,9], pm2.5)) +
  geom_point() + xlab("TEMP")


p10 = ggplot(df, aes(df[,10], pm2.5)) +
  geom_point() + xlab("cbwd")

p11 = ggplot(df, aes(df[,11], pm2.5)) +
  geom_point() + xlab("Iws")


p12 = ggplot(df, aes(df[,12], pm2.5)) +
  geom_point() + xlab("precipitation")

p13 = ggplot(df, aes(df[,13], pm2.5)) +
  geom_point() + xlab("Iprec")


p13 = ggplot(df, aes(df[,14], pm2.5)) +
  geom_point() + xlab("city")

ggarrange(p1, p2, p3, p4 + rremove("x.text"), 
          ncol = 2, nrow = 2)

library("cowplot")
plot_grid(p1, p2, p3, p4,
          labels = c("A", "B", "C","D"),
          ncol = 2, nrow = 2)


for(i in 7:12) {
  if(!is.factor(df[,i])) {
    print(paste("pm2.5 & ", colnames(df)[i], sep = ""))
    print(cor(df$pm2.5, df[, i], use = "complete.obs"))
    print(ggplot(df, aes(df[,i], pm2.5)) +
            geom_point() +
            xlab(colnames(df)[i]))
  }
  else {
    print(ggplot(df, aes(prsa[,i], pm2.5)) +
            geom_boxplot() +
            xlab(colnames(df)[i]))
  }
}


#city = factor(df$city)
#years = factor(df$year)
quality_boundaries <- c(12, 35.4, 55.4, 
                        150.4, 250.4, 500.4)
cities = list("shanghai","shenyang","beijing","Guangzhou","chengdu")

filter(df,df$city =="chengdu") %>% 
    ggplot(mapping = aes(x = pm2.5, y = ..density..,col = city)) +
    geom_density(fill = "lightblue") +
    geom_vline(xintercept = quality_boundaries, 
               col = c("green", "yellow", "orange", 
                       "darkorange2", "firebrick", "red")) +
    ggtitle("Chengdu, China: 2010 - 2015", 
            subtitle = "PM2.5 measure of air quality") +
    annotate("text", x = 750, y = 0.001, 
             hjust = 0, vjust = 0.5, angle = 90,
             label = "Off the charts!", col = "firebrick")+ 
    annotate("text", x = 375, y = 0.001, 
             hjust = 0, vjust = 0.5, angle = 90,
             label = "Hazardous", col = "grey50")+
    annotate("text", x = 200, y = 0.001, 
             hjust = 0, vjust = 0.5, angle = 90,
             label = "Very Unhealthy", col = "grey50") +
    annotate("text", x = 100, y = 0.001, 
             hjust = 0, vjust = 0.5, angle = 90,
             label = "Unhealthy", col = "grey50") +
    annotate("text", x = 43, y = 0.001, 
             hjust = 0, vjust = 0.5, angle = 90,
             label = "Unhealthy for Sensitive Groups", col = "grey50") +
    annotate("text", x = 22, y = 0.001, 
             hjust = 0, vjust = 0.5, angle = 90,
             label = "Moderate", col = "grey50") +
    annotate("text", x = 6, y = 0.001, 
             hjust = 0, vjust = 0.5, angle = 90,
             label = "Good", col = "grey50") 

  
filter(df,df$city =="Guangzhou") %>% 
  ggplot(mapping = aes(x = pm2.5, y = ..density..,col = city)) +
  geom_density(fill = "lightblue") +
  geom_vline(xintercept = quality_boundaries, 
             col = c("green", "yellow", "orange", 
                     "darkorange2", "firebrick", "red")) +
  ggtitle("Guangzhou, China: 2010 - 2015", 
          subtitle = "PM2.5 measure of air quality") +
  annotate("text", x = 750, y = 0.001, 
           hjust = 0, vjust = 0.5, angle = 90,
           label = "Off the charts!", col = "firebrick")+ 
  annotate("text", x = 375, y = 0.001, 
           hjust = 0, vjust = 0.5, angle = 90,
           label = "Hazardous", col = "grey50")+
  annotate("text", x = 200, y = 0.001, 
           hjust = 0, vjust = 0.5, angle = 90,
           label = "Very Unhealthy", col = "grey50") +
  annotate("text", x = 100, y = 0.001, 
           hjust = 0, vjust = 0.5, angle = 90,
           label = "Unhealthy", col = "grey50") +
  annotate("text", x = 43, y = 0.001, 
           hjust = 0, vjust = 0.5, angle = 90,
           label = "Unhealthy for Sensitive Groups", col = "grey50") +
  annotate("text", x = 22, y = 0.001, 
           hjust = 0, vjust = 0.5, angle = 90,
           label = "Moderate", col = "grey50") +
  annotate("text", x = 6, y = 0.001, 
           hjust = 0, vjust = 0.5, angle = 90,
           label = "Good", col = "grey50") 
 


filter(df,df$city =="shanghai"|df$city =="shenyang") %>% 
  ggplot(mapping = aes(x = pm2.5, y = ..density..,col =city)) +
  geom_density(fill = "lightblue") +
  geom_vline(xintercept = quality_boundaries, 
             col = c("green", "yellow", "orange", 
                     "darkorange2", "firebrick", "red")) +
  ggtitle("Shanghai, China: 2010 - 2015", 
          subtitle = "PM2.5 measure of air quality") +
  annotate("text", x = 750, y = 0.001, 
           hjust = 0, vjust = 0.5, angle = 90,
           label = "Off the charts!", col = "firebrick")+ 
  annotate("text", x = 375, y = 0.001, 
           hjust = 0, vjust = 0.5, angle = 90,
           label = "Hazardous", col = "grey50")+
  annotate("text", x = 200, y = 0.001, 
           hjust = 0, vjust = 0.5, angle = 90,
           label = "Very Unhealthy", col = "grey50") +
  annotate("text", x = 100, y = 0.001, 
           hjust = 0, vjust = 0.5, angle = 90,
           label = "Unhealthy", col = "grey50") +
  annotate("text", x = 43, y = 0.001, 
           hjust = 0, vjust = 0.5, angle = 90,
           label = "Unhealthy for Sensitive Groups", col = "grey50") +
  annotate("text", x = 22, y = 0.001, 
           hjust = 0, vjust = 0.5, angle = 90,
           label = "Moderate", col = "grey50") +
  annotate("text", x = 6, y = 0.001, 
           hjust = 0, vjust = 0.5, angle = 90,
           label = "Good", col = "grey50") 
 # facet_grid(.~city)
count(filter(df,df$city=="shenyang"))
count(filter(df,df$city=="beijing"))
count(filter(df,df$city=="shanghai"))
count(filter(df,df$city=="chengdu"))
count(filter(df,df$city=="Guangzhou"))
# facet_grid(df$year ~ .)
summary(filter(df, df$year==2010)$city)
summary(filter(df, df$year==2011)$city)
summary(filter(df, df$year==2012)$city)
summary(filter(df, df$year==2013)$city)
summary(filter(df, df$year==2014)$city)
summary(filter(df, df$year==2015)$city)



##

sample = sample.split(df$pm2.5, SplitRatio = .75)
train = subset(df, sample == TRUE)
test  = subset(df, sample == FALSE)


rf=randomForest(formula = df$pm2.5 ~ . , data = df, ntree=50)

print(rf)

colnames(df)[which(names(df) == "PM_US.Post")] <- "pm2.5"

sy = filter(df, df$city=="shenyang")
bj = filter(df, df$city=="beijing")
sh = filter(df, df$city=="shanghai")
cd = filter(df, df$city=="chengdu")
gz = filter(df, df$city=="Guangzhou")

       
for(i in 6:13) {
  if(!is.factor(df[,i])) {
    print(paste("pm2.5 & ", colnames(df)[i], sep = ""))
    print(cor(df$pm2.5, df[, i], use = "complete.obs"))
    print(ggplot(df, aes(df[,i], pm2.5, col = city)) +
            geom_point() +
            xlab(colnames(df)[i]))
  }
  else {
    print(ggplot(df, aes(df[,i], pm2.5)) +
            geom_boxplot() +
            xlab(colnames(df)[i])+facet_wrap(~ city))
  }
}

ggplot(df, aes(df[,5], pm2.5)) +
  geom_point() +
  xlab(colnames(df)[5])+facet_grid(year~city)


for(i in 6:13) {
  for(j in (i+1):12) {
    if(j <= 12 & j > i & !is.factor(df[,i]) & !is.factor(df[,j])) {
      print(paste(colnames(df)[i], "&", colnames(df)[j], sep = " "))
      print(cor(df[,i], df[,j]))
      print(ggplot(df, aes(df[,i], df[,j])) + 
              geom_point() +
              xlab(colnames(df)[i]) +
              ylab(colnames(df)[j]))
    }
  }
}


for(i in 6:12) {
  if(!is.factor(df[,i])) {
    print(ggplot(df, aes(cbwd, df[,i], col = city)) +
            geom_boxplot() +
            xlab("cbwd") +
            ylab(colnames(df)[i]))
  }
}


rf=randomForest(formula = df$pm2.5 ~ . , data = df)

print(rf)

