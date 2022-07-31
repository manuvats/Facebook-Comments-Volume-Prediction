#Loading relevant libraries for current session
library(readxl)
library(caTools)
library(caret)
library(ROCR)
library(corrplot)
library(tidyverse) # for data manipulation and viz
library(naniar)# for missing values
library(reshape2)
library(scales) 
library(DataExplorer)
library(questionr)
library(Hmisc)
library(moments)
library(car)
library(recipes)
library(ggplot2)
library(forecast)
library(psych)
#Setting up the directory 
setwd("~/GREAT_LEARNING/Capstone Projects/4. Facebook Comment Volume Prediction")

#Reading the data files in R
dataset=read_excel("Training.xlsx", sheet = 1, col_names = TRUE)

names(dataset)<-make.names(names(dataset),unique = TRUE)

#Removing constant and id variable 
dataset<-dataset[,-c(1,39)]

#fixing categorical variable
dataset$Page.Category<-as.factor(dataset$Page.Category)
rec<-recipe(~Page.Category, data=dataset)
rec <- rec %>%
  step_other(Page.Category, threshold = 0.01, other = "Others")
rec <- prep(rec, training = dataset)

collapsed<- bake(rec, dataset)
dataset$Page.Category<-collapsed$Page.Category

ggplot(dataset, aes(x=Page.Category)) + geom_histogram()

#summary(dataset$Feature.25)

#dataset$Feature.25<-dataset$Feature.25+1366
#for (variable in names(dataset)) {
#  if(any(dataset[,variable]<0)){
#    dataset[,variable]<-dataset[,variable] + min(dataset[,variable])
#  }
#}


#Transforming variables
#Normality of Data
##Transformations
#If lambda = -2.5 to -1.5, inverse square (1/y2)
#If lambda = -2.5 to -0.75, reciprocal (1/y)
#If lambda = -0.75 to -0.25, inverse square root (1/sqrt(y))
#If lambda = -0.25 to 0.25, natural log
#If lambda = 0.25 to 0.75, square root (sqrt(y))
#If lambda = 0.75 to 1.5, none
#If lambda = 1.5 to 2.5, square (y2)
#calculating transforming parameters
#BNobject<-bestNormalize(norm.train$Page.likes, allow_orderNorm = FALSE)
normModel<-preProcess(dataset[,-c(4,39,40)], method="YeoJohnson")

#transform the dataset using the parameters
norm.dataset<-predict(normModel, dataset[,-c(4,39,40)])
norm.dataset<-cbind(dataset[,c(4,39,40)], norm.dataset)
summary(norm.train)
norm.train[,c(36:41)]%>%
  gather(metric, value) %>%
  ggplot(aes(value, fill = metric)) +
  geom_density(show.legend = FALSE) +
  facet_wrap( ~ metric, scales = "free")

ptrain[,c(26:38)]%>%
  gather(metric, value) %>%
  ggplot(aes(value, fill = metric)) +
  geom_density(show.legend = FALSE) +
  facet_wrap( ~ metric, scales = "free")



#Outlier treatment
# look at the 1st and 99th percentile and compare with min and max
aa <- quantile(dataset$Page.talking.about, probs = c(0.01,0.99), na.rm = TRUE)
aa
summary(dataset$Page.talking.about)

p.range <- NA
p.newrange<-NA
num.dataset<-dataset[,-c(4,39,40)]
for ( i in 1 :(ncol(num.dataset))){
  Statistic <- data.frame(
    "Column" = colnames(num.dataset[i]),
    "Min Value" = min(num.dataset[[i]],  na.rm = TRUE),
    "1st Percentile" = quantile(num.dataset[[i]], probs = c(0.01), na.rm = TRUE),
    "Max Value" = max(num.dataset[[i]],  na.rm = TRUE),
    "99th Percentile" = quantile(num.dataset[[i]], probs = c(0.99),na.rm = TRUE))
  p.range <- rbind(p.range, Statistic)
}


p.range <- data.table::data.table(p.range)
p.range

p.newrange <- data.table::data.table(p.newrange)
p.newrange

# set capping and floor to the data as per 1st and 99th percentile
norm.dataset[,-c(1:3)] <- norm.dataset[,-c(1:3)] %>%
  transmute_all(funs(squish(., quantile(.,c(0.01,0.99), na.rm = TRUE))))

#Now Feature.15 has become a constant variable, we shall remove it
norm.dataset<-norm.dataset[,-17]

# view plots after outlier treatment
p.stack <- melt(dataset[,c(25:41)])
ggplot(data = p.stack, aes(y=value)) + 
  geom_boxplot(color="red", fill="orange", alpha=0.3 ) +
  facet_wrap(~variable,scales = "free_x", nrow = 3)+
  coord_flip()

dataset[,c(31,32,37)]%>%
  gather(metric, value) %>%
  ggplot(aes(value, fill = metric)) +
  geom_density(show.legend = FALSE) +
  facet_wrap( ~ metric, scales = "free")

# Now impute missing values. we will use aregImpute to impute data
fmla <- as.formula(paste(" ~ ", paste(colnames(norm.dataset), collapse=" +")))
impute_arg <- aregImpute(fmla, data = norm.dataset , n.impute = 5, nk=0)
impute_arg

# Get the imputed values
imputed<-impute_arg$imputed
imputed <-impute.transcan(impute_arg, data=norm.dataset, 
                          imputation=5, list.out=TRUE, pr=FALSE, check=FALSE)

# convert the list to the database
imputed.data <- as.data.frame(do.call(cbind,imputed))

# arrange the columns accordingly
imputed.data <- imputed.data[, colnames(norm.dataset), drop = FALSE]
full.data<-imputed.data

str(full.data)
summary(full.data)
sum(is.na(full.data))

full.data$Post.published.weekday<-as.factor(full.data$Post.published.weekday)
full.data$Base.DateTime.weekday<-as.factor(full.data$Base.DateTime.weekday)

full.data[-c(1:3)] <- sapply(full.data[-c(1:3)],as.character)
full.data[-c(1:3)] <- sapply(full.data[-c(1:3)],as.double)

plot(density(na.omit(norm.dataset$Page.Checkins)), main="Page.Checkins")
lines(density(full.data$Page.Checkins), col="red")
legend("topright", legend=c("Original Values", "Predicted Values"),
       col=c("black", "red"), lty=1, cex=0.7, box.lty = 0)

plot(density(na.omit(norm.dataset$CC5)), main="CC5")
lines(density(full.data$CC5), col="red")
legend("topright", legend=c("Original Values", "Predicted Values"),
       col=c("black", "red"), lty=1, cex=0.7, box.lty = 0)


numeric.data<-norm.train[,-c(1:3)]

NA2mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
numeric.data <- lapply(numeric.data, NA2mean)
norm.train<-cbind(norm.train[,c(1:3)], numeric.data)

Mode <- function(x) { 
  ux <- sort(unique(x))
  ux[which.max(tabulate(match(x, ux)))] 
}

norm.train[,c(1:3)] <- lapply(norm.train[,c(1:3)], function(x)
  replace(x, is.na(x), Mode(x[!is.na(x)])))
full.data<-norm.train

str(full.data)
summary(full.data)
sum(is.na(full.data))

impModel<-lm(Target.Variable~.,
             data = full.data)
impfit=randomForest(factor(Target.Variable)~., data=full.data)

#impVariables<-varImp(impModel)
impVar<-varImp(impModel) 
impVar$Variable<-row.names(impVar)
rownames(impVar) <- 1:nrow(impVar)

top_n(impVar, n=15, Overall) %>%
  ggplot(mapping=aes(x =Variable , y = Overall)) +
  geom_bar(stat = 'identity')+theme_minimal()+
theme(text = element_text(size=8),
  axis.text.x = element_text(angle=0, hjust=1))
#norm.train$Feature.28<- norm.train$Feature.28 - min(norm.train$Feature.28)
#Removing multicollinearity
modelData<-full.data[,-c(1:3)]

#Correlation Matrix to check correlation among variables
cor.mat<-cor(modelData)

#Bartlett Test to check if data is appropriate for FA
cortest.bartlett(cor.mat, nrow(modelData))

library(psych)
KMO(modelData)

alias(model)
summary(model)

# remove alias variables
#modelData<-full.data[,-c(1:3,9,10,11,14,16,18,21,23,24,25,30,34)]
model=lm(Target.Variable~.,
         data = modelData)

summary(model)
vif(model)

exp(coef(model))
#Factor Analysis


#Calculating Eigen values
ev = eigen(cor(modelData)) # get eigenvalues
ev

EigenValue=ev$values
EigenValue

#Scree plot
Factor=c(1:36)
Scree=data.frame(Factor,EigenValue)
plot(Scree,main="Scree Plot", col="Blue", ylim=c(0,3))
lines(Scree,col="Red")


#Unrotate factors
Unrotate=principal(modelData, nfactors=6, rotate="none")
print(Unrotate,digits=3)

# Orthogonal graph
UnrotatedProfile=plot(Unrotate,row.names(Unrotate$loadings))

#Rotating factors
Rotate=principal(modelData,nfactors=6,rotate="varimax")
print(Rotate,digits=3)

library(FactoMineR)
analysispca<-PCA(modelData)
analysispca$call$call
# Orthogonal graph (After rotation)
RotatedProfile=plot(Rotate,row.names(Rotate$loadings),cex=1.0)

#Diagram of factors and variables
fa.diagram(Rotate$loadings,simple = FALSE)
fa.parallel(modelData)
vss(modelData)

library(nFactors)
ap <- parallel(subject=nrow(modelData),var=ncol(modelData), rep=100, cent=.05)
nS <- nScree(x=EigenValue, aparallel=ap$eigen$qevpea)
plotnScree(nS)
#Regression

#Storing factor scores with new factor names
newData<-as.data.frame(factor.scores(full.data[,-c(1:3,40)], f=Rotate$loadings,  method = "Harman" )$scores)
newData<-cbind(full.data[,c(1:3,40)],newData)
#colnames(newData)<-c("PurchaseExp", "MarkDist", "AfterSale", "General")


# view plots after outlier treatment
p.stack <- melt(newData[,-c(1:4)])
ggplot(data = p.stack, aes(y=value)) + 
  geom_boxplot(color="red", fill="orange", alpha=0.3 ) +
  facet_wrap(~variable,scales = "free_x", nrow = 3)+
  coord_flip()

newData[,-c(1:4)]%>%
  gather(metric, value) %>%
  ggplot(aes(value, fill = metric)) +
  geom_density(show.legend = FALSE) +
  facet_wrap( ~ metric, scales = "free")

#Checking data
str(newData)
summary(newData)


#Backtracking
Prediction=predict(MLRModel)
Actual=Satisfaction
BackTrack=data.frame(Actual,Prediction)
BackTrack
library(lattice)
plot(Actual,col="Red",xlab="Data Point")
lines(Actual,col="Red")
#plot(Prediction,col="Blue",xlab="Data Point")
lines(Prediction,col="Blue")



#Creating dummy variables
library(fastDummies)
newData <- dummy_cols(newData, 
                           select_columns = c("Page.Category", "Post.published.weekday", "Base.DateTime.weekday")
                    ,remove_first_dummy = TRUE)
newData<-newData[,-c(1:3)]
table(newData$Target.Variable)


#Paritioning the data into training and test dataset
set.seed(2000)
#?sample
n=nrow(newData)
split= sample(c(TRUE, FALSE), n, replace=TRUE, prob=c(0.70, 0.30))
ptrain = newData[split, ]
ptest = newData[!split,]

c(nrow(ptrain), nrow(ptest))
str(ptrain)
summary(ptrain)


fmla <- as.formula(paste("Target.Variable ~ ", paste(colnames(ptrain[,-1]), collapse=" +")))
linearmod<-lm(formula = fmla, data = ptrain)
summary(linearmod)
ptest$lmPred <- predict(linearmod, ptest)
table(sign(ptest$lmPred))
lmPred[lmPred<0]<-0

lmactuals_preds<-data.frame(cbind(actuals=ptest$Target.Variable, predicteds=ptest$lmPred))
lmcorrelation_accuracy <- cor(lmactuals_preds)  # 82.7%
head(lmactuals_preds)

library(Metrics)
rmsle(lmactuals_preds$actuals, lmactuals_preds$predicteds)
# Min-Max Accuracy Calculation
min_max_accuracy <- mean(apply(lmactuals_preds, 1, min) / apply(lmactuals_preds, 1, max))  
# => 38.00%, min_max accuracy

DMwR::regr.eval(lmactuals_preds$actuals, lmactuals_preds$predicteds)

postProcess.pred <- function(predCohort, preProcess){
  stopifnot(class(predCohort) == "data.frame")
  stopifnot(class(preProcess) == "preProcess")
  binVars <- findBinary(predCohort)
  # drop bestThresh
  binVars <- binVars[!binVars %in% "bestThresh"]
  allVars <- names(predCohort)
  conVars <- names(preProcess$mean)
  predCohort[, conVars] <- unPreProc(preProcess, predCohort[, conVars])
  predCohort[, binVars] <- apply(predCohort[, binVars], 2, uncenter)
  return(predCohort)
}

lmactuals_preds.trans<-postProcess.pred(lmactuals_preds, normModel)
revPredict <- function(preproc, data, digits=0) {
  data %>%
    select(everything(preproc$mean %>% names)) %>%
    map2_df(preproc$std, ., function(sig, dat) dat * sig) %>%
    map2_df(preproc$mean, ., function(mu, dat) dat + mu)
}
x<-revPredict(normModel, lmactuals_preds)
mutate_if(is.numeric,funs(round(.,digits = digits)))

revPredict(preprocess_params, df_needing_reverse_transformation)
unPreProc <- function(preProc, data){
  stopifnot(class(preProc) == "preProcess")
  stopifnot(class(data) == "data.frame")
  for(i in names(preProc$mean)){
    tmp <- data[, i] * preProc$std[[i]] + preProc$mean[[i]]
    data[, i] <- tmp
  }
  return(data)  
}
lmactuals_preds.trans<-unPreProc(normModel, lmactuals_preds)

library(DAAG)
cvResults <- suppressWarnings(lm(train.data, form.lm=fmla, m=5, dots=FALSE, seed=29, legend.pos="topleft",  printit=FALSE, main="Small symbols are predicted values while bigger ones are actuals."));  # performs the CV
attr(cvResults, 'ms')  

#Random Forest
library(randomForest)
library(foreach)
library(doParallel)
nb_trees<-34 #this changes with each test, see table below
  nb_cores <-12 #this changes with each test, see table below
  cl <- makeCluster(nb_cores)
registerDoParallel(cl)
ptm <- proc.time()
fit <- foreach(ntree = rep(nb_trees, nb_cores), .combine = combine, .packages = "randomForest") %dopar% {
  randomForest(y=ptrain[,1],
               x=ptrain[,-1],
               ntree = ntree,
               do.trace=TRUE)}
proc.time() - ptm
stopCluster(cl)
View(ptrain)
## Calling syntax to build the Random Forest
fmla <- as.formula(paste("Target.Variable ~ ", paste(colnames(ptrain[,-c(1,17)]), collapse=" +")))

#RF <- randomForest(formula=fmla, data = ptrain, ntree=50)


print(fit)

plot(fit, main="")
legend("topright", c("OOB", "0", "1"), text.col=1:6, lty=1:3, col=1:3)
title(main="Error Rates Random Forest ptrain")


## List the importance of the variables.
impVar <- randomForest::importance(fit)
impVar[order(impVar[,3], decreasing=TRUE),]

#?tuneRF
## Tuning Random Forest
#tRF <- tuneRF(x = ptrain[,-c(1,17)], 
#              y=ptrain$Target.Variable,
#              mtryStart = 4, 
#              ntreeTry=500, 
#              stepFactor = 1.5, 
#              improve = 0.01, 
#              trace=TRUE, 
#              plot = TRUE,
#              doBest = TRUE,
#              #nodesize = 210, 
#              importance=TRUE
#)

#tRF$importance


View(ptrain)
summary(fit)
## Scoring syntax
fitPred <- predict(fit, ptest)
table(sign(fitPred))

fitactuals_preds<-data.frame(cbind(actuals=ptest$Target.Variable, predicteds=fitPred))
#fitactuals_preds <- data.frame(cbind(actuals=exp(ptest$Target.Variable), predicteds=exp(fitPred)))  # make actuals_predicteds dataframe.
fitcorrelation_accuracy <- cor(fitactuals_preds)  # 82.7%
head(fitactuals_preds)

# Min-Max Accuracy Calculation
fitmin_max_accuracy <- mean(apply(fitactuals_preds, 1, min) / apply(fitactuals_preds, 1, max))  
# => 76.96%, min_max accuracy

# MAPE Calculation
fitmape <- mean(abs((fitactuals_preds$predicteds - fitactuals_preds$actuals))/fitactuals_preds$actuals)  
# => 41.52%, mean absolute percentage deviation
rmsle(fitactuals_preds$actuals, fitactuals_preds$predicteds)
DMwR::regr.eval(fitactuals_preds$actuals, fitactuals_preds$predicteds)

# XGBoost works with matrices that contain all numeric variables
# we also need to split the training data and label
library(xgboost)
train.mtx<-as.matrix(ptrain[,-1])
label.train<-as.matrix(ptrain[,1])
test.mtx<-as.matrix(ptest[,-1])

xgb.fit <- xgboost(
  data = train.mtx,
  label = label.train,
  eta = 0.1,#this is like shrinkage in the previous algorithm
  max_depth = 7,#Larger the depth, more complex the model; higher chances of overfitting. There is no standard                      value for max_depth. Larger data sets require deep trees to learn the rules from data.
  min_child_weight = 3,#it blocks the potential feature interactions to prevent overfitting
  nrounds = 400,#controls the maximum number of iterations. For classification, it is similar to the number of                       trees to grow.
  nfold = 3,
  objective = "reg:linear",  # for regression models
  verbose = 0,               # silent,
  early_stopping_rounds = 10 # stop if no improvement for 10 consecutive trees
)

#gd_features_test<-as.matrix(gd_features_test[,1:ncol(gd_features_test)-1])
xgb.fit
summary(xgb.fit)
#test.mtx<-as.data.frame(test.mtx)
xgb.pred <- predict(xgb.fit, test.mtx)
table(sign(xgb.pred))
xgb.pred[xgb.pred<0]<-0

xgbfitactuals_preds<-data.frame(cbind(actuals=ptest$Target.Variable, predicteds=xgb.pred))
#fitactuals_preds <- data.frame(cbind(actuals=exp(ptest$Target.Variable), predicteds=exp(fitPred)))  # make actuals_predicteds dataframe.
xgbcorrelation_accuracy <- cor(xgbfitactuals_preds)  # 82.7%
head(xgbfitactuals_preds)

rmsle(xgbfitactuals_preds$actuals, xgbfitactuals_preds$predicteds)
# Min-Max Accuracy Calculation
xgbmin_max_accuracy <- mean(apply(xgbfitactuals_preds, 1, min) / apply(xgbfitactuals_preds, 1, max))  
# => 76.96%, min_max accuracy

# MAPE Calculation
xgbmape <- mean(abs((xgbfitactuals_preds$predicteds - xgbfitactuals_preds$actuals))/xgbfitactuals_preds$actuals)  
# => 41.52%, mean absolute percentage deviation

DMwR::regr.eval(xgbfitactuals_preds$actuals, xgbfitactuals_preds$predicteds)
xgb.importance(feature_names = colnames(train.mtx), model = xgb.fit)
print(xgb.plot.importance(importance_matrix = xgb.importance(feature_names = colnames(train.mtx), 
                                                             model = xgb.fit), top_n =6 ))
#in this code chunk we will playing around with all the values untill we find the best fit
#let's play with shrinkage, known as eta in xbg
tp_xgb<-vector()
lr <- c(0.001, 0.01, 0.1, 0.3, 0.5, 0.7, 1)
md<-c(1,3,5,7,9,15)
nr<-c(2, 50, 100, 1000, 10000)
for (i in md) {
  
  xgb.fit <- xgboost(
    data = gd_features_train,
    label = gd_label_train,
    eta = 0.7,
    max_depth = 5,
    nrounds = 50,
    nfold = 5,
    objective = "binary:logistic",  # for regression models
    verbose = 1,               # silent,
    early_stopping_rounds = 10 # stop if no improvement for 10 consecutive trees
  )
  
  gd_test$xgb.pred.class <- predict(xgb.fit, gd_features_test)
  
  tp_xgb<-cbind(tp_xgb,sum(gd_test$Class==1 & gd_test$xgb.pred.class>=0.5))
  #if your class=1 and our prediction=0.5, we are going to display it with the next line compare the same algorithm     for different values
  
}

tp_xgb
table(gd_test$Class,gd_test$xgb.pred.class>=0.5)
#here there is significant imporvement over all the models that we ahve done so far



#############Neural Networks################333333
library(neuralnet)
?"neuralnet"

#train.scaled<-train.data
train.scaled<-scale(ptrain)

##quantile(nn.dev$Balance, c(0,1,5,10,25,50,75,90,95,99,100)/100)
fmla <- as.formula(paste("Target.Variable ~ ", paste(colnames(train.scaled[,-1]), collapse=" +")))

nn1 <- neuralnet(formula = fmla, 
                 data = train.scaled, 
                 hidden = c(8,3),
                 err.fct = "sse",
                 linear.output = TRUE,
                 lifesign = "full",
                 lifesign.step = 10,
                 threshold = 1,
                 stepmax = 3000,
                 rep = 1
                 #startweights = weights1
)
weights1 <- nn1$weights
weights

plot (nn1)

test.scaled<-ptest[,-60]
test.scaled<-scale(ptest[,-60])
compute.output = compute(nn1, test.scaled)
compute.output
ptest$NNPred = round(compute.output$net.result,0)
test.scaled$NNPred = round(compute.output$net.result,0)
View(test.scaled)


NNActulasvsPred<-data.frame(cbind(actuals=ptest$Target.Variable, predicteds=ptest$NNPred))
#fitactuals_preds <- data.frame(cbind(actuals=exp(ptest$Target.Variable), predicteds=exp(fitPred)))  # make actuals_predicteds dataframe.
NNcorrelation_accuracy <- cor(NNActulasvsPred)  # 82.7%
head(NNActulasvsPred)

# Min-Max Accuracy Calculation
NNmin_max_accuracy <- mean(apply(NNActulasvsPred, 1, min) / apply(NNActulasvsPred, 1, max))  
# => 76.96%, min_max accuracy

# MAPE Calculation
NNmape <- mean(abs((NNActulasvsPred$V2 - NNActulasvsPred$actuals))/NNActulasvsPred$actuals)  
# => 41.52%, mean absolute percentage deviation

DMwR::regr.eval(NNActulasvsPred$actuals, NNActulasvsPred$V2)

#Poisson Model
#model poisson regression usin glm()
fmla <- as.formula(paste("Target.Variable ~ ", paste(colnames(ptrain[,-1]), collapse=" +")))
poisson.model<-glm(fmla, ptrain, family = poisson(link = "log"))
quasi.poisson.model<-glm(fmla, ptrain, family = quasipoisson(link = "log"))
summary(poisson.model)

ptest$poisson.pred<-predict(poisson.model, newData = ptest, type = "response")
table(sign(ptest$poisson.pred))
poissonActulasvsPred<-data.frame(cbind(actuals=ptest$Target.Variable, predicteds=ptest$poisson.pred))
#fitactuals_preds <- data.frame(cbind(actuals=exp(ptest$Target.Variable), predicteds=exp(fitPred)))  # make actuals_predicteds dataframe.
possioncorrelation_accuracy <- cor(poissonActulasvsPred)  # 82.7%
head(NNActulasvsPred)

# Min-Max Accuracy Calculation
poissonmin_max_accuracy <- mean(apply(poissonActulasvsPred, 1, min) / apply(poissonActulasvsPred, 1, max))  
# => 76.96%, min_max accuracy

# MAPE Calculation
poissonmape <- mean(abs((poissonActulasvsPred$predicteds - poissonActulasvsPred$actuals))/poissonActulasvsPred$actuals)  
# => 41.52%, mean absolute percentage deviation
rmsle(poissonActulasvsPred$actuals, poissonActulasvsPred$predicteds)
DMwR::regr.eval(poissonActulasvsPred$actuals, poissonActulasvsPred$predicteds)
