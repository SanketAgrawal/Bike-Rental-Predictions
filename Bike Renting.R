x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
     "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees','xlsx','psych', 'miscTools', 'tree', 'mlr', 'xgboost')
lapply(x, require, character.only = TRUE)

rm(x)
list.files(path = "../input")
#Load the data
dataset=read.csv('../input/day.csv')
str(dataset)

#Print the unique values for each columns
sapply(dataset, function(x) length(unique(x)))

##################################Missing Values Analysis###############################################
missing_val = data.frame(apply(dataset,2,function(x){sum(is.na(x))}))
missing_val$Columns = row.names(missing_val)
names(missing_val)[1] =  "Missing_percentage"
missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(dataset)) * 100
missing_val = missing_val[order(-missing_val$Missing_percentage),]
row.names(missing_val) = NULL
missing_val = missing_val[,c(2,1)]
write.csv(missing_val, "Miising_perc.csv", row.names = F)
print(missing_val)

############################################Outlier Analysis#############################################
# ## BoxPlots - Distribution and Outlier Check
# numeric_index = sapply(dataset,is.numeric) #selecting only numeric
numeric_index=c(10,11,12,13)
numeric_data = dataset[,numeric_index]

cnames = colnames(numeric_data)
print(cnames)
for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i])), data = subset(dataset))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i])+
           ggtitle(paste("Box plot of",cnames[i])))
}

## Plotting plots together
gridExtra::grid.arrange(gn1,gn2,ncol=2)
gridExtra::grid.arrange(gn3,gn4,ncol=2)

## Removing the outliers
dataset=dataset[-c(69),]

cont_var_set = c("temp", "atemp", "hum", "windspeed", "casual", 'registered', 'cnt')
cat_var_set = c('season', 'yr', 'mnth','holiday','weekday','workingday','weathersit' )

#Univariate Analysis
multi.hist(dataset[,cont_var_set], main = NA, dcol = c("blue", "red"),
dlty = c("solid", "solid"), bcol = "linen")

#Correlation matrix
corrgram(dataset[,c(10, 11, 12, 13, 14,15,16)], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")
         
#remove the variables
dataset = subset(dataset, select = -c(instant, dteday, temp))

print(dim(dataset))
#Split the dataset
set.seed(5)
train.index = createDataPartition(dataset$cnt, p = .70, list = FALSE)
train_data = dataset[ train.index,]
test.index=-(train.index)
test_data  = dataset[-train.index,]
print(dim(train_data))
print(dim(test_data))

# Manual Hyper-Parameter Search for RandomForest
store_maxtrees <- list()
for (ntree in c(25, 50)) {
    for (node in c(1, 3, 5, 8, 10, 12)){
        set.seed(5)
        print(paste(ntree,node))
        rf_maxtrees <- randomForest(cnt~season+yr+mnth+holiday+weekday+workingday+weathersit+atemp+hum+windspeed,
            data = dataset,
            subset=train.index,
            mtry = 4,
            nodesize = node,
            ntree = ntree)
        key <- paste(toString(ntree),toString(node))
        store_maxtrees[[key]] <- rf_maxtrees
        predictions = predict(rf_maxtrees,dataset[train.index,])
        print(paste("Train RMSE=",mean((predictions-dataset$cnt[train.index])^2)^0.5))
        print(paste("Train MAPE=",mean(abs(predictions-dataset$cnt[train.index])/dataset$cnt[train.index])))
        print(paste("Train R2=",rSquared(predictions, resid = predictions-dataset$cnt[train.index])))
        predictions = predict(rf_maxtrees,dataset[test.index,])
        print(paste("Test RMSE=",mean((predictions-dataset$cnt[test.index])^2)^0.5))
        print(paste("Test R2=",rSquared(predictions, resid = predictions-dataset$cnt[test.index])))
        print(paste("Test MAPE=",mean(abs(predictions-dataset$cnt[test.index])/dataset$cnt[test.index])))
    }
}
print(store_maxtrees)

# Column names: season+yr+mnth+holiday+weekday+workingday+weathersit+atemp+hum+windspeed casual registered cnt
#Applying Random forest for cnt
model=randomForest(cnt~season+yr+mnth+holiday+weekday+workingday+weathersit+atemp+hum+windspeed,data = dataset,subset = train.index, mtry =4,ntree=50,nodesize = 4)
predictions_train_rf_count = predict(model,dataset[train.index,])
print(paste("Train RMSE=",mean((predictions_train_rf_count-dataset$cnt[train.index])^2)^0.5))
print(paste("Train MAPE=",mean(abs(predictions_train_rf_count-dataset$cnt[train.index])/dataset$cnt[train.index])))
print(paste("Train R2=",rSquared(predictions_train_rf_count, resid = predictions_train_rf_count-dataset$cnt[train.index])))
predictions_validation_rf_count = predict(model,dataset[test.index,])
print(paste("Test RMSE=",mean((predictions_validation_rf_count-dataset$cnt[test.index])^2)^0.5))
print(paste("Test R2=",rSquared(predictions_validation_rf_count, resid = predictions_validation_rf_count-dataset$cnt[test.index])))
print(paste("Test MAPE=",mean(abs(predictions_validation_rf_count-dataset$cnt[test.index])/dataset$cnt[test.index])))
# "Train RMSE= 325.712495312762"
# "Train MAPE= 0.241343700004556"
# "Test RMSE= 623.1031475159"
# "Test MAPE= 0.173613440078525"

#Applying Random forest for casual
model=randomForest(casual~season+yr+mnth+holiday+weekday+workingday+weathersit+atemp+hum+windspeed,data = dataset,subset = train.index, mtry =4,ntree=50,nodesize = 4)
predictions_train_rf_casual = predict(model,dataset[train.index,])
print(paste("Train RMSE=",mean((predictions_train_rf_casual-dataset$casual[train.index])^2)^0.5))
print(paste("Train MAPE=",mean(abs(predictions_train_rf_casual-dataset$casual[train.index])/dataset$casual[train.index])))
print(paste("Train R2=",rSquared(predictions_train_rf_casual, resid = predictions_train_rf_casual-dataset$casual[train.index])))
predictions_validation_rf_casual = predict(model,dataset[test.index,])
print(paste("Test RMSE=",mean((predictions_validation_rf_casual-dataset$casual[test.index])^2)^0.5))
print(paste("Test R2=",rSquared(predictions_validation_rf_casual, resid = predictions_validation_rf_casual-dataset$casual[test.index])))
print(paste("Test MAPE=",mean(abs(predictions_validation_rf_casual-dataset$casual[test.index])/dataset$casual[test.index])))

#Applying Random forest for registered
model=randomForest(registered~season+yr+mnth+holiday+weekday+workingday+weathersit+atemp+hum+windspeed,data = dataset,subset = train.index, mtry =4,ntree=50,nodesize = 4)
predictions_train_rf_registered = predict(model,dataset[train.index,])
print(paste("Train RMSE=",mean((predictions_train_rf_registered-dataset$registered[train.index])^2)^0.5))
print(paste("Train MAPE=",mean(abs(predictions_train_rf_registered-dataset$registered[train.index])/dataset$registered[train.index])))
print(paste("Train R2=",rSquared(predictions_train_rf_registered, resid = predictions_train_rf_registered-dataset$registered[train.index])))
predictions_validation_rf_registered = predict(model,dataset[test.index,])
print(paste("Test RMSE=",mean((predictions_validation_rf_registered-dataset$registered[test.index])^2)^0.5))
print(paste("Test R2=",rSquared(predictions_validation_rf_registered, resid = predictions_validation_rf_registered-dataset$registered[test.index])))
print(paste("Test MAPE=",mean(abs(predictions_validation_rf_registered-dataset$registered[test.index])/dataset$registered[test.index])))

#Adding the casual and registered predictions
predictions_train_rf_sum = predictions_train_rf_registered+predictions_train_rf_casual
print(paste("Train RMSE=",mean((predictions_train_rf_sum-dataset$cnt[train.index])^2)^0.5))
print(paste("Train MAPE=",mean(abs(predictions_train_rf_sum-dataset$cnt[train.index])/dataset$cnt[train.index])))
predictions_validation_rf_sum = predictions_validation_rf_registered+predictions_validation_rf_casual
print(paste("Test RMSE=",mean((predictions_validation_rf_sum-dataset$cnt[test.index])^2)^0.5))
print(paste("Test MAPE=",mean(abs(predictions_validation_rf_sum-dataset$cnt[test.index])/dataset$cnt[test.index])))
# "Train RMSE= 334.977611367088"
# "Train MAPE= 0.236063892262861"
# "Test RMSE= 641.999544858643"
# "Test MAPE= 0.178068128886136"

#Average of sum and cnt predictions
predictions_train_rf_ensemble = (predictions_train_rf_sum+predictions_train_rf_count)/2
print(paste("Train RMSE=",mean((predictions_train_rf_ensemble-dataset$cnt[train.index])^2)^0.5))
print(paste("Train MAPE=",mean(abs(predictions_train_rf_ensemble-dataset$cnt[train.index])/dataset$cnt[train.index])))
predictions_validation_rf_ensemble = (predictions_validation_rf_sum+predictions_validation_rf_count)/2
print(paste("Test RMSE=",mean((predictions_validation_rf_ensemble-dataset$cnt[test.index])^2)^0.5))
print(paste("Test MAPE=",mean(abs(predictions_validation_rf_ensemble-dataset$cnt[test.index])/dataset$cnt[test.index])))
# "Train RMSE= 323.035618445321"
# "Train MAPE= 0.237458836026778"
# "Test RMSE= 617.86501541496"
# "Test MAPE= 0.172949178583804"



###############################################################           XGBoost         ###########################################

# put our testing & training data into two seperates Dmatrixs objects with cnt as labels
dtrain <- xgb.DMatrix(data = as.matrix(subset(train_data, select = -c(cnt, casual, registered),list=F)), label= train_data[,c("cnt")])
dtest <- xgb.DMatrix(data = as.matrix(subset(test_data, select = -c(cnt, casual, registered),list=F)), label= test_data[,c("cnt")])

# Manual Hyper-Parameter Search for XGBoost
store_maxtrees <- list()
for (gamma in c(0.3, 0.4, 0.5, 0.6)) {
    for (max_depth in c(3,5,7,11)){
        for (colsample_bytree in c(0.6, 0.8, 1.0)){
            set.seed(5)
            print(paste(gamma,max_depth,colsample_bytree))
            xg_maxtrees <- xgboost(data = dtrain, 
                 max_depth = max_depth, 
                 nround=25, 
                 subsample = 0.7,
                 colsample_bytree =colsample_bytree,
                 eval_metric = "rmse",
                 gamma=gamma,
                 nthread = 3,
                 silent=1
                )
            key <- paste(toString(ntree),toString(node))
            store_maxtrees[[key]] <- xg_maxtrees
            predictions = predict(xg_maxtrees,dtrain)
            print(paste("Train RMSE=",mean((predictions-dataset$cnt[train.index])^2)^0.5))
            print(paste("Train MAPE=",mean(abs(predictions-dataset$cnt[train.index])/dataset$cnt[train.index])))
            print(paste("Train R2=",rSquared(predictions, resid = predictions-dataset$cnt[train.index])))
            predictions = predict(xg_maxtrees,dtest)
            print(paste("Test RMSE=",mean((predictions-dataset$cnt[test.index])^2)^0.5))
            print(paste("Test R2=",rSquared(predictions, resid = predictions-dataset$cnt[test.index])))
            print(paste("Test MAPE=",mean(abs(predictions-dataset$cnt[test.index])/dataset$cnt[test.index])))
        }
    }
}
print(store_maxtrees)


#Applying XGBoost on cnt

# put our testing & training data into two seperates Dmatrixs objects with cnt as labels
dtrain <- xgb.DMatrix(data = as.matrix(subset(train_data, select = -c(cnt, casual, registered),list=F)), label= train_data[,c("cnt")])
dtest <- xgb.DMatrix(data = as.matrix(subset(test_data, select = -c(cnt, casual, registered),list=F)), label= test_data[,c("cnt")])

model <- xgboost(data = dtrain, max_depth = 3, nround=25, subsample = 0.7, colsample_bytree =1, eval_metric = "rmse",
                 gamma=0.4, nthread = 3, silent=1,seed=5 )
predictions_train_xg_count = predict(model,dtrain)
print(paste("Train RMSE=",mean((predictions_train_xg_count-dataset$cnt[train.index])^2)^0.5))
print(paste("Train MAPE=",mean(abs(predictions_train_xg_count-dataset$cnt[train.index])/dataset$cnt[train.index])))
print(paste("Train R2=",rSquared(predictions_train_xg_count, resid = predictions_train_xg_count-dataset$cnt[train.index])))
predictions_validation_xg_count = predict(model,dtest)
print(paste("Test RMSE=",mean((predictions_validation_xg_count-dataset$cnt[test.index])^2)^0.5))
print(paste("Test R2=",rSquared(predictions_validation_xg_count, resid = predictions_validation_xg_count-dataset$cnt[test.index])))
print(paste("Test MAPE=",mean(abs(predictions_validation_xg_count-dataset$cnt[test.index])/dataset$cnt[test.index])))
# [1] "Train RMSE= 475.030679820529"
# [1] "Train MAPE= 0.188174651141745"
# [1] "Train R2= 0.931933327146108"
# [1] "Test RMSE= 615.377373183598"
# [1] "Test R2= 0.886974937791431"
# [1] "Test MAPE= 0.150061797475048"


#Applying XGBoost on casual
# put our testing & training data into two seperates Dmatrixs objects with casual as labels
dtrain <- xgb.DMatrix(data = as.matrix(subset(train_data, select = -c(cnt, casual, registered),list=F)), label= train_data[,c("casual")])
dtest <- xgb.DMatrix(data = as.matrix(subset(test_data, select = -c(cnt, casual, registered),list=F)), label= test_data[,c("casual")])

model <- xgboost(data = dtrain, max_depth = 3, nround=25, subsample = 0.7, colsample_bytree =1, eval_metric = "rmse",
                 gamma=0.3, nthread = 3, silent=1 )
predictions_train_xg_casual = predict(model,dtrain)
print(paste("Train RMSE=",mean((predictions_train_xg_casual-dataset$casual[train.index])^2)^0.5))
print(paste("Train MAPE=",mean(abs(predictions_train_xg_casual-dataset$casual[train.index])/dataset$casual[train.index])))
print(paste("Train R2=",rSquared(predictions_train_xg_casual, resid = predictions_train_xg_casual-dataset$casual[train.index])))
predictions_validation_xg_casual = predict(model,dtest)
print(paste("Test RMSE=",mean((predictions_validation_xg_casual-dataset$casual[test.index])^2)^0.5))
print(paste("Test R2=",rSquared(predictions_validation_xg_casual, resid = predictions_validation_xg_casual-dataset$casual[test.index])))
print(paste("Test MAPE=",mean(abs(predictions_validation_xg_casual-dataset$casual[test.index])/dataset$casual[test.index])))


#Applying XGBoost on registered
# put our testing & training data into two seperates Dmatrixs objects with registered as labels
dtrain <- xgb.DMatrix(data = as.matrix(subset(train_data, select = -c(cnt, casual, registered),list=F)), label= train_data[,c("registered")])
dtest <- xgb.DMatrix(data = as.matrix(subset(test_data, select = -c(cnt, casual, registered),list=F)), label= test_data[,c("registered")])

model <- xgboost(data = dtrain, max_depth = 3, nround=25, subsample = 0.7, colsample_bytree =1, eval_metric = "rmse",
                 gamma=0.3, nthread = 3, silent=1 )
predictions_train_xg_registered = predict(model,dtrain)
print(paste("Train RMSE=",mean((predictions_train_xg_registered-dataset$registered[train.index])^2)^0.5))
print(paste("Train MAPE=",mean(abs(predictions_train_xg_registered-dataset$registered[train.index])/dataset$registered[train.index])))
print(paste("Train R2=",rSquared(predictions_train_xg_registered, resid = predictions_train_xg_registered-dataset$registered[train.index])))
predictions_validation_xg_registered = predict(model,dtest)
print(paste("Test RMSE=",mean((predictions_validation_xg_registered-dataset$registered[test.index])^2)^0.5))
print(paste("Test R2=",rSquared(predictions_validation_xg_registered, resid = predictions_validation_xg_registered-dataset$registered[test.index])))
print(paste("Test MAPE=",mean(abs(predictions_validation_xg_registered-dataset$registered[test.index])/dataset$registered[test.index])))

#Adding the casual and registered predictions
predictions_train_xg_sum = predictions_train_xg_registered+predictions_train_xg_casual
print(paste("Train RMSE=",mean((predictions_train_xg_sum-dataset$cnt[train.index])^2)^0.5))
print(paste("Train MAPE=",mean(abs(predictions_train_xg_sum-dataset$cnt[train.index])/dataset$cnt[train.index])))
predictions_validation_xg_sum = predictions_validation_xg_registered+predictions_validation_xg_casual
print(paste("Test RMSE=",mean((predictions_validation_xg_sum-dataset$cnt[test.index])^2)^0.5))
print(paste("Test MAPE=",mean(abs(predictions_validation_xg_sum-dataset$cnt[test.index])/dataset$cnt[test.index])))
# [1] "Train RMSE= 443.644038950277"
# [1] "Train MAPE= 0.148867532810942"
# [1] "Test RMSE= 600.968614247579"
# [1] "Test MAPE= 0.163892594310755"

#Avearage of sum and cnt predictions
predictions_train_xg_ensemble = (predictions_train_xg_sum+predictions_train_xg_count)/2
print(paste("Train RMSE=",mean((predictions_train_xg_ensemble-dataset$cnt[train.index])^2)^0.5))
print(paste("Train MAPE=",mean(abs(predictions_train_xg_ensemble-dataset$cnt[train.index])/dataset$cnt[train.index])))
predictions_validation_xg_ensemble = (predictions_validation_xg_sum+predictions_validation_xg_count)/2
print(paste("Test RMSE=",mean((predictions_validation_xg_ensemble-dataset$cnt[test.index])^2)^0.5))
print(paste("Test MAPE=",mean(abs(predictions_validation_xg_ensemble-dataset$cnt[test.index])/dataset$cnt[test.index])))
# [1] "Train RMSE= 440.559864168616"
# [1] "Train MAPE= 0.162603962000167"
# [1] "Test RMSE= 585.013652068901"
# [1] "Test MAPE= 0.152410465775738"



##############################         Finally         ###########################
# XGBoost is best with custom ensemble(Taking the predictions from the sum and predictions of total count and taking their averages)