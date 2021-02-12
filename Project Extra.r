analyst = "Devina Sen" # Replace this with your name

f = "setup.R"; for (i in 1:10) { if (file.exists(f)) break else f = paste0("../", f) }; source(f)
options(repr.matrix.max.rows=674)
options(repr.matrix.max.cols=200)
update_geom_defaults("point", list(size=1))                                

# Set the business parameters.
budget = 1000000
portfolio = 12

threshold = 0.4 #super growth threshold - a little higher than the original

# Retrieve the data dictionary.
data_dictionary = read.csv("Data Dictionary.csv")

data_dictionary

# Retrieve the 2017 data.
co17 = read.csv("Company Fundamentals 2017.csv")

# Partition the dataset as described.
co17$quarter = quarter(mdy(co17[,2]))
 
co17.q1 = co17[(co17$quarter==1) & !is.na(co17$prccq) & (co17$prccq>=3), -ncol(co17)]
co17.q2 = co17[(co17$quarter==2) & !is.na(co17$prccq) & (co17$prccq>=3), -ncol(co17)]
co17.q3 = co17[(co17$quarter==3) & !is.na(co17$prccq) & (co17$prccq>=3), -ncol(co17)]
co17.q4 = co17[(co17$quarter==4) & !is.na(co17$prccq) & (co17$prccq>=3), -ncol(co17)]
 
co17.q1 = co17.q1[!duplicated(co17.q1$gvkey),]
co17.q2 = co17.q2[!duplicated(co17.q2$gvkey),]
co17.q3 = co17.q3[!duplicated(co17.q3$gvkey),]
co17.q4 = co17.q4[!duplicated(co17.q4$gvkey),]
 
colnames(co17.q1)[-c(1, 10, 12)] = paste0(colnames(co17.q1)[-c(1, 10, 12)], ".q1")
colnames(co17.q2)[-c(1, 10, 12)] = paste0(colnames(co17.q2)[-c(1, 10, 12)], ".q2")
colnames(co17.q3)[-c(1, 10, 12)] = paste0(colnames(co17.q3)[-c(1, 10, 12)], ".q3")
colnames(co17.q4)[-c(1, 10, 12)] = paste0(colnames(co17.q4)[-c(1, 10, 12)], ".q4")

# Consolidate the partitions as described.
m12 = merge(co17.q1, co17.q2, by=c("gvkey", "tic", "conm"), all=TRUE)
m34 = merge(co17.q3, co17.q4, by=c("gvkey", "tic", "conm"), all=TRUE)
 
fun17 = merge(m12, m34, by=c("gvkey", "tic", "conm"), all=TRUE, sort=TRUE)
fun17 = fun17[!is.na(fun17$prccq.q4),]
 
size(fun17)
head(fun17)

# Retrieve the 2018 data.
co18 = read.csv("Company Fundamentals 2018.csv")

# Filter the dataset as described.
co18$quarter = quarter(mdy(co18[,2]))
co18.q4 = co18[(co18$quarter==4) & !is.na(co18$prccq), c("gvkey", "prccq")]
co18.q4 = co18.q4[!duplicated(co18.q4$gvkey),]

size(co18.q4)
head(co18.q4)

# Consolidate the datasets as described.

# Consolidate the datasets as described.
# How many observations and variables in the resulting dataset?
# How many predictor variables?
# How many outcome variables?
# Present the first few observations of the resulting dataset.

data = merge(fun17, co18.q4, by = c('gvkey'))
data['growth'] = (data$prccq - data$prccq.q4)/data$prccq.q4
data.supergrowth = data['growth'] >= threshold

sgrowth = vector()
for (growth in data.supergrowth) {
    if (growth) {
        sgrowth = c(sgrowth, 'Y')
    }
    else {
        sgrowth = c(sgrowth, 'N')
    }
}

data['super_growth'] = sgrowth

size(data)

#first few observations
head(data)

# Present some interesting statistics.

#some statistics about growth
describe(data$growth)

# What fraction of observations are missing price data (i.e., prccq.q1, prccq.q2, prccq.q3, prccq.q4)?
missing.q1 = mean(is.na(data$prccq.q1))
missing.q2 = mean(is.na(data$prccq.q2))
missing.q3 = mean(is.na(data$prccq.q3))
missing.q4 = mean(is.na(data$prccq.q4))

#fraction of missing values in quarter 1, 2, 3, and 4 respectively
missing.q1
missing.q2
missing.q3
missing.q4

#fraction of missing values in the total dataset
missing.q1 + missing.q2 + missing.q3 + missing.q4

#correlation between prccq.q4 and prccq
cor(data$prccq.q4, data$prccq)
#correlation between prccq.q4 and growth
cor(data$prccq.q4, data$growth)

# Present some additional interesting statistics.
sum(data.supergrowth) #Number of companies with growth above the threshold
sum(data.supergrowth)/length(data$super_growth) #Proportion of companies with growth above the threshold

# Present some interesting data visualizations. 

# Present a bar chart to visualize growth across companies (sorted lowest to highest).
data.bar = data[, c('conm','growth')]
data.bar = data.bar[order(data$growth),]
data.bar$conm = factor(data.bar$conm, levels = data.bar$conm)

ggplot(data.bar, aes(x = conm, y = growth)) + geom_bar(stat="identity") + labs(title = 'Growth by Company in Ascending Order')

data.frame(variable=c("revtq.q1","revtq.q4"), aspect=c("horizontal position","vertical position"))

dv1 = data[,c("revtq.q1","revtq.q4")]
output_size(3,3)
ggplot(dv1) + xlim(0,6) + ylim(0,6) + ggtitle("Revenue in Q1 vs. Q4") +
geom_point(aes(x=revtq.q1, y=revtq.q4))
output_size(restore)

ggplot(data) +
    geom_point(aes(x=prccq.q4, y=growth), alpha=0.1, color=PALETTE[1]) +
    ggtitle("Stock Price and Growth") +
    ylab("Growth in Q1") + xlab("Stock Price")

rev = data[!is.na(data$revtq.q4),]
ggplot(rev) +
geom_line(aes(x=revtq.q4, y=growth), color=PALETTE[1])+ ggtitle("Revenue vs. Growth in Q4")

# Select variables.

#filtering to include only columns with less that 20% missing data
data = data[,colMeans(is.na(data)) < 0.2]

# Impute missing data.
impute_vals = get_impute(data)
data = impute(data)

# Perform a principal component analysis.

#filtering data to only include numeric variables
filter = data[sapply(data,is.numeric)]
pca_data = filter[sapply(filter, var) > 0]

#performing PCA
pc = prcomp(pca_data, scale=TRUE)

#variables as principal components
head(as.data.frame(pc$rotation))

#showing first few observations as principle components
head(pc$x)

#Showing how much variance is captured by the first 20 principal components
pc20 = as.data.frame(pc$x[,1:20])
vars = sapply(pc20,var)

barplot(vars, xlab = 'Component', ylab = 'Variance') 
title('Variance of First 20 Principal Components')

#2D scatterplot to visualize PC1 vs PC2 vs big_growth
super_growth = data[,'super_growth']
ggplot(pc20) + geom_point(aes(x = PC1, y = PC2, color = as.factor(super_growth)), size = 0.01) + xlim(-100,10) + ylim(-50,100)

# Perform a cluster analysis.

#used different column variables than the original project and made more clusters
#this was not particularly more insightful than the original clustering analysis as all the 
#clusters have a high spread and the data is not easily separable

cluster_data = data[,c('spcsrc.q4', 'ivchy.q4', 'state.q4')]
dummies = dummify(cluster_data)
dummies = dummies[, !names(dummies) %in% c('spcsrc.q4', 'ivchy.q4', 'state.q4')]
head(dummies)

#k-means clustering
set.seed(12345)
cluster_data$cluster = factor(kmeans(dummies, 5, nstart=1)$cluster)
table(cluster_data$cluster) #counts of datapoints per cluster

# 2D scatterplot to visualize PC1 vs PC2 vs cluster
ggplot(pc20) + geom_point(aes(x = PC1, y = PC2, color = cluster_data$cluster), size = 0.01) + xlim(-100,10) + ylim(-50,100)

# Change the representation of the data.

#included PC3 but as we see in the seventh section, including this column was not helpful to 
#improving the model's accuracy

new_data = data.frame(gvkey = data['gvkey'], tic = data['tic'],
                      conm = data['conm'], PC1 = pc20['PC1'], PC2 = pc20['PC2'], PC3 = pc20['PC3'],
                      prccq = data['prccq'], growth = data['growth'], super_growth = data['super_growth'])
head(new_data)

# Construct a model to predict growth or big_growth.
# Present a brief summary of the model parameters.
mymodel = lm(growth ~ PC1 + PC2, new_data)
mymodel

# Present the model's in-sample estimated profit and profit rate.
# Present the model's in-sample estimated RMSE, profit, and profit rate.
# Present the model's in-sample estimated RMSE, profit, and profit rate.
pred = predict(mymodel, new_data)

#in-sample estimated RMSE
rmse = sqrt(mean((pred - new_data$growth)^2))
rmse

#portfolio recommendation
rec = head(arrange(new_data, -pred), 12)

#in-sample profit
profit = sum((rec['growth'] + 1)*(budget/portfolio)) - budget
profit

#in-sample profit rate
profit_rate = profit / budget
profit_rate

# Partition the data into training and validation.
set.seed(12345)

train = sample_frac(new_data, size = 0.75)
valid = new_data[!new_data$gvkey %in% train$gvkey,]

#observations and variables
size(train)
size(valid)

# Present the model's out-of-sample estimated profit and profit rate.
lm_model_out = lm(growth ~ PC1 + PC2, train)
lm_out = predict(lm_model_out, valid)

#out-sample RMSE
rmse = sqrt(mean((lm_out - valid$growth)^2))
rmse

rec = head(arrange(valid, -lm_out), 12)

#out-sample profit
profit = sum((rec['growth'] + 1)*(budget/portfolio)) - budget
profit

#out-sample profit rate
profit_rate = profit / budget
profit_rate

# Partition the data into 5 folds.
set.seed(12345)

folds = createFolds(new_data$gvkey, 5)

head(folds$Fold1)
head(folds$Fold2)
head(folds$Fold3)
head(folds$Fold4)
head(folds$Fold5)

# Present the model's fold #1 estimated profit.
fold1 = new_data[folds$Fold1,]

lm_model1 = lm(growth ~ PC1 + PC2, new_data[-folds$Fold1,])
fold1$pred = predict(lm_model1, fold1)

#fold #1 estimated RMSE
rmse1 = sqrt(mean((fold1$pred - fold1$growth)^2))
rmse1

rec = head(arrange(fold1, -fold1$pred), 12)

#fold #1 estimated profit
profit1 = sum((rec['growth'] + 1)*(budget/portfolio)) - budget
profit1

# Present the model's fold #2 estimated profit.
lm_model2 = lm(growth ~ PC1 + PC2, new_data[-folds$Fold2,])
fold2 = new_data[folds$Fold2,]
lm_pred2 = predict(lm_model2, fold2)

#fold #1 estimated RMSE
rmse2 = sqrt(mean((lm_pred2 - fold2$growth)^2))
rmse2

rec = head(arrange(fold2, desc(lm_pred2)), 12)

#fold #1 estimated profit
profit2 = sum((rec['growth'] + 1)*(budget/portfolio)) - budget
profit2

# Present the model's fold #3 estimated profit.
lm_model3 = lm(growth ~ PC1 + PC2, new_data[-folds$Fold3,])
fold3 = new_data[folds$Fold3,]
lm_pred3 = predict(lm_model3, fold3)

#fold #1 estimated RMSE
rmse3 = sqrt(mean((lm_pred3 - fold3$growth)^2))
rmse3

rec = head(arrange(fold3, -lm_pred3), 12)

#fold #1 estimated profit
profit3 = sum((rec['growth'] + 1)*(budget/portfolio)) - budget
profit3

# Present the model's fold #4 estimated profit.
lm_model4 = lm(growth ~ PC1 + PC2, new_data[-folds$Fold4,])
fold4 = new_data[folds$Fold4,]
lm_pred4 = predict(lm_model4, fold4)

#fold #1 estimated RMSE
rmse4 = sqrt(mean((lm_pred4 - fold4$growth)^2))
rmse4

rec = head(arrange(fold4, -lm_pred4), 12)

#fold #1 estimated profit
profit4 = sum((rec['growth'] + 1)*(budget/portfolio)) - budget
profit4

# Present the model's fold #5 estimated profit.
lm_model5 = lm(growth ~ PC1 + PC2, new_data[-folds$Fold5,])
fold5 = new_data[folds$Fold5,]
lm_pred5 = predict(lm_model5, fold5)

#fold #1 estimated RMSE
rmse5 = sqrt(mean((lm_pred5 - fold5$growth)^2))
rmse5

rec = head(arrange(fold5, -lm_pred5), 12)

#fold #1 estimated profit
profit5 = sum((rec['growth'] + 1)*(budget/portfolio)) - budget
profit5

# Present the model's 5-fold cross-validation estimated profit and profit rate.
rmse_5fold = mean(c(rmse1, rmse2, rmse3, rmse4, rmse5))
rmse_5fold

# estimated profit
profit_5fold_lm = mean(c(profit1, profit2, profit3, profit4, profit5))
profit_5fold_lm

#estimated profit rate
profit_rate_5fold_lm = profit_5fold_lm / budget
profit_rate_5fold_lm

# Show some of the work you did to build and tune resulting in your best model.


# SVM MODEL

#build
set.seed(12345)
svm = svm(super_growth ~ PC1 + PC2 + PC3, new_data, type="C-classification", 
          kernel="polynomial", degree=2, cost=100, scale=TRUE, probability=TRUE)

#predict
prob = attr(predict(svm, new_data, probability=TRUE), "probabilities")
supergrowth.predicted = as.class(prob, "Y", 0.5)

cm = confusionMatrix(factor(supergrowth.predicted, levels = c('Y', 'N')), 
                     factor(new_data$super_growth, levels = c('Y', 'N')))$table

cm

#since the svm model failed to predict any positive results, it is unable to make a recommendation

# LINEAR REGRESSION MODEL

#build
lm_model = lm(growth ~ PC1 + PC2 + PC3, new_data)

#predict
lm_pred = predict(lm_model, new_data)

#portfolio recommendation generated
rec = head(arrange(new_data, -lm_pred), 12)

#evaluate
rmse = sqrt(mean((lm_pred - new_data$growth)^2))
rmse

#in-sample profit
profit = sum((rec['growth'] + 1)*(budget/portfolio)) - budget
profit

#in-sample profit rate
profit_rate = profit / budget
profit_rate

#this model gave the highest accuracy and highest profit rate

# NAIVE BAYES

#build
model = naiveBayes(super_growth ~ PC1 + PC2, new_data, laplace = TRUE)

#predict
in_prob = predict(model, new_data, type="raw")
in_class.predicted = as.class(in_prob, 'Y', 0.5)

#portfolio recommendation generated
rec = head(arrange(new_data[in_class.predicted == 'Y',], -gvkey), 12)

#in-sample estimated accuracy
sum(new_data$super_growth == in_class.predicted)/length(new_data$super_growth)

#in-sample profit
profit = sum((rec['growth'] + 1)*(budget/portfolio)) - budget
profit

#in-sample profit rate
profit_rate = profit / budget
profit_rate

#this model did decently well but was outperformed by linear regression

# ENSEMBLE METHOD - 3 LINEAR MODELS

df = new_data

#model 1
model1 = naiveBayes(super_growth ~ PC1 + PC2, df, laplace = TRUE)

#predict
prob1 = predict(model1, df, type="raw")
pred1 = as.class(prob1, 'Y', 0.5)

hit1 = df$super_growth == pred1

set.seed(12345)
data2 = focus_data(df, hit1, emphasis = 10)


#model 2
model2 = naiveBayes(super_growth ~ PC1 + PC2, data2, laplace = TRUE)

#predict
prob2 = predict(model2, data2, type="raw")
pred2 = as.class(prob2, 'Y', 0.5)

hit2 = df$super_growth == pred2

set.seed(12345)
data3 = focus_data(data2, hit2, emphasis = 10)

#model 3
model3 = naiveBayes(super_growth ~ PC1 + PC2, data3, laplace = TRUE)

#predict
prob3 = predict(model3, data3, type="raw")
pred3 = as.class(prob3, 'Y', 0.5)

hit3 = df$super_growth == pred3

#final prediction
class.predicted = vote(pred1, pred2, pred3)

#portfolio recommendation generated
rec = head(arrange(new_data[class.predicted == 'Y',], -gvkey), 12)

#in-sample estimated accuracy
sum(new_data$super_growth == class.predicted)/length(new_data$super_growth)

#in-sample profit
profit = sum((rec['growth'] + 1)*(budget/portfolio)) - budget
profit

#in-sample profit rate
profit_rate = profit / budget
profit_rate

#I expected the ensemble method to outperform the other models but has very low accuracy
#The profit rate is surprising still low for its accuracy though

#of the above models, linear regression gave us the best results.
#in the following cells, I tune the model by choose different parameters for the model to see
#which gives the best accuracy (and hopefully profit rate), the chosen metric of model success

#All principle components

#build
lm_model = lm(growth ~ PC1 + PC2 + PC3, new_data)

#predict
lm_pred = predict(lm_model, new_data)

#portfolio recommendation generated
rec = head(arrange(new_data, -lm_pred), 12)

#evaluate
rmse = sqrt(mean((lm_pred - new_data$growth)^2))
rmse

#in-sample profit
profit = sum((rec['growth'] + 1)*(budget/portfolio)) - budget
profit

#in-sample profit rate
profit_rate = profit / budget
profit_rate

#2 components

#build
lm_model = lm(growth ~ PC1 + PC2, new_data)

#predict
lm_pred = predict(lm_model, new_data)

#portfolio recommendation generated
rec = head(arrange(new_data, -lm_pred), 12)

#evaluate
rmse = sqrt(mean((lm_pred - new_data$growth)^2))
rmse

#in-sample profit
profit = sum((rec['growth'] + 1)*(budget/portfolio)) - budget
profit

#in-sample profit rate
profit_rate = profit / budget
profit_rate

#1 principle component

#build
lm_model = lm(growth ~ PC1, new_data)

#predict
lm_pred = predict(lm_model, new_data)

#portfolio recommendation generated
rec = head(arrange(new_data, -lm_pred), 12)

#evaluate
rmse = sqrt(mean((lm_pred - new_data$growth)^2))
rmse

#in-sample profit
profit = sum((rec['growth'] + 1)*(budget/portfolio)) - budget
profit

#in-sample profit rate
profit_rate = profit / budget
profit_rate

data.opportunities = read.csv("Investment Opportunities.csv", header=TRUE)
size(data.opportunities)
data.opportunities[1:6,]

# Retrieve the 2018 data (or use the 2018 data retrieved earlier).
# Filter the data to include only companies listed as investment opportunities.

data18 = co18[co18$gvkey %in% data.opportunities$gvkey,]

# Partition the dataset as described.
data18.q1 = data18[(data18$quarter==1) & !is.na(data18$prccq), -ncol(data18)]
data18.q2 = data18[(data18$quarter==2) & !is.na(data18$prccq), -ncol(data18)]
data18.q3 = data18[(data18$quarter==3) & !is.na(data18$prccq), -ncol(data18)]
data18.q4 = data18[(data18$quarter==4) & !is.na(data18$prccq), -ncol(data18)]


data18.q1 = data18.q1[!duplicated(data18.q1$gvkey),]
data18.q2 = data18.q2[!duplicated(data18.q2$gvkey),]
data18.q3 = data18.q3[!duplicated(data18.q3$gvkey),]
data18.q4 = data18.q4[!duplicated(data18.q4$gvkey),]

 
colnames(data18.q1)[-c(1, 10, 12)] = paste0(colnames(data18.q1)[-c(1, 10, 12)], ".q1")
colnames(data18.q2)[-c(1, 10, 12)] = paste0(colnames(data18.q2)[-c(1, 10, 12)], ".q2")
colnames(data18.q3)[-c(1, 10, 12)] = paste0(colnames(data18.q3)[-c(1, 10, 12)], ".q3")
colnames(data18.q4)[-c(1, 10, 12)] = paste0(colnames(data18.q4)[-c(1, 10, 12)], ".q4")

# Consolidate the partitions as described.
m1 = merge(data18.q1, data18.q2, by=c("gvkey", "tic", "conm"), all=TRUE)
m2 = merge(data18.q3, data18.q4, by=c("gvkey", "tic", "conm"), all=TRUE)
 
fun18 = merge(m1, m2, by=c("gvkey", "tic", "conm"), all=TRUE, sort=TRUE)
fun18 = fun18[!is.na(fun18$prccq.q4),]

# Change the representation of the test data appropriately.
fun18 = fun18[, colnames(fun18) %in% colnames(data)]
fun18 = put_impute(fun18, impute_vals)

pca_data18 = fun18[, colnames(fun18) %in% colnames(pca_data)]

pcs = as.matrix(pc$rotation)

pc18 = as.matrix(as.matrix(scale(pca_data18)) %*% pcs[1:737,])

test_data = data.frame(gvkey = fun18['gvkey'], tic = fun18['tic'],
                      conm = fun18['conm'], PC1 = pc18[,1], PC2 = pc18[,2])

# Predict outcomes of test data.
# Recommend a portfolio of allocations to 12 companies and store as a data.frame with these columns:
#   gvkey, tic, conm, allocation
#
# Companies must be from investment opportunities.  Allocations must sum to budget.
#
# Present the portfolio recommendation.

test_pred = predict(mymodel, test_data)

rec = head(arrange(test_data[,1:3], -test_pred), 12)
rec['allocation'] = rep(budget/portfolio, portfolio)

# write.csv(portfolio, paste0(analyst, ".csv"), row.names=FALSE)
 write.csv(rec, paste0(analyst, ".csv"), row.names=FALSE)

recommend = read.csv(paste0(analyst, ".csv"), header=TRUE)
recommend


columns = all(colnames(recommend) == c("gvkey", "tic", "conm", "allocation"))
companies = all(recommend$gvkey %in% data.opportunities$gvkey)
allocations = round(sum(recommend$allocation)) == budget
                         
check = data.frame(analyst, columns, companies, allocations)
fmt(check, "Portfolio Recommendation | Format Check")

