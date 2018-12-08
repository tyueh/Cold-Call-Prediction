## read the data and take a look into it ----
f = read.csv(file.choose(), header=T)
head(f)
summary(f)

############################################################################################################
## preprocessing----

## transform the temporal variables
times = c("CallStart", "CallEnd")
for (col in times) {f[,col] = as.POSIXct(f[,col], format="%H:%M:%S")}
f[,"CallDur"] = as.numeric(f$CallEnd -f$CallStart, "secs")
f[,"CallStart"] = NULL
f[,"CallEnd"] = NULL

## exam data since glm cannot handle missing value and categorical variables
colnames(f)[colSums(is.na(f)) > 0] ## find columns with missing value
names(Filter(is.factor, f)) ## get all names of categorical variables 

## create dummy variable)
glm_f = f
levels(glm_f$Job)
glm_f[,'Job_admin'] = ifelse(glm_f$Job=='admin.', 1, 0)
glm_f[,'Job_blue-collar'] = ifelse(glm_f$Job=='blue-collar', 1, 0)
glm_f[,'Job_entrepreneur'] = ifelse(glm_f$Job=='entrepreneur', 1, 0)
glm_f[,'Job_housemaid'] = ifelse(glm_f$Job=='housemaid', 1, 0)
glm_f[,'Job_management'] = ifelse(glm_f$Job=='management', 1, 0)
glm_f[,'Job_retired'] = ifelse(glm_f$Job=='retired', 1, 0)
glm_f[,'Job_self-employed'] = ifelse(glm_f$Job=='self-employed', 1, 0)
glm_f[,'Job_services'] = ifelse(glm_f$Job=='services', 1, 0)
glm_f[,'Job_student'] = ifelse(glm_f$Job=='student', 1, 0)
glm_f[,'Job_technician'] = ifelse(glm_f$Job=='technician', 1, 0)
glm_f[,'Job_unemployed'] = ifelse(glm_f$Job=='unemployed', 1, 0)
glm_f[,"Job"] = NULL
levels(glm_f$Marital)
glm_f[,'Mar_divorced'] = ifelse(glm_f$Marital=='divorced', 1, 0)
glm_f[,'Mar_married'] = ifelse(glm_f$Marital=='married', 1, 0)
glm_f[,"Marital"] = NULL
levels(glm_f$Education)
glm_f[,'Edu_primary'] = ifelse(glm_f$Education=='primary', 1, 0)
glm_f[,'Edu_secondary'] = ifelse(glm_f$Education=='secondary', 1, 0)
glm_f[,'Edu_tertiary'] = ifelse(glm_f$Education=='tertiary', 1, 0)
glm_f[,"Education"] = NULL
levels(glm_f$Communication)
glm_f[,'Com_cellular'] = ifelse(glm_f$Communication=='cellular', 1, 0)
glm_f[,'Com_telephone'] = ifelse(glm_f$Communication=='telephone', 1, 0)
glm_f[,"Communication"] = NULL
levels(glm_f$LastContactMonth)
glm_f[,'jan'] = ifelse(glm_f$LastContactMonth=='jan', 1, 0)
glm_f[,'feb'] = ifelse(glm_f$LastContactMonth=='feb', 1, 0)
glm_f[,'mar'] = ifelse(glm_f$LastContactMonth=='mar', 1, 0)
glm_f[,'apr'] = ifelse(glm_f$LastContactMonth=='apr', 1, 0)
glm_f[,'may'] = ifelse(glm_f$LastContactMonth=='may', 1, 0)
glm_f[,'jun'] = ifelse(glm_f$LastContactMonth=='jun', 1, 0)
glm_f[,'jul'] = ifelse(glm_f$LastContactMonth=='jul', 1, 0)
glm_f[,'aug'] = ifelse(glm_f$LastContactMonth=='aug', 1, 0)
glm_f[,'sep'] = ifelse(glm_f$LastContactMonth=='sep', 1, 0)
glm_f[,'oct'] = ifelse(glm_f$LastContactMonth=='oct', 1, 0)
glm_f[,'nov'] = ifelse(glm_f$LastContactMonth=='nov', 1, 0)
glm_f[,"LastContactMonth"] = NULL
levels(glm_f$Outcome)
glm_f[,'OutC_failure'] = ifelse(glm_f$Outcome=='failure', 1, 0)
glm_f[,'OutC_other'] = ifelse(glm_f$Outcome=='other', 1, 0)
glm_f[,'OutC_success'] = ifelse(glm_f$Outcome=='success', 1, 0)
glm_f[,"Outcome"] = NULL
colnames(f)[colSums(is.na(f)) > 0]
names(Filter(is.factor, glm_f)) ## check 


## holdout data
## for trasformed data (logistic regression)
set.seed(100)
s = sample(4000, 3000)
glm_train = glm_f[s,]
glm_test = glm_f[-s,]

## for original data (decision tree)
set.seed(100)
s = sample(4000, 3000)
train = f[s,]
test = f[-s,]




############################################################################################################
## logistic regression ----

## build logistic regression model
glm_fullmod = glm(CarInsurance~.,data=glm_train, family=binomial(link='logit'))
summary(glm_fullmod)

glm_backmod = step(glm_fullmod) # default backward determine AIC
summary(glm_backmod)

formula(glm_backmod)
## manually feature selection
## remove "Job_unemployed"
glm_mod = glm(formula = CarInsurance ~ HHInsurance + CarLoan + NoOfContacts + 
                PrevAttempts + CallDur + Job_admin + Job_retired + Job_student + 
                Mar_married + Edu_primary + Edu_secondary + 
                Com_cellular + Com_telephone + jan + feb + mar + apr + may + 
                jul + aug + nov + OutC_success, family = binomial(link='logit'), data = glm_train)
summary(glm_mod)

## remove "Job_admin"
glm_mod = glm(formula = CarInsurance ~ HHInsurance + CarLoan + NoOfContacts + 
                PrevAttempts + CallDur + Job_retired + Job_student + 
                Mar_married + Edu_primary + Edu_secondary + 
                Com_cellular + Com_telephone + jan + feb + mar + apr + may + 
                jul + aug + nov + OutC_success, family = binomial(link='logit'), data = glm_train)
summary(glm_mod) ## all beta seem good (all p-value in 0.05)

formula(glm_mod)

## statistical examination
1-pchisq(4051.5-2241.7, 2999-2978)
aov(glm_mod)
anova(glm_mod)

## training set result
train_res = glm_mod$fitted.values
train_res = ifelse(train_res<=0.5, 0, 1)
glm.train.perf = table(glm_train[,'CarInsurance'], train_res, dnn=c('Actual', 'Predict'))

## testing  set result
test_res = predict(glm_mod, glm_test, type='response') ## response: for link='logit'
test_res = ifelse(test_res<=0.5, 0, 1)
glm.test.perf = table(glm_test[,'CarInsurance'], test_res, dnn=c('Actual', 'Predict'))

## define a function to compute accuracy, sensitivity, specificity
ASF = function(tbl){
  res = matrix(0,0,2)
  for (n in 1:sqrt(length(tbl))){
    TP = tbl[n,n]   ## [row,col]
    TN = sum(tbl[-n,-n])
    FP = sum(tbl[-n,n])
    FN = sum(tbl[n,-n])
    sen = TP/(TP+FN)
    spc = TN/(TN+FP)
    res = rbind(res, c(sen, spc))
  }
  colnames(res) = c('Sensitivity', 'Specificity')
  rownames(res) = labels(tbl)$Actual
  print(paste('Accuracy:', sum(diag(tbl))/sum(tbl)))
  return(res)
}

## compare test with train result
glm.train.perf
glm.test.perf
ASF(glm.train.perf)
ASF(glm.test.perf)

## Cross Validation
library(boot)
cv10 = NULL
for (i in 1:10){
  mod= glm(formula = CarInsurance ~ poly(HHInsurance + CarLoan + NoOfContacts + 
             PrevAttempts + CallDur + Job_retired + Job_student + 
             Mar_married + Edu_primary + Edu_secondary + 
             Com_cellular + Com_telephone + jan + feb + mar + apr + may + 
             jul + aug + nov + OutC_success, i), family = binomial(link='logit'), data = glm_train)
  cv10[i] = cv.glm(glm_train,mod, K=10)$delta[1]
}
cv10
mean(cv10)




############################################################################################################
## decision tree ----

library(rpart)
library(rpart.plot)

res = data.frame()
## loop throgh all possible tree
for (p_lim in 500:1){
  c_lim = floor(p_lim/3)
  fit = rpart(CarInsurance~., train, method='class', parms = list(split = 'gini'),
              control = rpart.control(minsplit = p_lim, minbucket = c_lim, cp=0, maxdepth=8, xval=10))
  
  train_res = predict(fit, train, type='class')
  train_misclf = mean(train_res != train[,'CarInsurance'])*100
  test_res = predict(fit, test, type='class')
  test_misclf = mean(test_res != test[,'CarInsurance'])*100
  t = table(fit$frame$var)
  cplx = t[names(t)=='<leaf>']
  cp_matrix = printcp(fit)
  xerror = cp_matrix[dim(cp_matrix)[1], 4]*100
  xstd = cp_matrix[dim(cp_matrix)[1], 5]
  res = rbind(res, c(cplx, p_lim, c_lim, train_misclf, test_misclf, xerror, xstd))
  
}
rm(c_lim, cplx, fit, p_lim, t, test_misclf, test_res, train_misclf, train_res)
colnames(res) = c('leaf_nodes_count', 'parent_size', 'children_size','train_misclf', 'test_misclf', 'xerror', 'xstd')

x = res$leaf_nodes_count
y1 = res$train_misclf
y2 = res$test_misclf


## line lowest point, mark node#, and return parent and children size
aux_line = function(df, target){
  min_val = min(target)
  node_count = df[target==min_val,][1,1]
  p_size = df[target==min_val,][1,2]
  c_size = df[target==min_val,][1,3]
  abline(h=min_val, col = 'black', lwd = 0.5)
  abline(v=node_count, col='black', lwd = 1.5)
  text(node_count, 0, as.character(node_count), col = "red", cex=1.5, ps = 0.1, font = 4)
  return(c(p_size, c_size))
}

## overfitting plot
plot(x, y1, main = 'Cold Call Data\nOverfitting Plot',
     xlab='Leaf Nodes Number', xlim=range(x), ylab ='Error(%)', ylim=c(0,35), type='n', las = 1)
lines(x[order(x)], y1[order(x)], col='blue', lwd = 2)
lines(x[order(x)], y2[order(x)], col='red', lwd = 2)

## auxiliary lines and text
pc_size = aux_line(res, res$test_misclf)
legend('topright', legend=c('Train','Test', 'overfit_point'), fill=c('blue', 'red', 'black'), cex=0.7)

## recreate the best tree
## node#:30 ----> minsplit:47 , minbucket:15
dtree = rpart(CarInsurance~., train, method='class', parms = list(split = 'gini'),
              control = rpart.control(minsplit =pc_size[1],minbucket =pc_size[2], cp=0, maxdepth=8, xval=10))
printcp(dtree)
plotcp(dtree)
dtree_prune = prune(dtree, cp = 0.0068474)

## print tree
prp(dtree_prune, type=1, extra=101,fallen.leaves=T, main="Cold Call Data\nDecision Tree", faclen=0, cex=0.8)

## pruned dtree performance
train_res = predict(dtree_prune, train, type="class")
dtree.prune.train.perf = table(train$CarInsurance, train_res, dnn=c("Actual", "Predicted"))
test_res = predict(dtree_prune, test, type="class")
dtree.prune.test.perf = table(test$CarInsurance, test_res, dnn=c("Actual", "Predicted"))

## show performance
dtree.prune.train.perf
dtree.prune.test.perf
ASF(dtree.prune.train.perf)
ASF(dtree.prune.test.perf)

## 3 importance variables
barplot(dtree_prune$variable.importance)
colors = c(rep("dodgerblue", 3), rep('grey', 20))
barplot(dtree_prune$variable.importance, col=colors, space = 1, main='Importance of Variables', xaxt="n", las=2)
text(seq(2,27,by=2),par("usr")[3]-0.5, labels = paste(names(dtree_prune$variable.importance)),
     srt = 30, adj= 1, xpd = TRUE, cex=1.2)
box()


############################################################################################################
## model comparison ----

## confusion matrix 
glm.test.perf
dtree.prune.test.perf
ASF(glm.test.perf)
ASF(dtree.prune.test.perf)


library(ROSE)
## ROC curve
glm_res = predict(glm_mod, glm_test, type='response')
tree_res = predict(dtree_prune, test, type="class")
roc.curve(glm_test$CarInsurance, glm_res, main='Logistic Regression\nROC Curve')
roc.curve(test$CarInsurance, tree_res, main='Decision Tree\nROC Curve')
