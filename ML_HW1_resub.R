library(tree)
library(caret)


set.seed(5)
n = 0.2*nrow(iris)
ind_test <- sample(1:nrow(iris), n)
iris_train <- iris[-ind_test,]
iris_test <- iris[ind_test,]
#grow tree
tree_model = tree(Species ~ ., data=iris)

plot(tree_model)
text(tree_model, pretty = 0)

#test
tree_pred = predict(tree_model, iris_test, type ='class')
ac = 1-mean(tree_pred!=iris_test$Species)

con_tr = confusionMatrix(tree_pred, iris_test$Species, dnn=c("Pred","Ref"))
print(con_tr$table)
print(ac)


#cross validation
#set.seed(5)
#cv_tree = cv.tree(tree_model, FUN=prune.misclass, K=2)
#plot(cv_tree)
#prune
pruned_model = prune.misclass(tree_model, best=3)
plot(pruned_model)
text(pruned_model, pretty = 0)
#pruned_model = prune.misclass(tree_model, best=4)
pruned_pred = predict(pruned_model, iris_test, type ='class')
pr_ac = 1 - mean(pruned_pred!=iris_test$Species)

con_pr = confusionMatrix(pruned_pred, iris_test$Species, dnn=c("Pred","Ref"))
print(con_pr$table)
print(pr_ac)