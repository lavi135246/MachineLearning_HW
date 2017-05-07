#rm(list = ls())
infile = read.csv("C:\\Users\\anthony\\Desktop\\NaiveBayes\\Q2_training.csv", header=TRUE, sep =",")#1~504 training

nin = ncol(infile)

target_n = infile[,nin]
infile_n = infile[, -nin]
colname = names(infile_n)

#normal distribution

model_list = lapply(unique(target_n), function(x){
  sd_ = sapply(infile_n[which(target_n==x),], function(x){sd(x, na.rm=TRUE)})
  mean_ = sapply(infile_n[which(target_n==x),], function(x){mean(x, na.rm=TRUE)})
  list(
    "class" = x,
    "mean" = mean_,
    "sd" = sd_
  )
})

test = read.csv("C:\\Users\\anthony\\Desktop\\NaiveBayes\\Q2_testing.csv", header=TRUE, sep =",")

for(i in 1:length(test$V2)){
  query = as.numeric(test[i,])
  naive_score = sapply(model_list, function(class_n){
    #print("")
    prod(sapply(1:length(infile_n), function(feature){
      #print(pnorm(as.numeric(query[feature]), mean=class_n[[2]][feature], sd=class_n[[3]][feature]))
      pnorm(as.numeric(query[feature]), mean=class_n[[2]][feature], sd=class_n[[3]][feature])
    }),na.rm=TRUE)
  })
  
  print(unique(target_n)[which.max(naive_score)])
}