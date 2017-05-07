infile = read.csv("C:\\Users\\anthony\\Desktop\\NaiveBayes\\Q1_data.csv", sep =",")

nin = ncol(infile)

target_n = infile[,nin]
infile_n = infile[, -nin]
colname = names(infile_n)

#normal distribution

model_list = lapply(unique(target_n), function(x){
  sd_ = sapply(infile_n[which(target_n==x),], function(x){sd(x)})
  mean_ = sapply(infile_n[which(target_n==x),], function(x){mean(x)})
  list(
    "class" = x,
    "mean" = mean_,
    "sd" = sd_
  )
})
#model_list[[3]]$class


#------------ Query -------------#
query = c(222,4.5,1518,74,0.25,1642)


naive_score = sapply(model_list, function(class_n){
  #print("")
  prod(sapply(1:length(infile_n), function(feature){
    #print(pnorm(as.numeric(query[feature]), mean=class_n[[2]][feature], sd=class_n[[3]][feature]))
    pnorm(as.numeric(query[feature]), mean=class_n[[2]][feature], sd=class_n[[3]][feature])
  }))
})

print(unique(target_n)[which.max(naive_score)])