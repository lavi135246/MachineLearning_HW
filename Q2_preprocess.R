library(readr)
infile = read.csv("C:\\Users\\anthony\\Downloads\\ML_assignment3_data.txt", header=FALSE, sep =",")#1~505 training
infile[infile=="?"] = NA #Remove ?

#modify time
infile$V1 = gsub("D-","", infile$V1)
#infile$V1 = gsub("(/)(9)", "\\119\\2", infile$V1)  ##incorrect
infile$V1 = gsub('^(.*)(.{2})$', '\\119\\2', infile$V1)
infile$V1 = as.POSIXct(strptime(infile$V1, "%d/%m/%Y"))
infile$V40 = 0# for classifying

for(class_num in 1:6){
  fileName = "C:\\Users\\anthony\\Desktop\\NaiveBayes\\class"
  fileName = paste0(fileName,class_num)
  fileName = paste0(fileName,".txt")
  class_in = read_file(fileName)
  rules = strsplit(class_in, ",|\\.")
  rules = gsub("D-| |\r|\n|\t", "", rules[[1]])
  
  
  #setting rules
  r_range = rules[grep("to", rules)]
  r_from = gsub('to(.*)$', '', r_range)
  r_to = gsub('^(.*)to', '', r_range)
  r_from = gsub('^(.*)(.{2})$', '\\119\\2', r_from)
  r_to = gsub('^(.*)(.{2})$', '\\119\\2', r_to)
  r_from = as.POSIXct(strptime(r_from, "%d/%m/%Y"))
  r_to = as.POSIXct(strptime(r_to, "%d/%m/%Y"))
  
  r_ind = lapply(1:length(r_from), function(x){
    intersect(which(infile$V1>=r_from[x]),which(infile$V1<=r_to[x]))
  })
  r_ind = unlist(r_ind)
  
  s_range = rules[-grep("to", rules)]
  s_range = gsub('^(.*)(.{2})$', '\\119\\2', s_range)
  s_range = as.POSIXct(strptime(s_range, "%d/%m/%Y"))
  s_ind = sapply(s_range, function(x){ which(infile$V1==x) })
  s_ind = unlist(s_ind)# when s_range is empty sapply will return a list

  infile$V40[s_ind] = class_num
  infile$V40[r_ind] = class_num
}

#split training and testing
training = infile[1:504,]
testing = infile[505:length(infile$V1),]

training$V1 = NULL
testing$V1 = NULL
testing$V40 = NULL

training = training[-which(training$V40==0),]

write.table(training, file="C:\\Users\\anthony\\Desktop\\NaiveBayes\\Q2_training.csv", sep= ",", eol="\n", row.names=FALSE)
write.table(testing, file="C:\\Users\\anthony\\Desktop\\NaiveBayes\\Q2_testing.csv", sep= ",", eol="\n", row.names=FALSE)