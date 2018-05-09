# Method 1: using the native R adist

source1.devices<-read.csv('data/source1.csv', header = T, sep = ";")
source2.devices<-read.csv('data/source2.csv', header = T,  sep = ";")
head(source1.devices)
# To make sure we are dealing with charts
source1.devices$name<-as.character(source1.devices$name)
source2.devices$name<-as.character(source2.devices$name)

# It creates a matrix with the Standard Levenshtein distance between the name fields of both sources
dist.name<-adist(source1.devices$name,source2.devices$name, partial = TRUE, ignore.case = TRUE)

# We now take the pairs with the minimum distance
min.name<-apply(dist.name, 1, min)

match.s1.s2<-NULL  
for(i in 1:nrow(dist.name)){
  s2.i<-match(min.name[i],dist.name[i,])
  s1.i<-i
  match.s1.s2<-rbind(data.frame(s2.i=s2.i,s1.i=s1.i,s2name=source2.devices[s2.i,]$name, s1name=source1.devices[s1.i,]$name, adist=min.name[i]),match.s1.s2)
}
# and we then can have a look at the results
View(match.s1.s2)


# Method 2: applying different string matching methods
#osa Optimal string aligment, (restricted Damerau-Levenshtein distance).
#lv Levenshtein distance (as in R's native adist).
#dl Full Damerau-Levenshtein distance.
#hamming Hamming distance (a and b must have same nr of characters).
#lcs Longest common substring distance.
#qgram q-gram distance.
#cosine cosine distance between q-gram profiles
#jaccard Jaccard distance between q-gram profiles
#jw Jaro, or Jaro-Winker distance.

#install.packages('stringdist')
library(stringdist)

distance.methods<-c('osa','lv','dl','hamming','lcs','qgram','cosine','jaccard','jw')
dist.methods<-list()
for(m in 1:length(distance.methods)){
  dist.name.enh<-matrix(NA, ncol = length(source2.devices$name),nrow = length(source1.devices$name))
  for(i in 1:length(source2.devices$name)) {
    for(j in 1:length(source1.devices$name)) { 
      dist.name.enh[j,i]<-stringdist(tolower(source2.devices[i,]$name),tolower(source1.devices[j,]$name),method = distance.methods[m])      
      #adist.enhance(source2.devices[i,]$name,source1.devices[j,]$name)
    }  
  }
  dist.methods[[distance.methods[m]]]<-dist.name.enh
}

match.s1.s2.enh<-NULL
for(m in 1:length(dist.methods)){
  
  dist.matrix<-as.matrix(dist.methods[[distance.methods[m]]])
  min.name.enh<-apply(dist.matrix, 1, base::min)
  for(i in 1:nrow(dist.matrix))
  {
    s2.i<-match(min.name.enh[i],dist.matrix[i,])
    s1.i<-i
    match.s1.s2.enh<-rbind(data.frame(s2.i=s2.i,s1.i=s1.i,s2name=source2.devices[s2.i,]$name, s1name=source1.devices[s1.i,]$name, adist=min.name.enh[i],method=distance.methods[m]),match.s1.s2.enh)
  }
}
# Let's have a look at the results
library(reshape2)
matched.names.matrix<-dcast(match.s1.s2.enh,s2.i+s1.i+s2name+s1name~method, value.var = "adist")
View(matched.names.matrix)




# Now my own method applying some more knowledge about the data

# First a small but really helpfull function
trim <- function (x) gsub("^\s+|\s+$", "", x)
# Then we implement our own distance function 
# taking the shortest string applying Levenshtein distance sliding over the largest one to take the minimum
adist.custom <- function (str1, str2, sliding = TRUE)
{  
  s.str1<-strsplit(trim(str1), split=' ')
  s.str2<-strsplit(trim(str2), split=' ')
  s.str2<-trim(unlist(s.str2))
  s.str1<-trim(unlist(s.str1))
  
  if (length(s.str2)>=length(s.str1))
  {
    short.str<-  s.str1
    long.str<-s.str2
  } else {
    short.str <- s.str2
    long.str<-s.str1
  }
  # sliding
  return.dist<-0
  if (sliding == TRUE)
  {
    min<-99999
    s1<-trim(paste(short.str,collapse = ' '))
    for (k in 1:(length(long.str)-length(short.str)))
    {
      s2<-trim(paste(long.str[k:(length(short.str)+(k-1))],collapse = ' '))    
      ads<-adist(s1,s2,partial = TRUE, ignore.case = TRUE)
      min <- ifelse(ads<min,ads,min)
    }
    return.dist<-min
  } else {
    #string start matching  
    s1<-trim(paste(short.str,collapse = ' '))
    s2<-trim(paste(long.str[1:length(short.str)],collapse = ' '))
    return.dist<-adist(s1,s2,partial = TRUE, ignore.case = TRUE)
  }
  return (return.dist)  
}


dist.name.custom<-matrix(NA, ncol = length(source2.devices$name),nrow = length(source1.devices$name))
for(i in 1:length(source2.devices$name)) {
  for(j in 1:length(source1.devices$name)) { 
    dist.name.custom[j,i]<-adist.custom(tolower(source2.devices[i,]$name),tolower(source1.devices[j,]$name))      
  }  
}

min.name.custom<-apply(dist.name.custom, 1, min)
match.s1.s2<-NULL
for(i in 1:nrow(dist.name.custom)){
  s2.i<-match(min.name.custom[i],dist.name.custom[i,])
  s1.i<-i
  match.s1.s2<-rbind(data.frame(s2.i=s2.i,s1.i=s1.i,s2name=source2.devices[s2.i,]$name, s1name=source1.devices[s1.i,]$name, adist=min.name.custom[i]),match.s1.s2)
}
# let's have a look at the results
View(match.s1.s2)


##
x <- c('foo', 'bar')

# if we decrease the maximum allowd distance, we get 
amatch('fu',x,maxDist=1)


# just like with 'match' you can control the output of no-matches:
amatch('fu',x,maxDist=1,nomatch=0)


# to see if 'fu' matches approximately with any element of x:
ain('fu',x)


# however, if we allow for larger distances
ain('fu',x,maxDist=2)




library(stringdist)
d <- expand.grid(source1.devices$name,source2.devices$name) # Distance matrix in long form
names(d) <- c("a_name","b_name")
d$dist <- stringdist(d$a_name,d$b_name, method="jw") # String edit distance (use your favorite function here)

# Greedy assignment heuristic (Your favorite heuristic here)
greedyAssign <- function(a,b,d){
  x <- numeric(length(a)) # assgn variable: 0 for unassigned but assignable, 
  # 1 for already assigned, -1 for unassigned and unassignable
  while(any(x==0)){
    min_d <- min(d[x==0]) # identify closest pair, arbitrarily selecting 1st if multiple pairs
    a_sel <- a[d==min_d & x==0][1] 
    b_sel <- b[d==min_d & a == a_sel & x==0][1] 
    x[a==a_sel & b == b_sel] <- 1
    x[x==0 & (a==a_sel|b==b_sel)] <- -1
  }
  cbind(a=a[x==1],b=b[x==1],d=d[x==1])
}
res = data.frame(greedyAssign(as.character(d$a_name),as.character(d$b_name),d$dist))
head(res)
View(res)


#install.packages('fuzzywuzzyR')
library(fuzzywuzzyR)

word = "new york jets"

choices = c("Atlanta Falcons", "New York Jets", "New York Giants", "Dallas Cowboys")


#------------
# processor :
#------------

init_proc = FuzzUtils$new()      # initialization of FuzzUtils class to choose a processor

PROC = init_proc$Full_process    # processor-method

PROC1 = tolower                  # base R function ( as an example for a processor )

#---------
# scorer :
#---------

init_scor = FuzzMatcher$new()    # initialization of the scorer class

SCOR = init_scor$WRATIO          # choosen scorer function


init <- FuzzExtract$new()        # Initialization of the FuzzExtract class


init$Extract(string = word, sequence_strings = choices, processor = PROC, scorer = SCOR)

