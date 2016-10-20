download.file("https://git.embl.de/hsanchez/BTM2016_RDatasets/raw/master/bodyfat.csv", "bodyfat.csv")

bodyfat<-read.csv("bodyfat.csv")

head(bodyfat)

# Names of the columns
names(bodyfat)

# Dimensions
dim(bodyfat)

# Checking only one column, try the TAB key
bodyfat$age
head( bodyfat[ ,4] )
head( bodyfat[ ,4], 12 )

# Numeric values describing age
summary(bodyfat$age)

# Getting the mean of the age values
summary(bodyfat$age)[4]
mean(bodyfat$age)



# How many are younger than 31 years old?
younger_than_31 <- bodyfat[ (bodyfat$age < 31), ]
length(younger_than_31)

# Another way
not_older_than_30 <- bodyfat[ !(bodyfat$age > 30), ]
length(not_older_than_30)

# Watch out!
head( bodyfat[  !(bodyfat$age  > 30), ] )
head( bodyfat[  !which(bodyfat$age  > 30), ] )
## not equivalent!



number_of_columns <- length(names(bodyfat))
number_of_columns <- dim(bodyfat)[2]
my_vector <- vector(length = number_of_columns)
for( i in 1:length( names(bodyfat) ) ){
  one_column <- bodyfat[ ,i]
  the_mean <- mean(one_column)
  my_vector[i] <- the_mean
}
names(my_vector) <- names(bodyfat)

my_vector

apply(bodyfat, MARGIN = 2, FUN = mean)

usingFor <- function(){
  my_vector <- vector(length = number_of_columns)
  for( i in 1:length( names(bodyfat) ) ){
   one_column <- bodyfat[ ,i]
   the_mean <- mean(one_column)
   my_vector[i] <- the_mean
  }
  names(my_vector) <- names(bodyfat)
}
usingApply <- function(){
  temp <- apply(bodyfat, MARGIN = 2, FUN = mean)
}

system.time(replicate(1000000, usingFor))
system.time(replicate(1000000, usingApply))

library(pryr)
mem_change(v <- 1:1e6)
mem_change(rm(v))
try(rm(new_variable))
mem_change(new_variable <- bodyfat$age)


# Going back!

apply(bodyfat, MARGIN = 2, FUN = mean)

Subset_1_indexes<-which(bodyfat$age < 31)
Subset_1<-bodyfat[Subset_1_indexes, ]

# Getting the 30+ 
temp_indexes<-which(bodyfat$age > 30)
Subset30plus<-bodyfat[temp_indexes, ]

Subset_2_indexes<-which(Subset30plus$age < 51)
Subset_2<-Subset30plus[Subset_2_indexes, ]

Subset_3_indexes<-which(Subset30plus$age > 50)
Subset_3<-Subset30plus[Subset_3_indexes, ]

summary(Subset_2$age)
summary(Subset_3$age)

# Finally, getting the weight per Subset:
mean(Subset_1$percent.fat)
mean(Subset_2$percent.fat)
mean(Subset_3$percent.fat)

# Young people
mean( bodyfat[  bodyfat$age < 31, ]$percent.fat )
# Not so young people
mean( bodyfat[  bodyfat$age > 30 & bodyfat$age < 51, ]$percent.fat )
# Definitely not young people
mean( bodyfat[  bodyfat$age > 50, ]$percent.fat )

Option1<-function(){
  Subset_1_indexes<-which(bodyfat$age < 31)
  Subset_1<-bodyfat[Subset_1_indexes, ]
  temp_indexes<-which(bodyfat$age > 30)
  Subset30plus<-bodyfat[temp_indexes, ]
  Subset_2_indexes<-which(Subset30plus$age < 51)
  Subset_2<-Subset30plus[Subset_2_indexes, ]
  Subset_3_indexes<-which(Subset30plus$age > 50)
  Subset_3<-Subset30plus[Subset_3_indexes, ]
  summary(Subset_2$age)
  summary(Subset_3$age)
  mean(Subset_1$percent.fat)
  mean(Subset_2$percent.fat)
  mean(Subset_3$percent.fat)
}

Option2<-function(){
  mean( bodyfat[  bodyfat$age < 31, ]$percent.fat )
  mean( bodyfat[  bodyfat$age > 30 & bodyfat$age < 51, ]$percent.fat )
  mean( bodyfat[  bodyfat$age > 50, ]$percent.fat )
}

system.time(replicate(1000000, Option1))
system.time(replicate(1000000, Option2))

# Which columns are not the index, nor the age?
I_do_not_want_these_ones<-which(names(bodyfat)==c("X", "age"))
names(bodyfat[-I_do_not_want_these_ones])

# I calculate the pearson correlations of age and all the other values
sapply(bodyfat[-I_do_not_want_these_ones], function(x){
  cor(bodyfat$age, x)
})


cor(bodyfat$percent.fat, bodyfat$density)

plot(bodyfat$age, bodyfat$percent.fat, main="Age vs Percent fat")
plot(bodyfat$age, bodyfat$density, main="Age vs Density")
plot(bodyfat$percent.fat, bodyfat$density, main="Percent fat vs Density")

pairs(bodyfat)

pairs(bodyfat[ , 1:6])

message("Your code is talking to you")

#for(i in c(2, 1, 0, -1)){
#  sqrt(i)
#}

for(i in c(2, 1, 0, -1)){
  if(i >= 0){
    sqrt(i)
    print(i)
  }
}

# Trying to calculate the mean of the columns 4th, 3rd, and 17th
# Unknown vector
UnknownVector <- c(c(4, 3, 17))
#sapply( UnknonwVector, function(x){
#  mean(bodyfat[, x])
#})

# bodyfat[ ,17]

# Lets prevent to have an error:
# option 1:
sapply( UnknownVector, function(x){
  try(mean(bodyfat[, x]))
} )

# Option 2:
sapply( UnknownVector[ which( UnknownVector %in% 1:dim(bodyfat)[2] ) ], function(x){
  mean( bodyfat[, x] )
} )

# Option 3:
options(show.error.messages = FALSE)
sapply( UnknownVector, function(x){
  to_return <- NA
  column <- try(bodyfat[, x])
  if( class(column) == "try-error" ){
    message( paste0("The column ", x, " does not exist, I'm sorry :( ") )
  }  else
    to_return <- mean( column )
  return(to_return)
} )
options(show.error.messages = TRUE)
                                                                 
                                                                 

                                                                                                                                  