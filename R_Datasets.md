---
title: "Datasets_in_R"
output: html_document
---

Download the dataset from:
```{r}
'URL'
```

Read the dataset:

```{r}
bodyfat<-read.csv("../BTM2016/R_datasets/bodyfat.csv")
```

Explore the dataset:
```{r}
# Firts lines
head(bodyfat)

# Names of the columns
names(bodyfat)

```

Brief explanation of the values:
* Density determined from underwater weighing
* Percent body fat from Siri's (1956) equation
* Age (years)
* Weight (lbs)
* Height (inches)
* Neck circumference (cm)
* Chest circumference (cm)
* Abdomen 2 circumference (cm)
* Hip circumference (cm)
* Thigh circumference (cm)
* Knee circumference (cm)
* Ankle circumference (cm)
* Biceps (extended) circumference (cm)
* Forearm circumference (cm)
* Wrist circumference (cm)

Lets continue exploring the dataset

```{r}
# Dimensions
dim(bodyfat)

# Numeric values describing age
summary(bodyfat$age)

# Getting the mean of the age values
summary(bodyfat$age)[4]
mean(bodyfat$age)

```

Getting the mean of all the values

Using a 'for' loop

```{r}
number_of_columns<-length(names(bodyfat))
number_of_columns<-dim(bodyfat)[2]
my_vector<-vector(length = number_of_columns)
for(i in 1:length(names(bodyfat))){
    one_column<-bodyfat[ ,i]
    the_mean<-mean(one_column)
    my_vector[i]<-the_mean
}
names(my_vector)<-names(bodyfat)

```

Using the function 'apply()'

```{r}
apply(bodyfat, MARGIN = 2, FUN = mean)
```

178 lbs equals to 80 kg, which I think is quite a lot... 
What if this value is age-depentent?
Lets make three subsets and calculate the average weight for each of them:
Subset_1: younger than 31 yo
Subset_2: older than 30 yo but younger than 51 yo
Subset_3: older than 50 yo

# Option 1: generating many variables
```{r}
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
mean(Subset_1$weight)
mean(Subset_2$weight)
mean(Subset_3$weight)
```



bodyfat[which(bodyfat$age == 60), ]



