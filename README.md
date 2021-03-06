# Homework-2-Econometrics
---
 "Homework 2, Lab 2, Econ B2000"
 
'Christopher Tinevra, Group Members: Isabela Vieira, Akimawe Kadiri, Nicole Kerrison, Mostafa Ragheb'

---

Question: How likely is that this dice is unfair? We are trying to determine whether the dice is generating random numbers or if it's actually following a certain pattern.  

##### Null Hypothesis Ho - No relationship; the die is generating random numbers and thus is fair.

##### Alternative Hypothesis Ha - The die is unfair; it has been fixed to generate more 6s on average.

So, we are trying to test whether or not the die is unfair, given that our naive assessment is that the die should be fair (random). 

Before we make our experimentation, we have to establish a protocol that will be the standard used for rejecting or failing to reject the null hypothesis, which is that the die is random. This will be the level of certainty we require for rejecting the null hypothesis (Significance Level).


##Experiment 1: Roll the die 30 times. Observe the quantity of 6's in the sample, is it more than the expected value of 5 occurrences of the number 6? If yes, how many occurrences we observed? 

The probability of getting a 6 when rolling a fair die is 1/6 or approximately 16.7%. For a small size of 30 rolls, we are willing to accept a greater difference from the theoretical probability to keep the null hypothesis that the die is fair. I chose arbitrarily to allow 0.3 difference from the 0.167 theoretical probability, meaning that any outcome between 0 and 14 appearances of the number 6 will be considered as "normal". 

```{r}
dice.roll.30 <- sample(x = 1:6, size = 30, replace = TRUE)
sample(x = 1:6, size = 30, replace = TRUE)
```
```{r}
a <- table(dice.roll.30)
as.data.frame(table(dice.roll.30))
hist(dice.roll.30)   
```
For this experiment, we got 7 occurences of the number 6, which is more than the expected value of 5 occurences but still bellow the previously set standard of x<14 (x= n times 6 occurs). So far, we must keep out null hypothesis. 

Since the experiment has a very limited sample, we are not entirely confident that this die is fair, however, we have little reason to believe that the die has been fixed to have more occurences of the number 6. We do note that the opposite face of 6 is 1 and in this case the combined output is 12, which is still within our initial parameter. 

##EP2: Roll the die 100 times. 

Since our sample size is now greater, we can expect the number of 6's in this sample to be closer to the theoretical probability of 16.7%, which would give us 16-17 number 6's in the sample. Based on that, we are willing to accept a 0.2 difference from the theoretical probability to keep the null hypothesis that the die is fair. Meaning that, any outcome between 0 and 37 appearances of the number 6 will be considered as "normal" and we will fail to reject the null hypothesis. 

```{r}
dice.roll.100 <- sample(x = 1:6, size = 100, replace = TRUE)
sample(x = 1:6, size = 100, replace = TRUE)
```
```{r}
a <- table(dice.roll.100)
as.data.frame(table(dice.roll.100))
hist(dice.roll.100)
```
In this experiment, the number 6 appears more than the expected value of 16 occurrences. 

It is a little weird that in both experiments we got more than the expected number of 6s. Nevertheless, this outcome is still within our parameters for keeping our null hypothesis, and despite our observation of a "pattern" in this "random" sample we must keep it. One observation is that as the number of rolls increased from 30 to 100, the frequency among the numbers was normalized as we can see in the histogram. 


##Experiment 3: Roll the die 100 times at least 10 times. 

Now we have 10 samples of 100 rolls, which gives us a spectrum of 1,000 outcomes. We will trace the appearance of number 6's in each sample, recording the experimental probability of rolling a 6 for each sample. Next step is to take the average of the experimental probability of the 10 samples. For the average of the population, we are willing to accept no more than a 10% difference. Meaning that, the average of the experimental probability of all 10 samples must be between 15.03% and 18.37%, or anything between 150 and 184 number 6 appearances in a total of 10000 rolls. 


###Sample 1:
```{r}

#Sample 1 

dice.roll.sample <- sample(x = 1:6, size = 100, replace = TRUE)
as.data.frame(table(dice.roll.sample))
v11 <- as.data.frame(table(dice.roll.sample))

sample.frequency1 <- v11 [ ,"Freq"]
Ttest1 <- t.test(sample.frequency1, mu= 0 )
print(Ttest1)
```
###Sample 2 

```{r}

#Sample 2 

dice.roll.sample <- sample(x = 1:6, size = 100, replace = TRUE)
as.data.frame(table(dice.roll.sample))
v2 <-as.data.frame(table(dice.roll.sample))
sample.frequency2 <- v2 [ ,"Freq"]
Ttest2 <- t.test(sample.frequency2, mu= 0 )
print(Ttest2)
```

###Sample 3

```{r}

#Sample 3 

dice.roll.sample <- sample(x = 1:6, size = 100, replace = TRUE)
as.data.frame(table(dice.roll.sample))
v3<- as.data.frame(table(dice.roll.sample))
sample.frequency3 <- v3 [ ,"Freq"]
Ttest3 <- t.test(sample.frequency3, mu= 0 )
print(Ttest3)
```

###Sample 4

```{r}

#Sample 4 

dice.roll.sample <- sample(x = 1:6, size = 100, replace = TRUE)
as.data.frame(table(dice.roll.sample))
v4<- as.data.frame(table(dice.roll.sample))
sample.frequency4 <- v4 [ ,"Freq"]
Ttest4 <- t.test(sample.frequency4, mu= 0 )
print(Ttest4)
```

###Sample 5 

```{r}

#Sample 5 

dice.roll.sample <- sample(x = 1:6, size = 100, replace = TRUE)
as.data.frame(table(dice.roll.sample))
v5 <- as.data.frame(table(dice.roll.sample))
sample.frequency5 <- v5 [ ,"Freq"]
Ttest5 <- t.test(sample.frequency5, mu= 0 )
print(Ttest5)
```

###Sample 6 
```{r}

#Sample 6 

dice.roll.sample <- sample(x = 1:6, size = 100, replace = TRUE)
as.data.frame(table(dice.roll.sample))
v6 <- as.data.frame(table(dice.roll.sample))
sample.frequency6<- v6 [ ,"Freq"]
Ttest6 <- t.test(sample.frequency6, mu= 0 )
print(Ttest6)
```

###Sample 7
```{r}

#Sample 7 

dice.roll.sample <- sample(x = 1:6, size = 100, replace = TRUE)
as.data.frame(table(dice.roll.sample))
v7<- as.data.frame(table(dice.roll.sample))
sample.frequency7 <- v7 [ ,"Freq"]
Ttest7 <- t.test(sample.frequency7, mu= 0 )
print(Ttest7)
```
###Sample 8
```{r}

#Sample 8 

dice.roll.sample <- sample(x = 1:6, size = 100, replace = TRUE)
as.data.frame(table(dice.roll.sample))
v8 <- as.data.frame(table(dice.roll.sample))
sample.frequency8 <- v8 [ ,"Freq"]
Ttest8 <- t.test(sample.frequency8, mu= 0 )
print(Ttest8)
```
###Sample 9
```{r}

#Sample 9 

dice.roll.sample <- sample(x = 1:6, size = 100, replace = TRUE)
as.data.frame(table(dice.roll.sample))
v9 <- as.data.frame(table(dice.roll.sample))

sample.frequency9 <- v9 [ ,"Freq"]
Ttest9 <- t.test(sample.frequency9, mu= 0 )
print(Ttest9)
```
###Sample 10
```{r}

#Sample 10 

dice.roll.sample <- sample(x = 1:6, size = 100, replace = TRUE)
as.data.frame(table(dice.roll.sample))
v10 <- as.data.frame(table(dice.roll.sample))
sample.frequency10 <- v10 [ ,"Freq"]
Ttest10 <- t.test(sample.frequency10, mu= 0 )
print(Ttest10)
```

So, a summary of our output for each sample of 100 rolls is below:

```{r}
PopulationData <- cbind(v11, v2, v3, v4, v5, v6, v7, v8, v9, v10)
colnames(PopulationData )[2]<-"Sample1_Freq"
colnames(PopulationData )[4]<-"Sample2_Freq"
colnames(PopulationData )[6]<-"Sample3_Freq"
colnames(PopulationData )[8]<-"Sample4_Freq"
colnames(PopulationData )[10]<-"Sample5_Freq"
colnames(PopulationData )[12]<-"Sample6_Freq"
colnames(PopulationData )[14]<-"Sample7_Freq"
colnames(PopulationData )[16]<-"Sample8_Freq"
colnames(PopulationData )[18]<-"Sample9_Freq"
colnames(PopulationData )[20]<-"Sample10_Freq"
PopulationData <- PopulationData[, !duplicated(colnames(PopulationData), fromFirst = TRUE)]
print(PopulationData)
summary(PopulationData)
```
```{r}
PopulationData$mean <- rowMeans(subset(PopulationData, select = c(Sample1_Freq, Sample2_Freq, Sample3_Freq, Sample4_Freq, Sample5_Freq, Sample6_Freq, Sample7_Freq ,Sample8_Freq, Sample9_Freq, Sample10_Freq )), na.rm = TRUE)

#Average of the 10 samples of 100 rolls by number of occurrences of each number: 
print.listof(PopulationData$mean)


```


The theoretical probability tells us that within a sample of 100 rolls, we should expect 16 to 17 appearances of the number 6. The average number of appearances of the number 6 in our population of 1000 rolls (10 samples of 100 rolls) is 17, which gives us a probability of 17% which falls within our previously set parameter of a probability of 6 occurring between 15.03% and 18.37%. In this case, the number 6 appears exactly the number of times predicted by the theoretical probability, meaning that we fail to reject the null hypothesis.

