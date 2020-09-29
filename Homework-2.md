Homework 2, Lab 2, Econ B2000
================
Christopher Tinevra, Group Members: Isabela Vieira, Akimawe Kadiri,
Nicole Kerrison, Mostafa Ragheb
9/29/2020

Question: How likely is that this dice is unfair? We are trying to
determine whether the dice is generating random numbers or if it’s
actually following a certain pattern.

**Null Hypothesis Ho ** - No relationship; the die is generating random
numbers and thus is fair. **Alternative Hypothesis Ha** - The die is
unfair; it has been fixed to generate more 6s on average.

So, we are trying to test whether or not the die is unfair, given that
our naive assessment is that the die should be fair (random).

Before we make our experimentation, we have to establish a protocol that
will be the standard used for rejecting or failing to reject the null
hypothesis, which is that the die is random. This will be the level of
certainty we require for rejecting the null hypothesis (Significance
Level).

\#\#Experiment 1: Roll the die 30 times. Observe the quantity of 6’s in
the sample, is it more than the expected value of 5 occurrences of the
number 6? If yes, how many occurrences we observed?

The probability of getting a 6 when rolling a fair die is 1/6 or
approximately 16.7%. For a small size of 30 rolls, we are willing to
accept a greater difference from the theoretical probability to keep the
null hypothesis that the die is fair. I chose arbitrarily to allow 0.3
difference from the 0.167 theoretical probability, meaning that any
outcome between 0 and 14 appearances of the number 6 will be considered
as “normal”.

``` r
dice.roll.30 <- sample(x = 1:6, size = 30, replace = TRUE)
sample(x = 1:6, size = 30, replace = TRUE)
```

    ##  [1] 3 3 5 6 2 5 4 4 2 3 4 5 4 3 6 6 6 5 2 6 6 6 5 1 2 3 4 2 2 1

``` r
a <- table(dice.roll.30)
as.data.frame(table(dice.roll.30))
```

    ##   dice.roll.30 Freq
    ## 1            1    5
    ## 2            2    3
    ## 3            3    5
    ## 4            4    3
    ## 5            5    5
    ## 6            6    9

``` r
hist(dice.roll.30)   
```

![](Homework-2_files/figure-gfm/unnamed-chunk-2-1.png)<!-- --> For this
experiment, we got 7 occurences of the number 6, which is more than the
expected value of 5 occurences but still bellow the previously set
standard of x\<14 (x= n times 6 occurs). So far, we must keep out null
hypothesis.

Since the experiment has a very limited sample, we are not entirely
confident that this die is fair, however, we have little reason to
believe that the die has been fixed to have more occurences of the
number 6. We do note that the opposite face of 6 is 1 and in this case
the combined output is 12, which is still within our initial parameter.

\#\#EP2: Roll the die 100 times.

Since our sample size is now greater, we can expect the number of 6’s in
this sample to be closer to the theoretical probability of 16.7%, which
would give us 16-17 number 6’s in the sample. Based on that, we are
willing to accept a 0.2 difference from the theoretical probability to
keep the null hypothesis that the die is fair. Meaning that, any outcome
between 0 and 37 appearances of the number 6 will be considered as
“normal” and we will fail to reject the null hypothesis.

``` r
dice.roll.100 <- sample(x = 1:6, size = 100, replace = TRUE)
sample(x = 1:6, size = 100, replace = TRUE)
```

    ##   [1] 2 3 1 5 3 1 5 6 4 4 1 5 6 6 4 2 4 5 6 1 3 6 1 6 1 4 2 4 4 5 1 6 2 1 1 3 4
    ##  [38] 4 6 2 5 4 5 2 4 5 4 5 3 4 5 2 4 1 4 1 6 1 5 5 1 2 3 6 6 2 1 4 4 6 5 6 1 2
    ##  [75] 1 3 6 1 5 1 3 1 6 2 6 6 2 5 1 2 4 6 6 4 2 2 5 3 2 1

``` r
a <- table(dice.roll.100)
as.data.frame(table(dice.roll.100))
```

    ##   dice.roll.100 Freq
    ## 1             1   16
    ## 2             2   17
    ## 3             3   25
    ## 4             4   14
    ## 5             5   13
    ## 6             6   15

``` r
hist(dice.roll.100)
```

![](Homework-2_files/figure-gfm/unnamed-chunk-4-1.png)<!-- --> In this
experiment, the number 6 appears more than the expected value of 16
occurrences.

It is a little weird that in both experiments we got more than the
expected number of 6s. Nevertheless, this outcome is still within our
parameters for keeping our null hypothesis, and despite our observation
of a “pattern” in this “random” sample we must keep it. One observation
is that as the number of rolls increased from 30 to 100, the frequency
among the numbers was normalized as we can see in the histogram.

\#\#Experiment 3: Roll the die 100 times at least 10 times.

Now we have 10 samples of 100 rolls, which gives us a spectrum of 1,000
outcomes. We will trace the appearance of number 6’s in each sample,
recording the experimental probability of rolling a 6 for each sample.
Next step is to take the average of the experimental probability of the
10 samples. For the average of the population, we are willing to accept
no more than a 10% difference. Meaning that, the average of the
experimental probability of all 10 samples must be between 15.03% and
18.37%, or anything between 150 and 184 number 6 appearances in a total
of 10000 rolls.

\#\#\#Sample 1:

``` r
#Sample 1 

dice.roll.sample <- sample(x = 1:6, size = 100, replace = TRUE)
as.data.frame(table(dice.roll.sample))
```

    ##   dice.roll.sample Freq
    ## 1                1   18
    ## 2                2   16
    ## 3                3   20
    ## 4                4   14
    ## 5                5   17
    ## 6                6   15

``` r
v11 <- as.data.frame(table(dice.roll.sample))

sample.frequency1 <- v11 [ ,"Freq"]
Ttest1 <- t.test(sample.frequency1, mu= 0 )
print(Ttest1)
```

    ## 
    ##  One Sample t-test
    ## 
    ## data:  sample.frequency1
    ## t = 18.898, df = 5, p-value = 7.643e-06
    ## alternative hypothesis: true mean is not equal to 0
    ## 95 percent confidence interval:
    ##  14.39963 18.93371
    ## sample estimates:
    ## mean of x 
    ##  16.66667

\#\#\#Sample 2

``` r
#Sample 2 

dice.roll.sample <- sample(x = 1:6, size = 100, replace = TRUE)
as.data.frame(table(dice.roll.sample))
```

    ##   dice.roll.sample Freq
    ## 1                1   22
    ## 2                2   19
    ## 3                3    6
    ## 4                4   13
    ## 5                5   28
    ## 6                6   12

``` r
v2 <-as.data.frame(table(dice.roll.sample))
sample.frequency2 <- v2 [ ,"Freq"]
Ttest2 <- t.test(sample.frequency2, mu= 0 )
print(Ttest2)
```

    ## 
    ##  One Sample t-test
    ## 
    ## data:  sample.frequency2
    ## t = 5.1736, df = 5, p-value = 0.003544
    ## alternative hypothesis: true mean is not equal to 0
    ## 95 percent confidence interval:
    ##   8.385651 24.947683
    ## sample estimates:
    ## mean of x 
    ##  16.66667

\#\#\#Sample 3

``` r
#Sample 3 

dice.roll.sample <- sample(x = 1:6, size = 100, replace = TRUE)
as.data.frame(table(dice.roll.sample))
```

    ##   dice.roll.sample Freq
    ## 1                1   19
    ## 2                2   17
    ## 3                3   15
    ## 4                4   13
    ## 5                5   19
    ## 6                6   17

``` r
v3<- as.data.frame(table(dice.roll.sample))
sample.frequency3 <- v3 [ ,"Freq"]
Ttest3 <- t.test(sample.frequency3, mu= 0 )
print(Ttest3)
```

    ## 
    ##  One Sample t-test
    ## 
    ## data:  sample.frequency3
    ## t = 17.461, df = 5, p-value = 1.129e-05
    ## alternative hypothesis: true mean is not equal to 0
    ## 95 percent confidence interval:
    ##  14.21299 19.12034
    ## sample estimates:
    ## mean of x 
    ##  16.66667

\#\#\#Sample 4

``` r
#Sample 4 

dice.roll.sample <- sample(x = 1:6, size = 100, replace = TRUE)
as.data.frame(table(dice.roll.sample))
```

    ##   dice.roll.sample Freq
    ## 1                1   19
    ## 2                2   17
    ## 3                3   22
    ## 4                4   19
    ## 5                5   10
    ## 6                6   13

``` r
v4<- as.data.frame(table(dice.roll.sample))
sample.frequency4 <- v4 [ ,"Freq"]
Ttest4 <- t.test(sample.frequency4, mu= 0 )
print(Ttest4)
```

    ## 
    ##  One Sample t-test
    ## 
    ## data:  sample.frequency4
    ## t = 9.2529, df = 5, p-value = 0.0002478
    ## alternative hypothesis: true mean is not equal to 0
    ## 95 percent confidence interval:
    ##  12.03645 21.29689
    ## sample estimates:
    ## mean of x 
    ##  16.66667

\#\#\#Sample 5

``` r
#Sample 5 

dice.roll.sample <- sample(x = 1:6, size = 100, replace = TRUE)
as.data.frame(table(dice.roll.sample))
```

    ##   dice.roll.sample Freq
    ## 1                1   22
    ## 2                2   15
    ## 3                3   17
    ## 4                4   20
    ## 5                5   13
    ## 6                6   13

``` r
v5 <- as.data.frame(table(dice.roll.sample))
sample.frequency5 <- v5 [ ,"Freq"]
Ttest5 <- t.test(sample.frequency5, mu= 0 )
print(Ttest5)
```

    ## 
    ##  One Sample t-test
    ## 
    ## data:  sample.frequency5
    ## t = 10.963, df = 5, p-value = 0.0001098
    ## alternative hypothesis: true mean is not equal to 0
    ## 95 percent confidence interval:
    ##  12.75878 20.57455
    ## sample estimates:
    ## mean of x 
    ##  16.66667

\#\#\#Sample 6

``` r
#Sample 6 

dice.roll.sample <- sample(x = 1:6, size = 100, replace = TRUE)
as.data.frame(table(dice.roll.sample))
```

    ##   dice.roll.sample Freq
    ## 1                1   21
    ## 2                2   14
    ## 3                3   22
    ## 4                4   11
    ## 5                5   16
    ## 6                6   16

``` r
v6 <- as.data.frame(table(dice.roll.sample))
sample.frequency6<- v6 [ ,"Freq"]
Ttest6 <- t.test(sample.frequency6, mu= 0 )
print(Ttest6)
```

    ## 
    ##  One Sample t-test
    ## 
    ## data:  sample.frequency6
    ## t = 9.7683, df = 5, p-value = 0.0001913
    ## alternative hypothesis: true mean is not equal to 0
    ## 95 percent confidence interval:
    ##  12.28075 21.05259
    ## sample estimates:
    ## mean of x 
    ##  16.66667

\#\#\#Sample 7

``` r
#Sample 7 

dice.roll.sample <- sample(x = 1:6, size = 100, replace = TRUE)
as.data.frame(table(dice.roll.sample))
```

    ##   dice.roll.sample Freq
    ## 1                1   15
    ## 2                2   14
    ## 3                3   21
    ## 4                4   10
    ## 5                5   20
    ## 6                6   20

``` r
v7<- as.data.frame(table(dice.roll.sample))
sample.frequency7 <- v7 [ ,"Freq"]
Ttest7 <- t.test(sample.frequency7, mu= 0 )
print(Ttest7)
```

    ## 
    ##  One Sample t-test
    ## 
    ## data:  sample.frequency7
    ## t = 9.3495, df = 5, p-value = 0.0002358
    ## alternative hypothesis: true mean is not equal to 0
    ## 95 percent confidence interval:
    ##  12.08426 21.24907
    ## sample estimates:
    ## mean of x 
    ##  16.66667

\#\#\#Sample 8

``` r
#Sample 8 

dice.roll.sample <- sample(x = 1:6, size = 100, replace = TRUE)
as.data.frame(table(dice.roll.sample))
```

    ##   dice.roll.sample Freq
    ## 1                1   17
    ## 2                2   23
    ## 3                3   11
    ## 4                4   13
    ## 5                5   15
    ## 6                6   21

``` r
v8 <- as.data.frame(table(dice.roll.sample))
sample.frequency8 <- v8 [ ,"Freq"]
Ttest8 <- t.test(sample.frequency8, mu= 0 )
print(Ttest8)
```

    ## 
    ##  One Sample t-test
    ## 
    ## data:  sample.frequency8
    ## t = 8.8113, df = 5, p-value = 0.0003126
    ## alternative hypothesis: true mean is not equal to 0
    ## 95 percent confidence interval:
    ##  11.80441 21.52893
    ## sample estimates:
    ## mean of x 
    ##  16.66667

\#\#\#Sample 9

``` r
#Sample 9 

dice.roll.sample <- sample(x = 1:6, size = 100, replace = TRUE)
as.data.frame(table(dice.roll.sample))
```

    ##   dice.roll.sample Freq
    ## 1                1   14
    ## 2                2   19
    ## 3                3   17
    ## 4                4   17
    ## 5                5   14
    ## 6                6   19

``` r
v9 <- as.data.frame(table(dice.roll.sample))

sample.frequency9 <- v9 [ ,"Freq"]
Ttest9 <- t.test(sample.frequency9, mu= 0 )
print(Ttest9)
```

    ## 
    ##  One Sample t-test
    ## 
    ## data:  sample.frequency9
    ## t = 18.137, df = 5, p-value = 9.364e-06
    ## alternative hypothesis: true mean is not equal to 0
    ## 95 percent confidence interval:
    ##  14.30446 19.02887
    ## sample estimates:
    ## mean of x 
    ##  16.66667

\#\#\#Sample 10

``` r
#Sample 10 

dice.roll.sample <- sample(x = 1:6, size = 100, replace = TRUE)
as.data.frame(table(dice.roll.sample))
```

    ##   dice.roll.sample Freq
    ## 1                1   23
    ## 2                2   17
    ## 3                3   18
    ## 4                4   11
    ## 5                5   17
    ## 6                6   14

``` r
v10 <- as.data.frame(table(dice.roll.sample))
sample.frequency10 <- v10 [ ,"Freq"]
Ttest10 <- t.test(sample.frequency10, mu= 0 )
print(Ttest10)
```

    ## 
    ##  One Sample t-test
    ## 
    ## data:  sample.frequency10
    ## t = 10.122, df = 5, p-value = 0.0001613
    ## alternative hypothesis: true mean is not equal to 0
    ## 95 percent confidence interval:
    ##  12.43409 20.89925
    ## sample estimates:
    ## mean of x 
    ##  16.66667

So, a summary of our output for each sample of 100 rolls is below:

``` r
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
```

    ##   dice.roll.sample Sample1_Freq Sample2_Freq Sample3_Freq Sample4_Freq
    ## 1                1           18           22           19           19
    ## 2                2           16           19           17           17
    ## 3                3           20            6           15           22
    ## 4                4           14           13           13           19
    ## 5                5           17           28           19           10
    ## 6                6           15           12           17           13
    ##   Sample5_Freq Sample6_Freq Sample7_Freq Sample8_Freq Sample9_Freq
    ## 1           22           21           15           17           14
    ## 2           15           14           14           23           19
    ## 3           17           22           21           11           17
    ## 4           20           11           10           13           17
    ## 5           13           16           20           15           14
    ## 6           13           16           20           21           19
    ##   Sample10_Freq
    ## 1            23
    ## 2            17
    ## 3            18
    ## 4            11
    ## 5            17
    ## 6            14

``` r
summary(PopulationData)
```

    ##  dice.roll.sample  Sample1_Freq    Sample2_Freq    Sample3_Freq  
    ##  1:1              Min.   :14.00   Min.   : 6.00   Min.   :13.00  
    ##  2:1              1st Qu.:15.25   1st Qu.:12.25   1st Qu.:15.50  
    ##  3:1              Median :16.50   Median :16.00   Median :17.00  
    ##  4:1              Mean   :16.67   Mean   :16.67   Mean   :16.67  
    ##  5:1              3rd Qu.:17.75   3rd Qu.:21.25   3rd Qu.:18.50  
    ##  6:1              Max.   :20.00   Max.   :28.00   Max.   :19.00  
    ##   Sample4_Freq    Sample5_Freq    Sample6_Freq    Sample7_Freq  
    ##  Min.   :10.00   Min.   :13.00   Min.   :11.00   Min.   :10.00  
    ##  1st Qu.:14.00   1st Qu.:13.50   1st Qu.:14.50   1st Qu.:14.25  
    ##  Median :18.00   Median :16.00   Median :16.00   Median :17.50  
    ##  Mean   :16.67   Mean   :16.67   Mean   :16.67   Mean   :16.67  
    ##  3rd Qu.:19.00   3rd Qu.:19.25   3rd Qu.:19.75   3rd Qu.:20.00  
    ##  Max.   :22.00   Max.   :22.00   Max.   :22.00   Max.   :21.00  
    ##   Sample8_Freq    Sample9_Freq   Sample10_Freq  
    ##  Min.   :11.00   Min.   :14.00   Min.   :11.00  
    ##  1st Qu.:13.50   1st Qu.:14.75   1st Qu.:14.75  
    ##  Median :16.00   Median :17.00   Median :17.00  
    ##  Mean   :16.67   Mean   :16.67   Mean   :16.67  
    ##  3rd Qu.:20.00   3rd Qu.:18.50   3rd Qu.:17.75  
    ##  Max.   :23.00   Max.   :19.00   Max.   :23.00

``` r
PopulationData$mean <- rowMeans(subset(PopulationData, select = c(Sample1_Freq, Sample2_Freq, Sample3_Freq, Sample4_Freq, Sample5_Freq, Sample6_Freq, Sample7_Freq ,Sample8_Freq, Sample9_Freq, Sample10_Freq )), na.rm = TRUE)

#Average of the 10 samples of 100 rolls by number of occurrences of each number: 
print.listof(PopulationData$mean)
```

    ## Component 1 :
    ## [1] 19
    ## 
    ## Component 2 :
    ## [1] 17.1
    ## 
    ## Component 3 :
    ## [1] 16.9
    ## 
    ## Component 4 :
    ## [1] 14.1
    ## 
    ## Component 5 :
    ## [1] 16.9
    ## 
    ## Component 6 :
    ## [1] 16

The theoretical probability tells us that within a sample of 100 rolls,
we should expect 16 to 17 appearances of the number 6. The average
number of appearances of the number 6 in our population of 1000 rolls
(10 samples of 100 rolls) is 17, which gives us a probability of 17%
which falls within our previously set parameter of a probability of 6
occurring between 15.03% and 18.37%. In this case, the number 6 appears
exactly the number of times predicted by the theoretical probability,
meaning that we fail to reject the null hypothesis.
