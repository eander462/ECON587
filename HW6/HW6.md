---
title: "HW6"
author: "Erik Andersen"
date: "2023-12-01"
output: html_document
---




```r
# Load packages
pacman::p_load(tidyverse, magrittr, estimatr, broom)
```


```r
# Load data
vote_df = haven::read_dta(here::here("HW6", "data", "GriffithNoonen2022_Econ587.dta"))

# Clean names
vote_df = janitor::clean_names(vote_df)

# Add variables of interest for Did. 
# Post = after 2017 which is the year of interest
# Seattle = in seattle
# Treat = interaction of post and seattle
# city_cycle = interaction of city and cycle
vote_df = vote_df |> 
  mutate(post = if_else(election_year>= 2017, 1, 0),
                    seattle = if_else(city == 'Seattle', 1, 0),
                    treatment = post * seattle,
                    city = as.factor(city),
                    cycle = as.factor(cycle),
                    city_cycle = city:cycle) |> 
  select(post, seattle, treatment, everything())
```

### Question 1

#### a)


```r
# did with regular SE's 
did_reg_classical = lm_robust(candidates_ballot ~ post + seattle + treatment + at_large * special,
                              vote_df,
                              se_type = 'classical')
# Extract standard error of coefficient of interest
tidy(did_reg_classical) |> select(term, std.error, p.value) |> filter(term == 'treatment') # 0.958 
```

```
##        term std.error  p.value
## 1 treatment    0.9546 0.002916
```

```r
# Rerun with HC robust SEs
did_reg_hc = lm_robust(candidates_ballot ~ post + seattle + treatment + at_large * special,
                              vote_df,
                              se_type = 'stata')
# Extract standard error again
tidy(did_reg_hc) |> select(term, std.error, p.value) |> filter(term == 'treatment') # 0.987
```

```
##        term std.error  p.value
## 1 treatment     0.985 0.003913
```

```r
rm(did_reg_hc)
```

#### b)


```r
# Construct residuals for both regressions. The lm_robust function doesn't calculat these automatically so we have to do it manually
vote_df = vote_df |> mutate(resids = candidates_ballot - did_reg_classical$fitted.values)

# Naive test for heteroskedasticity
# resids^2 ~ treatment
reg_hetero = lm_robust(I(resids^2) ~ treatment, 
                       vote_df,
                       se_type = 'classical')

tidy(reg_hetero)
```

```
##          term estimate std.error statistic   p.value conf.low conf.high  df
## 1 (Intercept)    5.991    0.8667    6.9117 1.096e-11    4.289     7.692 686
## 2   treatment    1.627    7.5780    0.2147 8.300e-01  -13.252    16.506 686
##       outcome
## 1 I(resids^2)
## 2 I(resids^2)
```

```r
rm(reg_hetero)
rm(did_reg_classical)
```

#### c)


```r
# Cluster by city_cycle
reg_city_cycle = lm_robust(candidates_ballot ~ post + seattle + treatment + at_large * special,
                    vote_df,
                    clusters = city_cycle,
                    se_type = 'CR2') # CR2 is stata standard errors

tidy(reg_city_cycle)
```

```
##               term estimate std.error statistic   p.value conf.low conf.high
## 1      (Intercept)   3.4816    0.1671    20.839 5.447e-37  3.14991    3.8134
## 2             post   0.4161    0.2447     1.701 9.717e-02 -0.07919    0.9113
## 3          seattle   1.3031    0.3317     3.928 3.706e-03  0.54871    2.0576
## 4        treatment   2.8517    0.5370     5.311 4.185e-02  0.28117    5.4222
## 5         at_large  -1.2364    0.1856    -6.663 1.354e-07 -1.61390   -0.8590
## 6          special   2.3456    0.6512     3.602 1.024e-03  1.02072    3.6704
## 7 at_large:special  -2.4827    0.8282    -2.998 1.198e-02 -4.30288   -0.6624
##       df           outcome
## 1 93.911 candidates_ballot
## 2 38.066 candidates_ballot
## 3  8.698 candidates_ballot
## 4  1.803 candidates_ballot
## 5 33.173 candidates_ballot
## 6 33.041 candidates_ballot
## 7 11.135 candidates_ballot
```

```r
rm(reg_city_cycle)
```

#### d)


```r
# Cluster by only city
reg_city = lm_robust(candidates_ballot ~ post + seattle + treatment + at_large * special,
                    vote_df,
                    clusters = city, 
                    se_type = 'CR2') # CR2 is stata standard errors
tidy(reg_city)
```

```
##               term estimate std.error statistic   p.value conf.low conf.high
## 1      (Intercept)   3.4816    0.3273    10.636 4.705e-07   2.7595    4.2038
## 2             post   0.4161    0.2914     1.428 1.787e-01  -0.2185    1.0507
## 3          seattle   1.3031    0.1786     7.296 1.341e-02   0.6065    1.9998
## 4        treatment   2.8517    0.2368    12.043 3.353e-07   2.3228    3.3806
## 5         at_large  -1.2364    0.3183    -3.885 3.551e-02  -2.3060   -0.1669
## 6          special   2.3456    1.0675     2.197 7.167e-02  -0.2850    4.9761
## 7 at_large:special  -2.4827    1.1434    -2.171 1.504e-01  -6.9967    2.0314
##       df           outcome
## 1 10.790 candidates_ballot
## 2 12.046 candidates_ballot
## 3  2.233 candidates_ballot
## 4  9.820 candidates_ballot
## 5  2.740 candidates_ballot
## 6  5.830 candidates_ballot
## 7  2.201 candidates_ballot
```

```r
rm(reg_city)

# Cluster by only cycle
reg_cycle = lm_robust(candidates_ballot ~ post + seattle + treatment + at_large * special,
                    vote_df,
                    clusters = cycle,
                    se_type = 'CR2') # CR2 is stata standard errors
tidy(reg_cycle)
```

```
##               term estimate std.error statistic   p.value conf.low conf.high
## 1      (Intercept)   3.4816   0.09349    37.240 9.346e-10   3.2633    3.6999
## 2             post   0.4161   0.09950     4.182 8.091e-02  -0.1607    0.9929
## 3          seattle   1.3031   0.28976     4.497 4.121e-03   0.5940    2.0123
## 4        treatment   2.8517   0.42981     6.635 3.931e-02   0.4170    5.2864
## 5         at_large  -1.2364   0.09288   -13.313 7.435e-07  -1.4496   -1.0233
## 6          special   2.3456   0.91581     2.561 3.435e-02   0.2233    4.4678
## 7 at_large:special  -2.4827   1.24859    -1.988 9.765e-02  -5.5965    0.6312
##      df           outcome
## 1 7.469 candidates_ballot
## 2 1.541 candidates_ballot
## 3 5.997 candidates_ballot
## 4 1.568 candidates_ballot
## 5 8.231 candidates_ballot
## 6 7.780 candidates_ballot
## 7 5.566 candidates_ballot
```

```r
rm(reg_cycle)
```

#### e)


```r
# Redo the earlier parts with the following new specifcation
# candidates_ballot = cycle_fixed_effct + city_fixed_effect + treatment 

# First redo part a 
# Classical errors
reg_two_way_classic = lm_robust(candidates_ballot ~ cycle + city + treatment, 
                                vote_df,
                                se_type = "classical")
tidy(reg_two_way_classic)
```

```
##                 term  estimate std.error statistic   p.value conf.low conf.high
## 1        (Intercept)  2.383268    0.4893   4.87097 1.390e-06   1.4225    3.3440
## 2          cycle2003 -0.201875    0.4128  -0.48905 6.250e-01  -1.0124    0.6087
## 3          cycle2005 -0.355016    0.4106  -0.86456 3.876e-01  -1.1613    0.4513
## 4          cycle2007 -0.634615    0.4162  -1.52481 1.278e-01  -1.4518    0.1826
## 5          cycle2009  0.006599    0.4199   0.01572 9.875e-01  -0.8179    0.8311
## 6          cycle2011 -0.419161    0.4143  -1.01167 3.121e-01  -1.2327    0.3944
## 7          cycle2013 -0.320691    0.4182  -0.76687 4.434e-01  -1.1418    0.5004
## 8          cycle2015 -0.286364    0.4053  -0.70653 4.801e-01  -1.0822    0.5095
## 9          cycle2017  0.164047    0.4268   0.38433 7.009e-01  -0.6741    1.0022
## 10         cycle2019  0.082922    0.4170   0.19884 8.425e-01  -0.7359    0.9018
## 11       cityEverett -0.021162    0.5520  -0.03834 9.694e-01  -1.1051    1.0627
## 12        cityFresno  0.930272    0.5628   1.65299 9.881e-02  -0.1748    2.0353
## 13          cityKent  0.162230    0.5665   0.28637 7.747e-01  -0.9501    1.2746
## 14    cityLong Beach  1.536457    0.5380   2.85594 4.425e-03   0.4801    2.5928
## 15   cityLos Angeles  1.812830    0.4769   3.80098 1.574e-04   0.8763    2.7493
## 16       cityOakland  1.425651    0.5489   2.59744 9.601e-03   0.3479    2.5034
## 17    citySacramento  0.557748    0.5428   1.02754 3.045e-01  -0.5081    1.6236
## 18     citySan Diego  2.529218    0.5274   4.79569 2.003e-06   1.4937    3.5648
## 19 citySan Francisco  3.378641    0.5042   6.70077 4.433e-11   2.3886    4.3687
## 20      citySan Jose  1.546846    0.5129   3.01576 2.661e-03   0.5397    2.5540
## 21       citySeattle  1.662756    0.5517   3.01393 2.677e-03   0.5795    2.7460
## 22       citySpokane  1.512491    0.5705   2.65105 8.216e-03   0.3922    2.6327
## 23        cityTacoma  0.688148    0.5519   1.24682 2.129e-01  -0.3956    1.7719
## 24     cityVancouver  0.696699    0.5794   1.20247 2.296e-01  -0.4410    1.8344
## 25         treatment  3.630804    0.9291   3.90795 1.026e-04   1.8065    5.4551
##     df           outcome
## 1  663 candidates_ballot
## 2  663 candidates_ballot
## 3  663 candidates_ballot
## 4  663 candidates_ballot
## 5  663 candidates_ballot
## 6  663 candidates_ballot
## 7  663 candidates_ballot
## 8  663 candidates_ballot
## 9  663 candidates_ballot
## 10 663 candidates_ballot
## 11 663 candidates_ballot
## 12 663 candidates_ballot
## 13 663 candidates_ballot
## 14 663 candidates_ballot
## 15 663 candidates_ballot
## 16 663 candidates_ballot
## 17 663 candidates_ballot
## 18 663 candidates_ballot
## 19 663 candidates_ballot
## 20 663 candidates_ballot
## 21 663 candidates_ballot
## 22 663 candidates_ballot
## 23 663 candidates_ballot
## 24 663 candidates_ballot
## 25 663 candidates_ballot
```

```r
rm(reg_two_way_classic)
# Het robust errors
reg_two_way_hc = lm_robust(candidates_ballot ~ cycle + city + treatment, 
                                vote_df,
                                se_type = "stata")
tidy(reg_two_way_hc)
```

```
##                 term  estimate std.error statistic   p.value conf.low conf.high
## 1        (Intercept)  2.383268    0.3176   7.50425 1.996e-13  1.75967   3.00687
## 2          cycle2003 -0.201875    0.4333  -0.46594 6.414e-01 -1.05260   0.64885
## 3          cycle2005 -0.355016    0.4376  -0.81129 4.175e-01 -1.21425   0.50422
## 4          cycle2007 -0.634615    0.3679  -1.72499 8.500e-02 -1.35700   0.08777
## 5          cycle2009  0.006599    0.4606   0.01433 9.886e-01 -0.89778   0.91098
## 6          cycle2011 -0.419161    0.3819  -1.09746 2.728e-01 -1.16911   0.33079
## 7          cycle2013 -0.320691    0.3860  -0.83076 4.064e-01 -1.07866   0.43728
## 8          cycle2015 -0.286364    0.3870  -0.74002 4.596e-01 -1.04620   0.47347
## 9          cycle2017  0.164047    0.4470   0.36697 7.138e-01 -0.71372   1.04182
## 10         cycle2019  0.082922    0.4002   0.20720 8.359e-01 -0.70288   0.86872
## 11       cityEverett -0.021162    0.2028  -0.10433 9.169e-01 -0.41943   0.37710
## 12        cityFresno  0.930272    0.3747   2.48238 1.330e-02  0.19443   1.66611
## 13          cityKent  0.162230    0.2081   0.77951 4.360e-01 -0.24642   0.57088
## 14    cityLong Beach  1.536457    0.3415   4.49970 8.036e-06  0.86599   2.20693
## 15   cityLos Angeles  1.812830    0.4045   4.48168 8.724e-06  1.01858   2.60708
## 16       cityOakland  1.425651    0.3267   4.36345 1.485e-05  0.78411   2.06719
## 17    citySacramento  0.557748    0.2781   2.00570 4.529e-02  0.01172   1.10378
## 18     citySan Diego  2.529218    0.4640   5.45098 7.073e-08  1.61814   3.44029
## 19 citySan Francisco  3.378641    0.5576   6.05915 2.295e-09  2.28375   4.47354
## 20      citySan Jose  1.546846    0.3471   4.45610 9.797e-06  0.86524   2.22845
## 21       citySeattle  1.662756    0.3107   5.35171 1.202e-07  1.05269   2.27282
## 22       citySpokane  1.512491    0.2904   5.20859 2.543e-07  0.94231   2.08267
## 23        cityTacoma  0.688148    0.2526   2.72433 6.613e-03  0.19217   1.18413
## 24     cityVancouver  0.696699    0.2490   2.79792 5.293e-03  0.20776   1.18563
## 25         treatment  3.630804    0.9895   3.66919 2.629e-04  1.68780   5.57381
##     df           outcome
## 1  663 candidates_ballot
## 2  663 candidates_ballot
## 3  663 candidates_ballot
## 4  663 candidates_ballot
## 5  663 candidates_ballot
## 6  663 candidates_ballot
## 7  663 candidates_ballot
## 8  663 candidates_ballot
## 9  663 candidates_ballot
## 10 663 candidates_ballot
## 11 663 candidates_ballot
## 12 663 candidates_ballot
## 13 663 candidates_ballot
## 14 663 candidates_ballot
## 15 663 candidates_ballot
## 16 663 candidates_ballot
## 17 663 candidates_ballot
## 18 663 candidates_ballot
## 19 663 candidates_ballot
## 20 663 candidates_ballot
## 21 663 candidates_ballot
## 22 663 candidates_ballot
## 23 663 candidates_ballot
## 24 663 candidates_ballot
## 25 663 candidates_ballot
```

```r
rm(reg_two_way_hc)

# Redo part c
# Clustered errors at city and cycle level
reg_two_way_city_cycle = lm_robust(candidates_ballot ~ cycle + city + treatment + at_large * special,
                    vote_df,
                    clusters = city_cycle,
                    se_type = 'CR2') # CR2 is stata standard errors

tidy(reg_two_way_city_cycle)
```

```
##                 term estimate std.error statistic   p.value conf.low conf.high
## 1        (Intercept)  2.99313    0.6747   4.43651 0.0003004   1.5784    4.4078
## 2          cycle2003 -0.10968    0.5780  -0.18977 0.8510046  -1.2993    1.0800
## 3          cycle2005 -0.38077    0.4228  -0.90048 0.3769442  -1.2542    0.4926
## 4          cycle2007 -0.55282    0.4279  -1.29192 0.2083597  -1.4347    0.3291
## 5          cycle2009  0.09001    0.5767   0.15609 0.8772564  -1.0996    1.2796
## 6          cycle2011 -0.30115    0.4249  -0.70870 0.4851368  -1.1768    0.5745
## 7          cycle2013 -0.26897    0.4652  -0.57820 0.5685008  -1.2289    0.6910
## 8          cycle2015 -0.30810    0.4356  -0.70725 0.4858319  -1.2044    0.5882
## 9          cycle2017  0.28212    0.4389   0.64281 0.5263061  -0.6226    1.1869
## 10         cycle2019  0.09605    0.4469   0.21492 0.8315953  -0.8250    1.0171
## 11       cityEverett -0.01794    0.2653  -0.06761 0.9468600  -0.5761    0.5403
## 12        cityFresno  0.15972    0.6828   0.23392 0.8173684  -1.2624    1.5819
## 13          cityKent -0.55572    0.6408  -0.86726 0.3959808  -1.8915    0.7800
## 14    cityLong Beach  0.55547    0.7010   0.79236 0.4374621  -0.9070    2.0179
## 15   cityLos Angeles  0.90663    0.7201   1.25896 0.2245400  -0.6092    2.4224
## 16       cityOakland  0.84596    0.6162   1.37295 0.1878130  -0.4552    2.1471
## 17    citySacramento -0.19787    0.6729  -0.29407 0.7717735  -1.6026    1.2068
## 18     citySan Diego  1.61675    0.7166   2.25609 0.0357711   0.1191    3.1144
## 19 citySan Francisco  2.51049    0.9169   2.73804 0.0133611   0.5866    4.4343
## 20      citySan Jose  0.77741    0.7045   1.10343 0.2838168  -0.6987    2.2535
## 21       citySeattle  1.56212    0.4066   3.84224 0.0013598   0.7026    2.4216
## 22       citySpokane  0.94533    0.5773   1.63744 0.1196765  -0.2715    2.1622
## 23        cityTacoma  0.02720    0.6524   0.04169 0.9671543  -1.3329    1.3873
## 24     cityVancouver -0.15675    0.6241  -0.25116 0.8041179  -1.4542    1.1407
## 25         treatment  3.23227    0.7230   4.47036 0.0419910   0.2772    6.1873
## 26          at_large -0.66214    0.5701  -1.16154 0.2790835  -1.9779    0.6537
## 27           special  2.09710    0.6401   3.27644 0.0024636   0.7953    3.3989
## 28  at_large:special -2.13876    0.8301  -2.57654 0.0253148  -3.9604   -0.3171
##        df           outcome
## 1  18.493 candidates_ballot
## 2  25.307 candidates_ballot
## 3  23.653 candidates_ballot
## 4  24.665 candidates_ballot
## 5  24.231 candidates_ballot
## 6  24.735 candidates_ballot
## 7  24.079 candidates_ballot
## 8  25.483 candidates_ballot
## 9  24.537 candidates_ballot
## 10 24.662 candidates_ballot
## 11 17.619 candidates_ballot
## 12 20.473 candidates_ballot
## 13 20.207 candidates_ballot
## 14 19.969 candidates_ballot
## 15 17.542 candidates_ballot
## 16 16.804 candidates_ballot
## 17 19.757 candidates_ballot
## 18 19.423 candidates_ballot
## 19 18.328 candidates_ballot
## 20 18.729 candidates_ballot
## 21 16.570 candidates_ballot
## 22 17.225 candidates_ballot
## 23 20.186 candidates_ballot
## 24 21.139 candidates_ballot
## 25  2.114 candidates_ballot
## 26  7.956 candidates_ballot
## 27 33.247 candidates_ballot
## 28 11.270 candidates_ballot
```

```r
# Redo part d
# Cluster by city only 
reg_two_way_city = lm_robust(candidates_ballot ~ cycle + city + treatment + at_large * special,
                    vote_df,
                    clusters = city, 
                    se_type = 'CR2') # CR2 is stata standard errors
tidy(reg_two_way_city)
```

```
##                 term estimate std.error statistic  p.value conf.low conf.high
## 1        (Intercept)  2.99313    0.8477   3.53099 0.041997   0.2103    5.7759
## 2          cycle2003 -0.10968    0.5104  -0.21490 0.833232  -1.2142    0.9948
## 3          cycle2005 -0.38077    0.4622  -0.82379 0.426155  -1.3880    0.6265
## 4          cycle2007 -0.55282    0.5030  -1.09905 0.292491  -1.6441    0.5384
## 5          cycle2009  0.09001    0.6183   0.14558 0.886622  -1.2542    1.4343
## 6          cycle2011 -0.30115    0.4421  -0.68120 0.508200  -1.2604    0.6581
## 7          cycle2013 -0.26897    0.4439  -0.60591 0.555699  -1.2346    0.6966
## 8          cycle2015 -0.30810    0.4445  -0.69308 0.500591  -1.2696    0.6534
## 9          cycle2017  0.28212    0.3161   0.89246 0.389235  -0.4046    0.9689
## 10         cycle2019  0.09605    0.4129   0.23264 0.819809  -0.7997    0.9918
## 11       cityEverett -0.01794    0.0304  -0.58993 0.625797  -0.1793    0.1434
## 12        cityFresno  0.15972    0.7601   0.21013 0.852774  -3.0643    3.3837
## 13          cityKent -0.55572    0.7668  -0.72471 0.543381  -3.8280    2.7166
## 14    cityLong Beach  0.55547    0.7743   0.71736 0.544940  -2.6481    3.7590
## 15   cityLos Angeles  0.90663    0.7600   1.19299 0.351544  -2.2557    4.0690
## 16       cityOakland  0.84596    0.6748   1.25366 0.334843  -2.0114    3.7033
## 17    citySacramento -0.19787    0.7620  -0.25969 0.819079  -3.4313    3.0355
## 18     citySan Diego  1.61675    0.7660   2.11068 0.165373  -1.5853    4.8188
## 19 citySan Francisco  2.51049    0.7669   3.27346 0.079399  -0.7150    5.7360
## 20      citySan Jose  0.77741    0.7613   1.02122 0.412731  -2.4361    3.9910
## 21       citySeattle  1.56212    0.1568   9.96305 0.001897   1.0712    2.0530
## 22       citySpokane  0.94533    0.6577   1.43744 0.285098  -1.8371    3.7277
## 23        cityTacoma  0.02720    0.7623   0.03568 0.974731  -3.2003    3.2547
## 24     cityVancouver -0.15675    0.7618  -0.20576 0.855584  -3.3542    3.0407
## 25         treatment  3.23227    0.5093   6.34631 0.003085   1.8220    4.6425
## 26          at_large -0.66214    0.7584  -0.87309 0.474055  -3.9008    2.5765
## 27           special  2.09710    1.1008   1.90512 0.108263  -0.6351    4.8293
## 28  at_large:special -2.13876    1.1735  -1.82262 0.198568  -6.7809    2.5034
##        df           outcome
## 1   2.845 candidates_ballot
## 2  12.790 candidates_ballot
## 3  11.979 candidates_ballot
## 4  12.485 candidates_ballot
## 5  12.239 candidates_ballot
## 6  12.471 candidates_ballot
## 7  12.181 candidates_ballot
## 8  12.854 candidates_ballot
## 9  12.327 candidates_ballot
## 10 12.486 candidates_ballot
## 11  1.650 candidates_ballot
## 12  2.030 candidates_ballot
## 13  2.017 candidates_ballot
## 14  2.086 candidates_ballot
## 15  2.073 candidates_ballot
## 16  2.034 candidates_ballot
## 17  2.029 candidates_ballot
## 18  2.062 candidates_ballot
## 19  2.049 candidates_ballot
## 20  2.041 candidates_ballot
## 21  3.090 candidates_ballot
## 22  2.036 candidates_ballot
## 23  2.034 candidates_ballot
## 24  2.054 candidates_ballot
## 25  4.028 candidates_ballot
## 26  2.016 candidates_ballot
## 27  5.668 candidates_ballot
## 28  2.195 candidates_ballot
```

```r
# Cluster by only cycle
reg_two_way_cycle = lm_robust(candidates_ballot ~ cycle + city + treatment + at_large * special,
                    vote_df,
                    clusters = cycle,
                    se_type = 'CR2') # CR2 is stata standard errors
tidy(reg_two_way_cycle)
```

```
##                 term estimate std.error statistic   p.value conf.low conf.high
## 1        (Intercept)  2.99313   0.53039   5.64331 2.153e-03  1.64434   4.34192
## 2          cycle2003 -0.10968   0.05259  -2.08556 6.846e-02 -0.22973   0.01036
## 3          cycle2005 -0.38077   0.01618 -23.53738 1.657e-06 -0.42181  -0.33972
## 4          cycle2007 -0.55282   0.06098  -9.06617 1.148e-05 -0.69191  -0.41373
## 5          cycle2009  0.09001   0.04652   1.93489 8.967e-02 -0.01759   0.19761
## 6          cycle2011 -0.30115   0.06156  -4.89196 1.020e-03 -0.44173  -0.16057
## 7          cycle2013 -0.26897   0.03646  -7.37807 1.330e-04 -0.35470  -0.18324
## 8          cycle2015 -0.30810   0.05532  -5.56939 4.524e-03 -0.45936  -0.15684
## 9          cycle2017  0.28212   0.06891   4.09404 5.748e-03  0.11550   0.44875
## 10         cycle2019  0.09605   0.03834   2.50530 1.837e-01 -0.16741   0.35952
## 11       cityEverett -0.01794   0.33557  -0.05345 9.586e-01 -0.77872   0.74285
## 12        cityFresno  0.15972   0.71201   0.22432 8.297e-01 -1.56733   1.88676
## 13          cityKent -0.55572   0.65160  -0.85286 4.266e-01 -2.15175   1.04030
## 14    cityLong Beach  0.55547   0.47865   1.16048 2.884e-01 -0.60515   1.71608
## 15   cityLos Angeles  0.90663   0.61352   1.47775 1.923e-01 -0.61282   2.42607
## 16       cityOakland  0.84596   0.63331   1.33577 2.320e-01 -0.71970   2.41162
## 17    citySacramento -0.19787   0.64983  -0.30450 7.710e-01 -1.78508   1.38934
## 18     citySan Diego  1.61675   0.62646   2.58076 4.189e-02  0.08228   3.15121
## 19 citySan Francisco  2.51049   1.00187   2.50582 4.791e-02  0.03233   4.98866
## 20      citySan Jose  0.77741   0.41295   1.88260 1.099e-01 -0.23891   1.79374
## 21       citySeattle  1.56212   0.37210   4.19807 2.716e-03  0.71062   2.41361
## 22       citySpokane  0.94533   0.56199   1.68213 1.443e-01 -0.43506   2.32573
## 23        cityTacoma  0.02720   0.63632   0.04275 9.673e-01 -1.52183   1.57623
## 24     cityVancouver -0.15675   0.53817  -0.29127 7.801e-01 -1.45687   1.14337
## 25         treatment  3.23227   0.65067   4.96764 5.136e-02 -0.05319   6.51773
## 26          at_large -0.66214   0.46316  -1.42962 2.330e-01 -2.00164   0.67737
## 27           special  2.09710   0.91738   2.28596 5.246e-02 -0.02855   4.22275
## 28  at_large:special -2.13876   1.23447  -1.73254 1.378e-01 -5.21770   0.94018
##       df           outcome
## 1  5.186 candidates_ballot
## 2  8.502 candidates_ballot
## 3  5.228 candidates_ballot
## 4  8.536 candidates_ballot
## 5  7.865 candidates_ballot
## 6  8.476 candidates_ballot
## 7  7.194 candidates_ballot
## 8  4.162 candidates_ballot
## 9  6.311 candidates_ballot
## 10 1.372 candidates_ballot
## 11 8.871 candidates_ballot
## 12 6.226 candidates_ballot
## 13 5.975 candidates_ballot
## 14 6.235 candidates_ballot
## 15 5.717 candidates_ballot
## 16 5.757 candidates_ballot
## 17 6.045 candidates_ballot
## 18 5.975 candidates_ballot
## 19 5.744 candidates_ballot
## 20 5.860 candidates_ballot
## 21 8.371 candidates_ballot
## 22 5.907 candidates_ballot
## 23 6.130 candidates_ballot
## 24 6.336 candidates_ballot
## 25 1.720 candidates_ballot
## 26 3.629 candidates_ballot
## 27 7.786 candidates_ballot
## 28 5.564 candidates_ballot
```

#### f)


```r
# Cameron Miller two way clustering decomposition
# Variance for clustering only by city
var_city = reg_two_way_city$std.error["treatment"]^2

# Variance for clustering only by cycle
var_cycle = reg_two_way_cycle$std.error["treatment"]^2

# Variance for clustering at the intersection of city and cycle
var_intersection = reg_two_way_city_cycle$std.error["treatment"]^2

# Calculate two way clustering variance
se_two_way = sqrt(var_city + var_cycle - var_intersection)

rm(reg_two_way_city, reg_two_way_cycle, reg_two_way_city_cycle,
        var_city, var_cycle, var_intersection, se_two_way)
```



