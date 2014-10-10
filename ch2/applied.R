# 8. (a)
college = read.csv("../data/College.csv")
# 8. (b)
fix(college)
rownames(college) = college[,1]
college = college[,-1]
fix(college)
# 8. (c)
# i.
summary(college)
# ii.
pairs(college[,1:10])
# iii.
plot(college$Private, college$Outstate)
# iv.
Elite = rep("No", nrow(college))
Elite[college$Top10perc>50] = "Yes"
Elite = as.factor(Elite)
college = data.frame(college, Elite)
summary(college$Elite)
plot(college$Elite, college$Outstate)
# v.
par(mfrow=c(2,2))
hist(college$Apps)
hist(college$perc.alumni, col=2)
hist(college$S.F.Ratio, col=3, breaks=10)
hist(college$Expend, breaks=100)
# vi.
par(mfrow=c(1,1))
plot(college$Outstate, college$Grad.Rate)
# High tuition correlates to high graduation rate.
plot(college$Accept / college$Apps, college$S.F.Ratio)
# Colleges with low acceptance rate tend to have low S:F ratio.
plot(college$Top10perc, college$Grad.Rate)
# Colleges with the most students from top 10% perc don't necessarily have
# the highest graduation rate. Also, rate > 100 is erroneous!


# 9.
Auto = read.csv("../data/Auto.csv", header=T, na.strings="?")
Auto = na.omit(Auto)
dim(Auto)
summary(Auto)

# (a)
# quantitative: mpg, cylinders, displacement, horsepower, weight,
# acceleration, year
# qualitative: name, origin

# (b)
# apply the range function to the first seven columns of Auto
sapply(Auto[, 1:7], range)
#       mpg cylinders displacement horsepower weight acceleration year
# [1,]  9.0         3           68         46   1613          8.0   70
# [2,] 46.6         8          455        230   5140         24.8   82

# (c)
sapply(Auto[, 1:7], mean)
#         mpg    cylinders displacement   horsepower       weight acceleration 
#   23.445918     5.471939   194.411990   104.469388  2977.584184    15.541327 
#        year 
#   75.979592

sapply(Auto[, 1:7], sd)
#         mpg    cylinders displacement   horsepower       weight acceleration 
#    7.805007     1.705783   104.644004    38.491160   849.402560     2.758864 
#        year 
#    3.683737

# (d)
newAuto = Auto[-(10:85),]
dim(newAuto) == dim(Auto) - c(76,0)
newAuto[9,] == Auto[9,]
newAuto[10,] == Auto[86,]

sapply(newAuto[, 1:7], range)
#       mpg cylinders displacement horsepower weight acceleration year
# [1,] 11.0         3           68         46   1649          8.5   70
# [2,] 46.6         8          455        230   4997         24.8   82
sapply(newAuto[, 1:7], mean)
#         mpg    cylinders displacement   horsepower       weight acceleration 
#   24.404430     5.373418   187.240506   100.721519  2935.971519    15.726899 
#        year 
#   77.145570 
sapply(newAuto[, 1:7], sd)
#         mpg    cylinders displacement   horsepower       weight acceleration 
#    7.867283     1.654179    99.678367    35.708853   811.300208     2.693721 
#        year 
#    3.106217 

# (e)
pairs(Auto)
plot(Auto$mpg, Auto$weight)
# Heavier weight correlates with lower mpg.
plot(Auto$mpg, Auto$cylinders)
# More cylinders, less mpg.
plot(Auto$mpg, Auto$year)
# Cars become more efficient over time.

# (f)
pairs(Auto)
# See descriptions of plots in (e).
# All of the predictors show some correlation with mpg. The name predictor has 
# too little observations per name though, so using this as a predictor is 
# likely to result in overfitting the data and will not generalize well.

# 10.
# (a)
library(MASS)
?Boston
dim(Boston)
# 506 rows, 14 columns
# 14 features, 506 housing values in Boston suburbs


# (b)
pairs(Boston)
# X correlates with: a, b, c
# crim: age, dis, rad, tax, ptratio
# zn: indus, nox, age, lstat
# indus: age, dis
# nox: age, dis
# dis: lstat
# lstat: medv

# (c)
plot(Boston$age, Boston$crim)
# Older homes, more crime
plot(Boston$dis, Boston$crim)
# Closer to work-area, more crime
plot(Boston$rad, Boston$crim)
# Higher index of accessibility to radial highways, more crime
plot(Boston$tax, Boston$crim)
# Higher tax rate, more crime
plot(Boston$ptratio, Boston$crim)
# Higher pupil:teacher ratio, more crime

# (d)
par(mfrow=c(1,3))
hist(Boston$crim[Boston$crim>1], breaks=25)
# most cities have low crime rates, but there is a long tail: 18 suburbs appear
# to have a crime rate > 20, reaching to above 80
hist(Boston$tax, breaks=25)
# there is a large divide between suburbs with low tax rates and a peak at 660-680
hist(Boston$ptratio, breaks=25)
# a skew towards high ratios, but no particularly high ratios

# (e)
dim(subset(Boston, chas == 1))
# 35 suburbs

# (f)
median(Boston$ptratio)
# 19.05

# (g)
> t(subset(Boston, medv == min(Boston$medv)))
#              399      406
# crim     38.3518  67.9208 above 3rd quartile
# zn        0.0000   0.0000 at min
# indus    18.1000  18.1000 at 3rd quartile
# chas      0.0000   0.0000 not bounded by river
# nox       0.6930   0.6930 above 3rd quartile
# rm        5.4530   5.6830 below 1st quartile
# age     100.0000 100.0000 at max
# dis       1.4896   1.4254 below 1st quartile
# rad      24.0000  24.0000 at max
# tax     666.0000 666.0000 at 3rd quartile
# ptratio  20.2000  20.2000 at 3rd quartile
# black   396.9000 384.9700 at max; above 1st quartile
# lstat    30.5900  22.9800 above 3rd quartile
# medv      5.0000   5.0000 at min
summary(Boston)
# Not the best place to live, but certainly not the worst.

# (h)
dim(subset(Boston, rm > 7))
# 64
dim(subset(Boston, rm > 8))
# 13
summary(subset(Boston, rm > 8))
summary(Boston)
# relatively lower crime (comparing range), lower lstat (comparing range)
