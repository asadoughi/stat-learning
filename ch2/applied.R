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
# acceleration, year, origin
# qualitative: name

# (b)
range(Auto$mpg)
# 9.0 46.6
range(Auto$cylinders)
# 3 8
range(Auto$displacement)
# 68 455
range(Auto$horsepower)
# 46 230
range(Auto$weight)
# 1613 5140
range(Auto$acceleration)
# 8.0 24.8
range(Auto$year)
# 70 82
range(Auto$origin)
# 1 3

# (c)
mean(Auto$mpg); sd(Auto$mpg)
# 23.44592 7.805007
mean(Auto$cylinders); sd(Auto$cylinders)
# 5.471939 1.705783
mean(Auto$displacement); sd(Auto$displacement)
# 194.412 104.644
mean(Auto$horsepower); sd(Auto$horsepower)
# 104.4694 38.49116
mean(Auto$weight); sd(Auto$weight)
# 2977.584 849.4026
mean(Auto$acceleration); sd(Auto$acceleration)
# 15.54133 2.758864
mean(Auto$year); sd(Auto$year)
# 75.97959 3.683737
mean(Auto$origin); sd(Auto$origin)
# 1.576531 0.8055182

# (d)
newAuto = Auto[-(10:85),]
dim(newAuto) == dim(Auto) - c(76,0)
newAuto[9,] == Auto[9,]
newAuto[10,] == Auto[86,]

mean(newAuto$mpg); sd(newAuto$mpg); range(newAuto$mpg)
# 24.40443 7.867283 (11.0 46.6)

mean(newAuto$cylinders); sd(newAuto$cylinders);
# 5.373418 1.654179
range(newAuto$cylinders)
# 3 8

mean(newAuto$displacement); sd(newAuto$displacement);
# 187.2405 99.67837
range(newAuto$displacement)
# 68 455

mean(newAuto$horsepower); sd(newAuto$horsepower); range(newAuto$horsepower)
# 100.7215
# 35.70885
# 46 230

mean(newAuto$weight); sd(newAuto$weight); range(newAuto$weight)
# 2935.972
# 811.3002
# 1649 4997

mean(newAuto$acceleration); sd(newAuto$acceleration);
# 15.7269
# 2.693721
range(newAuto$acceleration)
# 8.5 24.8

mean(newAuto$year); sd(newAuto$year); range(newAuto$year)
# 77.14557
# 3.106217
# 70 82

mean(newAuto$origin); sd(newAuto$origin); range(newAuto$origin)
# 1.601266
# 0.81991
# 1 3

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
# Virtually all of the predictors minus origin, name.
# See descriptions of plots in (e).

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
range(Boston$crim)
# Virtually 0 to 88.98; very wide range: two orders of magnitude
range(Boston$tax)
# 187 to 711; not as wide of a range as crime rate
range(Boston$ptratio)
# 12.6 to 22.0; not as wide of a range as crime rate

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
