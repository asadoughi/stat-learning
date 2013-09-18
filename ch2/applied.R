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
suburbs = rep("No", nrow(Boston))
suburbs[Boston$zn>50] = "Yes"
suburbs = as.factor(suburbs)
Boston = data.frame(Boston, suburbs)
plot(Boston$suburbs, Boston$crim)
# It doesn't appear that highly residential areas are subject to as much crime
range(Boston$crim)
# Virtually 0 to 88.98, with no values higher than 20 in suburbs.
plot(Boston$suburbs, Boston$tax)
# It doesn't appear that highly residential areas are subject to more tax
range(Boston$tax)
# 187 to 711, 2 urban areas are particularly at the high end, followed by
# 1 suburban town in third
plot(Boston$suburbs, Boston$ptratio)
# Yes, it appears that some highly residential areas have high P:T ratios.
range(Boston$ptratio)
# 12.6 to 22.0; a suburb at 80% residential has highest P:T ratio

# (e)
subset(Boston, suburbs == "Yes" & chas == 1)
# Only 1.

# (f)
median(Boston$ptratio)
# 19.05

# (g)
burbs = subset(Boston, suburbs == "Yes")
t(subset(burbs, medv == min(burbs$medv)))
summary(Boston)
# crim    "0.04301" below 1st quartile
# zn      "80"      above 3rd quartile
# indus   "1.91"    below 1st quartile
# chas    "0"       not near river, like most towns
# nox     "0.413"   below 1st quartile
# rm      "5.663"   below 1st q.
# age     "21.9"    below 1st q.
# dis     "10.5857" above 3rd q.
# rad     "4"       at 1st q.
# tax     "334"     near median
# ptratio "22"      highest pt ratio in Boston
# black   "382.8"   between 1st q. and median
# lstat   "8.05"    between 1st q. and median
# medv    "18.2"    between 1st q. and median
# Not the best place to live, but certainly not the worst.

# (h)
dim(subset(burbs, rm > 7))
# 15
dim(subset(burbs, rm > 8))
# 1
t(subset(burbs, rm > 8))
# crim    "0.02009" low crime
# zn      "95"      highly residential
# indus   "2.68"    low industrial use
# chas    "0"       not near river, like most
# nox     "0.4161"  low NOX
# rm      "8.034"
# age     "31.9"    younger homes
# dis     "5.118"   at 3rd q.; fairly distant
# rad     "4"       relatively accessible by highway
# tax     "224"     low tax rate
# ptratio "14.7"    low (good) p:t ratio
# black   "390.55"  around median proportion of blacks
# lstat   "2.88"    below 1st q.; low lower status of population
# medv    "50"      highest value of all Boston
# I am guessing this is near colleges/universities, such that dorms consist of
# 8+ people per home. Just a guess.
