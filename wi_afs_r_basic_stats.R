

### WI AFS WORKSHOP - BASIC STATS ###
### INSTALL PACKAGES ####
### ONLY NEED TO RUN ONCE ###
install.packages(c("psych","car","agricolae","broom"))


### LOAD PACKAGES ####
library(psych)
library(car)
library(agricolae)
library(broom)

### LOAD DATA ####
data("iris")
data("Loblolly")
data("ToothGrowth")

### BASIC STATS ####
summary.stats<-describeBy(iris$Petal.Length, iris$Species, mat = TRUE) 


### T - TEST ####

### SUBSET IRIS DATA TO TWO GROUPS - SETOSA AND VERSICOLOR ###

iris.dat<-subset(iris, Species == c("setosa", "versicolor"))

### HERE WE WILL DETERMINE IF TWO SPECIES OF IRIS HAVE DIFFERENT PETAL LENGTHS ###


### FIRST PLOT THE DATA WITH CARS BOXPLOT ###
### THIS FUNCITON HIGHLIGHTS OUTLIERS IF ANY ARE PRESENT WITH THE CORRESPONDING ROW NUMBER IN THE DATA ###

Boxplot(Petal.Length ~ Species, data = iris.dat)


### RUN THE T - TEST --> THIS FUNCTION ASSUMES UNEQUAL VARIANCES ###

t.test.result<-t.test(Petal.Length ~ Species, data = iris.dat)
t.test.result

### RERUN WITH EQUAL VARIANCES ###

t.test.result.ev<-t.test(Petal.Length ~ Species, data = iris.dat, var.equal = TRUE)
t.test.result.ev


### IN THIS CASE THERE IS NO DIFFERENCE BETWEEN THE TWO TESTS ###



### ANOVA ####

### HERE WE WILL TEST TO SEE IF THE IRIS SPECIES HAVE DIFFERENT SEPAL LENGTHS ###

### SUMMARY STATS ###

summary.stats<-describeBy(iris$Sepal.Length, iris$Species, mat = TRUE) 


### PLOT WITH BOXPLOT AGAIN ###

Boxplot(Sepal.Length ~ Species, data = iris)


### CHECK VARIANCE AND NORMALITY ###

### NORMALITY ### 

setosa<-subset(iris, Species == "setosa")
versicolor<-subset(iris, Species == "versicolor")
virginica<-subset(iris, Species == "virginica")

shapiro.test(setosa$Sepal.Length)
shapiro.test(versicolor$Sepal.Length)
shapiro.test(virginica$Sepal.Length)

## P VALUE < 0.05 SUGGESTS THAT THE DATA IS NORMALLY DISTRIBUTED ###

### VARIANCE ###

leveneTest(Sepal.Length ~ Species, data = iris)

### WHILE THIS IS SIGNIFICANT, WE ARE GOING TO MOVE FORWARD WITH THE PARAMETRIC ANOVA ###

### USE AOV TO CREATE ANOVA TEST ###
### ** NOTE ** THE FUNCTION LM CAN BE USED INSTEAD OF AOV,BUT MANY OF THE POST HOC FUNCTIONS WILL NOT WORK ###

mod<-aov(Sepal.Length ~ Species, data = iris)
summary(mod)

### USE ANOVA TO GET ANOVA TABLE ###

Anova(mod)

### POST HOC TESTS THREE WAYS! ###

post.hoc<-pairwise.t.test(iris$Sepal.Length, iris$Species, p.adj = "bonferroni")
post.hoc

## ADJUSTMENT OPTIONS ###

# c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY",
#   "fdr", "none")

### TUKEYS ###

tukey.sepal<-TukeyHSD(mod, conf.level = 0.95)

### TUKEYS WITH GROUP MEMBERSHIP ###

tukey.iris<-HSD.test(mod,"Species", group = TRUE)
tukey.iris


### TWO WAY ANOVA ####

### SKIPPING SUMMARY AND PLOTTING, MOVING RIGHT TO MODEL ###



### MODEL ###

tooth.mod <- aov(len ~ supp * as.factor(dose), data = ToothGrowth)
summary(tooth.mod)


### POST HOC -  GET GROUPINGS ### 

interaction <- with(ToothGrowth, interaction(supp, as.factor(dose)))
int.mod<-aov(len~interaction,data = ToothGrowth)
tukey.tooth<-HSD.test(int.mod,"interaction",group=TRUE)
tukey.tooth




### LINEAR REGRESSION ####

### HERE WE WILL SEE IF THERE IS A RELATIONSHIP BETWEEN THE HEIGHT AND AGE OF LOBLOLLY PINES ###

### PLOT DATA TO VIEW ###

plot(Loblolly$height, Loblolly$age)

### MODEL - USE LM ###

tree.mod<-lm(height~age, data = Loblolly)
summary(tree.mod)


### CHECK MODEL FIT ###
# THIS WILL GENERATE FOUR PLOTS #
# PLOT ONE - RESIDUALS - RED LINE SHOULD BE FLAT AND DATA SHOULD HAVE NO OBVIOUS PATTERNS #
# PLOT TWO - NORMAL QQ PLOT - SHOULD BE A STRAIGHT LINE AND BE CLOSE TO THE DASHED LINE - OUTLIERS HIGHLIGHTED #
# PLOT THREE - SCALE LOCATION  - SHOULD BE A FLAT LINE #
# PLOT FOUR - LEVERAGE PLOT - OUTLIERS AND POTENTIAL INFLUENCE (HIGH COOKS DISTANCE) ARE HIGHLIGHTED)

## PLOTS 1 AND 2 MOST EASILY INTERPRETATED ##

plot(tree.mod)





### MAKE TIDY OUTPUTS ####
### THE FUNCTION "TIDY" ALLOWS YOU TO TAKE OUTPUT OBJECTS AND MAKE THEM EASILY EXPORTED TABLES ### 
## WE WILL DEMONSTRATE WITH THE ONE WAY ANOVA OUTPUT ###

tidy.sepal<-tidy(tukey.sepal)

### WRITE TO A CSV ###

write.csv(tidy.sepal,"sepal.length.results.csv")



### NON PARAMETRIC EQUIVALENTS ####

## MANN WHITNEY U TEST FOR T-TEST ###

wilcox.test(Petal.Length ~ Species, data=iris.dat) 

## KRUSKAL WALLIS TEST FOR ANOVA WITH PAIRWISE WILCOX TEST FOR POST HOC ###

kruskal.test(Sepal.Length ~ Species, data = iris)

pairwise.wilcox.test(iris$Sepal.Length, iris$Species,
                     p.adjust.method = "bonferroni")


### DONESO! ####
