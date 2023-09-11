Life <- read.csv("2015 Life Expectancy Data.csv", header = TRUE, sep = ",")
attach(Life)
New_Life_Expectancy <- Life_Expectancy[-c(63,119,76,149,84)]
New_Population <- Population[-c(63,119,76,149,84)]

###Individual Plots#
#Status####
lm.Status <- lm(Life_Expectancy ~ as.factor(Status))
par(mfrow=c(1,1))
plot(Life_Expectancy ~ as.factor(Status))
abline(lm.Status)
par(mfrow = c(2,2))
plot(lm.Status)

#Adult.Mortality####
lm.Adult_Mortality <- lm(Life_Expectancy ~ Adult_Mortality)
par(mfrow=c(1,1))
plot(Life_Expectancy ~ Adult_Mortality)
abline(lm.Adult_Mortality)
par(mfrow = c(2,2))
plot(lm.Adult_Mortality)

#infant.deaths########
lm.Infant_Deaths <- lm(Life_Expectancy ~ Infant_Deaths)
par(mfrow=c(1,1))
plot(Life_Expectancy ~ Infant_Deaths)
abline(lm.Infant_Deaths)
par(mfrow = c(2,2))
plot(lm.Infant_Deaths)

#under.five.deaths####
lm.Under_Five_Deaths <- lm(Life_Expectancy ~ Under_Five_Deaths)
par(mfrow=c(1,1))
plot(Life_Expectancy ~ Under_Five_Deaths)
abline(lm.Under_Five_Deaths)
par(mfrow = c(2,2))
plot(lm.Under_Five_Deaths)

#BMI####
lm.BMI <- lm(Life_Expectancy ~ BMI)
par(mfrow=c(1,1))
plot(Life_Expectancy ~ BMI)
abline(lm.BMI)
par(mfrow = c(2,2))
plot(lm.BMI)

#HIV_AIDS####
lm.HIV_AIDS <- lm(Life_Expectancy ~ HIV_AIDS)
par(mfrow = c(1,1))
plot(Life_Expectancy ~ HIV_AIDS)
abline(lm.HIV_AIDS)
par(mfrow = c(2,2))
plot(lm.HIV_AIDS)

lm.log.HIV_AIDS <- lm(Life_Expectancy ~ log(HIV_AIDS))
par(mfrow = c(1,1))
plot(Life_Expectancy ~ log(HIV_AIDS))
abline(lm.log.HIV_AIDS)
par(mfrow = c(2,2))
plot(lm.log.HIV_AIDS)

#Polio####
lm.Polio <- lm(Life_Expectancy ~ Polio)
par(mfrow=c(1,1))
plot(Life_Expectancy ~ Polio)
abline(lm.Polio)
par(mfrow = c(2,2))
plot(lm.Polio)

#Hepatitis.B####
lm.Hepatitis_B <- lm(Life_Expectancy ~ Hepatitis_B)
par(mfrow=c(1,1))
plot(Life_Expectancy ~ Hepatitis_B)
abline(lm.Hepatitis_B)
par(mfrow = c(2,2))
plot(lm.Hepatitis_B)


#Diphtheria####
lm.Diphtheria <- lm(Life_Expectancy ~ Diphtheria)
par(mfrow=c(1,1))
plot(Life_Expectancy ~ Diphtheria)
abline(lm.Diphtheria)
par(mfrow = c(2,2))
plot(lm.Diphtheria)

#GDP####
lm.GDP <- lm(Life_Expectancy ~ GDP)
par(mfrow = c(1,1))
plot(Life_Expectancy ~ GDP)
abline(lm.GDP)
par(mfrow = c(2,2))
plot(lm.GDP)

lm.log.GDP <- lm(Life_Expectancy ~ log(GDP))
par(mfrow = c(1,1))
plot(Life_Expectancy ~ log(GDP))
abline(lm.log.GDP)
par(mfrow = c(2,2))
plot(lm.log.GDP)

#Population#####
lm.Population <- lm(Life_Expectancy ~ Population)
par(mfrow=c(1,1))
plot(Life_Expectancy ~ Population)
abline(lm.Population)
par(mfrow = c(2,2))
plot(lm.Population)

lm.New_Population <- lm(New_Life_Expectancy ~ New_Population)
par(mfrow=c(1,1))
plot(New_Life_Expectancy ~ New_Population)
abline(lm.New_Population)
par(mfrow = c(2,2))
plot(lm.New_Population)


#Income.composition.of.resources####
lm.Income_Composition_of_Resources <- lm(Life_Expectancy ~ Income_Composition_of_Resources)
par(mfrow=c(1,1))
plot(Life_Expectancy ~ Income_Composition_of_Resources)
abline(lm.Income_Composition_of_Resources)
par(mfrow = c(2,2))
plot(lm.Income_Composition_of_Resources)

#Schooling####
lm.Schooling <- lm(Life_Expectancy ~ Schooling)
par(mfrow=c(1,1))
plot(Life_Expectancy ~ Schooling)
abline(lm.Schooling)
par(mfrow = c(2,2))
plot(lm.Schooling)

###Testing Hypotheses####
#What predicting variables are important?
Model.Full <- lm(Life_Expectancy ~ Adult_Mortality + log(HIV_AIDS) + Population + Infant_Deaths + Hepatitis_B + BMI + Under_Five_Deaths + Polio + Diphtheria + log(GDP)+ Schooling + as.factor(Status) + Income_Composition_of_Resources)
summary(Model.Full)
par(mfrow = c(2,2))
plot(Model.Full)

Model.Result <- lm(Life_Expectancy ~ Adult_Mortality + log(HIV_AIDS) + Income_Composition_of_Resources)
summary(Model.Result)
par(mfrow = c(2,2))
plot(Model.Result)
anova(Model.Result)


#Are certain disease immunisations better than others?
Model.Disease <- lm(Life_Expectancy ~ Hepatitis_B + Polio + Diphtheria)
summary(Model.Disease)
par(mfrow=c(2,2))
plot(Model.Disease)

#Is there a difference between infant mortality and 5year deaths?
Model.Child.Deaths <- lm(Life_Expectancy ~ Infant_Deaths + Under_Five_Deaths)
summary(Model.Child.Deaths)
par(mfrow=c(2,2))
plot(Model.Child.Deaths)