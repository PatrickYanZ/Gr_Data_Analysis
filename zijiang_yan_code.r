---
  title: "4939_project"
author: "Zijiang Yan"
date: "4/9/2020"
output: html_document
---
  

#the 8th column V8 is convert logit value success to probability value. because in logit regression, the response variable must be positive.





#We introduced the dummy variable in rater and decision. 



data = Greene


data[,3] <- ifelse(data[,3]=="yes", 1, 0) 
# change rater to dummy variable if it is no convert it to 0, otherwise(yes),convert it to 1
data[,4] <- ifelse(data[,4]=="yes", 1, 0) 
# change decision to dummy variable if it is no convert it to 0, otherwise(yes),convert it to 1

judge = data[,1]
nation = data[,2]
rater = data[,3]
decision =data[,4]
language = data[,5]
location = data[,6]
success =data[,7]
success = exp(success)/(1+exp(success))




#the binomial GLM regression in this dataset

Greene

success2 =exp(success)/(1+exp(success)) #convert logit to success
#model = glm(formula =  success ~ judge+nation+rater+language+decision+location,family = binomial,data=Greene)
model1 = glm(formula =  decision ~ judge+nation+rater+language+location+success2,family = binomial,data=Greene)

#summary(model1)
anova(model1)

# explainnation
cat("Gineralized Linear Model : calculate the p-value for the deviance goodness of fit ")

pchisq(model1$deviance,df=model1$residuals,lower.tail=FALSE)



#In this model ,similar to the multivariate modeling selection, the judge and judge contributes more in this model.

#Due to the all pvalue is small in the pearson chi square test table, we assume these variables are independent except location and language. Location and language are highly correlation.

#For example, we ususally use English in Toronto and use French in Montreal.

#We will continue to discuss  correlation matrix in the facorial design parts.

model2 = glm(formula =  decision ~ judge+nation+rater+location+language+success ,family = binomial,data=Greene)
model3 <- step(model2, direction = "backward", trace = FALSE)
#summary(model3)
anova(model3)

# explaination
cat("Gineralized Linear Model : calculate the p-value for the deviance goodness of fit ")

pchisq(model1$deviance,df=model1$residuals,lower.tail=FALSE)



#In order to simplify this GLM model, we introduced a model selection here.


step(model2,direction="both")


#Based on this model selection.
#the model is selected by deviance test and AIC, that is we would like to find the highest deviance difference and smallest AIC.

#The original 
#glm(formula = decision ~ judge + nation + rater + language + location + success2, family = binomial, data = Greene)

#The reduced model we have

#glm(formula = decision ~ judge + nation + rater + location, family = binomial, 
# data = Greene)

#in this reduced model, we find decision only concerned about judge nation rater and location. we remove the other variables.


model5 = glm(formula = decision ~ judge + nation + location, family = binomial, 
             data = Greene)

summary(model5)


#In this model, we find judge and location contributes more.
#So the appeal decision depends on judge and location.

anova(model5)	
#For likelihood ratio test, judge nation  matters a lot

#In this analysis table 

#Type I SS:   fits the SS sequentially.   order of the variables matters. 
#Type II SS:  hierarchical, or partially sequential.  order still matters a bit.
#Type III SS: Marginal or orthogonal.   Order does not matter at all.

#For likelihood Ratio Test, judge and nation  matters a lot.



anova(model5, test = "Cp")
anova(model5, test = "Chisq")

#From chisquare test and C statistics draws a similar conclusion,
#judge,nation and location  matters a lot.



#anova(model5, test = "Chisq")

# installing these packages .



#Extended Quantile Plots

library(spida2)
xqplot(Greene)
xqplot(Greene, ptype = 'normal') 

#In the Extended Quantile Plots, this is the plot for overall dataset. we can simply conclude 

#the rater is more optimistic than the judge, because the number of rator said yes is greater than thwe final appeal passed.

#the number of cases occured in Toronto and Montreal is higher than in the other place.

# judge Judges hear many cases. The appeals is not equalized distribute to the judges.

#Some nations are eager to appeal.




library(spida2)
levels_nation = levels(Greene$nation)
levels_nation[0] #List nations name

#define a for loop to access every nation xqplot by their names.

for (val in levels_nation) {
  print(val)
  judgeval <- Greene[ which(Greene$nation==val), ]
  xqplot(judgeval)
}



#From the plot above



library(spida2)
levels_judge = levels(Greene$judge)
#levels[0] #List judges name

for (val in levels_judge) {
  print(val)
  judgeval <- Greene[ which(Greene$judge==val), ]
  xqplot(judgeval)
}

#print(count)



#These plot illustrate the following conclusions:
  
#the judge has their own language preference as well as their location preference.
#The judge has their own decision, that is some judges are easy to pass but some judges are not.



#Greene[ which(Greene$nation=="rator")
#Greene$rator/

cat("rator analysis with nation")
x = length(Greene$rater)
x
y = length(which(Greene$rater=="yes"))
y
table <- table(Greene$nation,Greene$rater)
table
margin.table(table,1)# sum the no and yes for specific country
margin.table(table,2)# sum the total number of no and yes
margin.table(table,1)/margin.table(table) # the proportion of failure for whole countries
margin.table(table,2)/margin.table(table)
cat("the proportion")
table[,1]/margin.table(table,1)
resulttable = table[,2]/margin.table(table,1)

barplot(resulttable, main="rator Desision vs nation ", horiz=TRUE,
        names.arg=levels(Greene$nation))

levels(Greene$nation)


#the plot shows the the probability to pass the appeal vs the country

#We assume there are two models
#1. rater decision vs the country
#2. judge decision vs the country

#The judge is more strict than rator.
#The judge has preference or have some prejudice to pass in some specific country,like Czechoslovakia.



#Greene[ which(Greene$nation=="rator")
#Greene$decision/
cat("rator analysis with nation")
x = length(Greene$decision)
#x
y = length(which(Greene$decision=="yes"))
#y
table2 <- table(Greene$nation,Greene$decision)
#table2
margin.table(table2,1)# sum the no and yes for specific country
margin.table(table2,2)# sum the total number of no and yes
margin.table(table2,1)/margin.table(table) # the proportion of failure for whole countries
margin.table(table2,2)/margin.table(table)
cat("the proportion")
table2[,1]/margin.table(table2,1)
resulttable2 = table2[,2]/margin.table(table2,1)

#resulttable2

barplot(resulttable2, main="Judge decision vs nation ", horiz=TRUE,
        names.arg=levels(Greene$nation))

counts <- table(resulttable, resulttable2)
counts



#resulttable2



# we defined countries as different numbers 
#Argentina	1
#Ghana	2
#Nigeria	3
#Bulgaria	4
#Guatemala	5
#Pakistan	6
#China	7
#India	8
#Poland	9
#Czechoslovakia	10
#Iran	11
#Somalia	12
#El.Salvador	13
#Lebanon	14
#Sri.Lanka	15
#Fiji	16
#Nicaragua	17

# we defined judge as different index
#Desjardins	1
#Heald	2
#Hugessen	3
#Iacobucci	4
#MacGuigan	5
#Mahoney	6
#Marceau	7
#Pratte	8
#Stone	9
#Urie	10

# defined the dummy variable as rater and decision
#Yes 1
#No 0

# defined the dummy variable as locations
#Toronto 1
#Montreal 2
#Other 0

#defined the dummy variable as language 
#English 0 
#French 1



#factorial design
library("readxl")
greene_factorial <- read_excel("greene_factorial.xlsx")
greene_factorial




#levels(greene_factorial$nation)

greene_factorial$nation




head(greene_factorial) #list heading 6 rows. 
dim(greene_factorial)



#factorial design
library("Hmisc")
res2 <- rcorr(as.matrix(greene_factorial))
res2

#From the correlation table, we find language and location are highly correlated.we need to drop one of these variables.
#Doing a regression analysis in this factorial design.


model7 = glm(formula =  decision ~ judge+nation+rater+location+language+success ,family = binomial,data=greene_factorial)
#model8 <- step(model2, direction = "backward", trace = FALSE)
summary(model7)
anova(model7)


# the original factorial model 
#Df Deviance Resid. Df Resid. Dev
#NULL                       383     467.09
#judge     1   0.9272       382     466.17
#nation    1   1.7923       381     464.37
#rater     1  25.7350       380     438.64
#location  1   2.7969       379     435.84
#language  1   1.8151       378     434.03
#success   1  29.0315       377     404.99

# the model after backward selection

#In this model we find 
#Df Deviance Resid. Df Resid. Dev
#NULL                       383     467.09
#judge     9   40.403       374     426.69
#nation   16   52.848       358     373.84
#rater     1   24.734       357     349.11
#location  2   12.425       355     336.68

#we find the 4 variables above are significant. Reduced model is better,which has higher deviance.


step(model7,direction="both")#Stepwise selection


#After stepwise selection, we find location,nation ,rater and judge has higher deviance than before. AIC is 394.68,which is lower than previous part.
#We determine to focus these 4 variables.


#load the packages
library(spida2)
library(car)
library(latticeExtra)




xqplot(Greene)
xqplot(Greene, ptype = 'normal') 

# here are the factorial plot

xqplot(greene_factorial)
xqplot(greene_factorial, ptype = 'normal')





Greene_tab <- tab_(Greene)
#Greene_tab

tab(Greene, ~judge)
tab(Greene, ~nation)
tab(Greene, ~rater)
tab(Greene, ~decision)
tab(Greene, ~location)
tab(Greene, ~language)

tab(Greene, ~decision, pr =0)
tab(Greene, ~location, pr =0)
tab(Greene, ~language, pr =0)



Greene %>%
  tab(~ judge + nation +rater +location)




Greene %>%
  tab(~ judge + nation +rater +location)%>%
  ftable





Greene %>%
  tab(~ judge + nation +rater +location, pct=c(1,2))
  ftable




Greene %>%
  tab(~ judge + nation +rater +location, pct=c(1,2))%>%
  ftable




Greene %>%
  tab_(~ judge + nation +rater +location, pct=c(1,2))%>% #removing one variable
  ftable




Greene %>%
  tab__(~ judge + nation +rater +location, pct=c(1,2))%>% #removing the other variable
  ftable




tt <- tab(Greene, ~ judge + nation)
tt
tab(tt, ~nation)
tab(tt, ~judge)
tab_(tt) %>%
  tab(~nation)
tab(~judge)



tab(~ judge + nation +rater +location)%>%
  barchart(auto.key=T,stack=F)






#prob = Greene[,8]
#prob
#qqmath(~success,Greene)
#qqmath(~prob,Greene)



quantile(Greene$success,na.rm = TRUE)
#quantile(prob,na.rm = TRUE)


plot(Greene_tab)
# this is a mosaic plot, however it isnot a fair example





  
  