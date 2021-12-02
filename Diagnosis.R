#Importing data
data=read.csv("C:/Users/VEDANG SAWANT/Desktop/Projects/Diagnosis/fertility_Diagnosis.csv")
View(data)
summary(data)
data$Diagnosis=ifelse(data$Diagnosis=='N',1,0)

#Compare Diagnosis patients
library(ggplot2)
ggplot(data = data,aes(Diagnosis))+geom_bar(fill='blue')

#Correlation plot
library(corrplot)
library(psych)
correlation=cor(data)
corPlot(correlation)

#Train Test Split
testindex=sample.int(nrow(data))[1:floor(0.1*nrow(data))]
testdata=data[testindex,]
traindata=data[-testindex]

#Model fitting logistic
model=glm(formula = Diagnosis~.,data = traindata,family = binomial)
summary(model)


#Prediction
fitted.values=predict(model,newdata = testdata, type='response')
yhat=ifelse(fitted.values>0.5,1,0)
y=testdata[,c('Diagnosis')]
#Accuracy
mean(yhat==y)

#Model evaluation
head(testdata)
p=predict(model,newdata=data[43,],type='response')
yhat1=ifelse(p>0.5,1,0)
yhat1
