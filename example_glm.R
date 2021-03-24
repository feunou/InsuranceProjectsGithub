library(readxl)

path_carsinsurance="/home/victor/Desktop/GLM Insurance Data/car.csv"

path_thirdparty_claims="/home/victor/Desktop/GLM Insurance Data/Third_party_claims.xls"


file_claims<-read_excel(path_thirdparty_claims)
head(file_claims)


plot(file_claims$accidents,file_claims$claims)


plot(log(file_claims$claims),log(file_claims$accidents))

hist(file_claims$accidents)
hist(file_claims$claims, density=3)


hist(log(file_claims$accidents))
hist(log(file_claims$claims))

help('hist')


help("glm")

file_data=read.csv2(path_carsinsurance,header=T,sep=",", dec=".")

head(file_data)

file_data_claim<-file_data[file_data$numclaims>0,]
head(file_data_claim)

xtu<-xtabs(clm~agecat+gender, data=file_data_claim)
xtu
ftable(xtu, row.vars = 2,col.vars = 1)

dim(file_data_claim)

?ftable

getwd()


fix(file_data)
help("read_csv", header=T,sep=",", dec=".")

data2<-read.table(path_carsinsurance, sep=",", header=T)
fix(data2)


model1<-glm(clm~veh_value, family=binomial, data=na.omit(data2))
summary(model1)

model2<-glm(clm~veh_value+I(veh_value^2), family=binomial, data=na.omit(data2))

summary(model2)


model3<-glm(clm~veh_value+I(veh_value^2)+I(veh_value^3), family=binomial, data=na.omit(data2))

summary(model3)




#creating banded values

valuecat<-cut(data2$veh_value, c(-1,2.5,5.0,7.5,10,12.5,100))
table(valuecat)
data2<-cbind(data2,valuecat)
head(data2,10)

new_model<-glm(clm~factor(valuecat), family=binomial, data=na.omit(data2))
summary(new_model)
