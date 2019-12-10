library(plm)
setwd("C:/Rifai/PROJECT/")
mydata <- read.csv("Data_Projectt.csv")
attach(mydata)

Y = cbind(Productivity)
X = cbind(Rainfall, Temperature)

# set data as panel data
pdata1 = pdata.frame(mydata, index=c("District","Time"))
View(pdata1)

# descriptive statistics
summary(Y)
summary(X)

# Pooled OLS estimator 
pooling = plm(Productivity~Rainfall+Temperature, data=pdata1, model="pooling")
summary(pooling)

# Fixed effects or within estimator
fixed = plm(Productivity~Rainfall+Temperature, data=pdata1, model="within")
summary(fixed)

# random effects estimator
random = plm(Productivity~Rainfall+Temperature, data=pdata1, model="random")
summary(random)

# Haussman test for fixed versus random effect model
phtest(fixed, random)

# Dapat disimpulkan berdasarkan phtest di atas, bahwa tidak terdapat efek random di dalam model

plmtest(fixed, effect = "twoways", type = "bp")
plmtest(fixed, effect = "individual", type = "bp")
plmtest(fixed, effect = "time", type = "bp")

# Selanjutnya, untuk mengekstrak efek dari setiap kategori (time, cross-section, atau keduanya), digunakan perintah fixef()
time_effect <- plm(Productivity~Rainfall+Temperature, data=pdata1, model="within", effect="time")
summary(time_effect)
fixef(time_effect, type = "level")

individual_effect <- plm(Productivity~Rainfall+Temperature, data=pdata1, model="within", effect="individual")
summary(individual_effect)
fixef(individual_effect, type = "level")

Predict_Value_Panel <- predict(pooling, pdata1)
View(Predict_Value_Panel)

diff = Predict_Value_Panel - pdata1$Productivity
View(diff)

RMSE = sqrt(mean(diff^2))
RMSE
