# สมาชิกกลุ่ม
# กลุ่ม 4 แซมบาสก้าไอซ์
# 65050689 ภาณุภณ สิริวรพาส
# 65050866 ศิรชัช เมธาศิลวัต
# 65050895 สรวิชญ์ หงษ์เกิด
# 65050989 อภิสิทธิ์ เยียระยงค์

# Install Package
# install.packages('fpp3', dependencies = TRUE)
# install.packages("ggplot2")
# install.packages("ggseasonplot")

# Call Library
# fpp2 เป็นพวก Example Dataset
library(fpp2)
library(forecast)
library(ggplot2)
library(dbplyr)

#### z ####
# Normal Test
## Install Package
# install.packages('nortest')
library('nortest')

### 1. Data Preparation
# ข้อมูลยอดขายของน้ำส้ม ตั้งแต่เดือนมกราคม พ.ศ. 2563 ถึง เดือนธันวาคม พ.ศ. 2565
z<-c(31, 36, 42, 48, 44, 42, 46, 52, 50, 53, 52, 46, 
     43, 45, 50, 45, 31, 35, 55, 57, 61, 50, 36, 47, 
     64, 72, 155, 158, 157, 156, 161, 168, 170, 172, 174, 152)

y <- ts(z, start=2563, frequency=12)
Seasonal=cycle(y)
Trend=rep(1:length(z))
dataframe=data.frame(y,Trend,Seasonal)

# To create dummary variable using ifelse function
dataframe$Jan<-ifelse(dataframe$Seasonal==1,1,0)
dataframe$Feb<-ifelse(dataframe$Seasonal==2,1,0)
dataframe$Mar<-ifelse(dataframe$Seasonal==3,1,0)
dataframe$Api<-ifelse(dataframe$Seasonal==4,1,0)
dataframe$May<-ifelse(dataframe$Seasonal==5,1,0)
dataframe$Jun<-ifelse(dataframe$Seasonal==6,1,0)
dataframe$Jul<-ifelse(dataframe$Seasonal==7,1,0)
dataframe$Aug<-ifelse(dataframe$Seasonal==8,1,0)
dataframe$Sep<-ifelse(dataframe$Seasonal==9,1,0)
dataframe$Oct<-ifelse(dataframe$Seasonal==10,1,0)
dataframe$Nov<-ifelse(dataframe$Seasonal==11,1,0)
dataframe


### 2. Testing of Normality
# Anderson Daring test ใช้วิธีนี้เนื่องจากข้อมูลมีจำนวนน้อยกว่า 50

ad.test(z)
## สรุปได้ว่า อนุกรมเวลาไม่มีการแจกแจงปกติ ##


### 3. Testing for trend 
# Daniel's Test ใช้วิธีนี้เนื่องจากข้อมูลเป็น Nonparametric 

cor.test(dataframe$y, dataframe$Trend, method=c("spearman"))
## สรุปได้ว่า อนุกรมเวลายอดขายของ น้ำส้ม มีแนวโน้ม
## ถ้าข้อมูลมี แนวโน้ม เราต้องไปทำการ detrend ก่อน


### 4. Seasonality Test 
# Kruskal-Wallis test ใช้วิธีนี้เนื่องจากข้อมูลเป็น Nonparametric
# ต้องทำการ detrend ก่อน เพราะข้อมูลมีแนวโน้ม โดยมี 3 Step

#Step 1: Detect the Trend
# ใช้ centre = T เพราะข้อมูลมันไม่ตกกึ่งกลางของชั้น โดยทำการหา ma ซ้ำอีกรอบนึง
trend = ma(y, order = 12, centre = T)

#Step 2: Detrend the Time Series
detrend = y - trend

#Step 3: Kruskal-Wallis test
kruskal.test(detrend ~ Seasonal, data = dataframe) 
## สรุปได้ว่า อนุกรมเวลาไม่มีอิทธิพลฤดูกาล