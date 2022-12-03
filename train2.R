
library(dplyr)

## ���� �ð�ȭ ��Ű�� 
install.packages("ggmap")
install.packages("ggplot2")
install.packages("raster")
install.packages("rgeos")
install.packages("maptools")
install.packages("rgdal")

library(ggmap)
library(ggplot2)
library(raster)
library(rgeos)
library(maptools)
library(rgdal)


options(max.print=9999999)

df_train <- read.csv("����� ����ö ������ �ο� ����.csv")
march_train <- head(df_train,609)



march_train_gap <- c() #������ ������ �ʱ�ȭ

## ȣ����, ����ö�� �־��ֱ�
ȣ���� <- c()

for(i in 0:609){
  ȣ����[i] <- FALSE
}

march_train_gap <- data.frame(ȣ����)

march_train_gap$ȣ���� <- march_train$ȣ����
march_train_gap$����ö�� <- march_train$����ö��


## �ð����� ������ �ο� ���� ���
march_train_gap$gap45 <- (march_train$X04��.05��.�����ο� - march_train$X04��.05��.�����ο�)
march_train_gap$gap56 <- (march_train$X05��.06��.�����ο� - march_train$X05��.06��.�����ο�)
march_train_gap$gap67 <- (march_train$X06��.07��.�����ο� - march_train$X06��.07��.�����ο�)
march_train_gap$gap78 <- (march_train$X07��.08��.�����ο� - march_train$X07��.08��.�����ο�)
march_train_gap$gap89 <- (march_train$X08��.09��.�����ο� - march_train$X08��.09��.�����ο�)
march_train_gap$gap910 <- (march_train$X09��.10��.�����ο� - march_train$X09��.10��.�����ο�)
march_train_gap$gap1011 <- (march_train$X10��.11��.�����ο� - march_train$X10��.11��.�����ο�)
march_train_gap$gap1112 <- (march_train$X11��.12��.�����ο� - march_train$X11��.12��.�����ο�)
march_train_gap$gap1213 <- (march_train$X12��.13��.�����ο� - march_train$X12��.13��.�����ο�)
march_train_gap$gap1314 <- (march_train$X13��.14��.�����ο� - march_train$X13��.14��.�����ο�)
march_train_gap$gap1415 <- (march_train$X14��.15��.�����ο� - march_train$X14��.15��.�����ο�)
march_train_gap$gap1516 <- (march_train$X15��.16��.�����ο� - march_train$X15��.16��.�����ο�)
march_train_gap$gap1617 <- (march_train$X16��.17��.�����ο� - march_train$X16��.17��.�����ο�)
march_train_gap$gap1718 <- (march_train$X17��.18��.�����ο� - march_train$X17��.18��.�����ο�)
march_train_gap$gap1819 <- (march_train$X18��.19��.�����ο� - march_train$X18��.19��.�����ο�)
march_train_gap$gap1920 <- (march_train$X19��.20��.�����ο� - march_train$X19��.20��.�����ο�)
march_train_gap$gap2021 <- (march_train$X20��.21��.�����ο� - march_train$X20��.21��.�����ο�)
march_train_gap$gap2122 <- (march_train$X21��.22��.�����ο� - march_train$X21��.22��.�����ο�)
march_train_gap$gap2223 <- (march_train$X22��.23��.�����ο� - march_train$X22��.23��.�����ο�)
march_train_gap$gap2324 <- (march_train$X23��.24��.�����ο� - march_train$X23��.24��.�����ο�)
march_train_gap$gap0001 <- (march_train$X00��.01��.�����ο� - march_train$X00��.01��.�����ο�)
march_train_gap$gap12 <- (march_train$X01��.02��.�����ο� - march_train$X01��.02��.�����ο�)
march_train_gap$gap23 <- (march_train$X02��.03��.�����ο� - march_train$X02��.03��.�����ο�)
march_train_gap$gap34 <- (march_train$X03��.04��.�����ο� - march_train$X03��.04��.�����ο�)



##�� ȣ���� �ð����� ������ �ο� ��� ������ֱ��(gap_average)


ȣ���� <- c()

for(i in 0:609){
  ȣ����[i] <- FALSE
}

gap_average <- data.frame(ȣ����)
gap_average$ȣ���� <- march_train$ȣ����
gap_average$����ö�� <- march_train$����ö��

## ����ȭ

gap_average$gap45 <- c(0)
gap_average$gap56 <- c(0)
gap_average$gap67 <- c(0)
gap_average$gap78 <- c(0)
gap_average$gap89 <- c(0)
gap_average$gap910 <- c(0)
gap_average$gap1011 <- c(0)
gap_average$gap1112 <- c(0)
gap_average$gap1213 <- c(0)
gap_average$gap1314 <- c(0)
gap_average$gap1415 <- c(0)
gap_average$gap1516 <- c(0)
gap_average$gap1617 <- c(0)
gap_average$gap1718 <- c(0)
gap_average$gap1819 <- c(0)
gap_average$gap1920 <- c(0)
gap_average$gap2021 <- c(0)
gap_average$gap2122 <- c(0)
gap_average$gap2223 <- c(0)
gap_average$gap2324 <- c(0)
gap_average$gap0001 <- c(0)
gap_average$gap12 <- c(0)
gap_average$gap23 <- c(0)
gap_average$gap34 <- c(0)


for (i in 1:609){
  gap_average[i,3] <- (march_train_gap[i,3] - mean(march_train_gap$gap45)) / sd(march_train_gap$gap45)
}
for (i in 1:609){
  gap_average[i,4] <- (march_train_gap[i,4] - mean(march_train_gap$gap56)) / sd(march_train_gap$gap56)
}
for (i in 1:609){
  gap_average[i,5] <- (march_train_gap[i,5] - mean(march_train_gap$gap67)) / sd(march_train_gap$gap67)
}
for (i in 1:609){
  gap_average[i,6] <- (march_train_gap[i,6] - mean(march_train_gap$gap78)) / sd(march_train_gap$gap78)
}
for (i in 1:609){
  gap_average[i,7] <- (march_train_gap[i,7] - mean(march_train_gap$gap89)) / sd(march_train_gap$gap89)
}
for (i in 1:609){
  gap_average[i,8] <- (march_train_gap[i,8] - mean(march_train_gap$gap910)) / sd(march_train_gap$gap910)
}
for (i in 1:609){
  gap_average[i,9] <- (march_train_gap[i,9] - mean(march_train_gap$gap1011)) / sd(march_train_gap$gap1011)
}
for (i in 1:609){
  gap_average[i,10] <- (march_train_gap[i,10] - mean(march_train_gap$gap1112)) / sd(march_train_gap$gap1112)
}
for (i in 1:609){
  gap_average[i,11] <- (march_train_gap[i,11] - mean(march_train_gap$gap1213)) / sd(march_train_gap$gap1213)
}
for (i in 1:609){
  gap_average[i,12] <- (march_train_gap[i,12] - mean(march_train_gap$gap1314)) / sd(march_train_gap$gap1314)
}
for (i in 1:609){
  gap_average[i,13] <- (march_train_gap[i,13] - mean(march_train_gap$gap1415)) / sd(march_train_gap$gap1415)
}
for (i in 1:609){
  gap_average[i,14] <- (march_train_gap[i,14] - mean(march_train_gap$gap1516)) / sd(march_train_gap$gap1516)
}
for (i in 1:609){
  gap_average[i,15] <- (march_train_gap[i,15] - mean(march_train_gap$gap1617)) / sd(march_train_gap$gap1617)
}
for (i in 1:609){
  gap_average[i,16] <- (march_train_gap[i,16] - mean(march_train_gap$gap1718)) / sd(march_train_gap$gap1718)
}
for (i in 1:609){
  gap_average[i,17] <- (march_train_gap[i,17] - mean(march_train_gap$gap1819)) / sd(march_train_gap$gap1819)
}
for (i in 1:609){
  gap_average[i,18] <- (march_train_gap[i,18] - mean(march_train_gap$gap1920)) / sd(march_train_gap$gap1920)
}
for (i in 1:609){
  gap_average[i,19] <- (march_train_gap[i,19] - mean(march_train_gap$gap2021)) / sd(march_train_gap$gap2021)
}
for (i in 1:609){
  gap_average[i,20] <- (march_train_gap[i,20] - mean(march_train_gap$gap2122)) / sd(march_train_gap$gap2122)
}
for (i in 1:609){
  gap_average[i,21] <- (march_train_gap[i,21] - mean(march_train_gap$gap2223)) / sd(march_train_gap$gap2223)
}
for (i in 1:609){
  gap_average[i,22] <- (march_train_gap[i,22] - mean(march_train_gap$gap2324)) / sd(march_train_gap$gap2324)
}
for (i in 1:609){
  gap_average[i,23] <- (march_train_gap[i,23] - mean(march_train_gap$gap0001)) / sd(march_train_gap$gap0001)
}
for (i in 1:609){
  gap_average[i,24] <- (march_train_gap[i,24] - mean(march_train_gap$gap12)) / sd(march_train_gap$gap12)
}
for (i in 1:609){
  gap_average[i,25] <- (march_train_gap[i,25] - mean(march_train_gap$gap23)) / sd(march_train_gap$gap23)
}
for (i in 1:609){
  gap_average[i,26] <- c(-13.66263) ## 3~4�ÿ� Ÿ�� �ο��� 0������ �ּڰ����� ����
}

## ��ħ ���� 05~12 // ���� ���� 12~19 // ����,���� ���� 19~05
## ȥ�⵵ ���� //  �ſ� ����(-2) -14<  <-8   // ����(-1) -8<= <0   // ����(0) 0<= <2   // ȥ��(1)  2<=  <10    //  �ſ� ȥ��(2) 10<=  <16
## ������ 0��2�� ���� ������ �� ������ 6���� ���� �� ������ ���� �ʾ���� ���Ƽ� �Ǵ��ϴµ��� ���ǹ������� ����.


honzap <- gap_average


x <- 3:26
min(gap_average[,x]) ## -13.66263
max(gap_average[,x]) ## 15.69135

for(i in 1 : 609){
  for(j in 3: 26){
    if(gap_average[i,j]>-14 & gap_average[i,j] < -8) honzap[i,j] <- c(-2) ## �ſ� ����
    else if (gap_average[i,j]>=-8 & gap_average[i,j] < 0 ) honzap[i,j] <- c(-1) ## ����
    else if (gap_average[i,j]>=0 & gap_average[i,j] < 2 ) honzap[i,j] <- c(0) ## ����
    else if (gap_average[i,j]>=2 & gap_average[i,j] < 10 ) honzap[i,j] <- c(1) ## ȥ��
    else if (gap_average[i,j]>=10 & gap_average[i,j] < 16 ) honzap[i,j] <- c(2) ## �ſ� ȥ��
  }
}

View(honzap)

## ��ħ,����,���� �ð����� ���ļ� ȥ�⵵ ���
sum<-c(0)

ȣ���� <- c()

for(i in 0:609){
  ȣ����[i] <- FALSE
}

sum_honzap <- data.frame(ȣ����)
sum_honzap$ȣ���� <- march_train$ȣ����
sum_honzap$����ö�� <- march_train$����ö��

sum_honzap$��ħ <- c(0)
sum_honzap$���� <- c(0)
sum_honzap$���� <- c(0)


for(i in 1 : 609){
  for(j in 4 : 10){
    sum = sum + honzap[i,j]
  }
  sum_honzap[i,3] <- sum
  sum <- c(0)
}

for(i in 1 : 609){
  for(j in 11 : 17){
    sum = sum + honzap[i,j]
  }
  sum_honzap[i,4] <- sum
  sum <- c(0)
}

for(i in 1 : 609){
  for(j in 18 : 25){ ## 3��~4�� ������ ���� ����(����,���� �ο��� �ƿ� ���� ���ǹ��� ������)
    sum = sum + honzap[i,j]
  }
  sum = sum + honzap[i,3]
  sum_honzap[i,5] <- sum
  sum <- c(0)
}



seoul_id <- read.csv("seoul_id.csv")

seoul_id$��ħ <- c(0)
seoul_id$���� <- c(0)
seoul_id$���� <- c(0)


seoul_id[,3] <- c(-6.4, -10.154, -6, -2.786, -1.727, -3.833, -0.5, -1.4, 2, 2, -0.25, -0.416, -4.5, -4.437, -0.857, -2.388, -0.429, -7, -4.812, -1.5, 3.4, -5.688, -6.056, -2.5, -0.818) 

seoul_id[,4] <- c(-0.933, -2.769, -4.1, -3.929, -4.182, -2.5, -4.375, -3.3, -4, -5, -4, -4.416, -1.833, -3.75, -4.857, -3.167, -3.714, 1, -2.875, -4.071, -2.8, -2.312, -1.667, -3.864, -4.455 )

seoul_id[,5] <- c(-0.8, -2.154, -2.1, -3.714, -4.545, -3.5 ,-6, -5.6, -6, -5.333, -6.75, -5.916, -3.167, -2.25, -5.571, -4.778, -5.429, 4, -3.1875, -5.714, -6.2, -1.438, 0.389, -3.773, -6.364)




## �ܼ� ����ȸ�͸�7
from_to <- c("���->���� 07��")y
df_model1<- data.frame(station1 = c("���", "��ǻ���", "���� ���", "�Ｚ", "����", "����", "����"),
                       honzap1 = c(-2.213, 0.154, -0.316, -4.07, -3.347, -4.151, -3.907),
                       sum1 = c(-2,-1,-3,-7,-7,-7,-8))
df_model1

answer1 <- lm(sum1~honzap1, data = df_model1)
answer1

summary(answer1)


par("mar")
par(mar=c(1,1,1,1))

plot(df_model1$honzap1, df_model1$sum1)  
abline(answer1)


## �ܼ� ����ȸ�͸�16
from_to <- c("���->���� 16��")
df_model2 <- data.frame(station2 = c("���", "��ǻ���", "���� ���", "�Ｚ", "����", "����", "����"),
                       honzap2 = c(0.127, -0.885, -1.189, 5.369, 7.340, 4.785, 4.762),
                       sum2 = c(-6, -6, -7, 2, 3, 2, 2))
df_model2

answer2 <- lm(sum2~honzap2, data = df_model2)
answer2

summary(answer2)


par("mar")
par(mar=c(1,1,1,1))

plot(df_model2$honzap2, df_model2$sum2)  
abline(answer2)