#####################################################################
#                            BOOTSTRAPS                             #
# CHUONG 17/ SACH PHAN TICH DU LIEU VOI R                           #
# DATE: 07 MAY 2017 CHU NHAT - Tim huong di moi cuu nuoc            #
# DATE: 09 MAY 2017 THU BA - Hoan thanh
#####################################################################
# Nạp thư viện
library(ggplot2)
library(tidyverse)
# Tìm 95% KTC cho trung vị
pain <- c(0.05, 0.15, 0.35, 0.25, 0.20, 0.05, 0.10, 0.05, 0.30, 0.05, 0.25)
mean(pain)
sd(pain)
# TB thấp hơn 2 lần ĐLC do đó biến số này không theo phân phối chuẩn. Tìm 95% KTC của trung vị
median(pain)
n <- length(pain)
# Lấy 10000 mẫu từ số liệu gốc
B <- 10000
# Tạo một biến chứa số trung vị
median.sample <- numeric(B)
# Bắt đầu lấy B mẫu và mỗi mẫu tính trung vị
for (i in 1:B) {
  bs.sample <- sample(pain, n, replace = TRUE)
  median.sample[i] <- median(bs.sample)
}
# Ước tính KTC 95%
quantile(median.sample, probs = c(0.025, 0.975))
# Vẽ biểu đồ
df <- as_tibble(median.sample)
ggplot(df, aes(x = median.sample)) + geom_histogram(bins = 15)

# So sanh hai trung vi -----
# Nhap du lieu hai nhom
control <- c(52, 104, 146, 10, 51, 30, 40, 27, 46)
treatment <- c(94, 197, 16, 38, 99, 141, 23)
# Mau Bootstrap 10000
B <- 10000
# Dinh danh bien cho hai nhom
con <- rep(NA, B)
trt <- rep(NA, B)
dif <- rep(NA, B)
# Lay mau bootstrap
for (i in 1:B) {
  con[i] <- median(sample(control, replace = TRUE))
  trt[i] <- median(sample(treatment, replace = TRUE))
  dif[i] <- trt[i] - con[i]
}
# Tim khoang tin cay 95% cua dif
ci.bs <- c(quantile(dif, c(0.025)), quantile(dif, c(0.975)))

# Ve bieu do
df <- as_tibble(dif)
ggplot(df, aes(x = dif)) +
  geom_histogram(
    aes(y = ..density..),
    binwidth = 5,
    color = "black",
    fill = "white"
  ) +
  geom_density(alpha = 0.2, fill = "#FF6666") +
  geom_vline(xintercept = 0,
             col = "#BB0000",
             linetype = "dashed") +
  geom_vline(xintercept = ci.bs[1],
             col = "#BB0000",
             linetype = "longdash") +
  geom_vline(xintercept = ci.bs[2],
             col = "#BB0000",
             linetype = "longdash") +
  xlab("Difference")

# So sanh hai ty le -----
# Nhap du lieu hai nhom
n <- c(11037, 11034)
disease <- c(104, 189)

# Tạo hai biến có tên aspirin và control
# 1: Bệnh, 0: Không bệnh
# Tạo ra 104 đối tượng với giá trị 1
# rep(1, disease[1])
# Tạo các đối tượng với giá trị 0
# rep(0, n[1] - disease[1])
aspirin <- c(rep(1, disease[1]), rep(0, n[1] - disease[1]))
control <- c(rep(1, disease[2]), rep(0, n[2] - disease[2]))
# Mau Boostraps
B <- 10000
# Tạo 2 vector trống
bs1 <- rep(NA, B)
bs2 <- rep(NA, B)
# Bootstraps sample
for (i in 1:B) {
  resample1 <- sample(aspirin, n[1], replace = TRUE)
  resample2 <- sample(control, n[2], replace = TRUE)
  bs1[i] <- sum(resample1) / n[1]
  bs2[i] <- sum(resample2) / n[2]
}

RR = bs1 / bs2
quantile(RR, c(0.025, 0.50, 0.975))
# Nhu vay Aspirin lam giam nguy co benh tu 31 den 57%
# Dua khoang tin cay vao mot vector
ci.bs <- c(quantile(RR, 0.025), quantile(RR, 0.975))
# Tao ra mot dataframe
rr <- data.frame(RR)
ggplot(rr, aes(x = RR)) + 
  geom_histogram(aes(y = ..density..), binwidth = 0.02, col = "blue", fill = "white") +
  geom_density(alpha = 0.2, fill = "#FF6666") +
  geom_vline(xintercept = 1, col = "#BB0000", linetype = "dashed") +
  geom_vline(xintercept = ci.bs[1], col = "#00AA00", linetype = "longdash") +
  geom_vline(xintercept = ci.bs[2], col = "#00AA00", linetype = "longdash")

# So sanh voi phuong phap co dien ----
treated <- c(0.05, 0.15, 0.35, 0.25, 0.20, 0.05, 0.10, 0.05, 0.30, 0.05, 0.25)
control <- c(0, 0.15, 0, 0.05, 0, 0, 0.05, 0.10)
t.test(treated, control)
# BS approach
n1 <- length(treated)
n2 <- length(control)
B <- 10000
difference <- numeric(B)
no.effect <- 0
for (i in 1:B) {
  bs.treat <- sample(treated, n1, replace = TRUE)
  bs.control <- sample(control, n2, replace = TRUE)
  difference[i] <- mean(bs.treat) - mean(bs.control)
  if (difference[i] < 0) {
    no.effect <- no.effect + 1
  }
}
# Xac suat can thiep co hieu qua la 
1 - no.effect/B 
quantile(difference, probs = c(0.025, 0.50, 0.975))
ci.bs <- c(quantile(difference, 0.025), quantile(difference, 0.975))
# Ve bieu do
df <- as.data.frame(difference)
ggplot(df, aes(x = difference)) +
  geom_histogram(
    aes(y = ..density..),
    binwidth = 0.01,
    col = "blue",
    fill = "white"
  ) +
  geom_density(alpha = 0.2, fill = "#FF6666") +
  geom_vline(xintercept = 0,
             col = "#BB0000",
             linetype = "dashed") +
  geom_vline(xintercept = ci.bs[1],
             col = "#00AA00",
             linetype = "longdash") +
  geom_vline(xintercept = ci.bs[2],
             col = "#00AA00",
             linetype = "longdash")
