library(TAM)
data <-read.csv("D:\\TaiLieu\\0.1NCKH\\DataSet\\Dicht_Data2.csv")
# Loại bỏ 2 cột đầu và đặt lại tên cho câu hỏi
data1 <- data[, -c(1,2)]
data1
colnames(data1) <- paste0("I", seq_len(ncol(data1)))
data1
dim(data1)

#Mô hình 1 tham số
mod1 <- TAM::tam.mml(resp = data1)
print(mod1$xsi)
print(mod1$item_irt)

dokho1pl<-mod1$xsi[[1]]
# Vẽ biểu đồ histogram
hist(dokho1pl, 
     main = "", 
     xlab = "Độ khó", 
     col = "skyblue", 
     border = "white")  # Điều chỉnh số lượng cột trong histogram

plot(mod1)
# EAP estimation cho năng lực (ability) của thí sinh
print(class(mod1$person$EAP))
# Vẽ biểu đồ histogram
hist(mod1$person$EAP, 
     main = "", 
     ylab = "Số lượng thí sinh",
     xlab = "Năng lực của thí sinh", 
     col = "skyblue", 
     border = "white",
     xlim = c(-1.1, 1.5))

print(mod1$person)

#MÔ hình 2 tham số
mod2 <- TAM::tam.mml.2pl(resp=data1,irtmodel="2PL")
print(mod2$item)
print(mod2$xsi)    # item difficulties
print(mod2$B)      # item slopes
print(mod2$item_irt)

# Trích xuất độ phân biệt (a) và độ khó (b) từ mod2$item
item_parameters <- data.frame(
  item = mod2$item$item,
  a = mod2$item$B.Cat1.Dim1,          # Độ phân biệt
  b = mod2$item$xsi.item             # Độ khó (đổi dấu)
)


# In bảng kết quả
print(item_parameters)
hist(mod2$item$xsi.item, 
     main = "", 
     ylab = "Số lượng câu hỏi",
     xlab = "Độ khó", 
     col = "skyblue", 
     border = "white")

print(max(mod2$item$xsi.item))

hist(mod2$item$B.Cat1.Dim1, 
     main = "", 
     ylab = "Số lượng câu hỏi",
     xlab = "Độ phân biệt", 
     col = "skyblue", 
     border = "white")

print(max(max(mod2$item$xsi.item)))

print(max(mod2$item$B.Cat1.Dim1))

print(class(mod2$person$EAP))
hist(mod2$person$EAP, 
     main = "", 
     ylab = "Số lượng thí sinh",
     xlab = "Năng lực của thí sinh", 
     col = "skyblue", 
     border = "white",
     xlim = c(-3, 3.1))

#Mô hình 3 tham số 


# Khởi tạo vector xác định item nào cần ước lượng đoán mò
I <- ncol(data1)
est.guess <- rep(0, I )
# set parameters 9 and 12 equal -> same integers
est.guess[ c(3,7,9,12) ] <- c( 1, 3, 2, 2 )
# starting values guessing parameter
guess <- .2*(est.guess > 0)
# estimate model
mod3 <- TAM::tam.mml.3pl(resp=data1, est.guess=est.guess, guess=guess)
mod3$item
params <- mod3$item
a <- params$B.Cat1.Dim1
b <- params$A
c <- params$guess

print(a)
print(b)
print(c)

hist(a, 
     main = "", 
     ylab = "Số lượng câu hỏi",
     xlab = "Độ phân biệt", 
     col = "skyblue", 
     border = "white",
     xlim = c(-1.5, 4))

hist(b, 
     main = "", 
     ylab = "Số lượng câu hỏi",
     xlab = "Độ khó", 
     col = "skyblue", 
     border = "white")

theta3pl = mod3$person$EAP
theta3pl



hist(theta3pl, 
     main = "", 
     ylab = "Số lượng thí sinh",
     xlab = "Năng lực của thí sinh", 
     col = "skyblue", 
     border = "white")

print(max(theta3pl))
    