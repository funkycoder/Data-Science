# Comment Lines
# Đây là dòng bình luận do đó R không thi hành lệnh nào cả
# Ví dụ summary(rivers)
summary(rivers) # Dòng này thì R thực thi

# Hàm trong R
# Cú pháp: function.name(arguments, options)
mean(islands, na.rm = TRUE)

# Hai ngoặc tròn () là bắt buộc
ls()
# Nếu không dùng dấu ngoặc tròn thì sao?
ls

# Trích dẫn R
citation()

# Thử khả năng vẽ biểu đồ của R
demo("graphics")

# Liệt kê các đối tượng có trong workspace
ls()
# Loại bỏ tất cả các đối tượng trong workspace
rm(list = ls())
ls()

# Nạp dữ liệu vào workspace
data("HairEyeColor")
?HairEyeColor # Thông tin về dữ liệu
HareEyeColor
HairEyeColor # Chú ý chính tả !
print(HairEyeColor)

# Xem tên của các biến và chiều của dữ liệu
dimnames(HairEyeColor)

# Một cách khác là bảng phẳng flat table cho thấy các chiều của bảng
ftable(HairEyeColor)
ftable(HairEyeColor, col.vars = "Eye")
ftable(HairEyeColor, row.vars = c("Sex", "Hair"), col.vars = "Eye")
ftable(HairEyeColor, row.vars = c("Hair"), col.vars = c("Eye","Sex"))

# Bạn không thích phải gõ nhiều?
HEC <- HairEyeColor
ftable(HEC)

# Thử Chi-square test về tính độc lập của các biến trong bảng
summary(HEC)

# Làm xẹp chiều của bảng (Collapse one or more factors) 
margin.table(HEC, c(1,2)) # Xẹp chiều sex (chỉ giứ lại chiều 1,2)
dimnames(HEC) # Xem lại các chiều
margin.table(HEC, c(2,3)) # Xẹp chiều Hair
margin.table(HEC, c(1,3)) # Xẹp chiều Eye Color
margin.table(HEC, c(1))   # Xẹp chiều Eye Color and Sex

# Thử làm kiểm định xem màu tóc và màu mắt có độc lập ở nhóm nam giới không
chisq.test(HairEyeColor[,,1]) # Tất cả các loại(levels) màu tóc và màu mắt, chỉ dùng loại giới tính đầu tiên (Sex = 1)

# Trong số liệu ước tính có một ô với dữ liệu nhỏ hơn 5
chisq.test(HairEyeColor[,,1])$expected

# Xem tỷ lệ thay vì số lượng?
prop.table(HairEyeColor[,,1], margin = 1)  # Tính theo hàng
prop.table(HairEyeColor[,,1], margin = 1)  # Tính theo cột

# Xem thử data frame của R
as.data.frame(HairEyeColor)
