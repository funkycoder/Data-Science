###########################################################
#             REFUGEE FROM UNHCR DATA                     #
# Original author : nguyen chi dung                       #
# url: http://rpubs.com/chidungkt/268783                  #
###########################################################
rm(list = ls())
pkg <- c("tidyverse",
         "scales",
         "grid",
         "extrafont",
         "wordcloud",
         "ggtheme"
         )
lapply(pkg, require, character.only = TRUE)
link <- "http://popstats.unhcr.org/en/persons_of_concern.csv"


ref.d <- read.csv(link,
                  # Sử dụng header:
                  header = TRUE,
                  # Bỏ qua 3 dòng đầu  của dữ liệu gốc:
                  skip = 3,
                  # Thay thế các kí tự trống, dấu _ và * thành NA:
                  na.strings = c("", "_", "*"))

# Đổi tên cho bộ dữ liệu:
new.names <- c("Year",
               "Country",
               "Country_Origin",
               "Refugees",
               "Asylum_Seekers",
               "Returned_Refugees",
               "IDPs",
               "Returned_IDPs",
               "Stateless_People",
               "Others_of_Concern",
               "Total")
names(ref.d) <- new.names

# Dữ  liệu trống:
sapply(ref.d, function(x) sum(is.na(x)))

# Xem lại cấu trúc dâtset
str(ref.d)

# Biến Country và Country_Origin thành character vì sẽ sửa tên
ref.d <- ref.d %>% mutate_at(.cols = c("Country", "Country_Origin"),
                             .funs = as.character)
ref.d <- ref.d %>% mutate_if(is.integer, as.numeric)
str(ref.d)

# Các tên duy nhất của các quốc gia
distinct(ref.d, Country)

# Result from distinct is a DATAFRAME
# Get country as a vector
country.name <- distinct(ref.d, Country)$Country
# Any country name longer than 20 chars?
country.name[which(nchar(country.name) > 20)]

# Dán lại nhãn cho  một số quốc gia vì quá dài
change_country_name <- function(name) {
  old.countries <- c("Bolivia (Plurinational State of)",
                     "China, Hong Kong SAR",
                     "China, Macao SAR",
                     "United Kingdom",
                     "United States of America",
                     "Dem. Rep. of the Congo",
                     "Central African Rep.",
                     "Iran (Islamic Rep. of)",
                     "United Arab Emirates",
                     "The former Yugoslav Republic of Macedonia",
                     "Micronesia (Federated States of)",
                     "Serbia and Kosovo (S/RES/1244 (1999))",
                     "Venezuela (Bolivarian Republic of)",
                     "Various/Unknown")

  new.countries <- c("Bolivia",
                     "Hong Kong",
                     "Macao",
                     "UK",
                     "US",
                     "Congo",
                     "Central Africa",
                     "Iran",
                     "UAE",
                     "Macedonia",
                     "Micronesia",
                     "Serbia & Kosovo",
                     "Venezuela",
                     "Unknown")
  for (k in 1:length(old.countries)) {
    name[name == old.countries[k]] <- new.countries[k]
  }
  name
}
ref.d$Country = change_country_name(ref.d$Country)
ref.d$Country_Origin = change_country_name(ref.d$Country_Origin)

# Original code:
# for (k in 1:length(old.countries)){
#   ref.d$Country_Origin[ref.d$Country_Origin == old.countries[k]] <- new.countries[k]
#   ref.d$Country[ref.d$Country == old.countries[k]] <- new.countries[k]
# }

# Tính  toán số  người rời  bỏ quê hương mình theo quốc gia:
  ref.count.country <- ref.d %>%
    group_by(Country_Origin) %>%
    summarise_each(funs(sum(., na.rm = TRUE)), Total)

# 10 quốc gia có nhiều  người bỏ quê hương nhất
(ref.top10 <-  ref.count.country %>%
  arrange(desc(Total)) %>%
  slice(1:10))

# Vẽ barplot bằng ggplot 2
ref.top10 %>%
  mutate(Total = Total/1000) %>%
  ggplot(aes(x = reorder(Country_Origin, Total), y = Total, fill = Country_Origin)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  labs(x = NULL,
       y = NULL,
       title = "10 Countries With the Most Refugees",
       subtitle = "Unit: thousand",
       caption = "Data Source: http://popstats.unhcr.org") +
       theme_classic()
# Nếu muốn  bạn có thể  lọc ra thông tin về người tị nạn Việt Nam để khai thác:
(vn <- ref.d %>%
      filter(Country_Origin == "Viet Nam") %>%
      select(Year, Country, Total))

# 10 quốc gia mà người Việt tị nạn nhiều nhất
(vn.top10.country <- vn %>%
                    group_by(Country) %>%
                    summarise_each(funs(sum(., na.rm = TRUE)), Total) %>%
                    arrange(desc(Total)) %>%
                    slice(1:10))
# Vẽ barplot bằng ggplot2
vn.top10.country %>%
  mutate(Total = Total/1000) %>%
  ggplot(aes(x = reorder(Country, Total), y = Total, fill = Country)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  labs( x = NULL,
        y = NULL,
        title = "Top 10 countries with the most Vietnamese refugees",
        subtitle = "Unit: Thousand",
        caption = "Data Source: http://popstats.unhcr.org") +
        theme_classic()

# Những năm nào người Việt bỏ xứ ra đi nhiều nhất?
(vn.year <- vn %>%
                  group_by(Year) %>%
                  summarise_each(funs(sum(., na.rm = TRUE)), Total) %>%
                  arrange(Year)
)

# OK graph it !
# YOU WILL NEED TO LEARN HOW TO WRITE A FUNCTION FOR THOSE REPETITIVE TASKS !
vn.year %>%
  mutate(Total = Total/1000) %>%
  ggplot(aes(x = reorder(Year, Total), y = Total, fill = Year)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  labs( x = NULL,
        y = NULL,
        title = "Top years with the most Vietnamese refugees",
        subtitle = "Unit: Thousand",
        caption = "Data Source: http://popstats.unhcr.org") +
  theme_minimal()

# Word Cloud những quốc gia có nhiều người bỏ xứ ra đi:
ref.count.country <- na.omit(ref.count.country) %>%
                     filter(Total !=0) %>%
                     as.data.frame()
par(bg = "black")
set.seed(1)
wordcloud(ref.count.country$Country_Origin,
          ref.count.country$Total,
          scale = c(3, 1),
          min.freq = 100,
          max.words = 100,
          family = "Garamond",
          font  = 2,
          random.order = F,
          colors =  brewer.pal(8, "Paired"))


# Hình ảnh  hóa dữ liệu về dòng người tị nạn từ 1951:
mydf <- ref.d %>%
        select(-Country, -Country_Origin, -Total) %>%
        gather(variable, value, - Year)
# Xem biến variable có gì?
distinct(mydf, variable)

# Nào ta vẽ đồ thị với ggplot2
# Lấy tên của các cột
column.name <- ref.d %>%
               select(-Country, -Country_Origin, -Total, -Year) %>%
               names()

# Chọn màu:
mycol <- c("#104E8B",
           "#8B1A1A",
           "#CD1076",
           "#0000CD",
           "#8B3A3A",
           "#008B00",
           "#EE0000")


g <- ggplot(mydf, aes(Year, value)) +
  geom_bar(aes(fill = variable), stat = "identity") +
  labs(
    title = "Number of refugees from 1951 to 2014",
    subtitle = "Unit: Millions",
    caption = "Data Source: http://popstats.unhcr.org",
    x = "",
    y = ""
  ) +
  theme_minimal() +
  scale_fill_manual(
    labels = gsub("*_", " ", column.name),
    values = mycol,
    guide_legend(title = "Class")
  )
g

# Làm lại cho đẹp  hơn:

myfunc <- function(x) {
  x / 1000000
}
g + scale_y_continuous(
  limits = c(0, 4.5e7),
  breaks = seq(0, 4.5e7, by = 0.5e7),
  labels = myfunc
) +
  scale_x_continuous(
    limits = c(1950, 2014),
    breaks = seq(1950, 2015, by = 5),
    expand = c(0.01, 0)
  ) +
  theme_light()


