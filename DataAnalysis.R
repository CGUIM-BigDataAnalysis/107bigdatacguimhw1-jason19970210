#library(jsonlite)
library(dplyr)
library(stringr)

edu_103_full <- read.csv("/Users/macbook/Desktop/三下/107bigdatacguimhw1-jason19970210/Input/103年各教育程度別初任人員經常性薪資─按大職類分.csv",stringsAsFactors = F)
edu_104_full <- read.csv("/Users/macbook/Desktop/三下/107bigdatacguimhw1-jason19970210/Input/104年各教育程度別初任人員經常性薪資─按大職類分.csv",stringsAsFactors = F)
edu_105_full <- read.csv("/Users/macbook/Desktop/三下/107bigdatacguimhw1-jason19970210/Input/105年各教育程度別初任人員每人每月經常性薪資─按大職類分.csv",stringsAsFactors = F)
edu_106_full <- read.csv("/Users/macbook/Desktop/三下/107bigdatacguimhw1-jason19970210/Input/106年各教育程度別初任人員每人每月經常性薪資─按大職類分.csv",stringsAsFactors = F)

# 大學薪資
join_103_106 <- inner_join(edu_103_full, edu_106_full, by="大職業別")
#取`大學薪資`欄位
join_103_106$大學.薪資.x <- as.numeric(join_103_106$大學.薪資.x)
join_103_106$大學.薪資.y <- as.numeric(join_103_106$大學.薪資.y)
join_103_106$percent <- (join_103_106$大學.薪資.y / join_103_106$大學.薪資.x)

join_103_106 <- arrange(join_103_106,desc(percent))
filter_105 <- filter(join_103_106 , percent > 1.05)
head(filter_105, 10)
career <- as.character(filter_105$大職業別) # 提高超過5%的的職業有哪些(5分)
tmp <- strsplit(career, "-") # by library(stringr)
#tmp <- unlist(tmp, use.names=F)
tmp[[1]][1]
table(unlist(lapply(tmp, "[", 1)))
grep("不動產",tmp)
#sum(str_count(join_103_106$大職業別,"-")) # `-` 數量

# How to create a data frame
# https://dzone.com/articles/learn-r-how-create-data-frames


df_103 <- edu_103_full[,c(2,12)]
df_104 <- edu_104_full[,c(2,12)]
df_105 <- edu_105_full[,c(2,12)]
df_106 <- edu_106_full[,c(2,12)]
joined_df_103_to_106 <- inner_join(df_103, df_104, by="大職業別")
joined_df_103_to_106 <- inner_join(joined_df_103_to_106, df_105, by="大職業別")
joined_df_103_to_106 <- inner_join(joined_df_103_to_106, df_106, by="大職業別")

# ===
# re-order the data frame
# example : `data <- data[c(1,3,2)]`
#joined_df_103_to_106 <- joined_df_103_to_106[c(2,1,3,4,5,6,7,8,9)]

# rename column names
# http://rprogramming.net/rename-columns-in-r/
# names(data) <- c("new_name", "another_new_name")
names(joined_df_103_to_106) <- c("大職業別","College_f/m_103","College_f/m_104","College_f/m_105","College_f/m_106")
