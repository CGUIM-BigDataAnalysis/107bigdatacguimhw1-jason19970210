#library(jsonlite)
library(dplyr)
library(stringr)

edu_103 <- read.csv("~/Desktop/三下/BigDataAnalyticalMethods/20190509HW/107bigdatacguimhw1-jason19970210/Input/103年各教育程度別初任人員經常性薪資─按大職類分.csv",stringsAsFactors = F)
edu_106 <- read.csv("~/Desktop/三下/BigDataAnalyticalMethods/20190509HW/107bigdatacguimhw1-jason19970210/Input/106年各教育程度別初任人員每人每月經常性薪資─按大職類分.csv",stringsAsFactors = F)

# 大學薪資
join_103_106 <- inner_join(edu_103, edu_106, by="大職業別")
#取column11,column24
join_103_106$大學.薪資.x <- as.numeric(join_103_106$大學.薪資.x)
join_103_106$大學.薪資.y <- as.numeric(join_103_106$大學.薪資.y)
join_103_106$percent <- join_103_106$大學.薪資.y / join_103_106$大學.薪資.x

join_103_106 <- arrange(join_103_106,desc(percent))
filter_105 <- filter(join_103_106 , percent > 1.05)
head(filter_105, 10)
career <- as.character(filter_105$大職業別) # 提高超過5%的的職業有哪些(5分)
tmp <- strsplit(career, "-")
#tmp <- unlist(tmp, use.names=F)
tmp[[1]][1]
table(unlist(lapply(tmp, "[", 1)))
table(tmp)
grep("不動產",tmp)


sum(str_count(join_103_106$大職業別,"-")) # 全表 `-` 分析數量

