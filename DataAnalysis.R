#library(jsonlite)
library(dplyr)
library(stringr)
library(ggplot2)
library(magrittr)

# Question 1

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


join_103_106 %>% arrange(. ,desc(percent)) %>% filter(. ,percent > 1) %>% select(.,2)
join_103_106 %>% arrange(. ,desc(percent)) %>% filter(. ,percent > 1.05) %>% select(.,2)
# join_103_106 <- arrange(join_103_106,desc(percent))
# filter_105 <- filter(join_103_106 , percent > 1.05)
# head(filter_105, 10)
career <- as.character(filter_105$大職業別) # 提高超過5%的的職業有哪些(5分)
tmp <- strsplit(career, "-") # by library(stringr) export `list` type
#tmp <- unlist(tmp, use.names=F)
#tmp[[1]][1]
# unlist `tmp`
# lapply
table <- table(unlist(lapply(tmp, "[", 1)))
df <- data.frame(table)


# count
#grep("不動產",tmp)
#sum(str_count(join_103_106$大職業別,"-")) # `-` 數量




# Question 2


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
names(joined_df_103_to_106) <- c("大職業別","College_f_m_103","College_f_m_104","College_f_m_105","College_f_m_106")
#joined_df_103_to_106[2:5] <- as.numeric(joined_df_103_to_106[2:5])
joined_df_103_to_106$College_f_m_103 <- as.numeric(joined_df_103_to_106$College_f_m_103)
joined_df_103_to_106$College_f_m_104 <- as.numeric(joined_df_103_to_106$College_f_m_104)
joined_df_103_to_106$College_f_m_105 <- as.numeric(joined_df_103_to_106$College_f_m_105)
joined_df_103_to_106$College_f_m_106 <- as.numeric(joined_df_103_to_106$College_f_m_106)

# replace `NA` to `0`
# https://bbs.pinggu.org/thread-3589221-1-1.html
#dat[is.na(dat)] <- 0
joined_df_103_to_106[is.na(joined_df_103_to_106)] <- 0

# order the table (女生>男生) (由大至小)
joined_df_103_to_106 %>% arrange(. ,desc(College_f_m_103)) %>% filter(. ,College_f_m_103 > 0) %>% head(10)
joined_df_103_to_106 <- arrange(joined_df_103_to_106,desc(College_f_m_103))
head(joined_df_103_to_106,10)
# 103 年度：礦業及土石採取業-技術員及助理專業人員(100), 用水供應及污染整治業-服務及銷售工作人員(100), 營造業-服務及銷售工作人員(100)
joined_df_103_to_106 <- arrange(joined_df_103_to_106,desc(College_f_m_104))
head(joined_df_103_to_106,10)
# 104 年度：專業_科學及技術服務業-技藝_機械設備操作及組裝人員(100.26), 用水供應及污染整治業-服務及銷售工作人員(100),
#   醫療保健服務業-服務及銷售工作人員(100), 其他服務業-專業人員(100), 不動產業-技藝_機械設備操作及組裝人員(100)
joined_df_103_to_106 <- arrange(joined_df_103_to_106,desc(College_f_m_105))
head(joined_df_103_to_106,10)
# 105年度：金融及保險業-專業人員(100.11), 用水供應及污染整治業-服務及銷售工作人員(100), 醫療保健服務業-技藝_機械設備操作及組裝人員(100),
#   藝術_娛樂及休閒服務業-技術員及助理專業人員(100), 教育服務業-服務及銷售工作人員(100), 礦業及土石採取業-服務及銷售工作人員(100)
joined_df_103_to_106 <- arrange(joined_df_103_to_106,desc(College_f_m_106))
head(joined_df_103_to_106,10)
# 106年度：**資訊及通訊傳播業-服務及銷售工作人員(100.33)**, 用水供應及污染整治業-服務及銷售工作人員(100), 金融及保險業-技藝_機械設備操作及組裝人員(100),
#   專業_科學及技術服務業-技藝_機械設備操作及組裝人員(100), 不動產業-專業人員(100), 礦業及土石採取業-技術員及助理專業人員(100),
#   不動產業-服務及銷售工作人員(100), 資訊及通訊傳播業-技藝_機械設備操作及組裝人員(100), 不動產業-技藝_機械設備操作及組裝人員(100)


# order the table (男生>女生) (由小至大)
joined_df_103_to_106 %>% filter(., College_f_m_103 > 0 & College_f_m_104 > 0, College_f_m_105 > 0, College_f_m_106 > 0,
                                College_f_m_103 < 100 & College_f_m_104 < 100, College_f_m_105 < 100, College_f_m_106 < 100)
joined_df_103_to_106 %>% arrange(. ,College_f_m_103) %>% filter(. ,College_f_m_103 > 0) %>% head(10)
joined_df_103_to_106 %>% arrange(. ,College_f_m_104) %>% filter(. ,College_f_m_104 > 0) %>% head(10)
joined_df_103_to_106 %>% arrange(. ,College_f_m_105) %>% filter(. ,College_f_m_105 > 0) %>% head(10)
joined_df_103_to_106 %>% arrange(. ,College_f_m_106) %>% filter(. ,College_f_m_106 > 0) %>% head(10)
# Output
# 103年度：礦業及土石採取業-技藝_機械設備操作及組裝人員(84.97), 教育服務業-技藝_機械設備操作及組裝人員(88.49), 其他服務業-技術員及助理專業人員(89.36)
# 104年度：電力及燃氣供應業-技藝_機械設備操作及組裝人員(91.69), 教育服務業-服務及銷售工作人員(91.90), 礦業及土石採取業-技術員及助理專業人員(92.42)
# 105年度：不動產業-技藝_機械設備操作及組裝人員(91.38), 醫療保健服務業-專業人員(94.98), 用水供應及污染整治業-事務支援人員(95.04)
# 106年度：電力及燃氣供應業-技藝_機械設備操作及組裝人員(95.51), 營造業-服務及銷售工作人員(95.93), 其他服務業-事務支援人員(96.23)



#joined_df_103_to_106 <- arrange(joined_df_103_to_106,College_f_m_103)
# head(joined_df_103_to_106
#      [joined_df_103_to_106$College_f_m_103 > 0,]
#      , 10)

#head(filter(joined_df_103_to_106 , College_f_m_103 > 0), 10)
#joined_df_103_to_106 <- arrange(joined_df_103_to_106,College_f_m_104)
#head(filter(joined_df_103_to_106 , College_f_m_104 > 0), 10)
#joined_df_103_to_106 <- arrange(joined_df_103_to_106,College_f_m_105)
#head(filter(joined_df_103_to_106 , College_f_m_105 > 0), 10)
#joined_df_103_to_106 <- arrange(joined_df_103_to_106,College_f_m_106)
#head(filter(joined_df_103_to_106 , College_f_m_106 > 0), 10)


# drawing plots
# https://zhuanlan.zhihu.com/p/30706019
# ggplot(data = df, mapping = aes(x = factor(Year), y = Weight, group = 1)) + geom_line() + xlab('Year')
ggplot(data = joined_df_103_to_106, mapping=aes(x="大職業別",y=College_f_m_103))+geom_line()






# Question 3
# process the data
df_1_103 <- edu_103_full[,c(2,11,13)]
df_1_104 <- edu_104_full[,c(2,11,13)]
df_1_105 <- edu_105_full[,c(2,11,13)]
df_1_106 <- edu_106_full[,c(2,11,13)]

joined_df_1_103_to_106 <- inner_join(df_1_103, df_1_104, by="大職業別")
joined_df_1_103_to_106 <- inner_join(joined_df_1_103_to_106, df_1_105, by="大職業別")
joined_df_1_103_to_106 <- inner_join(joined_df_1_103_to_106, df_1_106, by="大職業別")
names(joined_df_1_103_to_106) <- c("大職業別","College103","Graduate103","College104","Graduate104","College105","Graduate105","College106","Graduate106")

#dim(joined_df_1_103_to_106)[2] 
#output : 9
for (i in 2:dim(joined_df_1_103_to_106)[2]) {
  joined_df_1_103_to_106[,i] <- as.numeric(joined_df_1_103_to_106[,i])
  joined_df_1_103_to_106[,i][is.na(joined_df_1_103_to_106[,i])] <- 0
}

joined_df_1_103_to_106$devide103 <- round(joined_df_1_103_to_106$Graduate103 / joined_df_1_103_to_106$College103, 2)
joined_df_1_103_to_106$devide104 <- round(joined_df_1_103_to_106$Graduate104 / joined_df_1_103_to_106$College104, 2)
joined_df_1_103_to_106$devide105 <- round(joined_df_1_103_to_106$Graduate105 / joined_df_1_103_to_106$College105, 2)
joined_df_1_103_to_106$devide106 <- round(joined_df_1_103_to_106$Graduate106 / joined_df_1_103_to_106$College106, 2)

# Process the value `NaN` cause by 0 / 0
for (i in 2:dim(joined_df_1_103_to_106)[2]) {
  joined_df_1_103_to_106[,i][is.na(joined_df_1_103_to_106[,i])] <- 0
}


# get the data which is by year 106
joined_df_1_103_to_106 %>% arrange(. ,desc(devide106)) %>% filter(. ,devide106 > 0) %>% select("大職業別","devide106") %>%head(10)
# 大職業別                            devide106
# 礦業及土石採取業-事務支援人員       1.208946
# 專業_科學及技術服務業               1.202982
# 其他服務業-技術員及助理專業人員     1.199470
# 專業_科學及技術服務業-事務支援人員  1.192306
# 批發及零售業                        1.191916
# 製造業                              1.188350
# 藝術_娛樂及休閒服務業-事務支援人員  1.187705
# 工業部門                            1.183455
# 工業及服務業部門                    1.182345
# 服務業部門                          1.181334

# Question 4
#資訊及通訊傳播業-專業人員
#工業及服務業部門-專業人員
#工業部門-專業人員
#製造業-專業人員
#joined_df_1_103_to_106[c(2,9,23,79),]
job <- joined_df_1_103_to_106[c(2,9,23,79),c(1,8,9,13)]
job$minus106 <- (job$Graduate106 - job$College106)
job[1:4,c(1,5)]



