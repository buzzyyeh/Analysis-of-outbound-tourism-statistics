#12/11 file creation
#data_folder 
data_path <- "~/R/dsR/final_data/"

#12/13 data collection

require(dplyr)
require(ggplot2)

City <- read.csv(paste0(data_path,"City.csv"),encoding = "utf-8")
ExchangeRate <- read.csv(paste0(data_path,"ExchangeRate.csv"),encoding = "utf-8")
ExchangeRate_annually <- read.csv(paste0(data_path,"ExchangeRate_annually.csv"),encoding = "utf-8")
Taiwan_abroad_population_annually <- read.csv(paste0(data_path,"Taiwan_abroad_population_annually.csv"),encoding = "utf-8")
Taiwan_GDPetc_data_annually <- read.csv(paste0(data_path,"Taiwan_GDPetc_data_annually.csv"),encoding = "utf-8")
Taiwan_GDPetc_data_season <- read.csv(paste0(data_path,"Taiwan_GDPetc_data_season.csv"),encoding = "utf-8")

Team13 <- mutate(Taiwan_abroad_population_annually,JPY.USD = ExchangeRate_annually$JPY.USD[-23])
Team13 <- mutate(Team13,Taiwan_GDP = Taiwan_GDPetc_data_annually$GDP_millionNTD[4:25])
#日本出國旅遊人數跟日圓匯率分布關係
ggplot(Team13,aes(x = Japan,y= JPY.USD)) + geom_point(stat = "identity")

#總出國旅遊人數跟日圓匯率分布關係
ggplot(Team13,aes(x = Totals,y= JPY.USD)) + geom_point(stat = "identity")

#日本出國旅遊人數跟台灣GDP分布關係
ggplot(Team13,aes(x = Japan,y= Taiwan_GDP)) + geom_point(stat = "identity")

#總出國旅遊人數跟台灣GDP分布關係
ggplot(Team13,aes(x = Totals,y= Taiwan_GDP)) + geom_point(stat = "identity")

#年份-(日本/總出國人數)
ggplot(Team13,aes(x = Years,y = Japan/Totals)) + geom_point(stat = "identity")
#從2011年開始去日本比率高升
#假設一：2011年開始出現多往日本的廉航班機
#假設二：日本開始發展觀光產業

City <- mutate(City,Japan_only = City$總計 - City$日本以外)

r <- Team13$Japan[7:22] / City$Japan_only[-17]
Clean_City <- as.data.frame(t(t(City[-17,2:13]) * r))

Clean_City <- mutate(Clean_City,Years = City$航線別與飛行班次[-17])
ggplot(Clean_City,aes(x = Clean_City$Years,y=Clean_City$東京)) +  geom_bar(stat = "identity")
ggplot(Clean_City,aes(x = Years)) + geom_line(aes(y = Clean_City$東京)) + geom_line(aes(y = Clean_City$大阪))+ geom_line(aes(y = Clean_City$名古屋)) + geom_line(aes(y = Clean_City$福岡)) +
            geom_line(aes(y = Clean_City$琉球)) + geom_line(aes(y = Clean_City$札幌)) +
            geom_line(aes(y = Clean_City$仙台)) + geom_line(aes(y = Clean_City$廣島)) +
            geom_line(aes(y = Clean_City$福島)) + geom_line(aes(y = Clean_City$宮崎)) +
            geom_line(aes(y = Clean_City$靜岡)) + geom_line(aes(y = Clean_City$鹿兒島))
