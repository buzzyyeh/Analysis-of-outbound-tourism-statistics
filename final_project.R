
#data_folder 
data_path <- "~/R/dsR/final_data/"

#data collection

require(dplyr)
require(ggplot2)

City <- read.csv(paste0(data_path,"City.csv"),encoding = "utf-8")
ExchangeRate <- read.csv(paste0(data_path,"ExchangeRate.csv"),encoding = "utf-8")
ExchangeRate_annually <- read.csv(paste0(data_path,"ExchangeRate_annually.csv"),encoding = "utf-8")
Taiwan_abroad_population_annually <- read.csv(paste0(data_path,"Taiwan_abroad_population_annually.csv"),encoding = "utf-8")
Taiwan_GDPetc_data_annually <- read.csv(paste0(data_path,"Taiwan_GDPetc_data_annually.csv"),encoding = "utf-8")
Taiwan_GDPetc_data_season <- read.csv(paste0(data_path,"Taiwan_GDPetc_data_season.csv"),encoding = "utf-8")
City_monthly <- read.csv(paste0(data_path,"City_monthly.csv"),encoding = "utf-8")
earthquake <- read.csv(paste0(data_path,"earthquake.csv"),encoding = "utf-8")
scourge <- read.csv(paste0(data_path,"scourge.csv"),encoding = "utf-8")
Taiwan_Japan_Monthly <- read.csv(paste0(data_path,"Taiwan_Japan_Monthly.csv"),encoding = "utf-8")
typhoon_landing2 <- read.csv(paste0(data_path,"typhoon_landing2.csv"),encoding = "utf-8")
#============================================================================

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

#從各月份觀察觀光人數趨勢
#一月份觀光人數從2013年開始突增
ggplot(Taiwan_Japan_Monthly,aes(year,JAN))+
  geom_line(stat="identity")+geom_point()
#二月份觀光人數從2012年開始突增
ggplot(Taiwan_Japan_Monthly,aes(year,FEB))+
  geom_line(stat="identity")+geom_point()
#三月份觀光人數從2011年開始突增
ggplot(Taiwan_Japan_Monthly,aes(year,MAR))+
  geom_line(stat="identity")+geom_point()
#四月份觀光人數從2011年開始突增
ggplot(Taiwan_Japan_Monthly,aes(year,APR))+
  geom_line(stat="identity")+geom_point()
#五月份觀光人數從2011年開始突增
ggplot(Taiwan_Japan_Monthly,aes(year,MAY))+
  geom_line(stat="identity")+geom_point()
#六月份觀光人數從2012年開始突增
ggplot(Taiwan_Japan_Monthly,aes(year,JUN))+
  geom_line(stat="identity")+geom_point()
#七月份觀光人數從2012年開始突增
ggplot(Taiwan_Japan_Monthly,aes(year,JUL))+
  geom_line(stat="identity")+geom_point()
#八月份觀光人數從2012年開始突增
ggplot(Taiwan_Japan_Monthly,aes(year,AUG))+
  geom_line(stat="identity")+geom_point()
#九月份觀光人數從2012年開始突增
ggplot(Taiwan_Japan_Monthly,aes(year,SEP))+
  geom_line(stat="identity")+geom_point()
#十月份觀光人數從2012年開始突增
ggplot(Taiwan_Japan_Monthly,aes(year,OCT))+
  geom_line(stat="identity")+geom_point()
#十一月份觀光人數從2011年開始突增
ggplot(Taiwan_Japan_Monthly,aes(year,NOV))+
  geom_line(stat="identity")+geom_point()
#十二月份觀光人數從2011年開始突增
ggplot(Taiwan_Japan_Monthly,aes(year,DEC))+
  geom_line(stat="identity")+geom_point()

#廉航或其他航線開啟
#2012開設鹿兒島航線
#2011-2012大增可能是因為三一一大地震造成觀光人數大跌，只是回歸到原始成長曲線上

#NTD/JPY  monthly匯率
mutate(ExchangeRate,NTDJPN=ExchangeRate$NTD.USD/ExchangeRate$JPY.USD)

#NTD/JPY  annually匯率
ExchangeRate_annually <- mutate(ExchangeRate_annually,NTDJPY=ExchangeRate_annually$NTD.USD/ExchangeRate_annually$JPY.USD)

#畫出匯率&訪日人數關係
data.frame(NTDJPY = ExchangeRate_annually$NTDJPY[-23],Japan = Taiwan_abroad_population_annually$Japan)%>%
ggplot(aes(Japan,NTDJPY)) + geom_line(stat="identity") + geom_point()
