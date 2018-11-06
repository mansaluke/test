##
#load data
options(java.parameters = "-Xmx8000m")
require(xlsx)
library(tidyr)
library(dplyr)
library(ggplot2)
require(xlsx)


#############id
#load file
i <- ("file:///C:/Users/lmcleary/Documents/test data/thesis/gdpa/ISO-3166-Countries-with-Regional-Codes-master/all/all.csv")
id1 <- read.csv(i, header=TRUE, sep=",")
#rename
names(id1)[names(id1) == "name"] = "country"
id1$country<- tolower(id1$country)
id1$country[id1$alpha.3=="COD"] <- "drc"
id1$country[id1$alpha.3=="SWZ"] <- "swaziland"
id1$country <- str_replace_all(id1$country, " ", "")
#take subset
id <- subset(id1, select=c(1,3))
###########################################
######exp data >1950 in africa - not used
#e <- "C:/Users/lmcleary/Documents/test data/thesis/PRE POST 1950/post1950/imf data 2003/export values.xlsx"
#exp <- read.xlsx(e, sheetName =  "EXPORTS (3)", stringsAsFactors = FALSE)
#exp <- subset(exp, COUNTRY.NAME !="AFRICA")

################benin data from mitchell
##########NEED TO CHANGE EX R
#load
b <- "C:/Users/lmcleary/Documents/test data/thesis/PRE POST 1950/pre1950/exportvaluebenin(mitchell).xlsx"
benin <- read.xlsx(b, 1, stringsAsFactors = FALSE, startRow = 2)
benin <- subset(benin,  exp!=0& !is.na(benin$exp))
benin <- benin[c(1,6)]
#rename
benin$alpha.3 ="BEN"
benin$country ="benin"
################################################
######exp data 1938-49 league of nations
l <- "C:/Users/lmcleary/Documents/test data/thesis/PRE POST 1950/pre1950/league of nations export values (1938-43).xlsx"
lon <- read.xlsx(l, 1, stringsAsFactors = FALSE)
#reshape
lon <- lon %>% gather(year, exp1, X1938:X1943)
#correct year format
lon$year <- substring(lon$year, 2, 5)
#add currency ----need to edit!!!!!!!!!!
lon$exp <- ifelse(lon$NA.=="£", lon$exp1 * 4, "")
lon$exp[lon$NA.=="franc"] <-  lon$exp1 * 0.03
lon$exp[lon$NA.=="escudo"] <-  lon$exp1 * 0.05
#removenulls and rename things
lon <- subset(lon, !is.na(lon$exp))
lon <- lon[c(1, 4, 6) ]
colnames(lon)[1] <- "country"
lon$country <- tolower(lon$country)
lon$country <- str_replace_all(lon$country, " ", "")
lon <- left_join(lon, id, by="country")
lon$alpha.3[lon$country=="cotedeivory(goldcoast)incltogoland)"] <- "CIV"
lon$alpha.3[lon$country=="belgiumcongo"] <- "COG"
#change formats
lon$year <- as.numeric(lon$year)
lon$exp <- as.numeric(lon$exp)*1000000
##################################################

##combine lon and BENIN
lon <- full_join(lon, benin)
#####################################

######federico and tena pre-1938
#load data
ft <- "C:/Users/lmcleary/Documents/test data/thesis/PRE POST 1950/pre1950/exportvaluesadj/federico 2016 trade/africa_1817_1938_FTWTHD_201710_v01(unedited).xlsx"
hist <- read.xlsx(ft,sheetName = "exportscurrent1913", stringsAsFactors = FALSE, startRow = 6)
#reshape
hist <- hist %>% gather(country, exp, Algeria:ncol(hist), na.rm = T) 
#change formats
hist$exp <- as.numeric(hist$exp) * 1000000
#rename
colnames(hist)[1] <- "year"
hist$country <- gsub('[.]', '', hist$country)
hist$country <- tolower(hist$country)
#merge with id by country
hist <- merge(x=hist, y=id, by = "country", all.x=TRUE)
#query to see which countries do not have alpha.3 codes assigned
hist %>% distinct(country, .keep_all=TRUE) %>%
  subset(is.na(alpha.3))
#manually assign alpha.3 codes
hist$alpha.3[hist$country=="angolaportuguessafrica"] <- "AGO"
hist$alpha.3[hist$country=="belgiumcongozaire"] <- "COG"
hist$alpha.3[hist$country=="caboverdeportugueseafrica"] <- "CPV"
hist$alpha.3[hist$country=="camerun"] <- "CMR"
hist$alpha.3[hist$country=="ghanagoldcoast"] <- "GHA"
hist$alpha.3[hist$country=="italianlibiacyrenaica"] <- "LBY"
hist$alpha.3[hist$country=="marocco"] <- "MAR"
hist$alpha.3[hist$country=="mozambiqueportugueseafrica"] <- "MOZ"
hist$alpha.3[hist$country=="stomeeprincipeportuguessafrica"] <- "STP"
hist$alpha.3[hist$country=="ctedivoire"] <- "CIV"
hist$alpha.3[hist$country=="germansouthwestafrica"] <- "NAM"
hist$alpha.3[hist$country=="rhodesia"] <- "ZWE" #zimb
hist$alpha.3[hist$country=="tanganicagermaneastafrica"] <- "TZA"
hist$alpha.3[hist$country=="guineabissauportugueseafrica"] <- "GNB"
hist$alpha.3[hist$country=="sudanangloegyptiansudan"] <- "SDN"
#now join with league of nations data without duplicating years/countries
#remove the data in the lon dataset which overlap with that of the historical dataset
lon <- anti_join(lon, hist, by= c("alpha.3", "year" ))
#now we can combine both datasets
histe <- full_join(hist, lon, by = c("country", "year", "alpha.3", "exp"))
#we add elements from the id dataset such as year
histe <- left_join(histe, id1, by="alpha.3")
histe <- subset(histe,select= c(1, 2, 3, 4, 5, 9, 10))
################uk exort price indices
#load uk exp prices from the imf and mitchell
u <- "C:/Users/lmcleary/Documents/test data/thesis/gdpa/uk/imf2003(1).XLS.xlsx"
uk <- read.xlsx(u, sheetName =  "uk1", stringsAsFactors = FALSE)
uk <- uk[c(1, 3)]
u1 <- "C:/Users/lmcleary/Documents/test data/thesis/gdpa/uk/mitchell 1988.xlsx"
uk1 <- read.xlsx(u1, 2, stringsAsFactors = FALSE, startRow = 3)
uk1 <- uk1[c(1, 6)]
#rename
colnames(uk)[1] <- "year"
colnames(uk1)[1] <- "year"
#join
uk <- full_join(uk, uk1, by = "year")
#drop not needed
rm(uk1)
#drop nulls
uk <- subset(uk, !is.na(year))
#set base year to 100
uk$e <- uk$EXPORT.PRICES / uk$EXPORT.PRICES[uk$year=="1950"]
uk$e1 <- uk$exports / uk$exports[uk$year=="1950"]
#plot both series
ggplot(uk, aes(year)) + 
  geom_line(aes(y = e, colour = "e")) + 
  geom_line(aes(y = e1, colour = "e1"))
#combine series 
uk$ukexp <- ifelse(uk$year>1950, uk$e, uk$e1)
#subset and remove nulls
uk <- uk[c(1, 6)]
uk <- subset(uk, !is.na(ukexp))
#################################################################################################
#####################historical pop data
#load post 1850
p <- "C:/Users/lmcleary/Documents/test data/thesis/PRE POST 1950/pre1950/population/pop.xlsx"
poppre1 <- read.xlsx(p, sheetName =  "mypop", stringsAsFactors = FALSE)
#reshape
poppre1<- gather(poppre1, year1, pop, X1850:X2016, factor_key = TRUE)
#rename
colnames(poppre1)[1] <- "country"
colnames(poppre1)[2] <- "year"
poppre1$country <- substring(poppre1$country, 4)
poppre1$year <- substring(poppre1$year, 2)
################pop pre 1850
p <- "C:/Users/lmcleary/Documents/test data/thesis/PRE POST 1950/pre1950/population/poppre1850.xlsx"
poppre <- read.xlsx(p, sheetName =  "Sheet6", stringsAsFactors = FALSE)
#drop cols
poppre <- subset(poppre, select=-c(1, 3, 4))
#reshape
poppre<- gather(poppre, year1, pop, y1850:y1790, factor_key = TRUE) 
#rename
colnames(poppre)[1] <- "country"
colnames(poppre)[2] <- "year"
poppre$year <- substring(poppre$year, 2)
#merge both pop datasets
poppre1 <-subset(poppre1, year!=1850)
poppre1 <- full_join(poppre1, poppre, by = c("country", "year", "pop"))
#merge with id
poppre1 <- merge(x=poppre1, y=id, by = "country", all.x=TRUE)
#query countries without alpha.3 assignes
poppre1 %>% distinct(country, .keep_all=TRUE) %>%
  subset(is.na(alpha.3))
#manually add alpha.3
poppre1$alpha.3[poppre1$country=="capeverde"] <- "CPV"
poppre1$alpha.3[poppre1$country=="comoro"] <- "COM"
poppre1$alpha.3[poppre1$country=="ctedivoire"] <- "CIV"
poppre1$alpha.3[poppre1$country=="namibia"] <- "NAM"
poppre1$alpha.3[poppre1$country=="rhodesia"] <- "ZWE" #zimb
poppre1$alpha.3[poppre1$country=="swaziland"] <- "SWZ"
poppre1$alpha.3[poppre1$country=="tanzania"] <- "TZA"
poppre1$alpha.3[poppre1$country=="guineabissau"] <- "GNB"
#drop if no alpha.3 assigned
poppre1 <- subset(poppre1, !is.na(alpha.3))
#recall histe is the historical federico and tena data set + league of nations
#drop if no alpha.3 assigned
histe <- subset(histe, !is.na(alpha.3))
#merge hist pop data ad histe 
hist1 <- merge( poppre1, histe, by = c("alpha.3","year"))
#rename
colnames(hist1)[3] <- "country"
#subset
hist1 <- subset(hist1, select = c(1, 2, 3, 4, 6))
#change format
hist1$year <- as.integer(hist1$year)
##########################################
###############ASIA historical data
#load
a <- "C:/Users/lmcleary/Documents/test data/thesis/asia/asia_1800_1938_FTWTHD_201710_v01.xlsx"
as <- read.xlsx(a,sheetName = "exportscurrent1913", stringsAsFactors = FALSE, startRow = 5)
#reshape
as <- as %>% gather(country, exp, Abu.Dhabi:ncol(as),na.rm = T) 
#rename
colnames(as)[1] <- "year"
#change formats
as$exp <- as.numeric(as$exp) * 1000000
#as$country <- str_replace_all(as$country, ".1", "")
as$country <- gsub('[.]', '', as$country)
as$country <- tolower(as$country)
#merge with id
as <- merge(x=as, y=id, by = "country", all.x=TRUE)
#query countries not assigned alpha.3
as %>% distinct(country, .keep_all=TRUE) %>%
  subset(is.na(alpha.3))
#manually assign alpha.3
as$alpha.3[as$country=="britishmalaya"] <- "MYS"
as$alpha.3[as$country=="brunei"] <- "BRN"
as$alpha.3[as$country=="ceylonsrilanka"] <- "LKA"
as$alpha.3[as$country=="indonesia"] <- "IDN"
as$alpha.3[as$country=="formosataiwan"] <- "TWN"
as$alpha.3[as$country=="northyemen"] <- "YEM"
as$alpha.3[as$country=="palestine"] <- "PSE"
as$alpha.3[as$country=="siamthailand"] <- "THA"
####################################################
######maddison data
#load excel
#m <- "C:/Users/lmcleary/Documents/test data/thesis/gdpa/data/mpd2018.xlsx"
#mad <-  read.xlsx(m,sheetName = "Full data", stringsAsFactors = FALSE)
#mad <- mad[1:6]
file <- "C:/Users/lmcleary/Documents/test data/thesis/gdpa/data/mad.txt"
#write to flat file
#write.table(mad, file , sep = ",")
mad <- read.table(file , sep = ",", header=T)
#maddison pop given in 000's
mad$pop <- mad$pop*1000
#rename
colnames(mad)[1] <- "alpha.3"
#############################################################
#other data - imf landlocked countries
#copy table landlocked countries in pdf prior to running this command
#http://www.un.org/en/development/desa/policy/wesp/wesp_current/wesp2014.pdf
#copdat <- read.delim("clipboard", header=F)
#landl<- copdat
#rename
#landl$V1 <- tolower(landl$V1) 
#landl$landl <- 1
#colnames(landl)[1] <- "country"
#landl$country <- gsub('[ ]', '', landl$country)
#merge with id
#landl <- left_join(landl, id, by="country")
#manually add aloha.3 to coutnries unassigned
#landl$alpha.3[landl$country=="laopeople'sdemocratic"] <- "LAO"
#landl$alpha.3[landl$country=="britishvirginislands"] <- "VBG"
#landl$alpha.3[landl$country=="republicofmoldova"] <- "MDA"
#landl$alpha.3[landl$country=="theformeryugoslavrepublic"] <- "MKD"
#landl <- subset(landl, country!="republic")
#write to flat file
file = "C:/Users/lmcleary/Documents/test data/thesis/gdpa/ISO-3166-Countries-with-Regional-Codes-master/landl.txt"
#write.table(landl, file , sep = ",")
#reload
landl <- read.table(file , sep = ",", header=T)
##################################################################3
########################EXTRACT EXPORTS FROM THE IMF DATASET
#load datasets and subset
#imf <- "C:/Users/lmcleary/Documents/test data/thesis/PRE POST 1950/post1950/imf data 2003/january/a.txt"
#imfkey <- "C:/Users/lmcleary/Documents/test data/thesis/PRE POST 1950/post1950/imf data 2003/january/b.txt"
#imfr <- read.delim(imf, sep=",")
#imfr <- subset(imfr, select = c(1, 2, 19))
#imfkeyr <- read.delim(imfkey, sep=",", header=FALSE)
#imfkey <- subset(imfkeyr, imfkeyr$V5 =="EXPORTS" & imfkeyr$V7=="MILLIONS OF US$", select=c(1, 5:7))
#rename
#colnames(imfkey)[1] <- "OID"
##leftjoin tables
#imf <- left_join(imfkey, imfr, by="OID")
#drop repeated values
#imf <- unique(imf)
#load imf code to alpha.3/isocode sheet (imf id)
#imfno <- "C:/Users/lmcleary/Documents/test data/thesis/PRE POST 1950/post1950/imf data 2003/january/co.xlsx"
#imfnor <-  read.xlsx(imfno,1, stringsAsFactors = FALSE, header = T, startRow = 2)
#subset
#imfnor <- imfnor[c(1:3)]
#imf$IMF.Code <- substring(imf$OID, 1, 3)
#combine datasets
#imf <- full_join(imf, imfnor, by="IMF.Code")
#find nulls
#imf %>% filter(is.na(IMF.Code))
#rename
#colnames(id)[2] <- "ISO.Code"
#merge with id
#imf <- left_join(imf, id, by = "ISO.Code")
#drop nulls/zeros
#imf <- subset(imf, !is.na(OValue17)& OValue17!=0)
#write to flat file
#file = "C:/Users/lmcleary/Documents/test data/thesis/PRE POST 1950/post1950/imf data 2003/january/c.txt"
#write.table(imf, file , sep = ",")
#load imf flat file 
im <- "C:/Users/lmcleary/Documents/test data/thesis/PRE POST 1950/post1950/imf data 2003/january/c.txt"
imf <- read.delim(im, sep=",")
#subset
imf <- imf[c(1:8)]
#rename columns
colnames(imf)[colnames(imf)=="ISO.Code"] <- "alpha.3"
colnames(imf)[colnames(imf)=="OStartYY"] <- "year"
#query countrys with null alpha.3/isocode
imf %>% distinct(V6, .keep_all=TRUE) %>%
  subset(is.na(alpha.3))
#manually add alpha.3
imf$alpha.3 <- as.character(imf$alpha.3)
imf$alpha.3[imf$IMF.Code==696] <- "REU" 
#merge with id
imf <- left_join(imf, id)
#drop if alpha.3 is null
imf <- subset(imf, !is.na(alpha.3))
#join with maddison data
post <- full_join(imf, mad, by = c("alpha.3", "year"))
#subset
post <- post[-c(1, 2, 4)]
#remove repeated obvservations (usa)
post <- unique(post) 
#rename
colnames(post)[3]="exp"
#subset
post <- subset(post, !is.na(post$alpha.3))
#add region etc from id1
post <- left_join(post, subset(id1, select= c(alpha.3, region)) )
######################################################
#add african pre 1948 maddison gdp data to hist1 
#note that here we only select the gdp to avoid duplicates!!!
hist1$year <- as.integer(hist1$year)
hist1 <- full_join(hist1, (post %>% filter(year<1948, region=="Africa")   %>% 
 select(year, alpha.3, rgdpnapc, cgdppc)))
#remove historical af data from post to avoid duplicates when joining later
post <- anti_join(post, (post %>% filter(year<1948, region=="Africa")   %>% 
                            select(year, alpha.3, rgdpnapc, cgdppc)))

#now lets add the historical data on asia in the same way but using 
#the pop data from maddison
as <- full_join(as, (post %>% filter(year<1948, region=="Asia")   %>% 
                       select(year, alpha.3, rgdpnapc, cgdppc, pop)))
#remove historical af data from post to avoid duplicates when joining later
post <- anti_join(post, (post %>% filter(year<1948, region=="Asia")   %>% 
                           select(year, alpha.3, rgdpnapc, cgdppc, pop)))
#subset - temp drop region
post <- subset(post, select = -region)
#join all 3 datasets
post <- full_join(post, hist1)      
post <- full_join(post, as)      
post <- subset(post, select = -c(1, 4, 6, 7, 11))
#rejoin region from id
post <- left_join(post, (id1 %>% select(country, alpha.3, region , sub.region, intermediate.region))
   , by="alpha.3")
#join landlocked data
post <- left_join(post, subset(landl, select=c(alpha.3, landl)), "alpha.3")
#if landlocked is not equal to 1 set equal to zero
post$landl[is.na(post$landl)] <- "0"
#join uk dataset
post <- left_join(post, uk, by="year")
#population and year as int
post$pop <- as.integer(post$pop)
post$year <- as.integer(post$year)
#generate x (terms of trade variable)
post$x <- post$exp / post$ukexp / (post$pop /1000)
post$logx <- log(post$x)
post$logx2 <- log(post$x) * log(post$x)
#check for duplicates
n_occur  <- data.frame(table(post$alpha.3, post$year))
n_occur[n_occur $Freq > 1,]
post[post$alpha.3 %in% n_occur$Var1[n_occur$Freq > 1],]
#if no duplicates write flat file
#file = "C:/Users/lmcleary/Documents/test data/thesis/gdpa/data/post.txt"
#write.table(post, file , sep = ",")

#post <- read.table(file , sep = ",", header=T)

###################################################
#lets have a look at the data
#test reg
reg <- lm(log(rgdpnapc) ~ I(log(x)) +  I(log(x)^2) , subset(post, year> 1947&  post$region=="Africa"))
summary(reg)

#graph relationship
ggplot(subset(post, year>"1947" &
   post$region=="Africa" ), aes(log(rgdpnapc), log(x), color = log(pop))) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ I(log(x)) +  I(log(x)^2), size = 1) 
#+  geom_text(aes(label=alpha.3,hjust=0, vjust=0))

#summarise table
str(post)
class(post)          # "data.frame"
sapply(post, class)  # show classes of all columns
typeof(post)         # "list"
names(post)          # show list components
dim(post)            # dimensions of object, if any
head(post)           # extract first few (default 6) parts
tail(post, 1)        # extract last row
head(1:10, -1)    # extract everything except the last element


#is gdp correlated with percentage exports? -- yes!
post$percentageexp <- post$exp/post$rgdpnapc
summary(lm(post$rgdpnapc ~post$percentageexp))


#will having a different percentage of exports affect prediction 
#accuracy?
#check export values as percentage of subsistence level gdp
post$subs <- post$pop *300
post$percentageexp <- post$exp/post$subs
#african countries with percentage subs level gdp greater than 10% before 1870
post  %>% filter(region=="Africa" & percentageexp>10 & year <1870 )  %>% distinct(country) 
post %>% filter(post$alpha.3== "GMB"&year<1950) %>% ggplot(aes(year, percentageexp))+geom_col()

