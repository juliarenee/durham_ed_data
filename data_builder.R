library(readxl) # read spreadsheets
library(dplyr) # wrangling
library(tidyr) # reshaping
library(stringr) # string manipulation
library(data.table) # working with tables

# get all older files, .xls format
old.dat = list.files("./data") %>%
  grep("freereduced.xls$", ., value = TRUE)

# read files as table
dat = lapply(paste("./data/",old.dat,sep=""), function(x) read_xls(x))

# clean empty rows
head(dat[[1]])
colnames(dat[[1]]) = c("lea_no","lea_name","school_no","school_name","adm","reduced","free","pct_needy","grade")
dat[[1]] = dat[[1]][2:dim(dat[[1]])[1],]
head(dat[[2]])
colnames(dat[[2]]) = c("lea_no","lea_name","school_no","school_name","adm","reduced","free","pct_needy","grade")
dat[[2]] = dat[[2]][5:dim(dat[[2]])[1],]
head(dat[[3]])
colnames(dat[[3]]) = c("lea_no","lea_name","school_no","school_name","adm","reduced","free","pct_needy","grade")
dat[[3]] = dat[[3]][7:dim(dat[[3]])[1],]
head(dat[[4]])
colnames(dat[[4]]) = c("lea_no","lea_name","school_no","school_name","adm","reduced","free","pct_needy","grade")
dat[[4]] = dat[[4]][7:dim(dat[[4]])[1],]
head(dat[[5]])
colnames(dat[[5]]) = c("lea_no","lea_name","school_no","school_name","cep","adm","free","reduced","pct_needy","pct_needy_mlt")

# get newer files, .xlsx format
new.dat = list.files("./data") %>%
  grep("freereduced.xlsx$", ., value = TRUE)
dat2 = lapply(paste("./data/",new.dat,sep=""), function(x) read_xlsx(x))

# check for cleaning
head(dat2[[1]])
colnames(dat2[[1]]) = c("lea_no","lea_name","school_no","school_name","cep","adm","free","reduced","pct_needy","pct_needy_mlt")
head(dat2[[2]])
colnames(dat2[[2]]) = c("lea_no","lea_name","school_no","school_name","cep","adm","free","reduced","pct_needy")
head(dat2[[3]])
colnames(dat2[[3]]) = c("lea_no","lea_name","school_no","school_name","cep","adm","free","reduced","pct_needy")

# join all data
dat[6:8] = dat2

# transform to data frames
dat = lapply(dat, function(x) data.frame(x))

# remove totals (last row)
dat = lapply(dat, function(x) x[-nrow(x),])

# add id for year
yrs = c(old.dat, new.dat) %>% substr(., 0, 7) # get year to bind as column
for(i in 1:length(dat)) {
  dat[[i]]$year = yrs[i]
}

# merge
df = do.call(plyr::rbind.fill, dat)

# filter to durham county
df = df[grepl("Durham", df$lea_name, ignore.case = TRUE),]

# indicator for CEP data
df$cep = ifelse(is.na(df$cep), 0, 1)

# for reduced < 20, set to 20
df$reduced[grepl("less than", df$reduced, ignore.case = TRUE)] = 20

# recalculate percent needy
df[,c(5:7)] = lapply(df[,c(5:7)],as.numeric)
df$pct_free_reduced = (df$free + df$reduced)/df$adm

# only keep percent needy to avoid conflicting adm
df = df %>% select(c("school_no","school_name","year","pct_free_reduced","adm","cep","grade"))

# load lunch debt data
debt = read.csv("./data/meal_debt.csv")
debt = debt[1:nrow(debt)-1,1:10] # remove totals at bottom, only full year data
colnames(debt)[1:2] = c("school_no", "school_name")
colnames(debt)[3:9] = yrs # reform years
colnames(debt)[10] = "2017-18"

# reshape wide to long
debt = gather(debt, year, unpaid, 3:10)

# convert to integer
debt$unpaid = debt$unpaid %>%
  gsub("\\$","",.) %>%
  gsub("\\,","",.) %>%
  as.numeric()

# convert to factor for joining
df$school_no = as.factor(df$school_no)
debt$school_no = as.factor(debt$school_no)

# join unpaid lunch with free/reduced lunch data
df = left_join(debt, df, by = c("year","school_no"))

# dedupe school name cols
df = df %>% select(-c("school_name.y"))
colnames(df)[which(colnames(df)=="school_name.x")] = "school_name"

# add demographic data
dem = read.csv("./data/nces_demographics.csv")

# remove state, only NC
dem = dem[-c(2)]

# also remove summary rows
spchars = c("†","‡","–")
dem = dem[!grepl(paste(spchars,collapse="|"),dem$School.Name),]

# split to datasets by year
dem11 = dem[c(1, grep("2010\\.11",colnames(dem)))]
dem12 = dem[c(1, grep("2011\\.12",colnames(dem)))]
dem13 = dem[c(1, grep("2012\\.13",colnames(dem)))]
dem14 = dem[c(1, grep("2013\\.14",colnames(dem)))]
dem15 = dem[c(1, grep("2014\\.15",colnames(dem)))]
dem16 = dem[c(1, grep("2015\\.16",colnames(dem)))]

# vectorize
x = list(dem11, dem12, dem13, dem14, dem15, dem16)

# FUNCTION: reformat column names
changeNames = function(z) {
  newNames = colnames(z) %>%
    gsub("\\.+","\\_",.) %>%
    tolower() %>%
    gsub("\\_students.*","",.) %>%
    gsub("\\_public_school.*","",.)
  colnames(z) = newNames
  return(z)
}

# apply to dataframes
x = lapply(x, changeNames)

# add year as variable
for(i in 1:length(x)) {
  x[[i]]$year = yrs[i]
}

# merge to dataframe
dem = do.call(rbind, x)

# set non-numeric values
dem[dem==spchars[1]] = NA
dem[dem==spchars[2]] = NA
dem[dem==spchars[3]] = NA

# all vars as char
dem[,1:14] = lapply(dem[,1:14],as.character)

# set alternative zeros to 0
dem[dem=="=0"] = 0
dem[dem=="=000"] = 0

# filter to durham
dem = dem[grepl("Durham", dem$county_name, ignore.case = TRUE),]

# clean school num
dem$state_school_id[nchar(dem$state_school_id) > 3] = dem$state_school_id[nchar(dem$state_school_id) > 3] %>% substr(., nchar(.)-2, nchar(.))

# vars to numeric
dem[,c(2:10)] = lapply(dem[,c(2:10)],as.numeric)

# calculate percentages
dem$adm = dem$male + dem$female
dem$pct_amin = dem$american_indian_alaska_native/dem$adm
dem$pct_asian = dem$asian_or_asian_pacific_islander/dem$adm
dem$pct_hispanic = dem$hispanic/dem$adm
dem$pct_black = dem$black/dem$adm
dem$pct_haw_pac = dem$hawaiian_nat_pacific_isl/dem$adm
dem$pct_white = dem$white/dem$adm
dem$pct_multi = dem$two_or_more_races/dem$adm

# only keep relevent cols for merging
dem = dem[c(12:14,16:22)]

# change col name
colnames(dem)[which(colnames(dem)=="state_school_id")] = "school_no"

# download durham 16-17, 17-18 demographic data
dem.new = list.files("./data") %>%
  grep("durham_dem", ., value = TRUE)

dem.new = lapply(paste("./data/",dem.new,sep=""), function(x) read_xlsx(x, skip = 2))

# only keep relevent cols
# school code
# and percentages of race
keep = c(2,seq(6,30,4))
dem.new = lapply(dem.new, function(x) x[keep])

# change column names
dem.names = c("school_no",
              "pct_amin",
              "pct_asian",
              "pct_hispanic",
              "pct_black",
              "pct_haw_pac",
              "pct_white",
              "pct_multi")
colnames(dem.new[[1]]) = dem.names
colnames(dem.new[[2]]) = dem.names

# add year
dem.new[[1]]$year = "2016-17"
dem.new[[2]]$year = "2017-18"

# clean school num
dem.new[[1]]$school_no = dem.new[[1]]$school_no %>% substr(., 4, 6)
dem.new[[2]]$school_no = dem.new[[2]]$school_no %>% substr(., 4, 6)

# merge
dem = plyr::rbind.fill(dem, data.frame(dem.new[[1]]), data.frame(dem.new[[2]]))

# merge with df
df = left_join(df, dem, by=c("school_no","year"))

# normalize debt by adm
df$debt_per_student = df$unpaid/df$adm

# standardize locales
df$urban_centric_locale[grepl("City", df$urban_centric_locale)] = "City"
df$urban_centric_locale[grepl("Suburb", df$urban_centric_locale)] = "Suburb"
df$urban_centric_locale[grepl("Rural", df$urban_centric_locale)] = "Rural"

# fill missing years for locale
locales = df %>%
  filter(!is.na(urban_centric_locale)) %>%
  group_by(school_no, urban_centric_locale) %>%
  summarise(count = n()) %>%
  distinct()

# some schools have multiple classifications
# choose the classification with more years
mult.locales = which(table(locales$school_no) > 1) %>% names() # schools that have more than 1 locale in data
for(i in 1:length(mult.locales)) {
  ix = which(locales$school_no == mult.locales[i]) # indeces
  m = min(locales$count[ix[1]], locales$count[ix[2]]) # minimums
  locales = locales[-ix[m],] # remove
}

# locales as list
locales.list = locales$urban_centric_locale
names(locales.list) = locales$school_no

# impute locales
df$urban_centric_locale = locales.list[df$school_no]

# many schools started offering Pre-K
# ignore this for now since only interested in elementary, middle and high schools
# replace Pre-K with K for consistency
df$grade = df$grade %>%
  trimws(.) %>% # trim trailing white space
  gsub("Pre-K","K",.) %>% # replace Pre-K
  gsub("[[:blank:]]+\\-[[:blank:]]+","\\-",.) %>% # remove space between dashes
  gsub("[[:blank:]]+","-",.) %>% # add dash for consistency
  gsub("0","",.) # remove 0 before single digit grades

df$grade[df$grade=="K--6"] = "K-6"

# see schools with more than 1 grade entry
gradeBySchool = df %>%
  filter(!is.na(grade)) %>%
  select(school_no, grade) %>%
  distinct() %>%
  group_by(school_no) %>%
  mutate(occ = n()) %>%
  arrange(school_no)

# only 342 poses a problem, since it seems that they have migrated from elementary to middle school (or vice versa) at some point in time... leave out of analysis for now

# list of unique grades
grade.key = unique(df$grade) %>% sort(decreasing = TRUE)
names(grade.key) = grade.key
grade.key[1:2] = "Elementary"
grade.key[c(3:4,8)] = "High"
grade.key[c(5:6)] = "Middle"
grade.key[7] = "Middle and High"

# add schcool level based on clean grades
gradeBySchool$grade_clean = grade.key[gradeBySchool$grade]

# 342 is Lakewood Middle
# ignore weird grades in data, clearly a middle school
gradeBySchool$grade_clean[gradeBySchool$school_no == "342"] = "Middle"

# only unique vals
gradeBySchool = gradeBySchool %>% select(school_no, grade_clean) %>% unique()

# as list for easy replacement
grade_clean = gradeBySchool$grade_clean
names(grade_clean) = gradeBySchool$school_no

# merge with data
df$grade_clean = grade_clean[df$school_no]

# remove DPS hospital school since non-standard school
df = df[-which(df$school_name=="DPS Hospital School"),]

# remove 389, 700 since no longer active
# remove 353, housed in Durham Tech
df = df[-which(df$school_no %in% c(389, 700, 353)),]

# grab schools with CEP status at any point in time
cep.schools = df %>%
  group_by(school_no) %>%
  summarise(years_cep = sum(cep, na.rm = TRUE)) %>%
  filter(years_cep > 0)

# indicator for whether school has had CEP at any point in time
# as of current, CEP var is on a yearly basis
df$cep_long = 0
df$cep_long[df$school_no %in% cep.schools$school_no] = 1

# longitudinal as factor
df$cep_long = as.factor(df$cep_long)

# CEP status as factor
df$cep = as.factor(df$cep)

# free/reduced for CEP schools to 1 when > 1
df$pct_free_reduced[df$pct_free_reduced > 1] = 1

# remove data where no CEP/adm data
df = df[!is.na(df$adm),]

# add percent minority
df$pct_minority = df$pct_black + df$pct_hispanic

# year as numeric
df$ts = df$year %>% substr(., 0, 4) %>% as.numeric()

# add 2017-18 school eligibility data
elig = read.csv("./data/2018_cep_annual_notif.csv", stringsAsFactors = FALSE)

# clean col names
colnames(elig) = colnames(elig) %>%
  tolower(.) %>%
  gsub("\\.+","_",.)

# filter to durham
elig = elig[elig$lea_id==320,]

# select cols
elig = elig[,c("school_id","eligible_to_participate","near_eligible_to_participate")]

# rename col
colnames(elig) = c("school_no","elig","near_elig")

# data type for merging
elig$school_no = elig$school_no %>% as.numeric()

# indicators
elig[,2] = ifelse(elig[,2]=="X",1,0)
elig[,3] = ifelse(elig[,3]=="X",1,0)
elig[,4] = ifelse(elig[,4]=="X",1,0)

# add year for merging
elig$year = "2017-18"

# merge eligibility data from 2017-18 with dataset
df = left_join(df, elig, by = c("year", "school_no"))

write.csv(df, "./data/lunchdebt.csv", row.names = FALSE)

