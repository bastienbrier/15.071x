# 1.1
CPS = read.csv("CPSData.csv")
str(CPS)

# 1.2 - 1.4 - 2.1
summary(CPS)

# 1.3
sort(table(CPS$State))

# 1.5
table(CPS$Race, CPS$Hispanic)

# 2.2
table(CPS$Region, is.na(CPS$Married))
table(CPS$Age, is.na(CPS$Married))
table(CPS$Sex, is.na(CPS$Married))
table(CPS$Citizenship, is.na(CPS$Married))

# 2.3
table(CPS$State, is.na(CPS$MetroAreaCode))

# 2.4
table(CPS$Region, is.na(CPS$MetroAreaCode))

# 2.5
sort(tapply(is.na(CPS$MetroAreaCode), CPS$State, mean))

# 3.1
MetroAreaMap = read.csv("MetroAreaCodes.csv")
CountryMap = read.csv("CountryCodes.csv")
str(MetroAreaMap)
str(CountryMap)

# 3.2
CPS = merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)
summary(CPS)

# 3.3
sort(table(CPS$MetroArea))

# 3.4
sort(tapply(CPS$Hispanic, CPS$MetroArea, mean))

# 3.5
sort(tapply(CPS$Race == "Asian", CPS$MetroArea, mean))

# 3.6
sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean, na.rm = TRUE))

# 4.1
CPS = merge(CPS, CountryMap, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)
summary(CPS)

# 4.2
sort(table(CPS$Country))

# 4.3
NewYork = subset(CPS, CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA", na.rm = TRUE)
mean(NewYork$Country != "United States", na.rm = TRUE)

# 4.4
sort(tapply(CPS$Country == "India", CPS$MetroArea, sum, na.rm = TRUE))
sort(tapply(CPS$Country == "Brazil", CPS$MetroArea, sum, na.rm = TRUE))
sort(tapply(CPS$Country == "Somalia", CPS$MetroArea, sum, na.rm = TRUE))