data = readRDS("C:/Users/KPainter/Desktop/coursera_misc/summarySCC_PM25.rds")
datasource = readRDS("C:/Users/KPainter/Desktop/coursera_misc/Source_Classification_Code.rds")

balt = data[data$fips=="24510",]
lac = data[data$fips=="06037",]


# 1. Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
# Using the base plotting system, make a plot showing the total PM2.5 emission from 
# all sources for each of the years 1999, 2002, 2005, and 2008.
us_year = aggregate(data$Emissions, list(year = data$year), sum)
#tapply(data$Emissions, data$year, sum)
#plot(us_year, type = 'l')
barplot(us_year$x, us_year$year, xlab="Year", ylab = "Emissions", col=c("red", "green", "brown", "yellow"), 
        main="Total United States Emissions", axisnames=TRUE, names.arg = us_year$year)



# 2. Have total emissions from PM2.5 decreased in the Baltimore City, Maryland 
# (fips == "24510") from 1999 to 2008? Use the base plotting system to make a plot 
# answering this question.
balt_year = aggregate(balt$Emissions, list(year = balt$year), sum)
#tapply(balt$Emissions, balt$year, sum)
#plot(balt_year, type = 'l')
barplot(balt_year$x, balt_year$year, xlab="Year", ylab = "Emissions", col=c("red", "green", "brown", "yellow"), 
        main="Baltimore Emissions", axisnames=TRUE, names.arg = balt_year$year)

# 3. Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) 
# variable, which of these four sources have seen decreases in emissions from 1999–2008 
# for Baltimore City? Which have seen increases in emissions from 1999–2008? Use the 
# ggplot2 plotting system to make a plot answer this question.
balt_point_year = aggregate(balt$Emissions, list(year = balt$year, type = balt$type), sum)
ggplot(balt_point_year) + geom_line(aes(x = year, y = x, colour=type))

# 4. Across the United States, how have emissions from coal combustion-related sources 
# changed from 1999–2008?
datasource_coal = 
datasource$SCC.Level.Three:  Motor Vehicles: SIC 371
coal_combustion <- datasource[grepl("coal", datasource$Short.Name, ignore.case=TRUE) &
                 grepl("comb", datasource$EI.Sector, ignore.case=TRUE)  , ]
us_coal = join(x = coal_combustion, y = data, by = "SCC")
us_coal_year = aggregate(us_coal$Emissions, list(year = us_coal$year), sum)
plot(us_coal_year, type = 'l')

# 5. How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
datasource_onroad = datasource[datasource$Data.Category == "Onroad",]
balt_mv = join(x = datasource_onroad, y = balt, by="SCC")
balt_mv_year = aggregate(balt_mv$Emissions, list(year = balt_mv$year), sum)
plot(balt_mv_year, type = 'l')

# 6. Compare emissions from motor vehicle sources in Baltimore City with emissions from
# motor vehicle sources in Los Angeles County, California (fips == "06037"). Which city
# has seen greater changes over time in motor vehicle emissions?
datasource_onroad = datasource[datasource$Data.Category == "Onroad",]
balt_mv = join(x = datasource_onroad, y = balt, by="SCC")
balt_mv_year = aggregate(balt_mv$Emissions, list(year = balt_mv$year), sum)
lac_mv = join(x = datasource_onroad, y = lac, by ="SCC")
lac_mv_year = aggregate(lac_mv$Emissions, list(year = lac_mv$year), sum)
plot(lac_mv_year, type = 'l')
