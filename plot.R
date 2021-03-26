NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

library(dplyr)
library(ggplot2)
#total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.
total<-NEI%>%group_by(year)%>%summarise(Emissions=sum(Emissions))
barplot(total$Emissions, names = total$year , 
        xlab = "Years", ylab = "Emissions",
        main = "Emissions over the Years")
dev.copy(png, file="plot1.png", height=480, width=480)
dev.off()

#Baltimore total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.
Baltimoretotal<-NEI%>%
  filter(fips=="24510")%>%
  group_by(year)%>%
  summarise(Emissions=sum(Emissions))
barplot(Baltimoretotal$Emissions, names = total$year , 
        xlab = "Years", ylab = "Emissions",
        main = "Baltimoretotal Emissions over the Years")
dev.copy(png, file="plot2.png", height=480, width=480)
dev.off()

#Baltimore 4 types of source emissions from 1999–2008
Baltimore<-NEI%>%
  filter(fips=="24510")%>%
  group_by(type,year)%>%
  summarise(Emissions=sum(Emissions))
ggplot(Baltimore,aes(factor(year),Emissions,fill=type)) +
  geom_bar(stat="identity")+
  facet_grid(.~type,scales = "free",space="free") + 
  labs(x="year", y=expression("Total PM"[2.5]*" Emission (Tons)")) + 
  labs(title=expression("PM"[2.5]*" Emissions, Baltimore City 1999-2008 by Source Type"))
dev.copy(png, file="plot3.png", height=480, width=480)
dev.off()

#fitler coal combustion-related sources 
coal_bus_scc<-filter(SCC,grepl("Combustion",SCC.Level.One),grepl("Coal",SCC.Level.Four), ignore.case=TRUE)
#sum emissions
coal_bus_emission<-NEI%>%
  filter(SCC%in%coal_bus_scc$SCC)%>%
  group_by(year)%>%
  summarise(Emissions=sum(Emissions))
barplot(coal_bus_emission$Emissions, names = coal_bus_emission$year , 
        xlab = "Years", ylab = "Emissions",
        main = "Coal Combustionl Emissions over the Years")
dev.copy(png, file="plot4.png", height=480, width=480)
dev.off()

#filter Vehicle in Baltimore
vehicle_scc<-filter(SCC,grepl("Vehicle",SCC.Level.Two))
vehicle_baltimore_emissions<-NEI%>%
  filter(SCC%in%vehicle_scc$SCC,fips=="24510") %>%
  group_by(year)%>%
  summarise(Emissions=sum(Emissions))
barplot(vehicle_baltimore_emissions$Emissions, names = vehicle_baltimore_emissions$year , 
        xlab = "Years", ylab = "Emissions",
        main = "Vehicle Emissions in Baltimore over the Years")
dev.copy(png, file="plot5.png", height=480, width=480)
dev.off()

vehicle_emissions<-NEI%>%
  filter(SCC%in%vehicle_scc$SCC,grepl("24510|06037",fips)) %>%
  group_by(year,fips)%>%
  summarise(Emissions=sum(Emissions))
ggplot2::ggplot(vehicle_emissions,aes(factor(year),Emissions,fill=fips)) +
  geom_bar(stat="identity",position = "dodge")+
  facet_grid(.~fips,scales = "free",space="free") +
  labs(x="year", y=expression("Total PM"[2.5]*" Emission (Tons)")) + 
  labs(title=expression("PM"[2.5]*" Emissions, Baltimore and LA 1999-2008"))+
  scale_fill_discrete(labels=c("Baltimore","LA"))
dev.copy(png, file="plot6.png", height=480, width=480)
dev.off()
