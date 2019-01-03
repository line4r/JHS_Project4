library(dplyr)
library(ggplot2)

if(!exists("NEI")){
    NEI = readRDS("summarySCC_PM25.rds")
}
if(!exists("SCC")){
    SCC = readRDS("Source_Classification_Code.rds")
}

Q1 = NEI %>% group_by(year) %>% summarize(total_Emissions = sum(Emissions,na.rm=TRUE))
png("plot1.png")
with(Q1,{
         barplot(total_Emissions,
         names.arg = unlist(Q1[1:4,1]),
         xlab = "year",
         ylab = "total Emissions",
         main = expression("Total Emissions PM" [2.5]* " at several years")
        )
})
dev.off()       # Q1




Q2 = NEI %>% filter(fips=="24510") %>% group_by(year) %>% 
    summarize(total_Emissions = sum(Emissions, na.rm=TRUE))
png("plot2.png")
with(Q2,{
         barplot(total_Emissions,
         names.arg = unlist(Q2[1:4,1]),
         ylab = "total Emissions",
         main = expression("Total Emissions PM" [2.5]* " in Baltimore City")
         )
})      
dev.off()       # Q2




Q3 = NEI %>% filter(fips=="24510") %>% group_by(type,year) %>%
    summarize(total_Emissions = sum(Emissions,na.rm=TRUE))
png("plot3.png", width=640, height=480)
gg <- ggplot(Q3, aes(year,total_Emissions,color=type))
gg <- gg + geom_line() +
    xlab("year") +
    ylab(expression("Total Emissions PM" [2.5]*"")) +
    ggtitle("Total Emissions in Baltimore City by several types")
print(gg)
dev.off()       # Q3




Q4.list = grepl("coal",SCC$Short.Name,ignore.case=TRUE) & grepl("comb",SCC$SCC.Level.One,ignore.case=TRUE)
Q4.list.SCC = SCC[Q4.list,1]
Q4 = NEI %>% filter(SCC %in% Q4.list.SCC) %>%  group_by(year) %>% 
    summarize(total_Emissions = sum(Emissions,na.rm=TRUE))
png("plot4.png")
gg <- ggplot(Q4, aes(as.factor(year),total_Emissions))
gg <- gg + geom_bar(stat="identity") +
    xlab("year") +
    ylab(expression("total PM"[2.5]* "Emissions")) +
    ggtitle("Emissions changing from combustion-related sources")
print(gg)
dev.off()       # Q4



Q5.list = SCC[grepl("Motor",SCC$SCC.Level.Three,ignore.case=TRUE),1]
Q5 = NEI %>% group_by(year) %>% filter(fips=="24510" & SCC %in% Q5.list) %>% 
    summarize(total_Emissions = sum(Emissions,na.rm=TRUE))
png("plot5.png")
gg <- ggplot(Q5, aes(as.factor(year),total_Emissions))
gg <- gg + geom_bar(stat="identity") +
    xlab("year") +
    ylab(expression("total PM"[2.5]* "Emissions")) +
    ggtitle("Emissions changing from motor vehicle sources")
print(gg)
dev.off()       # Q5




Q6.list = SCC[grepl("Motor",SCC$SCC.Level.Three,ignore.case=TRUE),1]
Q6.Balti = NEI %>% group_by(year) %>% filter(fips=="24510" & SCC %in% Q6.list) %>% 
    summarize(total_Emissions = sum(Emissions,na.rm=TRUE))
Q6.Balti$city = rep("Baltimore",nrow(Q6.Balti))
Q6.LA = NEI %>% group_by(year) %>% filter(fips=="06037" & SCC %in% Q6.list) %>% 
    summarize(total_Emissions = sum(Emissions,na.rm=TRUE))
Q6.LA$city = rep("LA",nrow(Q6.LA))
Q6 = Q6.Balti %>% rbind(Q6.LA)
png("plot6.png")
gg <- ggplot(Q6, aes(as.factor(year),total_Emissions,fill=as.factor(year)))
gg <- gg +
    geom_bar(stat="identity") +
    facet_grid(. ~ city) +
    xlab("year") +
    ylab(expression("total PM"[2.5]* "Emissions")) +
    ggtitle("Comparison of emissions") +
    labs(fill="year")
print(gg)
dev.off()       # Q6
