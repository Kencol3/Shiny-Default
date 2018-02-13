#2014 update building on AEJ Macro 2013 paper "Sovereign Defaults: The Price of Haircuts", by Juan Cruces and Christoph Trebesch
#The table below provides a list of 187 distressed sovereign debt restructurings with external private creditors (banks and bondholders) occuring between 1970 and 2013. 
#We also provide haircut estimates and further information on each deal.
#Data coverage: worldwide, 1970-2013
#This update: August 20, 2014

getwd()
setwd('~/Desktop/Shiny-Default')
library(dplyr)
library(shiny)
library(data.table)
library(wbstats)
library(tidyr)
library(googleVis)
library(ggplot2)


countries <- data.table(wbcountries())
ISO <- countries[,0:3]
RegInc <- countries[,c('iso2c','region','income','lending')]
View(countries)


Default <- fread('CTDefault.csv', stringsAsFactors = FALSE)
Default <- as.data.frame(Default)
Default <- merge(Default, ISO, by.x = 'Code', by.y = 'iso3c')
Default <- Default[,c(2,20,21,4:19,3,1)]
Default <- merge(Default, RegInc, by = 'iso2c')
Default[Default == 'none (buy back)'] <- NA
Default <- separate(Default, Date, into = c('Month', 'Year'), sep = '-')
Default <- Default %>% mutate(Year = ifelse(Year <70, 
                                            paste('20', Year, sep=''), 
                                            paste('19', Year, sep='')))
Default <- unite(Default, 'Date', Month, Year, sep = '-01-')
Default$Date <- as.Date(Default$Date, '%b-%d-%Y')
class(Default$Date)
Default$`Preferred Haircut HSZ` <- as.numeric(sub('%','',Default$`Preferred Haircut HSZ`))  
Default$`Underlying Discount Rate` <- as.numeric(sub('%','',Default$`Underlying Discount Rate`))
Default$`Market Haircut HM` <- as.numeric(sub('%','',Default$`Market Haircut HM`))
Default$`Face Value Reduction (in %)` <- as.numeric(sub('%','',Default$`Face Value Reduction (in %)`))
Default$`Debt Restructured (USDmn)` <- as.numeric(sub(',','',Default$`Debt Restructured (USDmn)`))
Default$`Debt Restructured (USDmn)` <- Default$`Debt Restructured (USDmn)`/1000

g <- ggplot(Default, aes(Date, `Preferred Haircut HSZ`)) + 
      labs(title = 'Sovereign Defaults', subtitle = '1970 to 2014') +
      geom_jitter(aes(col=region, size = `Debt Restructured (USDmn)`))
plot(g)

#Use plotly, we want to be able to select/deselect regions, 
    #when we hover over a point we show the isocode, amount restructured, haircut, discount rate
    #we want to use a time range, to look at the graph at different intervals








class(Default$`Preferred Haircut HSZ`)
View(Default)


Default$`Preferred Haircut HSZ` = as.numeric(Default$`Preferred Haircut HSZ`)

Annote <- Default[,c('Date','iso2c', 'Debt Restructured (USDmn)', 'Preferred Haircut HSZ')]
View(Annote)
View(Fruits)

Bubble <- gvisBubbleChart(Default, idvar="iso2c", xvar="Date", 
                          yvar="Preferred Haircut HSZ", colorvar="region", 
                          sizevar="Debt Restructured (USDmn)") 
                          #options=list(hAxis='{minValue:75, maxValue:125}'))

plot(Bubble)
Default$Date <- as.Date(Default$Date)

View(Stock)
Anno <- gvisAnnotationChart(Stock, 
                            +                             datevar="Date",
                            +                             numvar="Value", 
                            +                             idvar="Device",
                            +                             titlevar="Title", 
                            +                             annotationvar="Annotation",
                            +                             options=list(
                              +                               width=600, height=350,
                              +                               fill=10, displayExactValues=TRUE,
                              +                               colors="['#0000ff','#00ff00']")
                            + )

> plot(Anno)
demo(googleVis)
dim(Default)

View(Default)

names(Default)

names(Default)

Paris <- fread('ParisClub.csv', stringsAsFactors = FALSE)
Paris <- as.data.frame(Paris)

names(Paris)

View(Paris)

#create a timeline with a default episode at each point
#how much was the haircut
#Check boxes to show or not show country, or regional defaults

#2) SZ haircut, where there is a comparison between the PV of new debt and the PV of old debt both using same discoun trate
#4) Market haircut, takes the new debt as present value vs the old debt at face value 


#1)The amount of debt restructured
#3)The discount rate used to value future cash flows
#6)Does the restructuring involve bond-debt only
#7) If the deal implies a reduction in face value of outstanding debt
#8) If the deal is a buy-back
#9) If the restructuring is a Brady deal
#10) If the deal is donor funded or supported by bilateral or multilateral money,
#11) If all the old debt being restructured has fallen due
#12) If the exchange inludes previously resturctured debt (PRD)
#13) If the agreement includes the provision of new money or concerted lending
#14) If the agreement also affects short-term debt, e.g. trade credits
#15) The Data Quality Index, reflecting the scope of information available

#1. We focus on sovereign debt restructurings, defined as restructurings of public or publicly guaranteed debt. 
    #Restructurings of private-to-private debt are not taken into account even when large-scale workouts 
    #of private sector debt were coordinated by governments, such as in Korea 1997 or Indonesia 1998. 

#2. We include restructurings with foreign private creditors only, thus excluding debt restructurings 
    #that predominantly affected domestic creditors and those affecting official creditors, including 
    #those negotiated under the chairmanship of the Paris Club. Foreign creditors include foreign 
    #commercial banks (i.e. “London Club”  creditors) as well as foreign bondholders. 
    #For recent deals, we follow the categorization into domestic and external debt exchanges of 
    #Sturzenegger and Zettelmeyer (2006, p. 263). We therefore explicitly include two domestic debt 
    #restructurings but only because they mainly involved external creditors: 
    #Russia’s July 1998 GKO exchange and Ukraine’s August 1998 exchange of OVDP bonds. 

#3. We focus on distressed debt exchanges, defined as restructurings of bonds (bank loans) at less 
    #favorable terms than the original bond (loan). We thereby follow the definition and data 
    #provided by Standard & Poor’s (2006 2011). Restructurings that are part of routine sovereign 
    #liability management such as debt swaps and buy backs in normal times are disregarded. 

#4. We restrict the sample to medium and long-term debt restructurings only. We thus disregard 
    #short-term agreements, such as 90-day debt rollovers or the maintenance of short-term 
    #credit lines (e.g. trade credit). We also exclude agreements with maturity extension of less 
    #than a year. We do include, however, cases in which short-term debt is exchanged into debt 
    #with a maturity of more than one year.

#5. We only regard restructurings that are actually implemented, thus ignoring cases in which negotiations
    #where never concluded or in which an agreement in principle or an exchange offer were never finalized.

#A list of cases that were not included can be found in the Online Appendix of the paper


