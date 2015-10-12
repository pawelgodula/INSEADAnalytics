#################################################### 
### INITIAL ANALYSIS
####################################################

### 0. Clearing the workspace
rm(list = ls())

### 1. Loading and cleaning the data 

rData = read.csv2(
  "C:/Users/PawelG/Desktop/MBA/E hotel/tabela z danymi/R data/new_2015.10.03 14.55.06 - Wykaz rezerwacji.csv", 
  sep = ",", header = TRUE)

newnames = c("Idx", "IdKey", "Payer", "Type", "RoomNr", "StayPeriod", "Check", "Allotment", 
             "OrderDate", "IdPayer", "IdClient" , "IdBiuro" ,"Client", "Biuro", "StartDate", 
             "ExpValue", "ExpCount", "RealValue", "RealCount", "Redundant")
colnames(rData) = newnames
rownames(rData) = rData[,"Idx"]

rData = rData[, colnames(rData) != "Allotment" & colnames(rData) != "Redundant" 
              & colnames(rData) != "IdBiuro"  & colnames(rData) != "Biuro"
              & colnames(rData) != "StartDate"]
SP = rData[,"StayPeriod"]
SPstart = substr(SP, 1, 10)
SPend = substr(SP, 14, 23)

rData = cbind(cbind(rData,SPstart), SPend)

#write.csv2(rData, file = "C:/Users/PawelG/Desktop/MBA/E hotel/tabela z danymi/R data/rDataTest.csv")


### 2. Calculating price per apartment
ppNight = as.numeric(as.vector(rData[,"RealValue"])) / rData[,"RealCount"]
rData = cbind(rData, ppNight)
rData = rData[ppNight != "NaN",]
rData = rData[rData[,"ppNight"] != 0,] 

rTypes = sort(as.vector(unique(rData[,"Type"])))
rownames(rData) = c(1:nrow(rData))

### 3. Summarizing revenue per client and plotting it 
# Do this step to a/ get familiar with the data b/ see whether segmentation makes sense


Clients = rData[,colnames(rData) == "Client"]
Clients = as.matrix(unique(Clients))
Clients = cbind(Clients, c(1:nrow(Clients)))
rownames(Clients) = Clients[,1]
RevClients = as.numeric(as.vector(rData[, "RealValue"]))
RevClients = as.data.frame(cbind(RevClients, as.vector(rData[, "Client"])))
b = 0
c = as.numeric(as.vector(RevClients$RevClients))
for (i in rownames(Clients))
{ b = 0
  for (j in 1:nrow(RevClients))
  {
    if ( RevClients[j,2] == i)
    {
      b = b + as.numeric(c[j]) 
    }
  }
Clients[i,2] = b
}

Clients = as.data.frame(Clients)
Clients = cbind(Clients,as.numeric(as.vector(Clients$V2)))
Clients = Clients[,colnames(Clients) != "V2"]
colnames(Clients) = c("V1" , "V2")

Clients = Clients[order(as.numeric(as.vector(Clients$V2))),]
RevenuePerClient = as.numeric(as.vector(Clients$V2))
maxBreak = max(RevenuePerClient) + 10000 - max(RevenuePerClient) %% 10000
Chart_RevenuePerClient = hist(RevenuePerClient, labels = TRUE, col = "yellow", breaks = seq(0,maxBreak,1000))

### 4. Calculating and plotting capacity utilization for rooms
Rooms = sort(unique(rData[,"Type"]))
number_of_rooms = nlevels(Rooms)
analysis_start = as.Date("2014-07-01")
analysis_end = as.Date("2015-09-30")
analysis_period = analysis_end - analysis_start + 1
Callendar = matrix(0, nrow = number_of_rooms, ncol = as.numeric(analysis_period))
Callendar = as.data.frame(Callendar)
colnames(Callendar) = as.Date(c(analysis_start:analysis_end), origin = "1970-01-01")
rownames(Callendar) = Rooms
RStayPeriods =  subset(rData, select = c(Type, SPstart, SPend))

n=1

for (n in c(1:nrow(RStayPeriods)))
{
  cur_room = as.character(RStayPeriods[n,1])
  cur_spstart = as.Date(RStayPeriods[n,2])
  cur_spend = as.Date(RStayPeriods[n,3]) - 1
  m=cur_spstart
  for (m in c(cur_spstart:cur_spend))
  {
    Callendar[cur_room,as.character(as.Date(m, origin = "1970-01-01"))] = 1
  }

}
total = colSums(Callendar)
Chart_CapacityUtilization = barplot(total/15*100, main = "Room Fill Rate (%)")
Callendar = rbind(Callendar, total)

### 5. Calculating and plotting Revenue in Peak Season and Off-season 

Start_OFF = as.Date("2014-10-01")
Start_OFF2 = as.Date("2015-10-01")
Start_PEAK = as.Date("2015-06-01")

Season = (as.Date(rData$SPstart) > Start_OFF & as.Date(rData$SPstart) < Start_PEAK) | as.Date(rData$SPstart) > Start_OFF2
Season = replace(Season, Season == TRUE, "OFF")
Season = replace(Season, Season == FALSE, "PEAK")

rData = cbind(rData, Season)

Seasons = unique(rData$Season)

RevenuePerSeason = lapply(Seasons, function(cl)
  {
res = rData[rData$Season == cl & as.Date(rData$SPstart) > as.Date("2014-10-01"), ]
res = sum(as.numeric(as.vector(res[, "RealValue"])))
return(res)
  })
names(RevenuePerSeason) = Seasons

RevenuePerSeason = matrix(unlist(RevenuePerSeason))
rownames(RevenuePerSeason) = Seasons
RevenuePerSeason = cbind(RevenuePerSeason, round(prop.table(RevenuePerSeason)*100, digits = 2))


Chart_RevenuePerSeason = barplot(t(RevenuePerSeason[, 2]), col = "GREEN", 
                                 main = "Percentage of Revenue In Peak vs. Off-Season (%)", 
                                 xpd = TRUE)

text(Chart_RevenuePerSeason, 5, t(RevenuePerSeason[, 2]), cex=1, pos=3)

###6. Calculating differences between peak season and offseason: part a - days to stay

advance = as.numeric(as.Date(as.vector(rData[,"SPstart"])) - as.Date(as.vector(rData[,"OrderDate"])))
rData = cbind(rData, advance)

AdvancePerSeason = lapply(Seasons, function(cl)
{
  res = rData[rData$Season == cl, ]
  res = res[, "advance"]
  return(res)
})
names(AdvancePerSeason) = Seasons

maxbreak_OFF = max(unlist(AdvancePerSeason["OFF"]))  + 10
maxbreak_PEAK = max(unlist(AdvancePerSeason["PEAK"])) + 10

Chart_AdvanceOFF = hist(unlist(AdvancePerSeason["OFF"]), 
     main = "Offseason: Median guest books stay 20 days in advance", 
     xlab = "Days from date of booking to date start of stay", ylim = c(0,0.1), freq = FALSE,
     col = "yellow", breaks = seq(0,maxbreak_OFF,10))
Chart_AdvancePEAK = hist(unlist(AdvancePerSeason["PEAK"]), 
     main = "PeakSeason: Median guest books stay 12 days in advance", 
     xlab = "Days from date of booking to date start of stay", ylim = c(0,0.1), freq = FALSE,
     col = "yellow", breaks = seq(0,maxbreak_PEAK,10))

###7. Calculating differences between peak season and offseason: part b - days to stay

StayperiodPerSeason = lapply(Seasons, function(cl)
{
  res = rData[rData$Season == cl, ]
  res = res[, "RealCount"]
  return(res)
})
names(StayperiodPerSeason) = Seasons

maxbreak_OFF = max(unlist(StayperiodPerSeason["OFF"]))  + 1
maxbreak_PEAK = max(unlist(StayperiodPerSeason["PEAK"])) 

Chart_StayOFF = hist(unlist(StayperiodPerSeason["OFF"]), 
     main = "Offseason: Average guest stays for 2.4 days", 
     xlab = "Lenght of stay period (days)", ylim = c(0,1), freq = FALSE,
     labels = TRUE, col = "yellow", breaks = seq(0,maxbreak_OFF,1))

Chart_StayPEAK = hist(unlist(StayperiodPerSeason["PEAK"]), 
     main = "Peakseason: Average guest stays for 2.9 days", 
     xlab = "Lenght of stay period (days)", ylim = c(0,1), freq = FALSE,
     labels = TRUE, col = "yellow", breaks = seq(0,maxbreak_PEAK,1)) 

###7. Calculating differences between peak season and offseason: part c - weekend vs weekday

CallendarSeason = (as.Date(colnames(Callendar)) > Start_OFF & as.Date(colnames(Callendar)) < Start_PEAK ) | as.Date(colnames(Callendar)) > Start_OFF2
CallendarSeason = replace(CallendarSeason, CallendarSeason == TRUE, "OFF")
CallendarSeason = replace(CallendarSeason, CallendarSeason == FALSE, "PEAK")
Callendar = rbind(Callendar, CallendarSeason)

Weekend = (weekdays(as.Date(colnames(Callendar))) == "Saturday") | 
  (weekdays(as.Date(colnames(Callendar))) == "Sunday")
Weekend = replace(Weekend, Weekend == TRUE, "Weekend")
Weekend = replace(Weekend, Weekend == FALSE, "Working")
Callendar = rbind(Callendar, Weekend)

NightsPerWeekend = lapply(Seasons, function(cl)
{
  res_weekend = Callendar[, as.character(Callendar[18,]) == cl 
                          & as.character(Callendar[19,]) == "Weekend"]
  res_weekend = sum(as.numeric(as.vector(res_weekend[17,])), na.rm = TRUE)
  return(res_weekend)
  })
names(NightsPerWeekend) = Seasons

NightsPerWorking = lapply(Seasons, function(cl)
{
    res_working = Callendar[, as.character(Callendar[18,]) == cl 
                          & as.character(Callendar[19,]) == "Working"]
  res_working = sum(as.numeric(as.vector(res_working[17,])), na.rm = TRUE)
  return(res_working)
})
names(NightsPerWorking) = Seasons

NightsPerWeekday = cbind(unlist(NightsPerWeekend), unlist(NightsPerWorking))
colnames(NightsPerWeekday) = c("Weekend" , "WorkingDay")
NightsPerWeekday = t(NightsPerWeekday)
Chart_NightsPerWeekday = barplot(NightsPerWeekday, legend.text = TRUE, 
                                 main = "Number of rooms sold split into Weekends vs Working Day ")

text(Chart_NightsPerWeekday, NightsPerWeekday[1, ]  - 100, labels = NightsPerWeekday[1, ], cex = 1)
text(Chart_NightsPerWeekday, colSums(NightsPerWeekday) - 100, labels = NightsPerWeekday[2, ], cex = 1)

#write.csv2(Callendar, file = "C:/Users/PawelG/Desktop/MBA/E hotel/tabela z danymi/R data/Callendar.csv")


# Order of rooms: 10, 11, 12, 13, 14, 
#                 15, 1, 2, 3, 4, 
#                 5, 6, 7, 8, 9, 
#                 CAR
#Start_dates = as.matrix(as.character(as.Date(c("2014-07-01", "2015-04-30", "2014-07-01", "2014-07-01", "2014-07-01", 
#                "2015-04-30", "2014-01-07", "2014-01-07", "2014-01-07", "2014-01-07", 
#                "2014-10-17", "2014-10-17", "2014-08-01", "2014-07-01", "2014-07-01", 
#                "2015-04-30"))))
#rownames(Start_dates) = Rooms
#write.csv2(Callendar, file = "C:/Users/PawelG/Desktop/MBA/E hotel/tabela z danymi/R data/Callendar.csv")

#Open_status = as.data.frame(matrix(0, nrow = nrow(Callendar), ncol = ncol(Callendar)))
#rownames(Open_status) = Rooms     











