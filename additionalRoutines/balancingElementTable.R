FinlandUtilizationT=fread("C:/Users/ROSA/Desktop/Fisheries/batch0_SUA_pilotCounties/utilizationTables/FinlandUtTable.CSV", header = TRUE)

FinlandUtilizationT=melt( FinlandUtilizationT,
           id.vars = colnames(FinlandUtilizationT)[c(1:3)],
           measure.vars = colnames(FinlandUtilizationT)[c(4:37)],
           variable.name = "timePointYears",
           value.name = "Value"
)

#Obtaine a country-specific balancingItem table
FinlandUtilizationT=FinlandUtilizationT[grepl("flag", timePointYears) & Value=="B",]
FinlandUtilizationT[,timePointYears:=gsub("_flag", "", timePointYears) ]


write.csv(FinlandUtilizationT,"C:/Users/ROSA/Desktop/Fisheries/batch0_SUA_pilotCounties/utilizationTables/Finland_balancingItems.csv", row.names = FALSE)

#Obtaine a country-specific F table
FinlandUtilization=fread("C:/Users/ROSA/Desktop/Fisheries/batch0_SUA_pilotCounties/utilizationTables/FinlandUtTable.CSV", header = TRUE)
FinlandUtilization=melt( FinlandUtilization,
                        id.vars = colnames(FinlandUtilization)[c(1:3)],
                        measure.vars = colnames(FinlandUtilization)[c(4:37)],
                        variable.name = "timePointYears",
                        value.name = "Value"
)

FinlandUtilizationF=FinlandUtilization[grepl("flag", timePointYears) & Value=="F",]
FinlandUtilizationF[,timePointYears:=gsub("_flag", "", timePointYears) ]
filter=unique(FinlandUtilizationF[,.(geographicAreaM49_fi,  ics, measuredElement ,timePointYears)])

protected=FinlandUtilization[filter,,on=c("geographicAreaM49_fi" , "ics", "measuredElement", "timePointYears")]


write.csv(protected,"C:/Users/ROSA/Desktop/Fisheries/batch0_SUA_pilotCounties/utilizationTables/protected/Finland_protected.csv", row.names = FALSE)

