files=list.files(path = "C:/Users/ROSA/Favorites/Github/sws_project/faoswsFisheryStandardization/data/localRun/SUA_standard", pattern = NULL, all.files = FALSE,
                 full.names = FALSE, recursive = FALSE,
                 ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

for(i in seq_along(files)){

  
currentFile=files[i]



utilizationT=fread(paste0("C:/Users/ROSA/Favorites/Github/sws_project/faoswsFisheryStandardization/data/localRun/SUA_standard/", currentFile), header = TRUE)

utilizationT=melt( utilizationT,
                          id.vars = colnames(utilizationT)[c(1:3)],
                          measure.vars = colnames(utilizationT)[c(4:37)],
                          variable.name = "timePointYears",
                          value.name = "Value"
)


#Obtaine a country-specific balancingItem table
utilizationT=utilizationT[grepl("flag", timePointYears) & Value=="B",]
utilizationT[,timePointYears:=gsub("_flag", "", timePointYears) ]

currentCountry=unique(utilizationT[,geographicAreaM49_fi])

write.csv(utilizationT,paste0("C:/Users/ROSA/Favorites/Github/sws_project/faoswsFisheryStandardization/data/localRun/balancingElement/",currentCountry,"_balancingElements.csv"), row.names = FALSE)

#Obtaine a country-specific F table
utilization=fread(paste0("C:/Users/ROSA/Favorites/Github/sws_project/faoswsFisheryStandardization/data/localRun/SUA_standard/", currentFile), header = TRUE)
utilization=melt( utilization,
                         id.vars = colnames(utilization)[c(1:3)],
                         measure.vars = colnames(utilization)[c(4:37)],
                         variable.name = "timePointYears",
                         value.name = "Value"
)

utilizationF=utilization[grepl("flag", timePointYears) & Value=="F",]
utilizationF[,timePointYears:=gsub("_flag", "", timePointYears) ]
filter=unique(utilizationF[,.(geographicAreaM49_fi,  ics, measuredElement ,timePointYears)])

protected=utilization[filter,,on=c("geographicAreaM49_fi" , "ics", "measuredElement", "timePointYears")]


write.csv(protected,paste0("C:/Users/ROSA/Favorites/Github/sws_project/faoswsFisheryStandardization/data/localRun/protected/",currentCountry,"_protected.csv"), row.names = FALSE)

}