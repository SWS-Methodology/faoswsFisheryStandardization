

##Compare imputations with Export quantities:

commodityDB_quantityImputed
trade_commodityDB_quantity=trade_commodityDB_quantity[measuredElement=="91"]


exportCompare=merge(trade_commodityDB_quantity,commodityDB_quantityImputed, by=c("geographicAreaM49",
                                                                   "measuredItemISSCFC",
                                                                   "timePointYears"),
      suffixes = c("_export","_imputed"), all.x = TRUE)


seriesToBeChecked=exportCompare[Value_export>Value_imputed]
seriesToBeChecked=unique(seriesToBeChecked[,.(geographicAreaM49 ,measuredItemISSCFC )])


## FIAS team request: the plot must contains all the time series, not only the timepoint where export exceeds production.

exportCompare=exportCompare[seriesToBeChecked, , on=c("geographicAreaM49" ,"measuredItemISSCFC")]
exportCompare=exportCompare[,.(geographicAreaM49, measuredItemISSCFC, timePointYears, measuredElement_export ,
                 Value_export, measuredElement_imputed ,Value_imputed)]

# dataset Reshape 
exportCompare=melt(exportCompare,id.vars = colnames(exportCompare[, c(1:3)]),
                   measure.vars = colnames(exportCompare[, c(5,7)]), value.name="Value")

exportCompare[, varableCompared:=ifelse(variable=="Value_export","Export", "imputedProduction")]


exportCompare[, measuredElement:="Imputed Production vs Exports"]

plotCompare(exportCompare,geoVar="geographicAreaM49",elVar="measuredElement", elVector=c("Imputed Production vs Exports"),itemVar="measuredItemISSCFC", 
            directory="C:/Users/ROSA/Desktop/Fisheries/commodityDBTradeCheck", title="comparison",status="variable", geomPoint="varableCompared")








