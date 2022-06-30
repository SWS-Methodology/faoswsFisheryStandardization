library(faosws)

# This returns FALSE if on the Statistical Working System
if(CheckDebug()){
  
  library(faoswsModules)
  SETTINGS <- ReadSettings("sws.yml")
  
  # Define where your certificates are stored
  SetClientFiles(SETTINGS[["certdir"]])
  
  # Get session information from SWS. Token must be obtained from web interface
  GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                     token = SETTINGS[["token"]])
  
}

if(is.null(swsContext.computationParams$delete_session) || swsContext.computationParams$delete_session == ""){
  stop("You must specify whether to delete the whole dataset or just a session")
}
DELETE_SESSION = as.logical(swsContext.computationParams$delete_session)

CONFIG <- GetDatasetConfig(swsContext.datasets[[1]]@domain, swsContext.datasets[[1]]@dataset)

if(DELETE_SESSION){
  delete_key <- swsContext.datasets[[1]]
} else {
  # Get all years except wildcard 0
  delete_key <- DatasetKey(domain = CONFIG$domain, dataset = CONFIG$dataset,
                           dimensions = lapply(CONFIG$dimensions, 
                                               function(x){Dimension(x, keys = GetCodeList(CONFIG$domain, CONFIG$dataset, x)[, code])})
  )
}

data_to_delete <- GetData(delete_key)

data_to_delete[, Value := NA_real_]
data_to_delete[, (CONFIG$flags) := NA_character_]

stats <- SaveData(CONFIG$domain, CONFIG$dataset , data_to_delete, waitTimeout = Inf)

sprintf("Module deleted %s values", stats$appended)

## Initiate email
from = "sws@fao.org"
to = swsContext.userEmail
subject = "Wipe data"
body = paste0("Data have been deleted.", "Dataset: ", CONFIG$dataset, ". SessionID: ", sessionKey@sessionId )


sendmail(from = from, to = to, subject = subject, msg = body)


message("The plugin the deleted the data you have selected!")
