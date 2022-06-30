suppressMessages({
  library(faosws)
  library(faoswsUtil)
  library(data.table)
})



if(CheckDebug()){
  library(faoswsModules)
  SETTINGS = ReadSettings("sws.yml")
  
  ## If you're not on the system, your settings will overwrite any others
  R_SWS_SHARE_PATH = SETTINGS[["share"]]
  
  ## Define where your certificates are stored
  SetClientFiles(SETTINGS[["certdir"]])
  
  ## Get session information from SWS. Token must be obtained from web interface
  GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                     token = SETTINGS[["token"]])
  
}

swstab <- ReadDatatable('link_table')
linkt <- read.csv('C:/Users/Taglionic/OneDrive - Food and Agriculture Organization/Github/faoswsFisheryStandardization/Input_documents/blnc_macro_updated_Nov2020.csv',
                  header = F)
linkt <- as.data.table(linkt)
setnames(linkt, names(linkt), names(swstab))
linkt$geographic_area_m49 <- as.character(linkt$geographic_area_m49)
linkt$flow <- as.character(linkt$flow)


changeset <- Changeset('link_table') 
AddInsertions(changeset, linkt)
Finalise(changeset)
