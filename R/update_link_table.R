suppressMessages({
  library(faosws)
  library(faoswsUtil)
  library(data.table)
})



if(CheckDebug()){
  library(faoswsModules)
  SETTINGS = ReadSettings("module/Balancing/sws.yml")
  
  ## If you're not on the system, your settings will overwrite any others
  R_SWS_SHARE_PATH = SETTINGS[["share"]]
  
  ## Define where your certificates are stored
  SetClientFiles(SETTINGS[["certdir"]])
  
  ## Get session information from SWS. Token must be obtained from web interface
  GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                     token = SETTINGS[["token"]])
  
}


swstab <- ReadDatatable('link_table', readOnly = FALSE)
aug2019 <- read.csv('C:/Users/Taglionic/Desktop/Fisheries/SUA-Aug-2019/blnc_macro.csv')
aug2019 <- as.data.table(aug2019)
setnames(aug2019, names(aug2019), names(swstab))
aug2019$geographic_area_m49 <- as.character(aug2019$geographic_area_m49)
aug2019$flow <- as.character(aug2019$flow)


class(swstab$to_code)

diff <- merge(swstab, aug2019, by = names(aug2019), all = TRUE)

setdiff(swstab, aug2019)

unique(aug2019[ ! geographic_area_m49 %in% swstab$geographic_area_m49]$geographic_area_m49)

changeset <- Changeset('link_table') 
AddDeletions(changeset, swstab)
Finalise(changeset)
AddInsertions(changeset, aug2019)
Finalise(changeset)