library(readxl)
library(openxlsx)
library(dplyr)

Sys.setenv(R_ZIPCMD= "C:/Rtools/bin/zip")

fat <- function(file, file2, mp, resultName){
  wb <- createWorkbook()
  wb2 <- createWorkbook()
  comparison <- sapply(mp, function(x){
    
    #read in shadow model results
    sm <- read_excel(file2, sheet = x)
    # read in moses model results
    mm <- read_excel(file, sheet = x)
    
    mm <- mm[1:nrow(sm),]
    res <- list()
    
    cols <- names(mm)
    cols <- cols[-c(1:8)]
    sub <- sapply(cols, function(col) round(sm[,col] - mm[,col],4))
    
    names(sub) <- cols
    sel <- sapply(sub, function(x) which(length(x) >0))
    res <- as_data_frame(sub[which(as.numeric(sel)==1)])
    
    #save to Excel
    addWorksheet(wb, x)
    writeDataTable(wb, x, res)
    
    #calculate means
    res2 <- res %>% summarise_each(funs(sum(.,na.rm = TRUE)))
    addWorksheet(wb2, x)
    writeDataTable(wb2, x, res2)
  })
  #save Workbook
  saveWorkbook(wb, resultName, overwrite = TRUE)
  saveWorkbook(wb2, paste0("Stat", resultName), overwrite = TRUE)
}

file = ""
file2 = ""
mp = excel_sheets(file2)
  
# test1 
fat(file, file2, mp, paste0("FAT_", format(Sys.time(), "%F_%H-%M-%S"), ".xlsx"))
