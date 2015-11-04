# load libraries _ if you don't have them install then first
library(readxl)
library(openxlsx)
library(dplyr)
library(foreign)
library(purrr)
library(tidyr)
library(lubridate)

#set your working directory - output will be written there
# setwd("C:/")

Sys.setenv(R_ZIPCMD= "C:/Rtools/bin/zip")

fat <- function(file, file2, mp, resultName){
  wb <- createWorkbook()
  wb2 <- createWorkbook()
  comparison <- sapply(mp, function(x){
    
    #get file extension
    ext1 <- tolower(substr(file, regexpr("\\.[^\\.]*$", file)[1]+1, nchar(file)))
    ext2 <- tolower(substr(file2, regexpr("\\.[^\\.]*$", file2)[1]+1, nchar(file2)))
    
    #read in file based on extension
    if (ext1 == "dbf"){
      mm <- read.dbf(file, TRUE)
      mm <- mm[,1:(ncol(mm)-8)]
    } else {
      if (ext1 == "xlsx" | ext1 =="xls"){
        mm <- read_excel(file, sheet = x)
      } else {
        mm <- read.csv(file)
      }
    }
    
    #read in file2 based on extension
    if (ext2 == "dbf"){
      sm <- read.dbf(file2)
      sm <- sm[,1:(ncol(sm)-8)]
    } else {
      if (ext2 == "xlsx" | ext2 =="xls"){
        sm <- read_excel(file2, sheet = x)
      } else {
        sm <- read.csv(file2)
      }
    }
    
    mm <- mm[1:nrow(sm),]
    res <- list()
    
    # adjust column names
    colnames(mm) <- tolower(colnames(mm))
    colnames(sm) <- tolower(colnames(sm))
    cols <- names(mm)
    colse <- names(sm)
    
    #select common set of names
    cols <- cols[cols %in% colse]
    
    #select only numerci column names
    classes <- map(mm[,cols], class) %>%
      dplyr::as_data_frame() %>% 
      gather(name, class)
    
    namestoselc <-  classes %>%
      filter(class == "numeric" | class == "int") %>%
      .$name %>% 
      as.character()
    
    #compare files
    if (ext1 == "xls" || ext1 == "xlsx" || ext2 == "xls" || ext2 == "xlsx"){
      sub <- sapply(namestoselc, function(col) round(sm[,col] - mm[,col],4))
    }else{
      sub <- lapply(namestoselc, function(col) round(sm[,col] - unlist(mm[,col]),4))
    }
  
    names(sub) <- namestoselc
    sel <- sapply(sub, function(x) which(length(x) >0))
    #res <- as_data_frame(sub[which(as.numeric(sel)==1)])
    res <- data.frame(sub[which(as.numeric(sel)==1)])
    #res <- sub[which(as.numeric(sel)==1)]
    
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
  "Done"
}

# If you need to rerun the tests only run this part
file = "C:/Users/omierzwa/Documents/file1.xls"
file2 = "C:/Users/omierzwa/Documents/file2.xlsx"
#set file from which read sheet names (this should be Moses file because Excel has >25 sheets)
mp = excel_sheets(file)

# ste mp = 1 if you are not runing Excel output
fat(file, file2, mp = mp, paste0("FAT_", format(Sys.time(), "%F_%H-%M-%S"), ".xlsx"))
