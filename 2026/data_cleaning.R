####################################################
### Cleaning ONS data into analysis ready format ###
####################################################

library(data.table)
library(readxl)
library(zoo) # for the function "na.locf"
library(openxlsx) # for the write.xlsx function

#---------------------------------------------------
# Function to clean a single sheet
# Only reads the specified rows and columns
#---------------------------------------------------


clean_sheet <- function(path, sheet_name,cell_range) {
  
  # Read entire sheet first (to detect last row/col)
  tmp <- read_excel(path, sheet = sheet_name, col_names = FALSE, range = cell_range)
  total_rows <- nrow(tmp)
  total_cols <- ncol(tmp)
  
  
  # Convert to data table
  dt <- as.data.table(tmp)
  class(dt)
  
  # Identify and remove the entirely empty row
  which(dt[, rowSums(is.na(.SD) | .SD == "") == ncol(dt)])
  # 22
  dt <- dt[rowSums(!is.na(as.data.frame(dt))) > 0]
  
  #---------------------------------------------------------
  # STEP 1 — Clean left metadata columns
  #---------------------------------------------------------
  
  # First two columns are: Area, Age group
  area_col <- dt[[1]]
  age_col  <- dt[[2]]
  
  area_col
  age_col
  
  # Fill down metadata
  area_col <- na.locf(area_col, na.rm = FALSE)
  age_col  <- na.locf(age_col,  na.rm = FALSE)
  
  area_col
  age_col
  
  #---------------------------------------------------------
  # STEP 2 — Identify the empty dividing column
  #---------------------------------------------------------
  empty_cols <- which(colSums(!is.na(dt)) == 0)
  empty_cols
  # 47
  
  if (length(empty_cols) == 0)
    stop("No empty dividing column found.")
  
  divider_col <- empty_cols[1]
  
  # Two blocks: left sex, right sex
  block1 <- dt[, 3:(divider_col - 1), with = FALSE]
  block2 <- dt[, (divider_col + 1):ncol(dt), with = FALSE]
  
  #---------------------------------------------------------
  # STEP 3 — Extract sex labels from row 1 (merged)
  #---------------------------------------------------------
  
  sex1 <- as.character(block1[1, 1])
  sex2 <- as.character(block2[1, 1])
  
  sex1 <- ifelse(is.na(sex1), "Sex1", sex1)
  sex2 <- ifelse(is.na(sex2), "Sex2", sex2)
  
  #---------------------------------------------------------
  # STEP 4 — Extract column names (row 2)
  #---------------------------------------------------------
  
  colnames(block1) <- as.character(block1[2])
  colnames(block2) <- as.character(block2[2])
  
  # Remove the first two rows (sex + header row)
  block1 <- block1[-c(1, 2)]
  block2 <- block2[-c(1, 2)]
  
  #---------------------------------------------------------
  # STEP 5 — Bind metadata to each block
  #---------------------------------------------------------
  
  block1[, Area := area_col[-c(1, 2)]]
  block1[, AgeGroup := age_col[-c(1, 2)]]
  block2[, Area := area_col[-c(1, 2)]]
  block2[, AgeGroup := age_col[-c(1, 2)]]
  
  # Add sex indicator
  block1[, Sex := sex1]
  block2[, Sex := sex2]
  
  #---------------------------------------------------------
  # STEP 6 — Reshape both blocks to long
  #---------------------------------------------------------
  
  long1 <- melt(
    block1,
    id.vars = c("Area", "AgeGroup", "Sex"),
    variable.name = "Year",
    value.name = "Death"
  )
  
  long2 <- melt(
    block2,
    id.vars = c("Area", "AgeGroup", "Sex"),
    variable.name = "Year",
    value.name = "Death"
  )
  
  combined <- rbindlist(list(long1, long2), fill = TRUE)
  
  #---------------------------------------------------------
  # STEP 7 — Add mortality causes
  #---------------------------------------------------------
  
  # Add sheet name
  combined[, Cause := sheet_name]
  
  return(combined[])
}

#---------------------------------------------------
#  Test on "Drug" sheet
#---------------------------------------------------

# "Drug" sheet covers only 1993-2024
drug<-clean_sheet("deathsnorthsouthdivideengland1981to2024.xlsx","Drug","A3:BO43")
summary(drug)
str(drug)
table(drug$Year)

# expecting 76*(2024-1993+1)=2432rows
#---------------------------------------------------
# Apply to all the other sheets except for "Drug"
#---------------------------------------------------
path <- "deathsnorthsouthdivideengland1981to2024.xlsx"
sheets <- excel_sheets(path)[4:11]
sheets <- sheets[sheets != "Drug"]
cell_range <- "A3:CM43"
#found out "drug" sheet covers only 1993-2024

death <- rbindlist(
  lapply(sheets, function(sh) {
    clean_sheet(path = path, sheet_name = sh, cell_range = cell_range)
  }),
  fill = TRUE,
  use.names = TRUE
)

death
summary(death)
str(death)
#expecting 23408=76*(2024-1981+1)*7 rows

#---------------------------------------------------
# Now combine all the sheets for all death causes
#---------------------------------------------------
death_all <- rbind(death,drug) 

#expecting 25840=76*(2024-1981+1)*7 + 76*(2024-1993+1) rows
table(death_all$Cause)
table(death_all$Area)
table(death_all$Sex)
# Females   Males 
table(death_all$AgeGroup)
# 19 age groups 19
table(death_all$Year)
summary(death_all)
str(death_all)

#---------------------------------------------------
# Merge with population data
#---------------------------------------------------
pop <- fread("population_estimates_1981_2024.csv") # Load population estimates
summary(pop)
str(pop)
#expecting 3344=76*(2024-1981+1) rows
table(pop$Area)
table(pop$Sex)
# Female   Male 
table(pop$AgeGroup)
# 19 age groups 19
table(pop$Year)


# Year in death is a factor so it needs conversion for merging
death_all[, Year := as.integer(as.character(Year))]
# Sex is coded as plural term in death_all and needs conversion
death_all[Sex=="Males", Sex:="Male"]
death_all[Sex=="Females", Sex:="Female"]
table(death_all$Sex)
# Female   Male 

death_pop <- merge(death_all, pop, by = c("Area", "Sex", "AgeGroup", "Year"), all.x = TRUE)
summary(death_pop)
#convert Death to integer
death_pop[, Death := as.integer(Death)]
summary(death_pop)


#---------------------------------------------------
# Filter out total mortality
#---------------------------------------------------
dt1 <- death_pop[Cause=="All causes",]
sum(is.na(dt1))
summary(dt1)
sum(is.na(dt1$Death))


write.xlsx(dt1,"tot_death.xlsx", sheetName = "Sheet1", overwrite = TRUE)

