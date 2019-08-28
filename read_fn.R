### A: Lucas Nathe
### D: 8/18/2019
### P: 
#sets
#i)download disclosure and transmittal files
#ii)read in cra disclosure data from .dat.zip
#iii) read in cra transmittal files .dat.zip
#iv) merge them together
#overview
# create a function that you feed years and a key word for loan type
# that returns a set of disclsoure data for that type of record_id andyears
#setwd('/Users/prnathe/Documents/github_data/')
read_data<- function(years,loan_type){
  #### helper functions ####
  get_widths <- function(year) {
    #short_year <- [1] %% 100L
    if (year == "96") {
      ret_val <- c(4, 10, 1, 4, 1, 1, 2, 3, 4, 4,
                   1, 1, 1, 3, 3, 6, 8, 6, 8, 6,
                   8, 6, 8, 6, 8)
    } else if (year %in% (c("97","98","99","00","01","03"))) {
      ret_val <- c(5, 10, 1, 4, 1, 1, 2, 3, 4, 4,
                   1, 1, 1, 3, 3, 6, 8, 6, 8, 6,
                   8, 6, 8, 6, 8)
    } else {
      ret_val <- c(5, 10, 1, 4, 1, 1, 2, 3, 5, 4,
                   1, 1, 1, 3, 3, 10, 10, 10, 10, 10,
                   10, 10, 10, 10, 10)
    }
    ret_val
  }
  factor_agency <- function(x) {
    x <- as.character(x)
    
    factor(x,
           levels = c('1', '2', '3', '4'),
           labels = c('OCC', 'FRB', 'FDIC', 'OTS'))
  }
  years<- as.character(years)
  #convert years to format of urls 
  if(sum(nchar(years,type = "width")==4)==length(years)){
    years<- substr(years,3,4)
  }
  if(sum(nchar(years,type = "width")==2)!=length(years)){
    print("You need to 2 or 4 digits years!")
  }
  loan_type<-toupper(loan_type)
  
  if(!loan_type %in% c("D3","D4","D5","D6","D11","D12","D21","D22")){
    print("You need to enter the proper loan type identifier!")
  }
  if(loan_type %in% c("D3","D4","D5","D6","D11","D12","D21","D22")){
    loan_var_name<-loan_type
    stri_sub(loan_var_name, 3, 2)
    stri_sub(loan_var_name, 3,2) <- "-"
  }  
  # set something to convert these codes to the codes in the dat file 
### download the data
root<-"https://www.ffiec.gov/CRA/xls/"
dest<-getwd()
y<-1
#loop through years
for(y in 1:length(years)){
    download.file(paste0(root,years[y],"exp_discl.zip"),
                  destfile = paste0(dest,"/discl",
                             years[y],".zip"))
  y<-y+1
}
y<-1
for(y in 1:length(years)){
  if(years[y] %in% c("16","17","18")){
    unzip(zipfile = paste0("discl",years[y],".zip"))
    file.rename(paste0("cra20",years[y],"_Discl_",loan_type,".dat"), paste0(years[y],
                                                                            "exp_discl.dat"))
    } else {
      unzip(zipfile = paste0("discl",years[y],".zip"))
      file_list<- list.files(pattern = "^exp_discl.dat")
      #file.rename("exp_discl.dat", paste0(years[y], "exp_discl.dat"))
      file.rename(from = file_list[1], paste0(years[y], "exp_discl.dat"))
      file.remove("exp_discl.dat")
    }
    y<-y+1
}
file_keep<- list.files(pattern = "..exp_discl.dat" )
file_dta<- list.files(pattern = "*.dat")
file_zip<- list.files(pattern = "*.zip")
file_remove<-setdiff(x = file_dta, y = file_keep)
file.remove(file_remove)

#read in dat files    
dat_name<-list.files(pattern = "*.dat")
y<-1
cra_full<- data.frame()
for(y in 1:length(years)){
#w <- get_widths(years[y]) # Widths by file year
w <- get_widths(substr(dat_name[y],1,2)) # Widths by file year
s <- cumsum(c(1, w))[1:length(w)] # starting point for each variable
e <- cumsum(w)                    # end point for each variable


    in_tbl <- readr::read_lines(dat_name[y],progress = FALSE) %>%
      tibble::tibble(V1 = .) %>%
     filter(substr(V1, 1, 4) == loan_var_name)  
    
    ret_dt <- mutate(in_tbl,
                     respondent = as.numeric(substr(V1, s[2], e[2])),
                     agency = factor_agency(substr(V1, s[3], e[3])),
                     year = as.numeric(substr(V1, s[4], e[4])),
                     loan_type = substr(V1, s[5], e[5]),
                     action = substr(V1, s[6], e[6]),
                     state = as.numeric(substr(V1, s[7], e[7])),
                     county = as.numeric(substr(V1, s[8], e[8])),
                     metro_area = as.numeric(substr(V1, s[9], e[9])),
                     assessment_area = as.numeric(substr(V1, s[10], e[10])),
                     partial_county_fl = as.logical(substr(V1, s[11], e[11])),
                     split_county_fl = as.logical(substr(V1, s[12], e[12])),
                     population_f = factor(substr(V1, s[13], e[13]),
                                           levels = c('S', 'L', ' '),
                                           labels = c('<= 500,000 in population',
                                                      '>500,000 in population',
                                                      'Total')),
                     income_group = factor(as.numeric(substr(V1, s[14], e[14])),
                                           levels = c(1:15, 101:106),
                                           labels = c('< 10% of MFI',
                                                      '10% to 20% of MFI',
                                                      '20% to 30% of MFI',
                                                      '30% to 40% of MFI',
                                                      '40% to 50% of MFI',
                                                      '50% to 60% of MFI',
                                                      '60% to 70% of MFI',
                                                      '70% to 80% of MFI',
                                                      '80% to 90% of MFI',
                                                      '90% to 100% of MFI',
                                                      '100% to 110% of MFI',
                                                      '110% to 120% of MFI',
                                                      '>= 120% of MFI',
                                                      'Unknown MFI',
                                                      'Unknown Tract',
                                                      'Low Income',
                                                      'Moderate Income',
                                                      'Middle Income',
                                                      'Upper Income',
                                                      'Unknown Income',
                                                      'Unknown Tract')),
                     report_level = factor(as.numeric(substr(V1, s[15], e[15])),
                                           levels = c(4, 6, 8, 10, 20, 30, 40, 50, 60),
                                           labels = c('Total Inside & Outside Assessment Area (AA)',
                                                      'Total Inside AA',
                                                      'Total Outside AA',
                                                      'State Total',
                                                      'Total Inside AA in State',
                                                      'Total Outside AA in State',
                                                      'County Total',
                                                      'Total Inside AA in County',
                                                      'Total Outside AA in County')),
                     num_le100k = as.numeric(substr(V1, s[16], e[16])),
                     amt_le100k = as.numeric(substr(V1, s[17], e[17])),
                     num_100to250k = as.numeric(substr(V1, s[18], e[18])),
                     amt_100to250k = as.numeric(substr(V1, s[19], e[19])),
                     num_250to1m = as.numeric(substr(V1, s[20], e[20])),
                     amt_250to1m = as.numeric(substr(V1, s[21], e[21])),
                     num_sbl = as.numeric(substr(V1, s[22], e[22])),
                     amt_sbl = as.numeric(substr(V1, s[23], e[23])),
                     num_affiliate = as.numeric(substr(V1, s[24], e[24])),
                     amt_affiliate = as.numeric(substr(V1, s[25], e[25]))) 
    
    if (substr(dat_name[y],1,2) %in% c("96","97","98")){
      ret_dt<- ret_dt %>% filter(is.na(income_group) & report_level %in%
                                   c('County Total',
                                     'Total Inside AA in County',
                                     'Total Outside AA in County'))
      ret_dt<- ret_dt[order(ret_dt$respondent,ret_dt$state,ret_dt$county, ret_dt$report_level),]
      ret_dt<- ret_dt %>% distinct(respondent,state,county,.keep_all = TRUE) # need to find a way to drop duplicates like stata
      ret_dt$report_level<- "County Total"
    }
    
    ret_dt<- ret_dt %>% filter(report_level == "County Total")
    cra_full<- bind_rows(cra_full,ret_dt)
    y<-y+1
}
file.remove(file_dta)
file.remove(file_zip)
return(cra_full)
}
