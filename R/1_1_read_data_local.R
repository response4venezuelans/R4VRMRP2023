# Read data 2022
# function

read_data_2023_local <- function(data)
{ 
  
  activityinfo::activityInfoToken(Sys.getenv("TOKEN_AI"))
  
  # Get data 

  df5W <- data 
  
  # format column names for easier data processing
  
  colnames(df5W) <- c("Country",
                      "Admin1",
                      "Admin2",
                      "Appealing_org",
                      "Implementation",
                      "Implementing_partner",
                      "Month",
                      "Subsector",
                      "Indicator",
                      "Activity_Name",
                      "Activity_Description",
                      "RMRPActivity",
                      "CVA",
                      "Value",
                      "Delivery_mechanism",
                      "Quantity_output",
                      "Total_monthly",
                      "New_beneficiaries",
                      "IN_DESTINATION",
                      "IN_TRANSIT",
                      "Host_Communities",
                      "PENDULARS",
                      "Returnees",
                      "Girls",
                      "Boys",
                      "Women",
                      "Men",
                      "Other_under",
                      "Other_above")

  # Short data wrangling for integer values

  df5W <<- df5W %>%
    mutate_at(c("Value",
                "Quantity_output",
                "Total_monthly",
                "New_beneficiaries",
                "IN_DESTINATION",
                "IN_TRANSIT",
                "Host_Communities",
                "PENDULARS",
                "Returnees",
                "Girls",
                "Boys",
                "Women",
                "Men",
                "Other_under",
                "Other_above"), as.numeric)%>%
    arrange(Country, Month)%>%
    na_if("")

  # Get other reference table used during the data quality check
  # Loaded from AI regardless the method for 5W used
  
  dfadmin1  <<- queryTable("ctfe4etlct8v92h2s2z",
                           "Country" = "c8u26b8kxeqpy0k4",
                           "Admin1" = "c3ns3zikxeqq4h95",
                           "ISOCode" = "cl3sspjkxeqq8yq6",truncateStrings = FALSE)%>%
    rowwise()%>%
    mutate(countryadmin1 = paste(Country, Admin1))%>%
    ungroup()
 
  
  dfadmin2 <<- queryTable("cxl7zn3lct8v92h2s32",
                   "Country" = "cnkb6jykxgdeemm4r.c8u26b8kxeqpy0k4",
                   "Admin1" = "cnkb6jykxgdeemm4r.c3ns3zikxeqq4h95",
                   "Admin2" = "cs2esadkx6hkt7j6", truncateStrings = FALSE)%>%
    rowwise()%>%
    mutate(admin1and2 = paste(Admin1, Admin2))%>%
    ungroup()
  

  
  
  dfindicator  <<- queryTable("cbumi0ulctcno232",
                              "CODE" = "cdhugiblctco28h3",
                              "Subsector" = "cagw22hlctcp2vu5",
                              "Indicator" = "c1oo0eclctcqtjp8",
                              "IndicatorType" = "cuskmf7lctcszoga", truncateStrings = FALSE)%>%
    rowwise()%>%
    mutate(sectindic = paste(Subsector, Indicator))%>%
    ungroup()

  
dfpartner <<- queryTable("cvbei1nlct8v92h2s31",
                 "AOIDORG" = "cnhvpo4kumvyqla8",
                 "Name" = "ckj5zamkumvyysv9",
                 "Type" = "c813krekumw0449j",
                 "RMlead" = "cu2zbr0l1z3adte7", truncateStrings = FALSE)

  
  return(df5W)
  
} 
