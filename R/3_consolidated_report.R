# Consolidated report RMRP 2022

# function writing

r4v_consolidated <- function(data,
                             countryname = NULL, 
                             proportions = "pin", 
                             totalmodel = "sum")
{
# Packages

  # SHINY Creating a list which will return all the dataframes
  return_data <- list()
  
# Filter by the needed country and PiN indicators only

if (is.null(countryname) || (countryname=="All")) {
  df5Wconsolidated <<- df5W %>%
    left_join(dfindicator, by = c("Subsector", "Indicator"))%>%
    select(-CODE, -sectindic)%>%
    filter(IndicatorType == "Direct Assistance" )%>%
    mutate_if(is.numeric, replace_na, replace = 0)
} else {
  df5Wconsolidated <<- df5W %>% 
    filter(Country == countryname)%>%
    left_join(dfindicator, by = c("Subsector", "Indicator"))%>%
    select(-CODE, -sectindic)%>%
    filter(IndicatorType == "Direct Assistance")%>%
  mutate_if(is.numeric, replace_na, replace = 0)  
}
 
# Create a vector based on Activity's months to the filter the template
  monthlist <- unique(as.vector(df5Wconsolidated["Month"]))
  
# Get consolidated template file

dftemplate <- read_excel("data/Consolidated_Template.xlsx")


if (is.null(countryname) || (countryname=="All")) {
  dftemplate <- dftemplate%>%
    semi_join(monthlist, by = "Month")
} else {
  dftemplate <- dftemplate%>%
    filter(Country == countryname)%>%
    semi_join(monthlist, by = "Month")
}

# Get prpoportions file for each sector in Admin1 and poptype categories

if (proportions == "pin")  {
  dfproportions <- read_excel("./data/Proportions.xlsx", sheet = "pinproportions")
} else if (proportions == "target") {
  dfproportions <- read_excel("data/Proportions.xlsx", sheet = "targetproportions")
}


#################### 1. Total Monthly figures #################################################################

# Get monthly total figures of persons per Admin1 per sector
# sum Total beneficiaries of every activities 

monthlysectors <- df5Wconsolidated%>%
  group_by(Country, Admin1, Month, Subsector)%>%
  summarise('Monthly Total Beneficiaries' = sum(Total_monthly))

monthlytotal <- monthlysectors%>%
  group_by(Country, Admin1, Month)%>%
  summarise(Subsector = "Intersector", 
            'Monthly Total Beneficiaries' = sum(`Monthly Total Beneficiaries`))

monthly<- rbind(monthlysectors, monthlytotal)

# Get CVA Beneficiaries monthly total figures of persons per Admin1 per sector
# sum Total beneficiaries of every activities 

CVAmonthlysectors <- df5Wconsolidated%>%
  filter(CVA == 'Yes')%>%
  group_by(Country, Admin1, Month, Subsector)%>%
  summarise('Monthly CVA Beneficiaries' = sum(Total_monthly))

CVAmonthlytotal <- CVAmonthlysectors%>%
  group_by(Country, Admin1, Month)%>%
  summarise(Subsector = "Intersector", 
            'Monthly CVA Beneficiaries' = sum(`Monthly CVA Beneficiaries`))

CVAmonthly<- rbind(CVAmonthlysectors, CVAmonthlytotal)

finalmonthlyadm1 <- monthly%>%
  full_join(CVAmonthly, by = c("Country", "Admin1", "Month", "Subsector"))%>%
  mutate(across(where(is.numeric),
                replace_na, 0))

# Get country level data

finalmonthcountry <- finalmonthlyadm1 %>%
  group_by(Country, Month, Subsector)%>%
  summarise(Admin1 = "Country level",
            'Monthly Total Beneficiaries' = sum(`Monthly Total Beneficiaries`),
            'Monthly CVA Beneficiaries' = sum(`Monthly CVA Beneficiaries`))

finalmonthlytotal <- rbind(finalmonthlyadm1, finalmonthcountry)

#################### 2. Consolidated figures #################################################################

###### 2.1 Sector level ################
# Figures at sector and admin1 level are solely calculated through a sum
# If required by some platform, we can develop a indicator based system tailored for countries.

conssectors <-  df5Wconsolidated%>%
  group_by(Country, Admin1, Month, Subsector)%>%
  summarise(Monthly_Consolidated = sum(New_beneficiaries),
            Consolidated_RMindestination = sum(IN_DESTINATION), 
            Consolidated_RM_in_transit = sum(IN_TRANSIT),
            Consolidated_Host_Community = sum(Host_Communities),
            Consolidated_RM_Pendulars = sum(PENDULARS),
            Consolidated_Colombian_Returnees = sum(Returnees),
            Consolidated_Girls = sum(Girls), 
            Consolidated_Boys = sum(Boys),
            Consolidated_Women = sum(Women),
            Consolidated_Men = sum(Men),
            Consolidated_Other_under_18 = sum(Other_under),
            Consolidated_Other_above_18 = sum(Other_above)
  )

consCVAsectors <- df5Wconsolidated%>%
  filter(CVA == "Yes")%>%
  group_by(Country, Admin1, Month, Subsector)%>%
  summarise(Consolidated_CVA_Beneficiaries = sum(New_beneficiaries))

consallsectors <- conssectors%>%
  full_join(consCVAsectors, by = c("Country", "Admin1", "Month", "Subsector"))%>%
  mutate(across(where(is.numeric),
                   replace_na, 0))

###### 2.2 Intersector level ################
# Chose the consoldated model you want to use (see "Consolidated guidance" document 
# in GitHub repository)


# Model 1: Sum All: sums all beneficiaries of all sectors to get intersector figures
# per admin1 level and then at national level
if (totalmodel == "sum")
{
  conssumadm1 <- consallsectors%>%
  group_by(Country, Admin1, Month)%>%
  summarise(Subsector = "Intersector",
            Monthly_Consolidated = sum(Monthly_Consolidated),
            Consolidated_RMindestination = sum( Consolidated_RMindestination), 
            Consolidated_RM_in_transit = sum(Consolidated_RM_in_transit),
            Consolidated_Host_Community = sum(Consolidated_Host_Community),
            Consolidated_RM_Pendulars = sum(Consolidated_RM_Pendulars),
            Consolidated_Colombian_Returnees = sum(Consolidated_Colombian_Returnees),
            Consolidated_Girls = sum(Consolidated_Girls), 
            Consolidated_Boys = sum(Consolidated_Boys),
            Consolidated_Women = sum( Consolidated_Women),
            Consolidated_Men = sum(Consolidated_Men),
            Consolidated_Other_under_18 = sum(Consolidated_Other_under_18),
            Consolidated_Other_above_18 = sum(Consolidated_Other_above_18),
            Consolidated_CVA_Beneficiaries = sum(Consolidated_CVA_Beneficiaries)
            )
# Join Admin1-Intersector table to Admin1-Sector table
consfinaladmin1 <- rbind(consallsectors, conssumadm1)

# Sum Admin1 figures for sector and intersector to get the full final table
consfinalcountrylevel <- consfinaladmin1 %>%
  group_by(Country, Month, Subsector)%>%
  summarise(Admin1 = "Country level",
            Monthly_Consolidated = sum(Monthly_Consolidated),
            Consolidated_RMindestination = sum( Consolidated_RMindestination), 
            Consolidated_RM_in_transit = sum(Consolidated_RM_in_transit),
            Consolidated_Host_Community = sum(Consolidated_Host_Community),
            Consolidated_RM_Pendulars = sum(Consolidated_RM_Pendulars),
            Consolidated_Colombian_Returnees = sum(Consolidated_Colombian_Returnees),
            Consolidated_Girls = sum(Consolidated_Girls), 
            Consolidated_Boys = sum(Consolidated_Boys),
            Consolidated_Women = sum( Consolidated_Women),
            Consolidated_Men = sum(Consolidated_Men),
            Consolidated_Other_under_18 = sum(Consolidated_Other_under_18),
            Consolidated_Other_above_18 = sum(Consolidated_Other_above_18),
            Consolidated_CVA_Beneficiaries = sum(Consolidated_CVA_Beneficiaries)
  )

consfinal <- rbind(consfinaladmin1, consfinalcountrylevel)%>%
  arrange(Country, Admin1)
}

if (totalmodel == "maxsector")
  { 
  # Model 2: Max Sector: sums all beneficiaries at sector and admin1 level, then take
  # the max of each age and gender categories for each population type to get the intersector figure
  
  consmaxmerge <- consallsectors%>%
  left_join(dfproportions, by = c("Country", "Admin1", "Subsector"))%>%
  # Drop AGD Categories as they will be substituted in the final file
  select(-Consolidated_Girls, 
         -Consolidated_Boys, 
         -Consolidated_Women, 
         -Consolidated_Men, 
         -Consolidated_Other_above_18, 
         -Consolidated_Other_under_18)%>%
  # get the breakdown of each age and gender categories by multiplying the pop type category by the proportions
  rowwise()%>%
  mutate(RMDestGirls = round(Consolidated_RMindestination * Dest_GIrls,0),
         RMDestBoys = round(Consolidated_RMindestination * Dest_Boys,0),
         RMDestWomen = round(Consolidated_RMindestination * Dest_Women,0),
         RMDestMen = round(Consolidated_RMindestination * Dest_Men,0),
         RMTransitGirls = round(Consolidated_RM_in_transit * Transit_GIrls,0),
         RMTransitBoys = round(Consolidated_RM_in_transit * Transit_Boys,0),
         RMTransitWomen = round(Consolidated_RM_in_transit * Transit_Women,0),
         RMTransitMen = round(Consolidated_RM_in_transit * Transit_Men,0),
         RMHCGirls = round(Consolidated_Host_Community * HC_Girls,0),
         RMHCBoys = round(Consolidated_Host_Community * HC_Boys,0),
         RMHCWomen = round(Consolidated_Host_Community * HC_Women,0),
         RMHCMen = round(Consolidated_Host_Community * HC_Men,0),
         RMPendularGirls = round(Consolidated_RM_Pendulars * Pendular_Girls,0),
         RMPendularBoys = round(Consolidated_RM_Pendulars * Pendular_Boys,0),
         RMPendularWomen = round(Consolidated_RM_Pendulars * Pendular_Women,0),
         RMPendularMen = round(Consolidated_RM_Pendulars * Pendular_Men,0),
         RMReturneesGirls = round(Consolidated_Colombian_Returnees*Returnees_Girls,0),
         RMReturneesBoys = round(Consolidated_Colombian_Returnees*Returnees_Boys,0),
         RMReturneesWomen = round(Consolidated_Colombian_Returnees*Returnees_Women,0),
         RMReturneesMen = round(Consolidated_Colombian_Returnees*Returnees_Men,0))%>%
  ungroup()%>%
  select(-(5:10),-(12:35))%>%
  group_by(Country, Admin1, Month)%>%
  # Get MAX Values for each Age and Gender categories in each population type
  summarise(Subsector = "Intersector",
            RMDestGirls = max(RMDestGirls),
            RMDestBoys = max(RMDestBoys),
            RMDestWomen = max(RMDestWomen),
            RMDestMen = max(RMDestMen),
            RMTransitGirls = max(RMTransitGirls),
            RMTransitBoys = max(RMTransitBoys),
            RMTransitWomen =max(RMTransitWomen),
            RMTransitMen = max(RMTransitMen),
            RMHCGirls = max(RMHCGirls),
            RMHCBoys = max(RMHCBoys),
            RMHCWomen = max(RMHCWomen),
            RMHCMen = max(RMHCMen),
            RMPendularGirls = max(RMPendularGirls),
            RMPendularBoys = max(RMPendularBoys),
            RMPendularWomen = max(RMPendularWomen),
            RMPendularMen = max(RMPendularMen),
            RMReturneesGirls = max(RMReturneesGirls),
            RMReturneesBoys = max(RMReturneesBoys),
            RMReturneesWomen = max(RMReturneesWomen),
            RMReturneesMen =max(RMReturneesMen),
            Consolidated_CVA_Beneficiaries = max(Consolidated_CVA_Beneficiaries))%>%
  # Sum values to determine the totals
  rowwise()%>%
  mutate(Consolidated_RMindestination = sum(RMDestGirls, RMDestBoys, RMDestWomen, RMDestMen, na.rm = TRUE),
         Consolidated_RM_in_transit = sum(RMTransitGirls, RMTransitBoys, RMTransitWomen, RMTransitMen, na.rm = TRUE),
         Consolidated_Host_Community = sum(RMHCGirls, RMHCBoys, RMHCWomen, RMHCMen, na.rm = TRUE),
         Consolidated_RM_Pendulars = sum(RMPendularGirls, RMPendularBoys, RMPendularWomen, RMPendularMen, na.rm = TRUE),
         Consolidated_Colombian_Returnees = sum(RMReturneesGirls, RMReturneesBoys, RMReturneesWomen, RMReturneesMen, na.rm = TRUE),
         Consolidated_Girls = sum(RMDestGirls, RMTransitGirls, RMHCGirls, RMPendularGirls, RMReturneesGirls, na.rm = TRUE),
         Consolidated_Boys = sum(RMDestBoys, RMTransitBoys, RMHCBoys, RMPendularBoys, RMReturneesBoys, na.rm = TRUE),
         Consolidated_Women = sum(RMDestWomen, RMTransitWomen, RMHCWomen, RMPendularWomen, RMReturneesWomen, na.rm = TRUE),
         Consolidated_Men = sum(RMDestMen, RMTransitMen, RMHCMen, RMPendularMen, RMReturneesMen, na.rm = TRUE),
         Consolidated_Other_under_18 = 0,
         Consolidated_Other_above_18 = 0)%>%
  # Drop extra column of breakdown values and reorder to match original format
  select(-(5:24))%>%
  # Get final Monthly Consolidated figures for Intersector
  mutate(Monthly_Consolidated = sum(Consolidated_RMindestination, Consolidated_RM_in_transit,
                                    Consolidated_Host_Community,Consolidated_RM_Pendulars,
                                    Consolidated_Colombian_Returnees, na.rm = TRUE))%>%
  ungroup()

  consmaxfinaladmin1 <- rbind(consallsectors, consmaxmerge)
  
  consmaxfinalcountry <- consmaxfinaladmin1 %>%
    group_by(Country, Month, Subsector)%>%
    summarise(Admin1 = "Country level",
              Monthly_Consolidated = sum(Monthly_Consolidated),
              Consolidated_RMindestination = sum(Consolidated_RMindestination),
              Consolidated_RM_in_transit = sum(Consolidated_RM_in_transit),
              Consolidated_Host_Community = sum(Consolidated_Host_Community),
              Consolidated_RM_Pendulars = sum(Consolidated_RM_Pendulars),
              Consolidated_Colombian_Returnees = sum(Consolidated_Colombian_Returnees),
              Consolidated_Girls = sum(Consolidated_Girls),
              Consolidated_Boys = sum(Consolidated_Boys),
              Consolidated_Women = sum(Consolidated_Women),
              Consolidated_Men = sum(Consolidated_Men),
              Consolidated_Other_under_18 = sum(Consolidated_Other_under_18),
              Consolidated_Other_above_18 = sum(Consolidated_Other_above_18),
              Consolidated_CVA_Beneficiaries = sum(Consolidated_CVA_Beneficiaries))

   consfinal <- rbind(consmaxfinaladmin1, consmaxfinalcountry)  
}
   
   if (totalmodel == "southernconemodel")
     {
     # Model 3: Mixed approach in 3 steps:
     # Max across Shelter, Food security, Humanitarian transport and WASH
     # Take Protection (General) data
     # Sum Other sectors mentionned: Integration, Multipurporse CBI, Health, Education
     
     # Step1: Get data Max across Food Security, Shelter, Humanitarian transport and WASH
     consCSstep1 <- consallsectors%>%
     filter(Subsector %in% c("Shelter", "Humanitarian Transportation", "WASH", "Food Security"))%>%
     left_join(dfproportions, by = c("Country", "Admin1", "Subsector"))%>%
     # Drop AGD Categories as they will be substituted in the final file
     select(-Consolidated_Girls, 
            -Consolidated_Boys, 
            -Consolidated_Women, 
            -Consolidated_Men, 
            -Consolidated_Other_above_18, 
            -Consolidated_Other_under_18)%>%
     # get the breakdown of each age and gender categories by multiplying the pop type category by the proportions
     rowwise()%>%
     mutate(RMDestGirls = round(Consolidated_RMindestination * Dest_GIrls,0),
            RMDestBoys = round(Consolidated_RMindestination * Dest_Boys,0),
            RMDestWomen = round(Consolidated_RMindestination * Dest_Women,0),
            RMDestMen = round(Consolidated_RMindestination * Dest_Men,0),
            RMTransitGirls = round(Consolidated_RM_in_transit * Transit_GIrls,0),
            RMTransitBoys = round(Consolidated_RM_in_transit * Transit_Boys,0),
            RMTransitWomen = round(Consolidated_RM_in_transit * Transit_Women,0),
            RMTransitMen = round(Consolidated_RM_in_transit * Transit_Men,0),
            RMHCGirls = round(Consolidated_Host_Community * HC_Girls,0),
            RMHCBoys = round(Consolidated_Host_Community * HC_Boys,0),
            RMHCWomen = round(Consolidated_Host_Community * HC_Women,0),
            RMHCMen = round(Consolidated_Host_Community * HC_Men,0),
            RMPendularGirls = round(Consolidated_RM_Pendulars * Pendular_Girls,0),
            RMPendularBoys = round(Consolidated_RM_Pendulars * Pendular_Boys,0),
            RMPendularWomen = round(Consolidated_RM_Pendulars * Pendular_Women,0),
            RMPendularMen = round(Consolidated_RM_Pendulars * Pendular_Men,0),
            RMReturneesGirls = round(Consolidated_Colombian_Returnees*Returnees_Girls,0),
            RMReturneesBoys = round(Consolidated_Colombian_Returnees*Returnees_Boys,0),
            RMReturneesWomen = round(Consolidated_Colombian_Returnees*Returnees_Women,0),
            RMReturneesMen = round(Consolidated_Colombian_Returnees*Returnees_Men,0))%>%
     ungroup()%>%
     select(-(5:10),-(12:35))%>%
     group_by(Country, Admin1, Month)%>%
     # Get MAX Values for each Age and Gender categories in each population type
     summarise(Subsector = "Intersector",
               RMDestGirls = max(RMDestGirls),
               RMDestBoys = max(RMDestBoys),
               RMDestWomen = max(RMDestWomen),
               RMDestMen = max(RMDestMen),
               RMTransitGirls = max(RMTransitGirls),
               RMTransitBoys = max(RMTransitBoys),
               RMTransitWomen =max(RMTransitWomen),
               RMTransitMen = max(RMTransitMen),
               RMHCGirls = max(RMHCGirls),
               RMHCBoys = max(RMHCBoys),
               RMHCWomen = max(RMHCWomen),
               RMHCMen = max(RMHCMen),
               RMPendularGirls = max(RMPendularGirls),
               RMPendularBoys = max(RMPendularBoys),
               RMPendularWomen = max(RMPendularWomen),
               RMPendularMen = max(RMPendularMen),
               RMReturneesGirls = max(RMReturneesGirls),
               RMReturneesBoys = max(RMReturneesBoys),
               RMReturneesWomen = max(RMReturneesWomen),
               RMReturneesMen =max(RMReturneesMen),
               Consolidated_CVA_Beneficiaries = max(Consolidated_CVA_Beneficiaries))%>%
     # Sum values to determine the totals
     rowwise()%>%
     mutate(Consolidated_RMindestination = sum(RMDestGirls, RMDestBoys, RMDestWomen, RMDestMen, na.rm = TRUE),
            Consolidated_RM_in_transit = sum(RMTransitGirls, RMTransitBoys, RMTransitWomen, RMTransitMen, na.rm = TRUE),
            Consolidated_Host_Community = sum(RMHCGirls, RMHCBoys, RMHCWomen, RMHCMen, na.rm = TRUE),
            Consolidated_RM_Pendulars = sum(RMPendularGirls, RMPendularBoys, RMPendularWomen, RMPendularMen, na.rm = TRUE),
            Consolidated_Colombian_Returnees = sum(RMReturneesGirls, RMReturneesBoys, RMReturneesWomen, RMReturneesMen, na.rm = TRUE),
            Consolidated_Girls = sum(RMDestGirls, RMTransitGirls, RMHCGirls, RMPendularGirls, RMReturneesGirls, na.rm = TRUE),
            Consolidated_Boys = sum(RMDestBoys, RMTransitBoys, RMHCBoys, RMPendularBoys, RMReturneesBoys, na.rm = TRUE),
            Consolidated_Women = sum(RMDestWomen, RMTransitWomen, RMHCWomen, RMPendularWomen, RMReturneesWomen, na.rm = TRUE),
            Consolidated_Men = sum(RMDestMen, RMTransitMen, RMHCMen, RMPendularMen, RMReturneesMen, na.rm = TRUE),
            Consolidated_Other_under_18 = 0,
            Consolidated_Other_above_18 = 0)%>%
     # Drop extra column of breakdown values and reorder to match original format
     select(-(5:24))%>%
     # Get final Monthly Consolidated figures for Intersector
     mutate(Monthly_Consolidated = sum(Consolidated_RMindestination, Consolidated_RM_in_transit,
                                       Consolidated_Host_Community,Consolidated_RM_Pendulars,
                                       Consolidated_Colombian_Returnees, na.rm = TRUE))%>%
     ungroup()
   
   # Step 2: Protection (General) data
   consCSstep2 <- consallsectors%>%
     filter(Subsector== "Protection (General)")%>%
     group_by(Country, Admin1, Month)%>%
     summarise(Subsector = "Intersector",
               Monthly_Consolidated = sum(Monthly_Consolidated),
               Consolidated_RMindestination = sum( Consolidated_RMindestination), 
               Consolidated_RM_in_transit = sum(Consolidated_RM_in_transit),
               Consolidated_Host_Community = sum(Consolidated_Host_Community),
               Consolidated_RM_Pendulars = sum(Consolidated_RM_Pendulars),
               Consolidated_Colombian_Returnees = sum(Consolidated_Colombian_Returnees),
               Consolidated_Girls = sum(Consolidated_Girls), 
               Consolidated_Boys = sum(Consolidated_Boys),
               Consolidated_Women = sum( Consolidated_Women),
               Consolidated_Men = sum(Consolidated_Men),
               Consolidated_Other_under_18 = sum(Consolidated_Other_under_18),
               Consolidated_Other_above_18 = sum(Consolidated_Other_above_18),
               Consolidated_CVA_Beneficiaries = sum(Consolidated_CVA_Beneficiaries))
   
   # Step 3: sum data for all the other sector: Integration, Multipurporse CBI, Health, Education
   consCSstep3 <- consallsectors%>%
     filter(Subsector %in% c("Multipurpose Cash Assistance (MPC)", "Integration", "Health", "Education"))%>%
     group_by(Country, Admin1, Month)%>%
     summarise(Subsector = "Intersector",
               Monthly_Consolidated = sum(Monthly_Consolidated),
               Consolidated_RMindestination = sum( Consolidated_RMindestination), 
               Consolidated_RM_in_transit = sum(Consolidated_RM_in_transit),
               Consolidated_Host_Community = sum(Consolidated_Host_Community),
               Consolidated_RM_Pendulars = sum(Consolidated_RM_Pendulars),
               Consolidated_Colombian_Returnees = sum(Consolidated_Colombian_Returnees),
               Consolidated_Girls = sum(Consolidated_Girls), 
               Consolidated_Boys = sum(Consolidated_Boys),
               Consolidated_Women = sum( Consolidated_Women),
               Consolidated_Men = sum(Consolidated_Men),
               Consolidated_Other_under_18 = sum(Consolidated_Other_under_18),
               Consolidated_Other_above_18 = sum(Consolidated_Other_above_18),
               Consolidated_CVA_Beneficiaries = sum(Consolidated_CVA_Beneficiaries))
   # Merge the 3 separate steps in a single file and sum the beneficiaries for intersector at Admin1
   consCSadmin1 <- rbind(consCSstep1, consCSstep2, consCSstep3)%>%
     group_by(Country, Admin1, Month)%>%
     summarise(Subsector = "Intersector",
               Monthly_Consolidated = sum(Monthly_Consolidated),
               Consolidated_RMindestination = sum( Consolidated_RMindestination), 
               Consolidated_RM_in_transit = sum(Consolidated_RM_in_transit),
               Consolidated_Host_Community = sum(Consolidated_Host_Community),
               Consolidated_RM_Pendulars = sum(Consolidated_RM_Pendulars),
               Consolidated_Colombian_Returnees = sum(Consolidated_Colombian_Returnees),
               Consolidated_Girls = sum(Consolidated_Girls), 
               Consolidated_Boys = sum(Consolidated_Boys),
               Consolidated_Women = sum( Consolidated_Women),
               Consolidated_Men = sum(Consolidated_Men),
               Consolidated_Other_under_18 = sum(Consolidated_Other_under_18),
               Consolidated_Other_above_18 = sum(Consolidated_Other_above_18),
               Consolidated_CVA_Beneficiaries = sum(Consolidated_CVA_Beneficiaries))

   consCSAdminFull <- rbind(consCSadmin1, consallsectors)
   
  # Get country level figures
   consCScountry <- consCSAdminFull %>%
     group_by(Country, Month, Subsector)%>%
     summarise(Admin1 = "Country level",
               Monthly_Consolidated = sum(Monthly_Consolidated),
               Consolidated_RMindestination = sum(Consolidated_RMindestination),
               Consolidated_RM_in_transit = sum(Consolidated_RM_in_transit),
               Consolidated_Host_Community = sum(Consolidated_Host_Community),
               Consolidated_RM_Pendulars = sum(Consolidated_RM_Pendulars),
               Consolidated_Colombian_Returnees = sum(Consolidated_Colombian_Returnees),
               Consolidated_Girls = sum(Consolidated_Girls),
               Consolidated_Boys = sum(Consolidated_Boys),
               Consolidated_Women = sum(Consolidated_Women),
               Consolidated_Men = sum(Consolidated_Men),
               Consolidated_Other_under_18 = sum(Consolidated_Other_under_18),
               Consolidated_Other_above_18 = sum(Consolidated_Other_above_18),
               Consolidated_CVA_Beneficiaries = sum(Consolidated_CVA_Beneficiaries))
   # Merge admin1 and country level figures final file
   consfinal <- rbind(consCSAdminFull, consCScountry)
}

##### Back to common consolidation process #############

  # Merge the tables before merging to template and final cleaning
   
   consfullmodel <- consfinal %>%
     full_join(finalmonthlytotal, by = c("Country", "Admin1", "Month", "Subsector"))

# For countries with no admin1, filter out to only keep the "Country level" data

countrynoadmin1 <- c("Aruba", "Curaçao", "Costa Rica", "Dominican Republic", "Trinidad and Tobago", "Guyana", 
                     "Mexico", "Panama")

if(countryname %in% countrynoadmin1){
  consfullmodel <- consfullmodel%>%
    filter(Admin1 == "Country level")
} else {
  consfullmodel <- consfullmodel
 }

  # Merge with template
   consolidated_report <- dftemplate %>%
     full_join(consfullmodel, by = c("Country", "Admin1", "Month", "Subsector"))%>%
     mutate_if(is.numeric, ~replace(., is.na(.), 0))
   
  # Do the cumulative sum for the consolidated columns
   
   FinalConsolidated <- consolidated_report%>%
     group_by(Country, Admin1, Subsector)%>%
     arrange(Country, Admin1, Month)%>%
     mutate('Consolidated Total' = cumsum(Monthly_Consolidated),
            'Consolidated In Destination' = cumsum(Consolidated_RMindestination),
            'Consolidated In Transit' = cumsum(Consolidated_RM_in_transit),
            'Consolidated Host Communities' = cumsum(Consolidated_Host_Community),
            'Consolidated Pendular' = cumsum(Consolidated_RM_Pendulars),
            'Consolidated Returnees' = cumsum(Consolidated_Colombian_Returnees),
            'Consolidated Girls' = cumsum(Consolidated_Girls),
            'Consolidated Boys' = cumsum(Consolidated_Boys),
            'Consolidated Women' = cumsum(Consolidated_Women),
            'Consolidated Men' = cumsum(Consolidated_Men),
            'Consolidated Other under 18' = cumsum(Consolidated_Other_under_18),
            'Consolidated Other above 18' = cumsum(Consolidated_Other_above_18),
            'Consolidated CVA Beneficiaries' = cumsum(Consolidated_CVA_Beneficiaries))%>%
     rowwise()%>%
     # Add 3 columns to check if the breakdowns are correct. 
     mutate('Check PopType Breakdown' = ifelse(`Consolidated Total` == 0 | (`Consolidated Total` > 0 & 
                                                                              `Consolidated Total` == sum(`Consolidated In Destination`,
                                                                                                          `Consolidated In Transit`,
                                                                                                          `Consolidated Host Communities`,
                                                                                                          `Consolidated Pendular`,
                                                                                                          `Consolidated Returnees`,
                                                                                                          na.rm = TRUE)), "Ok", "Check Population Type breakdown"),
            'Check AGD Breakdown' = ifelse(`Consolidated Total` == 0 | (`Consolidated Total` > 0 & 
                                                                          `Consolidated Total`== sum(`Consolidated Girls`,
                                                                                                     `Consolidated Boys`,
                                                                                                     `Consolidated Women`,
                                                                                                     `Consolidated Men`,
                                                                                                     `Consolidated Other under 18`,
                                                                                                     `Consolidated Other above 18`,
                                                                                                     na.rm = TRUE)), "Ok", "Check AGD breakdown"),
            'Check CVA higher Total' = ifelse(`Consolidated CVA Beneficiaries` > `Consolidated Total`, "CVA Value higher than total", "ok"))%>%
     ungroup()%>%
     select(Platform,
            Country,
            Admin1,
            Month,
            Subsector,
            `Monthly Total Beneficiaries`,
            `Monthly CVA Beneficiaries`,
            `Consolidated Total`,
            `Consolidated In Destination`,
            `Consolidated In Transit`,
            `Consolidated Host Communities`,
            `Consolidated Pendular`,
            `Consolidated Returnees`,
            `Consolidated Girls`,
            `Consolidated Boys`,
            `Consolidated Women`,
            `Consolidated Men`,
            `Consolidated Other under 18`,
            `Consolidated Other above 18`,
            `Consolidated CVA Beneficiaries`,
            `Check PopType Breakdown`, 
            `Check AGD Breakdown`,
            `Check CVA higher Total`)
            
   # Print file
   write_xlsx(FinalConsolidated, './out/RMRP_2022_AI_consolidated.xlsx')
   
   rm(monthlist,
      dftemplate,
      dfproportions,
      monthlysectors,
      monthlytotal,
      monthly,
      CVAmonthlysectors,
      CVAmonthlytotal,
      CVAmonthly,
      finalmonthlyadm1,
      finalmonthcountry,
      finalmonthlytotal,
      conssectors,
      consCVAsectors,
      consallsectors,
      consolidated_report,
      countrynoadmin1,
      consfullmodel,
      consfinal)
   
   ## SHINY
   return_data$ConsolidatedReport <- FinalConsolidated
   return(return_data)
 
   
}  
     
     
     