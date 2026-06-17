# R script for "carob"

# ## ISSUES 

#RH: this is 


# ....


carob_script <- function(path) {
  
  "
• APSIM is an internationally well-recognized cropping systems models for scientists. An SRFSI team conducted many on-station trials in partners’ research station, which managed by SRFSI NARES partners’ scientists for validation of APSIM. The models simulated biophysical performances on the systems on a daily basis in terms of crop yields, water use, soil dynamics, climate, the fate of nutrients, and residues management. • APSIM was regionally validated using also results from on-farm experiments; validated models were used to explore the profitability, sustainability, and risk reduction potential of technological options under different contemporary and future climate scenarios. Using actual, historical, and predicted sets of climate data, the model was used to explore technology effects, sustainability, and riskiness over longer time periods than possible for field results in a 4-year project. Researchers from the region developed expertise in the use of the APSIM model, and the modeling effort was supported by CSIRO scientists

"
  
  uri <- "hdl:11529/10548052"
  group <- "simulation"
  ff <- carobiner::get_data(uri, path, group)
 
  meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1,
    project=NA,
    ## if there is a paper, include the paper's doi here
    ## also add a RIS file in references folder (with matching doi)
    publication= NA,
    data_organization = "CIMMYT",
    data_type="on_station",
    carob_contributor="Fredy Chimire",
    carob_effort = NA,
    carob_date="2023-09-30"
  )
  
  
  
  
  f <- ff[basename(ff) == "APSIM-IrrXN- rabi maize 2015-16-SRFSI-OFRD-Rangpur.xlsx"]
  f1 <- ff[basename(ff) == "APSIM-IrrXN rabi maize 2016-17-SRFSI-OFRD-Rangpur.xlsx"]
  f2 <- ff[basename(ff) == "APSIM-IrrXN- Rice 2016-SRFSI-OFRD-Rangpur.xlsx"]
  f3 <- ff[basename(ff) == "APSIM-IrrXN-rabi maize 2014-15-SRFSI-OFRD-Rangpur.xlsx"]
  
  
  
  # Select sheet with revised data from the excel file 
  r <- carobiner::read.excel(f, sheet = "Stat")
  r1 <- carobiner::read.excel(f1, sheet = "Stat")
  r2 <- carobiner::read.excel(f2, sheet = "Stat")
  r3 <- carobiner::read.excel(f3, sheet = "Stat")
   
  #### about the data #####
  ## (TRUE/FALSE)
  
  d <- data.frame(plant_height = r$`Plant height`, yield = r$`Grain Yield`*1000)
  d1 <- data.frame(plant_height = r1$`Plant height`, yield = r1$`Grain Yield`*1000)
  d2 <- data.frame(plant_height = r2$`Plant height(cm)`, yield = r2$`Grain Yield`*1000)
  d3 <- data.frame(plant_height = r3$`Plant height`, yield = r3$`Grain Yield`*1000)
  
  
  # for first dataset
  
  d$on_farm <- FALSE
  d$is_survey <- FALSE
  d$irrigated <- r1$Irrigation=="I1"
  
   # For second dataset
  d1$on_farm <- FALSE
  d1$is_survey <- FALSE
  d1$irrigated <- r1$Irrigation!="I1" # I1 corresponds to T5 which has 0 irrigations
  ## the treatment code	

  # For Third dataset
  d2$on_farm <- FALSE
  d2$is_survey <- FALSE
  d2$irrigated <- r2$Irrigation!="T5" #  T5 has 0 irrigations
  
  # For 4th  dataset
  d3$on_farm <- FALSE
  d3$is_survey <- FALSE
  d3$irrigated <- r3$Irrigation!="I1" #  I1 has 0 irrigations
  
  
	d$treatment <- 
		ifelse(r$Irrigation == "I5",
             "CT Maize (6 irrigations) (irrig at V4, V8, V12, tasseling, milking, soft-dough)",
        ifelse(r$Irrigation == "I4",
             "ST Maize (6 irrigations) (irrig at V4, V8, V12, tasseling, milking, soft-dough)",
        ifelse(r$Irrigation == "I3",
             "ST Maize (4 irrigations) (irrig at V6, V10, tasseling, soft-dough)",
        ifelse(r$Irrigation == "I2",
              "ST Maize (2 irrigations) (irrig at V6, tasseling)",
              "ST Maize (0 irrigations)"
         )))
	)

	
	d1$treatment <- 
	  ifelse(r1$Irrigation == "I5",
	         "CT Maize (6 irrigations) (irrig at V4, V8, V12, tasseling, milking, soft-dough)",
	         ifelse(r1$Irrigation == "I4",
	                "ST Maize (6 irrigations) (irrig at V4, V8, V12, tasseling, milking, soft-dough)",
	                ifelse(r1$Irrigation == "I3",
	                       "ST Maize (4 irrigations) (irrig at V6, V10, tasseling, soft-dough)",
	                       ifelse(r1$Irrigation == "I2",
	                              "ST Maize (2 irrigations) (irrig at V6, tasseling)",
	                              "ST Maize (0 irrigations)"
	                       )))
	  )
	
	
# Description of treatment available in sheet name='Input'
	d2$treatment <- 
	  ifelse(r2$Irrigation == "T1",
	         "CT Maize (6 irrigations)-Transplanted rice (common irrigation)",
	         ifelse(r2$Irrigation == "T2",
	                "ST Maize (6 irrigations)-Direct seeded rice (common irrigation)",
	                ifelse(r2$Irrigation == "T3",
	                       "ST Maize (4 irrigations)-Direct seeded rice (common irrigation)",
	                       ifelse(r2$Irrigation == "T4",
	                              "ST Maize (2 irrigations)-Direct seeded rice (common irrigation)",
	                              "STMaize (0 irrigations)-Direct seeded rice (common irrigation)"
	                       )))
	  )
	
	
	d3$treatment <- ifelse(r3$Irrigation == "I5",
	         "CT Maize (6 irrigations) (irrig at V4, V8, V12, tasseling, milking, soft-dough)",
	         ifelse(r3$Irrigation == "I4",
	                "ST Maize (6 irrigations) (irrig at V4, V8, V12, tasseling, milking, soft-dough)",
	                ifelse(r3$Irrigation == "I3",
	                       "ST Maize (4 irrigations) (irrig at V6, V10, tasseling, soft-dough)",
	                       ifelse(r3$Irrigation == "I2",
	                              "ST Maize (2 irrigations) (irrig at V6, tasseling)",
	                              "ST Maize (0 irrigations)"
	                       )))
	  )
   ##### Location #####
  ## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
  ## you can use carobiner::fix_name()
  d$country <- "Bangladesh"
	d1$country <- "Bangladesh"
	d2$country <- "Bangladesh"
	d3$country <- "Bangladesh"
	
  
  #d$trial_id <- d$`HH-ID`
  d$latitude <- 25.72093 # Gps are available from General and Phenology sheet
  d$longitude <- 89.26442
  d$planting_date <- "2016-11-23"
  d$yield_part <- "grain"
  d$variety <-  "Kavery25K60"
  d$crop <- "maize"
  d$rep <- as.integer(sub("R", "", r$Replication))
  d$N_fertilizer <- ifelse(r$Nitrogen=="N1",0,ifelse(r$Nitrogen=="N2",125,250))
  
  d$P_fertilizer <- 75
  d$K_fertilizer <- 80
  d$S_fertilizer <- 52
  d$B_fertilizer <- 1.5
  d$Zn_fertilizer <- 3
  d$Mg_fertilizer <- 10
  
  # Standardised dataset for columns dataframe d1

  d1$latitude <- 25.72093 # Gps are available from General and Phenology sheet
  d1$longitude <- 89.26442
  d1$planting_date <- "2015-11-23"
  d1$yield_part <- "grain"
  d1$variety <-  "Kavery25K60"
  d1$crop <- "maize"
  d1$rep <- as.integer(sub("R", "", r1$Replication))
  d1$N_fertilizer <- ifelse(r1$Nitrogen=="N1",0,ifelse(r1$Nitrogen=="N2",125,250))
  d1$P_fertilizer <- 75
  d1$K_fertilizer <- 80
  d1$S_fertilizer <- 52
  d1$B_fertilizer <- 1.5
  d1$Zn_fertilizer <- 3
  d1$Mg_fertilizer <- 10
  
  
  
  # Standardised dataset for columns dataframe d2
  
  d2$latitude <- 25.72093 
  d2$longitude <- 89.26442
  d2$planting_date <- "2016-07-19"
  d2$transplanting_date <- "2016-08-10"
  d2$harvest_date <- "2016-10-24"
  d2$yield_part <- "grain"
  d2$variety <-  "BRRI dhan62"
  d2$crop <- "rice"
  d2$rep <- as.integer(sub("R", "", r2$Replication))
  d2$N_fertilizer <- ifelse(r2$Nitrogen=="N1",0,ifelse(r2$Nitrogen=="N2",34.5,69))
  d2$P_fertilizer <- 14.25
  d2$K_fertilizer <- 35.63
  d2$S_fertilizer <- 6.6
  d2$Zn_fertilizer <- 4 # from sheet name=input
  
  
  # Standardised dataset for columns dataframe d3
  d3$latitude <- 25.72093 # Gps are available from General and Phenology sheet
  d3$longitude <- 89.26442
  d3$planting_date <- "2014-12-04"
  d3$yield_part <- "grain"
  d3$variety <-  "Kavery25K60"
  d3$crop <- "maize"
  d3$rep <- as.integer(sub("R", "", r3$Replication))
  d3$N_fertilizer <- ifelse(r3$Nitrogen=="N1",0,ifelse(r3$Nitrogen=="N2",125,250))
  d3$P_fertilizer <- 75
  d3$K_fertilizer <- 80
  d3$S_fertilizer <- 52
  d3$B_fertilizer <- 1.5
  d3$Zn_fertilizer <- 3
  d3$Mg_fertilizer <- 10
  d3$harvest_date <- "2015-05-19"
  
  
  
  

print("many more variables need to be included")  
 
#  carobiner::write_files(meta, d, path=path)
 # carobiner::write_files(meta, d1, path=path)
  #carobiner::write_files(meta, d2, path=path)
  
  
}
