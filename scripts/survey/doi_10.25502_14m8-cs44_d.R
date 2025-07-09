# R script for "carob"


# RH: not included because there are no yield data

"Title: N2Africa farm monitoring - Malawi, 2011 - 2012

Description: N2Africa is to contribute to increasing biological nitrogen fixation and productivity of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility, improving household nutrition and increasing income levels of smallholder farmers. As a vision of success, N2Africa will build sustainable, long-term partnerships to enable African smallholder farmers to benefit from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants and fertilizers adapted to local settings. A strong national expertise in grain legume production and N2-fixation research and development will be the legacy of the project.The project is implemented in five core countries (Ghana, Nigeria, Tanzania, Uganda and Ethiopia) and six other countries (DR Congo, Malawi, Rwanda, Mozambique, Kenya & Zimbabwe) as tier one countries.
"

carob_script <- function(path) {
  
	uri <- "doi:10.25502/14m8-cs44/d"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)
  
  ## data set level data
	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		project="N2Africa",
		publication=NA,
		data_organization = "IITA;ICRAF;WUR",
		carob_contributor="Rachel Mukami",
		carob_date="2022-09-26",
		data_type = "survey",
		response_vars = "none",
		treatment_vars = "none"
	)

	meta$dataset_id <- paste0(meta$dataset_id, "_nodata")
	carobiner::write_files(path, meta)
}

  
  
  # # reading the datasets
  # f <- ff[basename(ff) == "a_general.csv"]
  # d <- data.frame(read.csv(f))
  # d$trial_id <- d$farm_id
  # d$adm1 <- carobiner::fix_name(d$district, "title")
  # d$adm2 <- carobiner::fix_name(d$sector_ward, "title")
  # d$adm3 <- carobiner::fix_name(d$vilage, "title")
  # d$latitude <- d$gps_latitude
  # d$longitude <- d$gps_longitude
  # d$elevation <- as.numeric(d$gps_altitude)

  # d <- d[,c("trial_id","season","country","adm1","adm2","adm3","latitude","longitude","elevation")]
  
  # f1 <- ff[basename(ff) == "c_use_of_package_2.csv"]
  # d1 <- data.frame(read.csv(f1))
  # d1$trial_id <- d1$farm_id
  # d1$crop <- tolower(d1$crop_1)
  # d1$crop[d1$crop =="bush bean"] <- "common bean"
  # d1$crop[d1$crop == "sweet potato"] <- "sweetpotato"
  # d1$variety <- ifelse(d1$variety_1 == "", NA, d1$variety_1)
  # d1$inoculated <- ifelse(d1$inoculant_used == "Y", TRUE,
                          # ifelse(d1$inoculant_used == "N", FALSE, NA))
  # d1$OM_used <- ifelse(d1$organic_fert_type %in% 
		# c("Animal dung","Animal manure","Compost","Compost manure",
          # "Farmyard","Farm yard","Compost, animal manure"), TRUE,
                       # ifelse(d1$organic_fert_amount > 0, TRUE, FALSE))
  # # EGB: Fixing OM_type to make it NA where 0 OM applied
  # d1$OM_type <- ifelse(d1$organic_fert_amount <= 0, NA,
                # ifelse(d1$organic_fert_type %in% 
                # c("Animal dung","Animal manure","Compost","Compost manure", "Farmyard", "Farm yard", "Compost, animal manure"), d1$organic_fert_type,NA))
# #   d1$OM_type <- ifelse(d1$organic_fert_type %in% 
# # 			c("Animal dung","Animal manure","Compost","Compost manure",
# #               "Farmyard","Farm yard","Compost, animal manure"), d1$organic_fert_type,
# #              ifelse(d1$organic_fert_amount > 0, d1$organic_fert_type, "none"))
  # # EGB: Leave it as kilograms
  # d1$OM_amount <- as.numeric(d1$organic_fert_amount)
  # # d1$OM_amount <- as.numeric(d1$organic_fert_amount)*1000 # converting into g
  # d1$fertilizer_type <- NA
                               
  # d1$fertilizer_type[grepl("TSP", d1$mineral_fert_type)] <- "TSP"
  # d1$fertilizer_type[grepl("phosphate", d1$mineral_fert_type)] <- "SSP"
  # d1$fertilizer_type[grepl("D", d1$mineral_fert_type)] <- "D compound"
  # d1$fertilizer_type[grepl("Super d", d1$mineral_fert_type)] <- "D compound" # Super D and D Compound seem to be the same blend (https://agra.org/wp-content/uploads/2020/08/Malawi-Report_Assessment-of-Fertilizer-Distribution-Systems-and-Opportunities-for-Developing-Fertilizer-Blends.pdf)
  # d1$fertilizer_type[grepl("S-", d1$mineral_fert_type)] <- "S compound"
  # d1$fertilizer_type[grepl("UREA", d1$mineral_fert_type)] <- "urea" # Assigning all fertilizations with UREA to urea, since NPK is not a fertilizer...
  # d1$fertilizer_type[grepl("ympal", d1$mineral_fert_type, ignore.case = TRUE)] <- "sympal" # This is a fertilizer specific to Kenya. For more info read here: https://www.researchgate.net/profile/Charlotte-Schilt/publication/283304707_N2Africa_Final_Report_of_the_first_Phase_-_2009_-_2013/links/5d77729c4585151ee4ab2639/N2Africa-Final-Report-of-the-first-Phase-2009-2013.pdf
  # d1$fertilizer_type[d1$mineral_fert_amount > 0 & is.na(d1$fertilizer_type)] <- "unknown"
  
  # # Adjusting NPK amounts
  # d1$N_fertilizer <- ifelse(d1$fertilizer_type %in% c("urea"), d1$mineral_fert_amount * 0.46,  # assumption is that mineral fert amount is in kg
                     # ifelse(d1$fertilizer_type == "23:21:0+4S", d1$mineral_fert_amount * 0.23,
                     # ifelse(d1$fertilizer_type %in% c("D compound", "S compound"), d1$mineral_fert_amount * 0.08, NA)))
                     # # ifelse(d1$fertilizer_type %in% c("Super D","Super d"),
                                                 # # d1$mineral_fert_amount * 0.01,NA))))
  
  # d1$P_fertilizer <- ifelse(d1$fertilizer_type == "TSP", d1$mineral_fert_amount * 0.46*((2*31)/(2*31+5*16)),
                     # ifelse(d1$fertilizer_type == "sympal", d1$mineral_fert_amount * 0.23*((2*31)/(2*31+5*16)),
                     # ifelse(d1$fertilizer_type %in% c("urea","unknown","S compound"), d1$mineral_fert_amount * 0.21*((2*31)/(2*31+5*16)),
                     # ifelse(d1$fertilizer_type == "D compound", d1$mineral_fert_amount * 0.18*((2*31)/(2*31+5*16)),
                     # ifelse(d1$fertilizer_type == "SSP", d1$mineral_fert_amount * 0.145, NA)))))
                     # # ifelse(d1$fertilizer_type %in% c("Super D","Super d"), d1$mineral_fert_amount * 0.24*((2*31)/(2*31+5*16)),
                     # # ifelse(d1$fertilizer_type %in% c("Single super phosphate", "Single super phosphate "," Single Super phosphate","Super phosphate"), d1$mineral_fert_amount * 0.145,NA)))))) # takes into account atomic weight of both P and O
  
  # d1$K_fertilizer <- ifelse(d1$fertilizer_type == "sympal", d1$mineral_fert_amount * 0.15,
                     # ifelse(d1$mineral_fert_type == "S compound", d1$mineral_fert_amount * 0.07,
                     # ifelse(d1$fertilizer_type == "D compound", d1$mineral_fert_amount * 0.20, NA)))
  
  # d1 <- d1[,c("trial_id","crop","variety","inoculated","OM_used","OM_amount","OM_type","fertilizer_type","N_fertilizer","P_fertilizer","K_fertilizer")]
  
  
  # f2 <- ff[basename(ff) == "c_use_of_package_4.csv"]
  # d2 <- data.frame(read.csv(f2))
  # d2$trial_id <- d2$farm_id
  # d2$row_spacing <- as.numeric(d2$crop_1_spacing_row_to_row)
  # d2$plant_spacing <- as.numeric(d2$crop_1_spacing_plant_to_plant)
  # d2 <- d2[,c("trial_id","row_spacing","plant_spacing")] # assumption is spacing is in cm
  

  # f3 <- ff[basename(ff) == "d_cropping_calendar.csv"]
  # d3 <- data.frame(read.csv(f3))
  # d3$trial_id <- d3$farm_id
  # d3$planting_date <- as.character(as.Date(paste(d3$date_planting_mm,d3$date_planting_dd,d3$date_planting_yyyy,sep = "/"),"%m/%d/%Y"))
  # d3 <- d3[,c("trial_id","planting_date")]
  
  # ##  EGB: Removing seed_weight variable, since it is not the 1000 grain weight.
  # # f4 <- ff[basename(ff) == "e_harvest.csv"]
  # # d4 <- data.frame(read.csv(f4))
  # # d4 <- d4[d4$crop_1_grain_unshelled == "Y",] # getting grain weights for only unshelled grains
  # # d4$trial_id <- d4$farm_id
  # # # EGB: This is not the 1000 grain weight
  # # # d4$seed_weight <- (as.numeric(d4$crop_1_area_harvested)*d4$crop_1_weight_grain *1000)/100 # standardizing to grain weight measured in g/100m2
  # # d4 <- d4[,c("trial_id","seed_weight")]
  
  # f5 <- ff[basename(ff) == "g_farmer_assessment.csv"]  
  # d5 <- data.frame(read.csv(f5))
  # d5$trial_id <- d5$farm_id
  # d5$yield <- 0 # yield is has no quantitative figures
  # d5 <- d5[,c("trial_id","yield")] 
  
  # # combining into one dataset
  # ## RH not with rbind!!
  # ### z <- carobiner::bindr(d,d1,d2,d3,d4,d5)
  # ### please check if this is correct
  # z <- Reduce(function(...) merge(..., all=T), list(d,d1,d2,d3,d5))
  
# ### RH
 # ## z$is_survey <- "yes"
 # ## z$on_farm <- "no"
 # z$is_survey <- TRUE
 # ## it is a survey, so per definition on-farm. 
 # z$on_farm <- TRUE

# ### RH how can the lon/lat be the same for all locations?? Please fix
  # ##z$longitude <- 34.30153
  # ##z$latitude <- -13.25431

  # z$country <- "Malawi"
  # z$crop[is.na(z$crop)] <- "common bean" # filled NAs in crops with common bean
  
  # z <- z[,c("trial_id","season","country","adm1","adm2","adm3","latitude","longitude","elevation","crop","variety",
            # "planting_date","inoculated","OM_used","OM_type","OM_amount","fertilizer_type",
            # "N_fertilizer","P_fertilizer","K_fertilizer","yield","row_spacing","plant_spacing",
            # "on_farm","is_survey")]
  
  # carobiner::write_files(meta, z, path, dataset_id, group)
# }

