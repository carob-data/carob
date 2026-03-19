# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
An integrated approach for understanding the factors that facilitate or constrain the adoption of soil carbon enhancing practices in East Africa, specifically Western Kenya

The survey data on soil carbon enhancing practices in western Kenya is systematically organized in Microsoft Excel tables. The data entails general household characteristics, plot characteristics, practices implemented, yield, inputs, livestock ownership, social capital, access to credit, access to extension services and sources of income.
"


	uri <- "doi:10.7910/DVN/HE6CEM"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIAT; UNR",# University of Nairobi
		publication = NA,
		project = NA,
		carob_date = "2026-03-12",
		design = "universe: The survey was carried out in the Western Kenya in Kakamega and Vihiga counties. The counties are characterized as high agricultural area but are of low agricultural productivity as a result of poor farming techniques, low soil fertility, soil erosion and degradation. The counties are also classified by high population density and high poverty rates. Vihiga is the most densely populated county in Kenya with 1044 persons per km2 and Kakamega county at 682 persons per km2. Approximately 51% and 41% of the population live below the poverty line in Kakamega and Vihiga county respectively compared to an average of 39%. The main source of livelihood is agriculture. Cropping is also widely practiced in the region with most farmers owning between 1-2 acres of land or less. Some of the crop species grown include maize, beans, sweet potatoes, local vegetables, cassava, sorghum, millet, sugarcane and tea. The snowballing technique was used to generate a sample of 334 farmers.",
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none", 
		carob_contributor = "Cedric Ngakou",
		completion = 70,	
		notes = NA
	)
	

	f1 <- ff[basename(ff) == "Adoption_of_Soil_Carbon_Practices"]

	r1 <- haven::read_dta(paste(f1, "Stata Data", "Crop_Output.dta", sep = "/"))|> carobiner:::unlabel()
	r2 <- haven::read_dta(paste(f1, "Stata Data", "Demographic.dta", sep = "/"))|> carobiner:::unlabel()
	r3 <- haven::read_dta(paste(f1, "Stata Data", "Input_Usage.dta", sep = "/"))|> carobiner:::unlabel()
	r4 <- haven::read_dta(paste(f1, "Stata Data", "Main_File.dta", sep = "/"))|> carobiner:::unlabel()
	r5 <- haven::read_dta(paste(f1, "Stata Data", "Marketing.dta", sep = "/"))|> carobiner:::unlabel()
	r6 <- haven::read_dta(paste(f1, "Stata Data", "Plot_Information.dta", sep = "/"))|> carobiner:::unlabel()
	r7 <- haven::read_dta(paste(f1, "Stata Data", "Residue_Use.dta", sep = "/"))|> carobiner:::unlabel()

### process files	
		
d1 <- data.frame(
  crop = tolower(r1$Crop),
  plot_id = r1$Plot_ID,
  yields1 = r1$YieldS1,
  yields2 = r1$YieldS2,
  trial_id = r1$KEY
)

dd <- reshape(d1, varying = c("yields1", "yields2"), v.names = "yield",
              direction = "long",
              timevar = "season",
              times = c("s1", "s2"))
dd$id <- NULL
dd$record_id <- as.integer(1:nrow(dd))

###########
# EGB:
# # Household information: Keeping only the household head
# # How do we keep also other household members but avoid duplicates?
d2 <- data.frame(
  hhid = as.character(r2$Household_Names[r2$HH_Relation == "Household Head"]),
  farmer_gender = r2$Gender[r2$HH_Relation == "Household Head"],
  farmer_age = r2$Age[r2$HH_Relation == "Household Head"],
  farmer_education_level = r2$Education_Level[r2$HH_Relation == "Household Head"],
  trial_id = r2$KEY[r2$HH_Relation == "Household Head"]
)

d <- merge(dd, d2, by = intersect(names(d2), names(dd)), all = TRUE) 
d <- d[!duplicated(d$record_id),]

###
d3 <- data.frame(
  plot_id = as.character(r3$Plot_ID),
  fertilizer_used = grepl("Yes", r3$Use_Inorganic),
  fertilizer_type = paste(r3$Typ_Fertilizer1, r3$Typ_Fertilizer2, r3$Typ_Fertilizer3, r3$Typ_Fertilizer4, sep = ";" ),
  fertilizer_amount = r3$TT_Fertilizer_Used,
  #r3$Source1,
  #r3$Source2,
  fertilizer_cost = r3$TT_Cost_Fertilizer,
  OM_used = grepl("Yes",r3$Manure_USe),
  OM_amount = r3$Manure_Used,
  #r3$Source_Manure,
  OM_price = r3$Manure_Price,
  trial_id = r3$KEY
)

# EGB:
# # Remove trailing NA
d3$fertilizer_type <- sapply(strsplit(d3$fertilizer_type, ";"), function(x) {
  x <- x[x != "NA"]
  paste(x, collapse = ";")
})
d3$N_fertilizer <- d3$P_fertilizer <- d3$N_fertilizer <- NA
# # Based on the total fertilizer amount, we can make assumptions on the application per fertilizer_type
d3$n <- ifelse(d3$fertilizer_type == "" | is.na(d3$fertilizer_type), 0, lengths(strsplit(d3$fertilizer_type, ";")))
d3$bag_wgt <- d3$fertilizer_amount / d3$n
d3$N_fertilizer[!grepl("NPK", d3$fertilizer_type)] <- d3$bag_wgt 
# # If only single source, we can calculate the elemental N, P and K
d3$N_fertilizer[d3$fertilizer_type == "Urea"] <- d3$bag_wgt[d3$fertilizer_type == "Urea"] * 0.46
d3$N_fertilizer[d3$fertilizer_type == "CAN"] <- d3$bag_wgt[d3$fertilizer_type == "CAN"] * 0.26
d3$N_fertilizer[d3$fertilizer_type == "DAP"] <- d3$bag_wgt[d3$fertilizer_type == "DAP"] * 0.18
d3$P_fertilizer[d3$fertilizer_type == "DAP"] <- d3$bag_wgt[d3$fertilizer_type == "DAP"] * 0.2
# # For multiple fertilizers
d3$N_fertilizer[d3$fertilizer_type == "CAN;DAP;Urea"] <- (d3$bag_wgt[d3$fertilizer_type == "CAN;DAP;Urea"] * 0.26) + (d3$bag_wgt[d3$fertilizer_type == "CAN;DAP;Urea"] * 0.18) + (d3$bag_wgt[d3$fertilizer_type == "CAN;DAP;Urea"] * 0.46)
d3$N_fertilizer[d3$fertilizer_type == "CAN;DAP;Urea;Liming"] <- (d3$bag_wgt[d3$fertilizer_type == "CAN;DAP;Urea;Liming"] * 0.26) + (d3$bag_wgt[d3$fertilizer_type == "CAN;DAP;Urea;Liming"] * 0.18) + (d3$bag_wgt[d3$fertilizer_type == "CAN;DAP;Urea;Liming"] * 0.46)
d3$N_fertilizer[d3$fertilizer_type == "CAN;DAP"] <- (d3$bag_wgt[d3$fertilizer_type == "CAN;DAP"] * 0.26) + (d3$bag_wgt[d3$fertilizer_type == "CAN;DAP"] * 0.18)
d3$N_fertilizer[d3$fertilizer_type == "DAP;Urea"] <- (d3$bag_wgt[d3$fertilizer_type == "DAP;Urea"] * 0.18) + (d3$bag_wgt[d3$fertilizer_type == "DAP;Urea"] * 0.46)
d3$N_fertilizer[d3$fertilizer_type == "CAN;Urea"] <- (d3$bag_wgt[d3$fertilizer_type == "CAN;Urea"] * 0.26) + (d3$bag_wgt[d3$fertilizer_type == "CAN;Urea"] * 0.46)
# # Lime
d3$lime_used <- grepl("Lim", d3$fertilizer_type)
d3$lime[d3$lime_used == TRUE] <- d3$bag_wgt[d3$lime_used == TRUE]
# # Remove unneeded cols
d3$n <- d3$bag_wgt <- NULL

### merge d and d3

d <- merge(d, d3, by = intersect(names(d), names(d3)), all= TRUE)

####
d4 <- data.frame(
  planting_date = as.character(r4$Date),
  adm1 = r4$County,
  adm2 = r4$Sub_county,
  adm3 = trimws(r4$Location),
  location = r4$Sub_location,
  #r4$resp_name,
  residue_used = grepl("Yes",r4$Use_Residue),
  land_prep_method = ifelse(grepl("Yes", r4$Minimum_Tillage)& !grepl("Yes", r4$Grass_Strips), "minimum tillage",
                     ifelse(grepl("Yes", r4$Grass_Strips) & !grepl("Yes", r4$Minimum_Tillage), "strip tillage", 
                     ifelse(grepl("Yes", r4$Grass_Strips) & grepl("Yes", r4$Minimum_Tillage), "minimum tillage;strip tillage", "none"))),
  
  OM_type = ifelse(grepl("Yes", r4$CompostManure)& !grepl("Yes", r4$FarmYardManure), "compost",
            ifelse(grepl("Yes", r4$FarmYardManure) & !grepl("Yes", r4$CompostManure), "farmyard manure",
            ifelse(grepl("Yes", r4$FarmYardManure) & grepl("Yes", r4$CompostManure), "compost;farmyard manure", "none"))),
  trial_id = r4$KEY
)

### merge d and d4

d <- merge(d, d4, by = intersect(names(d), names(d4)), all= TRUE)

### 
d5 <- data.frame(
  crop = tolower(r5$Crop_Sold),
  yield_marketable = r5$Sold_Quantity,
  crop_price = r5$Income_Crops,
  trial_id = r5$KEY
  
)

### merge d and d5

d <- merge(d, d5, by = intersect(names(d), names(d5)), all= TRUE)

#### ########
d6 <- data.frame(
  plot_id = as.character(r6$Plot_ID),
  plot_area = r6$Plot_Size*0.4047, # ha
  land_tenure = r6$Tenure,
  #plot_fertility = r6$Plot_Fertility,
  plot_slope = r6$Plot_Slope,
  soil_texture = gsub("sandy", "sand", tolower(r6$Soil_Type)),
  #soil_erosion = r6$Soil_Erosion,
  trial_id = r6$KEY
)

### merge d and d6

d <- merge(d, d6, by = intersect(names(d), names(d6)), all= TRUE)

###	######
d7 <- data.frame(
  residue_prevcrop = r7$Proportion_Left,
  trial_id = r7$KEY
)

### merge d and d7
d <- merge(d, d7[!duplicated(d7$trial_id),], by = intersect(names(d), names(d7)), all.x = TRUE)

d$crop_price <- ifelse(d$yield!=0, d$crop_price/d$yield, NA)  # KES/kg
d$yield <- d$yield/d$plot_area ##m2
d$yield_marketable <- d$yield_marketable/d$plot_area
d$plot_area <- d$plot_area*10000
d$fertilizer_price <- as.character(d$fertilizer_cost/d$fertilizer_amount)
d$fertilizer_cost <- NULL
### adding Lon and lat coordinate

geo <- data.frame(
  location = c("Mayoni", "Shieywe","Ivola", "Makale","Ebukhala", "Chagenda", "Tombo", "Kamasha", "Kidundu", "Bukulunya", "Ekomero", "Eshirombe", "Mambai", "Emusutswi", "Masana", "Emukaya", "Emmutsa", "Ebusiralo", "Kamoi","Chepkwekwen", "Maraba", "Matioli", "Mukaya", "Malekha", "Munganga","Matungu", "Shikomoli", "Gisambayi", "Ikumba", "Imanda", "Makunga", "Shivanga", "Sirungai", "Ebusundi", "Wodanga", "Eluche","Ebukutenga", "Shianda", "Galona", "Municipality", "Khaimba", "Eluanga", "Shibiriri", "Emusonga", "Wekhomo", "Hamutua", "Izava north"),
  longitude = c(34.4433, 34.7491, 34.7819, 34.6303,34.607, 34.6777,34.827, 34.506, 34.7064, 34.742, 34.870, 34.5854, 34.7847, 34.6703, 34.7089, 34.621,  34.611, 34.608, 35.252,35.452, 34.870, 34.6141, 34.681, 34.789, 38.1036, 34.4620, 34.784, 34.7494, 34.7300, 37.8961, 34.6006, 34.838, 34.930, 34.635, 34.7884, 34.5296,34.5199,34.569, 34.7615, 34.750, 34.6096, 36.579, 34.4877, 34.7518, 34.632, 34.83005,  34.7407),
  latitude = c(0.3815, 0.30347, 0.04150, 0.254, 0.007544,0.0004058, 0.5610, 0.29266, 0.0489, 0.107, 0.5202, 0.1834, 0.0910, 0.1168, -0.0030, 0.2329, 0.0196, 0.0303, 1.0782,1.0301, 0.00345, 0.2052, 0.0563,0.458, -1.221, 0.389, 0.0518, 0.05249, 0.0551, 1.1865, 0.2913, 0.552, 0.5495, 0.0288, 0.1103, 0.3346,0.1301,0.3215, 0.0485, 0.29694, 0.3320, -1.8836, 0.3334, 0.28295, 0.0311, 0.5702, 0.1021)
)

d <-  merge(d, geo, by= "location", all.x = TRUE)

i <- is.na(d$longitude) & grepl("Kakamega", d$adm1)
d$longitude[i] <- 34.751
d$latitude[i] <- 0.2823

i <- is.na(d$longitude) & grepl("Vihiga", d$adm1)
d$longitude[i] <-  34.695
d$latitude[i] <- 0.0404

### fixing fertilizer_type

d$fertilizer_type <- gsub(";NA|NA;|;Dont Know", "", d$fertilizer_type) 
d$fertilizer_type <- gsub("Liming", "lime", d$fertilizer_type) 
d$fertilizer_type <- gsub("Urea", "urea", d$fertilizer_type) 
d$fertilizer_type <- gsub("Dont Know|NA", NA, d$fertilizer_type) 

#### Fixing crop 
P <- carobiner::fix_name(d$crop)
P <- gsub("beans", "common bean", P)
P <- gsub("ground nuts", "groundnut", P)
P <- gsub("bananas", "banana", P)
P <- gsub("sweat potatoes", "sweetpotato", P)
P <- gsub("irish potatoes", "potato", P)
P <- gsub("cow peas", "cowpea", P)
P <- gsub("african leafy vegetables", "vegetable", P)
P <- gsub("fruit trees", "unknown", P)
P <- gsub("sukuma wiki", "unknown", P)
P <- gsub("nappier grass", "napier grass", P)
P <- gsub("cabbages", "cabbage", P)
d$crop <- P 


d$country <- "Kenya"
d$currency = "KES"
d$on_farm <- FALSE
d$is_survey <- TRUE
d$geo_from_source <- FALSE
d$yield_part <- "none"
d$yield_moisture <- as.numeric(NA)
d$yield_isfresh <- NA
d$irrigated <- NA
d$trial_id <- paste(d$trial_id, d$season, sep = "-")
d$season <- NULL 
#### remove rows with missing location (adm1,adm2, adm3, location)

d <- d[!is.na(d$adm1),]
## remove rows with duplicate record_id
d <- d[!duplicated(d$record_id)& !is.na(d$record_id),]

carobiner::write_files(path, meta, d)
}


