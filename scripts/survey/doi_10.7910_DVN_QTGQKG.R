# R script for "carob"
# license: GPL (>=3)

## ISSUES
### The raw data contain many unrecognized insecticide and herbicide names.

carob_script <- function(path) {

"
Nature+ Quantitative Baseline Survey, India

In 2023, the Nature Positive Solutions (Nature+) Baseline survey was conducted in India, specifically focusing on the districts of Ahmednagar, Nashik, and Nandurbar in Maharashtra. The primary objective of the study was to assess the socio-economic conditions and agricultural systems in these regions, establishing a baseline to guide ongoing Nature+ interventions.  The survey encompassed 1,228 smallholder farmer households, comprising 610 treated and 618 control households. Data collection utilized a two-stage sampling method and evaluated a range of variables, including socio-economic characteristics, agricultural practices, land use, nutrition, and the adoption of Nature+ practices. This comprehensive data will facilitate the assessment of Nature+ impacts on inclusion, poverty alleviation, food security, livelihoods, and environmental sustainability. In addition, the survey included interviews with 27 communities, gathering information on socio-demographic characteristics, access to essential services, land ownership patterns, agricultural practices, extension services, and community-wide shocks. All monetary values are reported in Indian Rupees (INR).
"

	uri <- "doi:10.7910/DVN/QTGQKG"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=3,
		data_organization = "IFPRI",
		publication = NA,
		project = NA,
		carob_date = "2026-03-24",
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none", 
		carob_contributor = "Cedric Ngakou",
		completion = 70,	
		notes = NA
	)
	

	r1 <- haven::read_dta(ff[basename(ff) == "SecA_Cover.dta"]) |> carobiner:::unlabel()
	r2 <- haven::read_dta(ff[basename(ff) == "SecB_Roster_&_Employment.dta"]) |> carobiner:::unlabel()
	r3 <- haven::read_dta(ff[basename(ff) == "SecE_Parcel.dta"]) |> carobiner:::unlabel()
	r4 <- haven::read_dta(ff[basename(ff) == "SecF_Crop_Area_Khalif.dta"]) |> carobiner:::unlabel()
	r5 <- haven::read_dta(ff[basename(ff) == "SecG_Crop_Area_Rabi.dta"]) |> carobiner:::unlabel()
	r6 <- haven::read_dta(ff[basename(ff) == "SecH_Crop_Area_Zabid.dta"]) |> carobiner:::unlabel()
	r7 <- haven::read_dta(ff[basename(ff) == "SecI_Crop_Area_Trees.dta"]) |> carobiner:::unlabel()
	r8 <- haven::read_dta(ff[basename(ff) == "SecK_Crop_Input_Output.dta"]) |> carobiner:::unlabel()
	r9 <- haven::read_dta(ff[basename(ff) == "SecU_WasteMangement.dta"]) |> carobiner:::unlabel()
	
	
	#####
	
	d1 <- data.frame(
	  hhid = as.character(r1$hhid),
	  adm1 = r1$A1_District,
	  adm2 = r1$A2_Cluster,
	  location = r1$A3_Block,
	  treatment = r1$A5_VillageType,
	  owner = r1$a11_1
	  )
	  
	d2 <- data.frame(
	  hhid = as.character(r2$hhid),
	  adm1 = r2$A1_District,
	  adm2 = r2$A2_SubCluster,
	  location = r2$A3_Block,
	  treatment = r2$A5_VillageType,
	  farmer_gender = r2$b1,
	  farmer_age = r2$b3,
	  farmer_education = r2$b4
	) 
	
	 ### merge d1 and d2 
  d <- merge(d1, d2, intersect(names(d1), names(d2)), all = TRUE)
	  
	  ###
	d3 <- data.frame(
	  hhid = as.character(r3$hhid),
	  adm1 = r3$A1_District,
	  adm2 = r3$A2_SubCluster,
	  location = r3$A3_Block,
	  treatment = r3$A5_VillageType,
	  plot_id = r3$Parcel_id,
	  irrigated = grepl("Irrigated", r3$e29),
	  irrigation_source = r3$e30,
	  irrigation_method = ifelse(grepl("Surface", r3$e31), "surface",
	                      ifelse(grepl("Sprinkler", r3$e31), "sprinkler",
	                      ifelse(grepl("Drip", r3$e31), "drip",
	                      ifelse(grepl("Manual|Localized", r3$e31), "unknown", r3$e31)))),
	  irrigation_amount = r3$e33,
	  land_prep_method = ifelse(grepl("Manual", r3$e35),"manual puddling" , 
	                            ifelse(grepl("Mechanized", r3$e35), "mechanical puddling", "none"))
	)
	
  d <- merge(d, d3, intersect(names(d), names(d3)), all = TRUE)
	  
	  #### 
	d4 <- data.frame(
	  hhid = as.character(r4$hhid),
	  adm1 = r4$A1_District,
	  adm2 = r4$A2_SubCluster,
	  location = r4$A3_Block,
	  treatment = r4$A5_VillageType,
	  #EGB:
	  # # Field = Plot in this dataset (?)
	  field_id = as.character(r4$Parcel_id),
	  field_size = as.numeric(r4$f5_Quantity*0.4047),
	  plot_id = as.character(r4$Parcel_id),
	  crop = tolower(r4$crop_name),
	  variety = r4$variety_name,
	  prop = r4$f4,
	  plot_area = r4$f5_Quantity*0.4047,## ha 
	  season = tolower(r4$season)
	)
	
	d4 <- merge(d4, aggregate(f5_Quantity*0.4047 ~ hhid, data = r4, FUN = sum, na.rm = TRUE), by = "hhid")
	colnames(d4)[ncol(d4)] <- "cropland_total"

	  ## merge d and d4
	d <- merge(d, d4, intersect(names(d), names(d4)), all = TRUE)
	  
	  #### 
	d5 <- data.frame(
	  hhid = as.character(r5$hhid),
	  adm1 = r5$A1_District,
	  adm2 = r5$A2_SubCluster,
	  location = r5$A3_Block,
	  treatment = r5$A5_VillageType,
	  plot_id = as.character(r5$Parcel_id),
	  crop = tolower(r5$crop_name),
	  variety = r5$variety_name,
	  prop = r5$g4,
	  plot_area = r5$g5_Quantity*0.4047,
	  season = tolower(r5$season)
	)
	  
	### merge d and d5
	d <- merge(d, d5, intersect(names(d), names(d5)), all = TRUE)
	  
	  ####
	d6 <- data.frame(
	  hhid = as.character(r6$hhid),
	  adm1 = r6$A1_District,
	  adm2 = r6$A2_SubCluster,
	  location = r6$A3_Block,
	  treatment = r6$A5_Type_of_Villages,
	  plot_id = as.character(r6$Parcel_id),
	  crop = tolower(r6$crop_name),
	  variety = r6$variety_name,
	  prop = r6$h4,
	  plot_area = r6$h5_Quantity*0.4047,
	  season = tolower(r6$season)
	)
	  
	  ### merge d and d6
	d <- merge(d, d6, intersect(names(d), names(d6)), all = TRUE)
	  
	 ####
	d7 <- data.frame(
	  hhid = as.character(r7$hhid),
	  adm1 = r7$A1_District,
	  adm2 = r7$A2_SubCluster,
	  location = r7$A3_Block,
	  treatment = r7$A5_VillageType,
	  plot_id = as.character(r7$Parcel_id),
	  crop = tolower(r7$cropid),
	  variety = r7$variety_name,
	  prop = r7$i4,
	  plot_area = r7$i5_Quantity*0.4047,
	  season = tolower(r7$season)
	)
	 ##### merge d and d7
  d <- merge(d, d7, intersect(names(d), names(d7)), all = TRUE)
	  
	  ###########
	d8 <- data.frame(
	  hhid = as.character(r8$hhid),
	  adm1 = r8$A1_District,
	  adm2 = r8$A2_SubCluster,
	  location = r8$A3_Block,
	  treatment = r8$A5_VillageType,
	  plot_id = as.character(r8$Parcel_id),
	  crop = tolower(r8$crop_name),
	  seed_type = r8$k6,
	  seed_rate = ifelse(grepl("Gram", r8$k10_Unit), r8$k10_Quantity/1000, r8$k10_Quantity),
	  OM_used = grepl("Yes",r8$k11),
	  OM_type = r8$k12,
	  OM_amount = r8$k13,
	  OM_price = r8$k14,
	  fertilizer_used = grepl("Yes",r8$k15),
	  fertilizer_type = r8$k16,
	  fertilizer_amount = r8$k17,
	  fertilizer_price = r8$k18,
	  insecticide_used = grepl("Yes",r8$k19),
	  insecticide_product = r8$k20,
	  insecticide_amount = r8$k21,
	  insecticide_price = r8$k22,
	  herbicide_used = grepl("Yes", r8$k23),
	  herbicide_product = r8$k24,
	  herbicide_amount = r8$k25,
	  herbicide_price = r8$k26,
	  yield = ifelse(grepl("100 Kilo", r8$k28_Unit), r8$k28_Quantity*100,
	          ifelse(grepl("200 Kilo Sack", r8$k28_Unit), r8$k28_Quantity*200, 
	          ifelse(grepl("50 Kilo", r8$k28_Unit), r8$k28_Quantity*50, 
	          ifelse(grepl("25 Kilo", r8$k28_Unit), r8$k28_Quantity*25,
	          ifelse(grepl("Gram", r8$k28_Unit), r8$k28_Quantity/1000, r8$k28_Quantity))))),
	  
	  yield_marketable = ifelse(grepl("100 Kilo", r8$k31_Unit), r8$k31_Quantity*100,
	                     ifelse(grepl("200 Kilo Sack", r8$k31_Unit), r8$k31_Quantity*200, 
	                     ifelse(grepl("50 Kilo", r8$k31_Unit), r8$k31_Quantity*50, 
	                     ifelse(grepl("25 Kilo", r8$k31_Unit), r8$k31_Quantity*25,
	                     ifelse(grepl("Gram", r8$k31_Unit), r8$k31_Quantity/1000, r8$k31_Quantity))))),
	  crop_price = r8$k32
	)
	 
	  ### merge d and d8 
	d <- merge(d, d8, intersect(names(d), names(d8)), all = TRUE)
	 
	 ### drop with missing crop 
	d <- d[!is.na(d$crop),]
	d$farmland_owned <- ifelse(grepl("Yes", d$owner), d$plot_area, NA) 
 
	###	 
   d$crop_price <- d$crop_price/d$yield_marketable
	d$yield <- d$yield/d$plot_area ## kg/ha
	d$yield_marketable <- d$yield_marketable/d$plot_area
	d$fertilizer_price <- as.character(d$fertilizer_price/d$fertilizer_amount)
	d$fertilizer_amount <- d$fertilizer_amount/d$plot_area
	d$OM_price <- d$OM_price/d$OM_amount 
	d$OM_amount <- d$OM_amount/d$plot_area 
	d$herbicide_price <- d$herbicide_price/d$herbicide_amount
	d$herbicide_amount <- d$herbicide_amount/d$plot_area
	d$insecticide_price <- d$insecticide_price/d$insecticide_amount
	d$insecticide_amount <- d$insecticide_amount/d$plot_area
	d$plot_area <- d$plot_area*10000 # m2
	d$season <- ifelse(grepl("perennials", d$season), "kharif", d$season) 
	d$owner <- NULL
	## Adding Lon and lat coordinate
  geo <- data.frame(
    location = c("Akole", "Shahada" ,"Igatpuri"),
    longitude = c(74.0047, 74.4742, 73.5718),
    latitude = c(19.5364, 21.553, 19.7052)
  )
	 
	d <- merge(d, geo, by="location", all.x = TRUE)
	 
	 ### Fixing crop names 
	P <- carobiner::fix_name(d$crop)
	P <- gsub("acacia spp", "unknown", P)
	P <- gsub("barnyard millet", "millet", P)
	P <- gsub("chick pea", "chickpea", P)
	P <- gsub("cluster bean", "common bean", P)
	P <- gsub("cow pea", "cowpea", P)
	P <- gsub("cucumber and gherkin", "cucumber", P)
	P <- gsub("deshi mango", "mango", P)
	P <- gsub("hyacinth bean", "common bean", P)
	P <- gsub("lentils", "lentil", P)
	P <- gsub("lime/lemon", "lemon", P)
	P <- gsub("moong or mung bean", "mung bean", P)
	P <- gsub("green gram", "mung bean", P)
	P <- gsub("onions", "onion", P)
	P <- gsub("other cereals|other plants|other pulses and nuts|other roots", "unknown", P)
	P <- gsub("other vegetables", "vegetable", P)
	P <- gsub("peanuts or groundnuts", "groundnut", P)
	P <- gsub("pigeon pea/red gram", "pigeon pea", P)
	P <- gsub("wild nuts and seeds|tubers|teak|wild trees and perennial crops", "unknown", P)
	P <- gsub("mung bean, mung bean", "mung bean", P)
	P <- gsub("unknown, unknown", "unknown", P)
	d$crop <- P
	 
	 ### Fixing fertilizer type 
	P <- carobiner::fix_name(d$fertilizer_type)
	P <- gsub("Other", "unknown", P)
	P <- gsub("D. Compound", "D-compound", P)
	P <- gsub("S. Compound", "S-compound", P)
	P <- gsub("Super D", "DSP", P)
	P <- gsub("Yara \\(and other branded blends\\)", "unknown", P)
	P <- gsub("Urea", "urea", P)
	d$fertilizer_type <- P
	d$OM_type <- ifelse(grepl("Animal", d$OM_type), "animal dung", "unknown")
	 
	 
	 ### Fixing herbicide product 
	 
	P <- carobiner::fix_name(d$herbicide_product)
	P <- gsub("ANKUR", "cloransulam-methyl", P)
	P <- gsub("ANU71", "glyphosate, diammonium salt", P)
	P <- gsub("AYEREST|AYERISH|AYERITH|DANEDAR|GLAKFOSHET", "unknown", P)
	P <- gsub("GRAMOGEN|GRAMOGEB", "paraquat dichloride", P)
	P <- gsub("HAMALA|AASHATAP", "unknown", P)
	P <- gsub("IRS", "acifluorfen, sodium salt", P)
	P <- gsub("KATARE", "cyhalothrin", P)
	P <- gsub("KHOT|KISAN|HERBICIDE|INFOSULPHONE|GLAYFOSHET", "unknown", P)
	P <- gsub("LOKRYA BREND|MAHITNHI|NIMONIGOLD|HB11", "unknown", P)
	P <- gsub("NPK1|PANIRA|PATELA|POLTRIN|GOLAN TARGA SUPER|DANEYDAR|SHATTI", "unknown", P)
	P <- gsub("ROUNDUP", "glyphosate", P)
	P <- gsub("WEEDIVIDE", "atrazine", P)
	P <- gsub("ZINC|TARKASUPER|TANNASHK|SWATI|SUPER FAST", "unknown", P)
	d$herbicide_product <- trimws(P)
	 
	P <- carobiner::fix_name(d$insecticide_product)
	P[P=="NA"] <- NA
	d$insecticide_product <- tolower(P)
	 
	d$country <- "India"
	d$currency = "INR"
	d$on_farm <- FALSE
	d$is_survey <- TRUE
	d$geo_from_source <- FALSE
	d$yield_part <- "none"
	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- TRUE
	d$planting_date <- as.character(NA) 
	d$trial_id <- paste(d$location, d$hhid, sep="-")
	d$prop <- NULL
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)

#### drop the duplicated rows
	 
 d <- unique(d)
	 
carobiner::write_files(path, meta, d)
}


