# R script for "carob"
# license: GPL (>=3)

## ISSUES
## Some units do not have a converter from local to standard units.
## Some plot areas are too small compared to the crop weight harvested.


carob_script <- function(path) {

"
Tanzania Africa Research in Sustainable Intensification for the Next Generation (Africa RISING) Follow-up Survey

Tanzania Africa RISING follow-up evaluation survey was implemented in 2022. The survey data were collected from the same households that were interviewed as part of the Tanzania baseline evaluation survey. Please refer to 'A User Guide to Tanzania Africa Research in Sustainable Intensification for the Next Generation (Africa RISING) Baseline Evaluation Survey Data' data paper for details about survey coverage and design. The Tanzania follow-up survey was implemented using structured questionnaires that were highly comparable with the questionnaires used in the Tanzania baseline evaluation survey.
"

	uri <- "doi:10.7910/DVN/CBYBOU"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "IFPRI",
		publication = NA,
		project = NA,
		carob_date = "2026-04-13",
		design = "unitOfAnalysis",
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none", 
		carob_contributor = "Cedric Ngakou",
		completion = 70,	
		notes = "We process only files with useful information for carob"
	)
	

	f1 <- ff[basename(ff) == "Community"]
	f2 <- ff[basename(ff) == "Household"]

	r1 <- haven::read_dta(ff[basename(ff) == "Interview.dta"]) |> carobiner:::unlabel()
	r2 <- haven::read_dta(ff[basename(ff) == "Section_B.dta"]) |> carobiner:::unlabel()
	r3 <- haven::read_dta(ff[basename(ff) == "Section_E.dta"]) |> carobiner:::unlabel()
	r4 <- haven::read_dta(ff[basename(ff) == "Section_F.dta"]) |> carobiner:::unlabel()
	r5 <- haven::read_dta(ff[basename(ff) == "Section_G1.dta"]) |> carobiner:::unlabel()
	r6 <- haven::read_dta(ff[basename(ff) == "Section_G2.dta"]) |> carobiner:::unlabel()
	r7 <- haven::read_dta(ff[basename(ff) == "Section_H.dta"]) |> carobiner:::unlabel()
	
	#### process
	
	d1 <- data.frame(
	   hhid = as.character(r1$hhid),
	   adm1 = r1$a1,
	   adm2 = r1$a2,
	   location = r1$a3
	)
	
	###
	
	d2 <- data.frame(
	   hhid = as.character(r2$hhid),
	   farmer_gender = r2$b3,
	   farmer_age = r2$b4a,
	   farmer_education = r2$b6,
	   farmer_civil_status = r2$b10
	) 
	
	### merge d1 and d2
	
	d <- merge(d1, d2, by= intersect(names(d1), names(d2)), all = TRUE)
	
	d3 <- data.frame(
	   hhid = as.character(r3$hhid),
	   field_id = as.character(r3$parcelid),
	   field_size = ifelse(grepl("Acre", r3$e2b), r3$e2a*0.4047,
	                ifelse(grepl("Meter square", r3$e2b), r3$e2a/10000, r3$e2a)) ,
	   farmland_owned = ifelse(grepl("Yes",  r3$e3), ifelse(grepl("Acre", r3$e2b), r3$e2a*0.4047,
	                                                  ifelse(grepl("Meter square", r3$e2b), r3$e2a/10000, r3$e2a)), NA),
	   irrigation_source = r3$e10,
	   soil_texture = tolower(gsub("Sand/loam", "sandy loam", r3$e12)),
	   soil_color = r3$e14,
	   plot_slope = r3$e15
	   
	)
	## merge d and d3
	d <- merge(d, d3, by= intersect(names(d), names(d3)), all = TRUE)
	
	
	d4 <- data.frame(
	   hhid = as.character(r4$hhid),
	   plot_id = as.character(r4$plotid),
	   field_id = as.character(r4$f2a),
	   field_size = ifelse(grepl("Acre", r4$f3b), r4$f3a*0.4047,
	                ifelse(grepl("Meter square", r4$f3b), r4$f3a/10000, r4$f3a)) ,
	   OM_amount =  ifelse(grepl("Bucket/Tin",  r4$f9b), r4$f9a*16.5, 
	                ifelse(grepl("Maxibag \\(90kg bag\\)",  r4$f9b), r4$f9a*90,
	                ifelse(grepl("Heap",  r4$f9b), r4$f9a*0.1,
	                ifelse(grepl("Bale",  r4$f9b), r4$f9a*0.1,
	                ifelse(grepl("Kilogram",  r4$f9b), r4$f9a, NA))))) ,
	   OM_type =  ifelse(!is.na(r4$f3a), paste("farmyard manure", r4$f12a, sep = ";")) ,
	   OM_amount1 = ifelse(grepl("Maxibag \\(90kg bag\\)", r4$f12c), r4$f12b*90, 
	                ifelse(grepl("Bucket/Tin", r4$f12c), r4$f12b*16.5,
	                ifelse(grepl("Bale", r4$f12c), r4$f12b*0.1,
	                ifelse(grepl("Minibag \\(50 kg bag\\)", r4$f12c), r4$f12b*50,
	                ifelse(grepl("Heap", r4$f12c), r4$f12b*0.1, 
	                ifelse(grepl("Kilogram", r4$f12c), r4$f12b, NA)))))) 
	)
	
	## merge d and d3
	d4$OM_amount <- rowSums(d4[, c("OM_amount", "OM_amount1")], na.rm = TRUE)
	d4$OM_amount1 <- NULL
	d <- merge(d, d4, by= intersect(names(d), names(d4)), all = TRUE)
	
	
	d5 <- data.frame(
	   hhid = as.character(r5$hhid),
	   field_id = as.character(r5$g1_2),
	   plot_id = as.character(r5$g1_3),
	   crop = tolower(r5$g1_4),
	   plot_area = ifelse(grepl("Acre", r5$g1_6b), r5$g1_6a*0.4047, r5$g1_6a/10000) ,
	   yield = ifelse(grepl("Bucket/Tin", r5$g1_7b), r5$g1_7a*16.5, 
	           ifelse(grepl("Maxibag \\(90kg bag\\)", r5$g1_7b), r5$g1_7a*90,
	           ifelse(grepl("Minibag \\(50 kg bag\\)", r5$g1_7b), r5$g1_7a*50,
	           ifelse(grepl("Unit or Piece", r5$g1_7b), r5$g1_7a*0.4,
	           ifelse(grepl("Heap", r5$g1_7b), r5$g1_7a*0.1,
	           ifelse(grepl("Bale", r5$g1_7b), r5$g1_7a*0.1,
	           ifelse(grepl("Kilogram", r5$g1_7b), r5$g1_7a, NA))))))) 

	)
	
	## merge d and d5
	Agg <- aggregate(. ~ hhid + field_id+ plot_id + crop , d5, function(x) mean(x))
	d <- merge(d, Agg, by= intersect(names(d), names(Agg)), all = TRUE)
	
	#####
	d6 <- data.frame(
	   hhid = as.character(r6$hhid),
	   field_id = as.character(r6$g2_2),
	   plot_id = as.character(r6$g2_3),
	   crop = tolower(r6$g2_4),
	   seed_rate = ifelse(grepl("Bucket/Tin", r6$g2_5b), r6$g2_5a*16.5, 
	               ifelse(grepl("Maxibag \\(90kg bag\\)", r6$g2_5b), r6$g2_5a*90,
	               ifelse(grepl("Unit or Piece", r6$g2_5b), r6$g2_5a,
	               ifelse(grepl("Minibag \\(50 kg bag\\)", r6$g2_5b), r6$g2_5a*50,
	               ifelse(grepl("Gram", r6$g2_5b), r6$g2_5a/1000,
	               ifelse(grepl("Bale", r6$g2_5b), r6$g2_5a*0.1,
	               ifelse(grepl("Kilogram", r6$g2_5b), r6$g2_5a, NA))))))) ,
	   seed_price = r6$g2_7a,
	   fertilizer_type = ifelse(is.na(r6$g2_11), r6$g2_11_other,  r6$g2_11) ,
	   fertilizer_amount =ifelse(grepl("Minibag \\(50 kg bag\\)", r6$g2_13b), r6$g2_13a*50, 
	                      ifelse(grepl("Bucket/Tin", r6$g2_13b), r6$g2_13a*16.5,
	                      ifelse(grepl("Gram", r6$g2_13b), r6$g2_13a/1000,
	                      ifelse(grepl("Kilogram", r6$g2_13b), r6$g2_13a, NA)))) ,
	   fertilizer_price = r6$g2_13c
	)

	## merge d and d6
	d <- merge(d, d6, by= intersect(names(d), names(d6)), all = TRUE)
	
		
	d7 <- data.frame(
	   hhid = as.character(r7$hhid),
	   crop = tolower(r7$h2),
	   yield_marketable = ifelse(grepl("Bucket/Tin", r7$h13b), r7$h13a, 
	                      ifelse(grepl("Maxibag \\(90kg\\)", r7$h13b), r7$h13a*90,
	                      ifelse(grepl("Minibag \\(50kg\\)", r7$h13b), r7$h13a*50,
	                      ifelse(grepl("Unit or Piece", r7$h13b), r7$h13a*0.4,
	                      ifelse(grepl("Heap", r7$h13b), r7$h13a*0.1,
	                      ifelse(grepl("Bale", r7$h13b), r7$h13a*0.1,
	                      ifelse(grepl("Kilogram", r7$h13b), r7$h13a, NA))))))) ,
	   crop_price = r7$h14,
	   market_type = r7$h15
	)
	
	## merge d and d7
	d7 <- d7[!duplicated(d7$hhid),]
	d <- merge(d, d7, by= intersect(names(d), names(d7)), all = TRUE)
	
	## Adding Lon and Lat coordinate 
	
	geo <- data.frame(
	   location = c("Bashnet", "Dabil", "Dareda", "Arri", "Gidas", "Madunga", "Gallapo", "Magugu", "Mwada", "Chitego", "Mlali", "Chiwe", "Ugogoni", "Sagara", "Makawa", "Njoge", "Dosidos", "Njoro", "Makame"),
	   longitude = c(35.4153, 35.4666, 35.5306, 35.6058, 35.6754, 35.4167, 35.8520, 35.7733, 35.9342, 36.313, 36.7499, 36.709, 36.4368, 36.5339, 36.5414, 36.688, 36.4176, 36.501, 36.727),
	   latitude = c(-4.233, -4.265, -4.2406, -4.226, -4.4198, -4.1488, -4.284, -4.0260, -3.8760, -5.6060, -6.282, -6.1815, -6.1379, -6.2455, -5.7523, -5.9434, -5.62008, -5.2485, -4.6387)
	)
	
	d <- merge(d, geo, by="location", all.x = TRUE)
	
	
	####
	d$fertilizer_price <- as.character(d$fertilizer_price/d$fertilizer_amount)
	d$fertilizer_amount <- d$fertilizer_amount/d$plot_area
	d$OM_amount <- d$OM_amount/d$plot_area
	d$yield <- d$yield/d$plot_area
	d$crop_price <- d$crop_price/d$yield_marketable
	d$yield_marketable <- d$yield_marketable/d$plot_area
	seed_price <- d$seed_price/d$seed_rate
	d$seed_rate <- d$seed_rate/d$plot_area
	d$plot_area <- d$plot_area*10000 # m2
	
	##### Fixing crop names 
	
	P <- carobiner::fix_name(d$crop)
	P <- gsub("bambara nuts", "bambara groundnut",  P)
	P <- gsub("^bean$", "common bean",  P)
	P <- gsub("chick-peas", "chickpea",  P)
	P <- gsub("cow-peas", "cowpea",  P)
	P <- gsub("irish potato", "potato",  P)
	P <- gsub("^peas$", "pea",  P)
	P <- gsub("pigeonpea", "pigeon pea",  P)
	P <- gsub("tomatoes", "tomato",  P)
	P <- gsub("other crops", "unknown",  P)
	d$crop <- P
	
	### Fixing OM type
	P <- carobiner::fix_name(d$OM_type)
	P <- gsub("Mulch/compost", "compost", P)
	P <- gsub("None", "none", P)
	P <- gsub("A combination of organic inputs|Household waste|Other", "unknown", P)
	P <- gsub("Crop residue from other farms|Other|Crop residue from this farm", "unknown", P)
	P <- gsub("farmyard manure;NA", "farmyard manure", P)
	d$OM_type <- P 
	
	### Fixing fertilizer type 
	P <- carobiner::fix_name(d$fertilizer_type)
	P <- gsub("D. Compound", "D-compound", P)
	P <- gsub("Urea", "urea", P)
	P <- gsub("A combination|Other \\(specify\\)", "unknown", P)
	P <- gsub("Super D", "DSP", P)
	P <- gsub("Yara", "YaraMila Star", P)
	P <- gsub("None", "none", P)
	d$fertilizer_type <- P
	d$soil_texture <- gsub("other", "unknown", d$soil_texture)
	
	
	d$country <- "Tanzania"
	d$currency = "TZS"
	d$on_farm <- FALSE
	d$is_survey <- TRUE
	d$planting_date <- as.character(NA)
	d$geo_from_source <- FALSE
	d$yield_part <- "none"
	d$irrigated <- ifelse(!is.na(d$irrigation_source), TRUE, FALSE)
	d$trial_id <- paste(d$location, d$hhid, sep="-")
	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- TRUE
	
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA) 
	
	### remove extreme high values
	
	### drop rows with missing adm1, adm2 and location
	d <- d[!is.na(d$location),] 
	### remove duplicate 
	
	d <- unique(d)
	
	
	carobiner::write_files(path, meta, d)
}


