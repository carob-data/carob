# R script for "carob"
# license: GPL (>=3)

## ISSUES
## Some units are not specified.
## Some plot areas are too small compared to the crop weight harvested.


carob_script <- function(path) {

"
Ghana Africa Research in Sustainable Intensification for the Next Generation (Africa RISING) Follow-up Survey

Ghana Africa RISING follow-up evaluation survey was implemented in 2020. The survey data were collected from the same households that were interviewed as part of the Ghana baseline evaluation survey. Please refer to 'A User Guide to Ghana Africa Research in Sustainable Intensification for the Next Generation (Africa RISING) Baseline Evaluation Survey Data' data paper for details about survey coverage and design. The Ghana follow-up survey was implemented using structured questionnaires that were highly comparable with the questionnaires used in the Ghana baseline evaluation survey.
"

	uri <- "doi:10.7910/DVN/TLKYUA"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "IFPRI",
		publication = NA,
		project = NA,
		carob_date = "2026-04-14",
		design = "unitOfAnalysis",
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none", 
		carob_contributor = "Cedric Ngakou",
		completion = 70,	
		notes = "We process only files with useful information for carob"
	)
	
	r1 <- haven::read_dta(grep("Data/Interview.dta", ff, value=TRUE)) |> carobiner:::unlabel()
	r2 <- haven::read_dta(grep("Data/Section B.dta", ff, value=TRUE)) |> carobiner:::unlabel()
	r3 <- haven::read_dta(grep("Data/Section E.dta", ff, value=TRUE)) |> carobiner:::unlabel()
	r4 <- haven::read_dta(grep("Data/Section G1.dta", ff, value=TRUE)) |> carobiner:::unlabel()
	r5 <- haven::read_dta(grep("Data/Section G2.dta", ff, value=TRUE)) |> carobiner:::unlabel()
	r6 <- haven::read_dta(grep("Data/Section H.dta", ff, value=TRUE)) |> carobiner:::unlabel()
	
	
	#### process
	
	d1 <- data.frame(
	   adm1 = r2$a1,
	   adm2 = r2$a2,
	   location = r2$a3,
	   hhid = as.character(r2$hhid),
	   farmer_gender = r2$b3,
	   farmer_age = r2$b4a,
	   farmer_marital_status = r2$b6,
	   farmer_education_level = r2$b7
	)
	
	
	d2 <- data.frame(
	   hhid = as.character(r3$hhid),
	   adm1 = r3$a1,
	   adm2 = r3$a2,
	   location = r3$a3,
	   field_id = as.character(r3$PID),
	   field_size = ifelse(grepl("Acre", r3$e3b), r3$e3a*0.4047,
	                 ifelse(grepl("Square meters", r3$e3b), r3$e3a/10000, r3$e3a)) ,
	   farmland_owned = ifelse(grepl("Yes",  r3$e4),  ifelse(grepl("Acre", r3$e3b), r3$e3a*0.4047,
	                                                         ifelse(grepl("Square meters", r3$e3b), r3$e3a/10000, r3$e3a)), NA),
	   irrigation_method = ifelse(grepl("Surface", r3$e9), "surface",
	                       ifelse(grepl("Groundwater", r3$e9), "groundwater", 
	                       ifelse(grepl("A combination", r3$e9), "unknown", "none"))),
	   irrigated = grepl("irrigation", r3$e9),
	   irrigation_source = r3$e10,
	   soil_texture = tolower(gsub("Sand/loam", "sandy loam", r3$e12)),
	   soil_color = r3$e14,
	   plot_slope = r3$e15,
	   treatment = r3$treat
	)
	
	### merge d1 and d2
	d <- merge(d1, d2[duplicated(d2$hhid),], by=intersect(names(d1), names(d2)), all = TRUE)
	
	#####
	r4[r4==""] <- NA
	d3 <- data.frame(
	   adm1 = r4$a1,
	   adm2 = r4$a2,
	   location = r4$a3,
	   hhid = as.character(r4$hhid),
	   crop = tolower(r4$cropid),
	   field_id = as.character(r4$g1_1),
	   plot_id = as.character(r4$g1_2),
	   plot_area = ifelse(grepl("Acre", r4$g1_6b), r4$g1_6a*0.4047, 
	                      ifelse(grepl("Square meters", r4$g1_6b), r4$g1_6a/10000, r4$g1_6a)) ,
	   unit_area = r4$g1_6b,
	   yield = r4$g1_7a,
	   treatment = r4$treat
	)
	
	d3 <- d3[!grepl("Football field", d3$unit_area),]
	d3$unit_area <- NULL
	
	Agg <- aggregate(. ~ adm1+ adm2 + location +hhid + field_id+ plot_id + crop+treatment , d3, function(x) mean(x))
	##### merge d and d3
	d <- merge(d, Agg, by=intersect(names(d), names(d3)), all = TRUE)
	
	############
	r5[r5==-99] <- NA
	d4 <- data.frame(
	   adm1 = r5$a1,
	   adm2 = r5$a2,
	   location = r5$a3,
	   hhid = as.character(r5$hhid),
	   crop = tolower(r5$g2_1),
	   seed_rate = r5$g2_2a,
	   seed_price = r5$g2_3c
	)
	
	### merge d and d4
	
	d <- merge(d, d4, by=intersect(names(d), names(d4)), all = TRUE)
	
	###
	r6[r6=="-99"] <- NA
	d5 <- data.frame(
	   adm1 = r6$a1,
	   adm2 = r6$a2,
	   location = r6$a3,
	   hhid = as.character(r6$hhid),
	   crop = tolower(r6$g1_4),
	   yield_marketable = r6$h15a,
	   crop_price = r6$h16,
	   market_type = r6$h17,
	   treatment = r6$treat
	)
	
	### merge d and d5
	d <- merge(d, d5, by=intersect(names(d), names(d5)), all = TRUE)
	
	
	### Adding Lon and lat coordinate 
	
	geo <- data.frame(
	   location = c("Disiga", "Duko", "Gushie", "Jana", "Kadia", "Kukobila", "Nabogu", "Pigu", "Tibali", "Tindan", "Cheyohi No. 2", "Gbanjon", "Tingoli", "Arigu", "Kukua",   "Nasia", "Gia", "Nyangua", "Shia", "Fian", "Goli", "Goriyiri", "Issa", "Naro", "Papu", "Sa Gie", "Tabiase", "Wogu", "Goripi", "Nyagli", "Pase", "Tanina"),
	   longitude = c(-0.806, -0.9486, -0.8602, -0.8063, -0.8568, -0.8062, -0.8232, -0.8269, -0.87605, -0.9032, -0.984, -1.102, -1.046, -0.874, -0.8182, -0.8035,  -0.250,  -1.1067, 0.5648, -2.467, -2.6335, -2.6190, -2.3343, -2.4629, -2.579, -2.354, -2.3418, -2.3867, -2.2711, -2.4004,  -2.7117, -2.482),
	   latitude = c(10.1243, 9.560, 9.808, 9.478, 9.906, 10.1240, 9.7477, 9.976, 9.6634, 9.6692, 9.4426, 9.4517, 9.33437, 10.5768, 10.304, 10.160, 5.5404,  10.880,  6.793, 10.3881, 10.2951, 10.3258, 10.389, 10.3264, 10.2354, 10.259, 10.386, 10.4250, 9.9737, 10.1305, 10.0375, 9.8064)
	) 
	
	d <- merge(d, geo, by="location", all.x = TRUE)
	
	geo1 <- data.frame(
	   adm2 = c("West Maprusi", "Kassena Nankana East", "Tolon/Kumbungu", "Nadowli", "Wa West", "Wa East", "Bongo", "Talensi-Nabdam", "Salvelugu"),
	   lon = c(-0.8895, -1.12508, -1.0665, -2.6648, -2.6629, -2.090, -0.8077, -0.7761,  -0.8240),
	   lat = c(10.4251, 10.6659, 9.432, 10.3672, 9.9834, 10.1662, 10.909, 10.6135, 9.6163)
	)
	
	d <- merge(d, geo1, by= "adm2", all.x = TRUE)
	
	d$longitude[is.na(d$longitude)] <- d$lon[is.na(d$longitude)]
	d$latitude[is.na(d$latitude)] <- d$lat[is.na(d$latitude)]
	d$lon <- d$lat <- NULL 
	
	##
	d <- d[which(d$plot_area >0),]
	d$yield <- d$yield/d$plot_area
	d$yield_marketable <- d$yield_marketable/d$plot_area
	seed_price <- d$seed_price/d$seed_rate
	d$seed_rate <- d$seed_rate/d$plot_area
	d$plot_area <- d$plot_area*10000 # m2
	
	### Fixing crop names
	P <- carobiner::fix_name(d$crop)
	P <- gsub("ayoyo|planted trees", "unknown", P)
	P <- gsub("bambara nuts", "bambara groundnut", P)
	P <- gsub("^bean$", "common bean", P)
	P <- gsub("fallow", "none", P)
	P <- gsub("garden eggs|bitter leaves", "unknown", P)
	P <- gsub("green pepper|red pepper", "pepper", P)
	P <- gsub("natural trees|other crops|other perennia|other uses", "unknown", P)
	P <- gsub("other cereals", "cereal", P)
	P <- gsub("other vegetable", "vegetable", P)
	P <- gsub("pasture/grazing", "pasture", P)
	P <- gsub("pigeonpea", "pigeon pea", P)
	P <- gsub("sweet potato", "sweetpotato", P)
	P <- gsub("tiger nut", "groundnut", P)
	P <- gsub("tomatoes", "tomato", P)
	d$crop <- P
	
	d$soil_texture <- ifelse(grepl("other", d$soil_texture), "unknown", d$soil_texture)
	
	d$country <- "Ghana"
	d$currency = "GHS"
	d$on_farm <- FALSE
	d$is_survey <- TRUE
	d$planting_date <- as.character(NA)
	d$geo_from_source <- FALSE
	d$yield_part <- "none"
	d$trial_id <- paste(d$location, d$hhid, sep="-")
	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- TRUE
	
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA) 
	
	### remove duplicate 
	
	d <- unique(d)
	
	carobiner::write_files(path, meta, d)
}


