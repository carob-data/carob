# R script for "carob"
# license: GPL (>=3)

## ISSUES
## Some units do not have a converter from local to standard units.
## Some plot areas are too small compared to the crop weight harvested, resulting in very high yield values (kg/ha).


carob_script <- function(path) {

"
Mali Africa Research in Sustainable Intensification for the Next Generation (Africa RISING) Follow-up Survey

Mali Africa RISING follow-up evaluation survey was implemented in 2022. The survey data were collected from the same households that were interviewed as part of the Mali baseline evaluation survey. Please refer to 'A user guide to Mali Africa Research in Sustainable Intensification for the Next Generation (Africa RISING) Baseline Evaluation Survey' data paper for details about survey coverage and design. The Mali follow-up survey was implemented using structured questionnaires that were highly comparable with the questionnaires used in the Mali baseline evaluation survey.
"

	uri <- "doi:10.7910/DVN/NOEIT1"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "IFPRI",
		publication = NA,
		project = NA,
		carob_date = "2026-04-09",
		design = "unitOfAnalysis",
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none", 
		carob_contributor = "Cedric Ngakou",
		completion = 70,	
		notes = "We process only useful files for carob"
	)
	

#	f1 <- ff[basename(ff) == "Community"]
#	f2 <- ff[basename(ff) == "Household"]
#	f3 <- ff[basename(ff) == "Market"]

	r1 <- haven::read_dta(ff[basename(ff) == "Section_B1_HouseholdMembers.dta"]) |> carobiner:::unlabel()
	r2 <- haven::read_dta(ff[basename(ff) == "Section_E_AgriculturalLand.dta"]) |> carobiner:::unlabel()
	r3 <- haven::read_dta(ff[basename(ff) == "Section_F_CropInputs.dta"])|> carobiner:::unlabel()
	r4 <- haven::read_dta(ff[basename(ff) == "Section_G1_CropProduction.dta"]) |> carobiner:::unlabel()
	r5 <- haven::read_dta(ff[basename(ff) == "Section_G2_CropInputs_Costs.dta"]) |> carobiner:::unlabel()
	r6 <- haven::read_dta(ff[basename(ff) == "Section_H_CropSales_Quantities.dta"]) |> carobiner:::unlabel()
	
	
	d1 <- data.frame(
	   adm1 = r1$a1,
	   location = r1$a2,
	   trial_id = r1$a3,
	   hhid = as.character(r1$a4),
	   farmer_gender = r1$b3,
	   farmer_age = r1$b4,
	   farmer_civil_status = r1$b6,
	   farmer_education = r1$b7
	)
	
	d2 <- data.frame(
	   adm1 = r2$a1,
	   location = r2$a2,
	   trial_id = r2$a3,
	   hhid = as.character(r2$a4),
	   field_id = trimws(as.character(r2$parcel_id)),
	   field_size = ifelse(grepl("Metres Carrés", r2$e3b), r2$e3a/10000, r2$e3a) ,
	   irrigation_source = r2$e10,
	   soil_texture = tolower(r2$e12),
	   soil_color = r2$e14,
	   plot_slope = r2$e15
	)
	
	####### merge d1 and d2
	
	d <- merge(d1, d2[!duplicated(d2$hhid),], by= intersect(names(d1), names(d2)), all = TRUE)
	
	d3 <- data.frame(
	   adm1 = r3$a1,
	   location = r3$a2,
	   trial_id = r3$a3,
	   hhid = as.character(r3$a4),
	   field_id = trimws(as.character(r3$parcel_id)),
	   plot_id = trimws(as.character(r3$plot_id)),
	   land_prep_method =  ifelse(!is.na(r3$f5b), "ploughing; minimum tillage", "ploughing") ,
	   OM_amount1 = ifelse(grepl("50 Kilo", r3$f7b),r3$f7a*50,
	                ifelse(grepl("100 Kilo", r3$f7b),r3$f7a*100,
	                ifelse(grepl("200 Kilo", r3$f7b),r3$f7a*200,
	                ifelse(grepl("25 Kilo", r3$f7b),r3$f7a*25,
	                ifelse(grepl("Gram", r3$f7b),r3$f7a/1000,
	                ifelse(grepl("75 Kilo", r3$f7b),r3$f7a*75,
	                ifelse(grepl("Kilo", r3$f7b),r3$f7a,
	                ifelse(grepl("Heap", r3$f7b),r3$f7a*0.10,
	                ifelse(grepl("Unit or Piece", r3$f7b),r3$f7a*0.4,
	                ifelse(grepl("Cup", r3$f7b),r3$f7a*0.06, NA )))))))))) ,
	   OM_price = rowSums(r3[, c("f7d", "f13d")], na.rm = TRUE),
	   #fertilizer_type = r3$f10a,
	   fertilizer_amount = ifelse(grepl("25 Kilo", r3$f11b), r3$f11a*25, 
	                       ifelse(grepl("50 Kilo", r3$f11b), r3$f11a*50, 
	                       ifelse(grepl("100 Kilo", r3$f11b), r3$f11a*100, 
	                       ifelse(grepl("75 Kilo", r3$f11b), r3$f11a*75, 
	                       ifelse(grepl("Gram", r3$f11b), r3$f11a/1000, 
	                       ifelse(grepl("Unit or Piece", r3$f11b), r3$f11a*0.4,
	                       ifelse(grepl("Kilo", r3$f11b), r3$f11a, NA))))))),
	                       
	   OM_type = ifelse(!is.na(r3$f7a), "farmyard manure", "none") ,
	   OM_amount2 = ifelse( grepl("50 Kilo", r3$f13b), r3$f13a*50,
	                ifelse( grepl("25 Kilo", r3$f13b), r3$f13a*25,
	                ifelse( grepl("200 Kilo", r3$f13b), r3$f13a*200,
	                ifelse( grepl("100 Kilo", r3$f13b), r3$f13a*100,
	                ifelse( grepl("Unit or Piece", r3$f13b), r3$f13a*0.4,
	                ifelse( grepl("Kilo", r3$f13b), r3$f13a, NA))))))
	        
	   
	)
	d3$OM_amount <- rowSums(d3[, c("OM_amount2", "OM_amount1")], na.rm = TRUE)
	d3$OM_amount1 <- d3$OM_amount2 <- NULL
	
	### merge d and d3
	d <- merge(d, d3, by= intersect(names(d), names(d3)), all = TRUE)
	
	
	###
	d4 <- data.frame(
	   adm1 = r4$a1,
	   location = r4$a2,
	   trial_id= r4$a3,
	   hhid = as.character(r4$a4),
	   field_id = trimws(as.character(r4$parcel_id)),
	   plot_id = trimws(as.character(r4$plot_id)),
	   crop = tolower(r4$g1_3),
	   planting_method = r4$g1_3c,
	   plot_area = ifelse(grepl("Metres Carrés",r4$g1_4b), r4$g1_4a/10000, r4$g1_4a),
	   unit_area = r4$g1_4b,
	   yield = ifelse(grepl("100 Kilo", r4$g1_6b), r4$g1_6a*100, 
	           ifelse(grepl("200 Kilo", r4$g1_6b), r4$g1_6a*200,
	           ifelse(grepl("Gram", r4$g1_6b), r4$g1_6a/1000,
	           ifelse(grepl("Unit or Piece", r4$g1_6b), r4$g1_6a*0.4,
	           ifelse(grepl("50 Kilo", r4$g1_6b), r4$g1_6a*50,
	           ifelse(grepl("25 Kilo", r4$g1_6b), r4$g1_6a*25,
	           ifelse(grepl("75 Kilo", r4$g1_6b), r4$g1_6a*75,
	           ifelse(grepl("Cup", r4$g1_6b), r4$g1_6a*0.06,
	           ifelse(grepl("Heap", r4$g1_6b), r4$g1_6a*0.1,
	           ifelse(grepl("Kilo", r4$g1_6b), r4$g1_6a, NA)))))))))) 
	)
	d4 <- d4[!grepl("Other", d4$unit_area), ]  
	d4 <- d4[!is.na(d4$unit_area),]
	d4$unit_area <- NULL
	
	### merge d and d4
	
	d <- merge(d, d4, by= intersect(names(d), names(d4)), all = TRUE)
	
	###
	d5 <- data.frame(
	   adm1 = r5$a1,
	   location = r5$a2,
	   trial_id = r5$a3,
	   hhid = as.character(r5$a4),
	   field_id = trimws(as.character(r5$parcel_id)),
	   plot_id = trimws(as.character(r5$plot_id)),
	   crop = tolower(r5$g2_1),
	   seed_rate = ifelse(grepl("100 Kilo", r5$g2_x2), r5$g2_x1*100, 
	               ifelse(grepl("200 Kilo", r5$g2_x2), r5$g2_x1*200,
	               ifelse(grepl("Gram", r5$g2_x2), r5$g2_x1/1000,
	               ifelse(grepl("Unit or Piece", r5$g2_x2), r5$g2_x1*0.4,
	               ifelse(grepl("50 Kilo", r5$g2_x2), r5$g2_x1*50,
	               ifelse(grepl("25 Kilo", r5$g2_x2), r5$g2_x1*25,
	               ifelse(grepl("75 Kilo", r5$g2_x2), r5$g2_x1*75,
	               ifelse(grepl("Cup", r5$g2_x2), r5$g2_x1*0.06,
	               ifelse(grepl("Heap", r5$g2_x2), r5$g2_x1*0.10,
	               ifelse(grepl("Kilo", r5$g2_x2), r5$g2_x1, NA)))))))))),
	   seed_price = r5$g2_x3
	)
	
	### merge d and d5
	d <- merge(d, d5, by= intersect(names(d), names(d5)), all = TRUE)
	
	d6 <- data.frame(
	   adm1 = r6$a1,
	   location = r6$a2,
	   trial_id = r6$a3,
	   hhid = as.character(r6$a4),
	   crop = tolower(r6$crop_id),
	   yield_marketable = ifelse(grepl("100 Kilo", r6$h13b), r6$h13a*100, 
	                      ifelse(grepl("25 Kilo", r6$h13b), r6$h13a*25, 
	                      ifelse(grepl("50 Kilo", r6$h13b), r6$h13a*50,
	                      ifelse(grepl("200 Kilo", r6$h13b), r6$h13a*200,
	                      ifelse(grepl("Unit or Piece", r6$h13b), r6$h13a*0.4,
	                      ifelse(grepl("Kilo", r6$h13b), r6$h13a, NA)))))) ,
	   crop_price = ifelse(grepl("50 Kilo", r6$h14b),r6$h14a/50,
	                ifelse(grepl("100 Kilo", r6$h14b),r6$h14a/100,
	                 ifelse(grepl("200 Kilo", r6$h14b),r6$h14a/200,
	                 ifelse(grepl("Unit or Piece", r6$h14b),r6$h14a/0.4,
	                 ifelse(grepl("Kilo", r6$h14b),r6$h14a/50,NA)))))

	)
	
	### merge d and d6
	
	d <- merge(d, d6, by= intersect(names(d), names(d6)), all = TRUE)
	
	### adding Lon and lat coordinate
	
	geo <- data.frame(
	   location = c("Danou", "Faradiele", "Faragouaran", "Keleya", "Kouroulamni", "Sido", "Syentoula", "Fakolo", "Gouadji-kao", "Konina", "Konseguela", "M'Pessoba","Sincina", "Songua", "Zanfigue", "Gouanan", "Wasselou- Balle", "N'golonianasso"),
	   longitude = c(-8.0189, -7.6331, -7.77698, -7.7917, -7.7313, -7.6027, -8.04124, -5.56547, -5.158839, -6.1592, -5.88073, -5.71746, -5.3960, -4.658, -5.0177, -8.10804, -8.13156, -5.6824),
	   latitude = c(11.4579, 11.34874, 11.3118, 11.82923, 11.3871, 11.6677, 11.6481, 12.6101, 12.5328, 12.51682, 12.4066, 12.6666, 12.3550, 12.217, 12.523, 10.9341, 11.2266, 12.42302)
	)
	
	d <- merge(d, geo, by= "location", all.x = TRUE)
	####
	d$fertilizer_amount <- d$fertilizer_amount/d$plot_area
	d$OM_amount <- d$OM_amount/d$plot_area
	d$yield <- d$yield/d$plot_area
	d$yield_marketable <- d$yield_marketable/d$plot_area
	seed_price <- d$seed_price/d$seed_rate
	d$seed_rate <- d$seed_rate/d$plot_area
	d$plot_area <- d$plot_area*10000 # m2
	
	
	
	### Fixing crop names 
	
	P <- carobiner::fix_name(d$crop)
	P <- gsub("african eggplant","eggplant", P)
	P <- gsub("bambara nut","bambara groundnut", P)
	P <- gsub("baobab","unknown", P)
	P <- gsub("corn","maize", P)
	P <- gsub("cow pea","cowpea", P)
	P <- gsub("manioc","cassava", P)
	P <- gsub("natural trees|other perennial|other plants|other pulses and nuts|other uses","unknown", P)
	P <- gsub("other cereals","cereal", P)
	P <- gsub("other vegetables","vegetable", P)
	P <- gsub("pepper \\(hot\\)","pepper", P)
	P <- gsub("sweet potato","sweetpotato", P)
	P <- gsub("sumbala","unknown", P)
	P <- gsub("planted trees","unknown", P)
	d$crop <- P
	
	### Fixing soil texture
	
	P <- carobiner::fix_name(d$soil_texture) 
	P <- gsub("sand/loam", "sandy loam", P)
	P <- gsub("clay and rocky soil", "clay coarse", P)
	P <- gsub("rocky soil", "coarse", P)
	P <- gsub("other", "unknown", P)
	P <- gsub("sand and clay", "sandy clay", P)
	d$soil_texture <- P
	
	d$planting_method <- ifelse(grepl("Broadcasting", d$planting_method), "broadcasting", 
	                     ifelse(grepl("Row planting", d$planting_method), "direct seeding", "unknown"))
  d[d== ""] <- NA
	
  d$country <- "Mali"
  d$currency = "XOF"
  d$on_farm <- FALSE
  d$is_survey <- TRUE
  d$planting_date <- as.character(NA)
  d$geo_from_source <- FALSE
  d$yield_part <- "none"
  d$irrigated <- grepl("Deep|Shallow", d$irrigation_source)
  d$trial_id <- paste(d$trial_id, d$location, sep="-")
  d$yield_moisture <- as.numeric(NA)
  d$yield_isfresh <- TRUE
  
  d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA) 
  
  ### remove extreme high values
  d <- d[which(d$fertilizer_amount < 1000),]
  
  ### remove duplicate rows
  
  d <- unique(d)
  
	carobiner::write_files(path, meta, d)
}


