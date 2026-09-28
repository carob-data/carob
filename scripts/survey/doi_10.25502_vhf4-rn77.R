# R script for "carob"
# license: GPL (>=3)

## ISSUES

carob_script <- function(path) {

"
N2Africa demonstration trial

N2Africa is to contribute to increasing biological nitrogen fixation and productivity of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility, improving household nutrition and increasing income levels of smallholder farmers. As a vision of success, N2Africa will build sustainable, long-term partnerships to enable African smallholder farmers to benefit from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants and fertilizers adapted to local settings. A strong national expertise in grain legume production and N2-fixation research and development will be the legacy of the project.



The project is implemented in five core countries (Ghana, Nigeria, Tanzania, Uganda and Ethiopia) and six other countries (DR Congo, Malawi, Rwanda, Mozambique, Kenya & Zimbabwe) as tier one countries.
"
	uri <- "doi:10.25502/vhf4-rn77"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA; World Agroforestry Centre (ICRAF); WUR; International Institute of Tropical Agriculture (IITA), Wageningen University",
		publication = NA,
		project = NA,
		design = NA,
		data_type = NA,
		treatment_vars = "fertilizer_amount",
		response_vars = "yield", 
		notes = NA,
		carob_contributor = "Mitchelle Njukuya",
		carob_date = "2026-09-01",
		carob_completion = 100,	
		carob_effort = 9
	)
	

	f1 <- ff[basename(ff) == "data_table.csv"]

	r1 <- read.csv(f1)
	
	#Stack treatment1...treatment16 into one "treatment" column
	treatment_cols <- paste0("treatment", 1:16)      
	other_cols     <- setdiff(names(r1), treatment_cols)
	
	#Every other variable that belongs to a given treatment slot, so we
	#can tell a genuinely unused slot apart from one where only the
	#treatment name itself is missing but other data for that slot exists.
	
	slot_col_templates <- c(
	  "germination_crop_1_treatment_%d",
	  "image_treatment%d",
	  "shel_%d",
	  "row_spacing_crop_1_plot_%d_cm",
	  "plant_spacing_crop_1_plot_%d_cm",
	  "no_plants_hole_crop_1_plot_%d_nr",
	  "width_of_harvested_plot_crop_1_plot_%d_m",
	  "number_of_rows_crop_1_plot_%d_nr",
	  "grain_weight_crop_1_plot_%d_kg",
	  "pod_weight_groundnut_crop_1_plot_%d_kg",
	  "above_ground_biomass_weight_crop_1_plot_%d_kg",
	  "row_spacing_crop_2_plot_%d_cm",
	  "plant_spacing_crop_2_plot_%d_cm",
	  "no_plants_hole_crop_2_plot_%d_nr",
	  "width_of_harvested_plot_crop_2_plot_%d_m",
	  "number_of_rows_crop_2_plot_%d_nr",
	  "grain_weight_crop_2_plot_%d_kg",
	  "above_ground_biomass_weight_crop_2_plot_%d_kg",
	  "i_6_0_%d_1_0",
	  "i_8_1_%d_1_0",
	  "i_8_3_%d_1_0",
	  "i_8_4_%d_1_0",
	  "i_8_5_%d_1_0",
	  "i_8_6_%d_1_0",
	  "i_9_1_%d_1_0",
	  "i_9_2_%d_1_0",
	  "i_9_3_%d_1_0"
	)
	
	#For slot,we return only the templated column names that actually exist
	slot_cols <- function(i) {
	  candidate <- sprintf(slot_col_templates, i)
	  intersect(candidate, names(r1))
	}
	
	r1 <- do.call(rbind, lapply(1:16, function(i) {
	  piece           <- r1[, other_cols]            
	  piece$treatment <- r1[[treatment_cols[i]]]      
	  piece$t_code    <- i                            
	  
	  #Keep the row if treatment has a value, OR if ANY other variable that
	  #belongs to slot (germination, shelling, plot measurements, yields...)
	  cols_i    <- slot_cols(i)
	  has_other <- if (length(cols_i) == 0) {
	    rep(FALSE, nrow(r1))
	  } else {
	    apply(!is.na(r1[, cols_i, drop = FALSE]), 1, any)
	  }
	  keep <- !is.na(piece$treatment) | has_other
	  
	  piece[keep, ]
	}))

	
	d <- data.frame(
		country = r1$country,
		date = r1$date_hhsurvey_1_date,
		adm1 = r1$lga_district_woreda,
		adm2 = r1$sector_ward,
		farmer_gender = r1$gender_of_farmer,
		field_id = r1$farm_id,
		hhid = NA,
		treatment = r1$treatment,
		crop = r1$legume_planted_in_the_n2africa_trial,
		longitude = r1$gps_field_device_longitude_decimal_degrees,
		latitude = r1$gps_field_device_latitude_decimal_degrees,
		elevation = r1$gps_field_device_altitude_m,
		precision = r1$gps_field_device_accuracy_m,
		sample_id = r1$soil_sample_collected,
		land_ownedby = r1$ownership_field,
		soil_drainage = r1$drainage_field,
		hail_damage = r1$severity_storm_hail,
		pest_severity = r1$severity_pests,
		disease_severity = r1$severity_disease,
	  disease = r1$type_of_disease,
		pest_species = r1$type_of_pest,
		weed_species = r1$type_of_weeds,
		planting_date = r1$date_of_planting_whole_n2africa_field_date,
		harvest_date = r1$date_of_final_harvest_whole_n2a_field_date,
		inoculated = r1$inoculation_n2africa_field
		
		)
	
	#fix location data
	d$country <- carobiner::fix_name(d$country, "title") 
	d$adm1 <- carobiner::fix_name(d$adm1, "title")
	d$adm2 <- carobiner::fix_name(d$adm2, "title")
	
	d$adm1[d$adm1 == "Zango Kataf"] <- "Zangon Kataf"
	d$adm1[d$adm1 == "Illu Gelan"] <- "Ilu Gelan"
	d$adm1[d$adm1 == "Tiroafeta"] <- "Tiro Afeta"
	d$adm1[d$adm1 == "Gobu Sayo"] <- "Gobu Seyo"
	d$adm1[d$adm1 == "nadowli"] <- "Nadowli-Kaleo"
	d$adm1[d$adm1 == "wa-west"] <- "Wa West"
	d$adm1[d$adm1 == "Bungoma  Central"] <- "Bungoma Central"
	d$adm2[d$adm2 == "Grim"]<- "Grim Damchoba"
	
	d$longitude[d$adm2 == "Kpasa"] <- 0.2911
	d$latitude[d$adm2 == "Kpasa"] <- 8.4922     #https://www.geonames.org/search.html?q=Kpasa&country=
	d$longitude[d$adm2 == "Kijegere/Kinunda"] <- 31.2500
	d$latitude[d$adm2 == "Kijegere/Kinunda"] <- 1.0333     #https://www.geonames.org/search.html?q=Kijegere&country= 
	d$longitude[d$adm2 == "Kulkpanga"] <- -0.0097
	d$latitude[d$adm2 == "Kulkpanga"] <- 9.4425            #Kulkpanga (Yendi, Ghana) - https://www.geonames.org/search.html?q=Yendi&country=GH
	
	d$longitude[d$adm2 == "Yimirshika"] <- 12.2461
	d$latitude[d$adm2 == "Yimirshika"] <- 10.5269          #https://www.geonames.org/search.html?q=%09+Yimirshika&country=  
	d$longitude[d$adm2 == "Kwaya Bura"] <- 12.1269
	d$latitude[d$adm2 == "Kwaya Bura"] <- 10.5397          #https://www.geonames.org/search.html?q=Kwaya+Bura&country=
	d$longitude[d$adm2 == "Grim Damchoba"] <- 12.0472
	d$latitude[d$adm2 == "Grim Damchoba"] <- 10.5700       #https://www.geonames.org/search.html?q=Hawul&country=NG
	d$longitude[d$adm1 == "Kwaya Kusar"] <- 11.9980
	d$latitude[d$adm1 == "Kwaya Kusar"] <- 10.4430
	# villages Saamanbo and Jonga were not found, both are in Wa West
	d$longitude[d$adm1 == "Wa West"] <- -2.6308
	d$latitude[d$adm1 == "Wa West"] <- 9.9655     # https://www.geonames.org/search.html?q=Wa+West&country=GH
	d$longitude[d$adm1 == "Makurdi"] <- 8.5358
	d$latitude[d$adm1 == "Makurdi"] <- 7.7041     #https://www.geonames.org/search.html?q=Makurdi&country=
	d$longitude[d$country == "Kenya" & d$adm1 == "Busia" & d$longitude == 34.1 & d$latitude == 0.4] <- 34.2
	d$longitude[d$adm2 == "Guomal" & d$longitude == -2.8] <- -2.7
	d$longitude[d$adm1 == "Nebbi" & d$longitude == 31.0 & d$latitude == 2.4] <- 31.1
	d$longitude[d$adm1 == "Kisoro" & d$longitude == 29.6 & d$latitude == -1.2] <- 29.7
	
	d$soil_drainage[d$soil_drainage == "good"] <- "well drained"
	d$soil_drainage[d$soil_drainage == "moderate"] <- "moderately well drained"
	d$soil_drainage[d$soil_drainage == "poor"] <- "poorly drained"
	d$inoculated <- ifelse(d$inoculated == "y", TRUE, FALSE)
	
	#previous_crops
	a <- r1$crop_1_season_before_previous_season
	b <- r1$other_crops_season_before_previous_season
	
	d$previous_crop <- ifelse(is.na(a) & is.na(b), NA,
	                           trimws(paste(ifelse(is.na(a), "", a),
	                                        ifelse(is.na(b), "", b))))

	#fix dates
	d$planting_date <- carobiner::eng_months_to_nr(d$planting_date)
	d$planting_date <- as.Date(carobiner::eng_months_to_nr(d$planting_date), "%d-%m-%y")
	
	d$harvest_date <- carobiner::eng_months_to_nr(d$harvest_date)
	d$harvest_date <- as.Date(carobiner::eng_months_to_nr(d$harvest_date), "%d-%m-%y")
	
	
  r1$date_of_1st_weeding_whole_n2africa_field__date <- carobiner::eng_months_to_nr(r1$date_of_1st_weeding_whole_n2africa_field__date)
	r1$date_of_1st_weeding_whole_n2africa_field__date <- as.Date(carobiner::eng_months_to_nr(r1$date_of_1st_weeding_whole_n2africa_field__date), "%d-%m-%y")
	r1$date_of_2nd_weeding_whole_n2africa_field_date <- carobiner::eng_months_to_nr(r1$date_of_2nd_weeding_whole_n2africa_field_date)
	r1$date_of_2nd_weeding_whole_n2africa_field_date <- as.Date(carobiner::eng_months_to_nr(r1$date_of_2nd_weeding_whole_n2africa_field_date), "%d-%m-%y")
	r1$date_of_3d_weeding_whole_n2africa_field_date <- carobiner::eng_months_to_nr(r1$date_of_3d_weeding_whole_n2africa_field_date)
	r1$date_of_3d_weeding_whole_n2africa_field_date <- as.Date(carobiner::eng_months_to_nr(r1$date_of_3d_weeding_whole_n2africa_field_date), "%d-%m-%y")
	
	#weeding dates
	d$weeding_dates <- apply(
	  r1[, c("date_of_1st_weeding_whole_n2africa_field__date",
	         "date_of_2nd_weeding_whole_n2africa_field_date",
	         "date_of_3d_weeding_whole_n2africa_field_date")],
	  1,
	  function(x) paste(x[!is.na(x)], collapse = ";")
	)
	
	#spacing
	pick_by_plot <- function(r1, prefix, suffix, n_max = 12) {
	  cols <- paste0(prefix, 1:n_max, suffix)
	  m <- as.matrix(r1[, cols])
	  col_idx <- ifelse(r1$t_code %in% seq_len(n_max), r1$t_code, NA)
	  m[cbind(seq_len(nrow(m)), col_idx)]
	}
	
	d$row_spacing   <- pick_by_plot(r1, "row_spacing_crop_1_plot_",   "_cm")
	d$plant_spacing <- pick_by_plot(r1, "plant_spacing_crop_1_plot_", "_cm")
	d$yield  <- pick_by_plot(r1, "grain_weight_crop_1_plot_",  "_kg")
  
	#fertilizer_type
	fert_names <- c(
	  "Agrium Plus","Ammoniated superphosphate","Ammonium nitrate limestone",
	  "Ammonium nitrate sulfate","Ammonium polyphosphate","AMo","AN",
	  "Aqua ammonia","ATS","basic slag","Bio-sulphur","Borate 48",
	  "Borate Granular","Borax","Boron frit","Burned lime","CaCl2","CaCO3",
	  "calcitic-lime","Calcium borate","Calcium cyanamide","Calcium nitrate/urea",
	  "CAN","C-compound","CMP","CN","Copper frits","Copper polyflavonoid",
	  "Copper sulfate monohydrate","Copper sulfate pentahydrate",
	  "Copper sulfate tribasic","Crotonylidene diurea","Cu EDTA","Cu HEDTA",
	  "Cupric ammonium phosphate","Cupric oxide","CuSO4","DAP","DAS",
	  "D-compound","dolomitic-lime","DSP","ERP","Fe DTPA","Fe EDDHA","Fe EDTA",
	  "Fe HEDTA","Ferric sulfate","Ferrous ammonium phosphate",
	  "Ferrous ammonium sulfate","Ferrous carbonate","Ferrous oxalate",
	  "Ferrous sulfate","Fertilizer borate (sodium tetraborate)","FeSO4",
	  "Flowable sulfur","Flowers of sulfur","FOMIBAGARA","FOMIIMBURA",
	  "FOMITOTAHAZA","Greensand","GRP","gypsum","H3BO3","Hydrated lime",
	  "Iron Frits","Iron ligninsulfonate","Iron polyflavonoid","iron slag",
	  "Isobutylidene diurea","KCl","KMgS","KNO","L-compound","lime",
	  "Magnesium ammonium phosphate","Magnesium borate","Magnesium oxide",
	  "Magnesium sulfate","Magnesium sulfate (epsom salt)",
	  "Manganese ammonium phosphate","Manganese carbonate",
	  "Manganese chelate Mn EDTA","Manganese chloride","Manganese frits",
	  "Manganese oxide","Manganese polyflavonoid","manganese slag",
	  "Manganese sulfate","MAP","MgSO4","Minjingu 1100","Molybdenum frit",
	  "NaNO3","NCaMg","NH3","NH4Cl","none","NP","NPK","NPKBFeMgMnSZn",
	  "NPKBMgSZn","NPKMgSZn","NPKS","NPS","PA","Phosphate rock","PKS",
	  "Potassium carbonate (liquid)","Potassium carbonate (solid)",
	  "Potassium magnesium sulfate",
	  "Potassium magnesium sulfate (sulfate of potash magnesia)",
	  "Potassium metaphosphate","S","S-compound","SCU","Selma chalk",
	  "Sodium molybdate","Sodium nitrate (nitrate of soda)","Solubor","SOP",
	  "SR_urea","SSP","Sulfuric acid","SuperNitro","Superphosphoric acid",
	  "sympal","TSP","UAN","unknown","urea","Urea (sulfur coated)",
	  "Urea phosphate","Urea-ammonium phosphate",
	  "Ureaform (urea + formaldehyde)","Wet-process phosphoric acid",
	  "Wettable sulfur","YaraBela NITROMAG","YaraBela Sulfan",
	  "YaraMila ACTYVA","YaraMila COMPLEX","YaraMila HYDRAN","YaraMila Star",
	  "YaraVita Thiotrac","ZAP","Zinc chelate","Zinc ligninsulfonate",
	  "Zinc oxide","Zinc polyflavonoid","Zinc sulfate monohydrate",
	  "Zinc sulfide","ZnCl2","ZnSO4"
	)
	fert_names <- fert_names[order(-nchar(fert_names))]
	
	escape_rx <- function(s) gsub("([\\^$.|?*+()\\[\\]{}])", "\\\\\\1", s, perl = TRUE)
	
	extract_fertilizers <- function(x, names_ref) {
	  if (is.na(x)) return(NA_character_)
	  hits <- character(0)
	  remaining <- x
	  for (nm in names_ref) {
	    pat <- paste0("\\b", escape_rx(nm), "\\b")
	    if (grepl(pat, remaining, ignore.case = TRUE, perl = TRUE)) {
	      hits <- c(hits, nm)
	      remaining <- gsub(pat, "", remaining, ignore.case = TRUE, perl = TRUE)
	    }
	  }
	  if (length(hits) == 0) {
	    if (grepl("control|no input|farmer.?s practice", x, ignore.case = TRUE)) return("none")
	    return("unknown")
	  }
	  paste(hits, collapse = ";")
	}
	
	d$fertilizer_type <- vapply(r1$treatment, extract_fertilizers, character(1), names_ref = fert_names)
	
	#variety_names
	d$variety <- NA
	variety_map <- list(
	  "ACOS Dube" = c(
	    "ACOS Dube, I",
	    "ACOS Dube, P",
	    "ACOS Dube, P+I",
	    "ACOS Dube, control"
	  ),
	  "Afayak" = c(
	    "Afayak",
	    "Afayak,  + I + P",
	    "Afayak,  + Inoculant",
	    "Afayak,  + Inoculant, + TSP",
	    "Afayak, + Farmer's Practice",
	    "Afayak, + TSP",
	    "Afayak, +P +I",
	    "Afayak, control"
	  ),
	  "Apagbaala" = c(
	    "Apagbaala +P",
	    "Apagbala, + P",
	    "Apagbala, control"
	  ),
	  "Arerti" = c(
	    "Arerti, I",
	    "Arerti, P",
	    "Arerti, P+I",
	    "Arerti, control"
	  ),
	  "Atawa" = c(
	    "Atawa, manure+ TSP",
	    "atawa, +manure+TSP"
	  ),
	  "Attawa (Local)" = c(
	    "Attawa (Local), Control, Row planting",
	    "Attawa (Local), Control, Row planting, Participatory Variety Selection",
	    "Attawa (Local), TSP+Manure, Row planting"
	  ),
	  "Belesa-95" = c(
	    "Belesa-95, I",
	    "Belesa-95, P",
	    "Belesa-95, P+I",
	    "Belesa-95, control"
	  ),
	  "Cheupe" = c(
	    "Cheupe, +FYM",
	    "Cheupe, +FYM +NPK +Inoculant"
	  ),
	  "Chinese" = c(
	    "Chinese",
	    "Chinese, + P",
	    "Chinese, + TSP",
	    "Chinese, +P, 60X20 cm",
	    "Chinese, control",
	    "Chinnese, + TSP"
	  ),
	  "Clark 63K" = c(
	    "Clark 63K, I",
	    "Clark 63K, P",
	    "Clark 63K, P+I",
	    "Clark 63K, control"
	  ),
	  "Degaga" = c(
	    "Degaga, I",
	    "Degaga, P",
	    "Degaga, P+I",
	    "Degaga, control"
	  ),
	  "Dhidhessa" = c(
	    "Dhidhessa, I",
	    "Dhidhessa, P",
	    "Dhidhessa, P+I",
	    "Dhidhessa, control"
	  ),
	  "Farmer's seed" = c(
	    "Farmer's seed",
	    "Farmer's seed, + TSP"
	  ),
	  "Farmer's variety" = c(
	    "Farmer variety, +P, 60X20 cm",
	    "Farmer variety, Spacing 75x10cm",
	    "Farmer variety, Spacing 75x30cm",
	    "Farmers variety, +P +I",
	    "Farmers' variety  - P",
	    "Farmers' variety +P",
	    "Farmers' variety, + I + P",
	    "Farmers' variety, + P",
	    "Farmers' variety, control",
	    "farmer variety, +SSP, intercrop (maize_NPK_UREA)",
	    "farmer's variety, Farmer's Practice",
	    "farmers variety, control, farmer management"
	  ),
	  "Flat white" = c(
	    "Flat white , Control, Row planting",
	    "Flat white , Control, Row planting, Nutrient Management",
	    "Flat white , Control, Row planting, Participatory Variety Selection",
	    "Flat white , Manure, Row planting, Nutrient Management",
	    "Flat white , TSP+Manure, Row planting, Nutrient Management",
	    "Flat white, Manure, Row planting",
	    "Flat white, TSP +manure"
	  ),
	  "Habru" = c(
	    "Habru, I",
	    "Habru, P",
	    "Habru, P+I",
	    "Habru, control"
	  ),
	  "IT89KD-288" = c(
	    "IT89KD-288, +SSP, intercrop (maize_NPK_UREA)",
	    "IT89KD-288, +SSP, sole double row"
	  ),
	  "IT98KD-288" = c(
	    "IT98KD-288  + EVDT 99-STR-W",
	    "IT98KD-288 Sole (Double row)",
	    "Improved cowpea (IT98K-288) + Improved Maize (2009 TZE-EVDT STR)"
	  ),
	  "IT99K 573-1-1" = c(
	    "IT99K 573-1-1, +P, 10cm spacing + row spacing",
	    "IT99K 573-1-1, +P, 20cm spacing + row spacing",
	    "cowpea IT99K 573-1-1, +P, 20cm plant spacing",
	    "cowpea IT99K 573-1-1,+P, 20cm plant spacing",
	    "cowpea,IT99K 573-1-1+P+20cm plant spacing",
	    "maize+cowpea IT99K 573-1-1, +SSP+NPK+N, 20cm plant spacing",
	    "maize+cowpea,IT99K 573-1-1+SSP+NPK+N+20cm plant spacing",
	    "sorghum+IT573-1-1+SSP+NPK+N+20cm plant spacing"
	  ),
	  "IT99K-573-2-1" = c(
	    "Sole improved cowpea IT99K-573-2-1 (double row)",
	    "Sole improved cowpea IT99K-573-2-1 (single row)"
	  ),
	  "JESCA" = c(
	    "JESCA, + Control",
	    "JESCA, + NPK",
	    "JESCA, + PK+ FYM"
	  ),
	  "Jenguma" = c(
	    "Jenguma, +I",
	    "Jenguma, +P",
	    "Jenguma, +P +I",
	    "Jenguma, +P +I, Farmer's practice",
	    "Jenguma, - P -I",
	    "jenguma, +P +I, 60X10 cm, 2 seeds per hole",
	    "jenguma, +P +I, 60X5 cm, 1 seed per hole",
	    "jenguma, +P +I, 75X10 cm, 2 seeds per hole",
	    "jenguma, +P +I, 75X10 cm, 3 seeds per hole",
	    "jenguma,, +P +I, 75X5 cm, 1 seeds per hole"
	  ),
	  "Kabonge red" = c(
	    "kabonge red, +TSP +gypsum",
	    "kabonge red, TSP",
	    "kabonge red, control"
	  ),
	  "Kabwesere (Local)" = c(
	    "Kabwesere (Local), Control, Row planting",
	    "Kabwesere (Local), Control, Row planting, Nutrient Management",
	    "Kabwesere (Local), Control, Row planting, Participatory Variety Selection",
	    "Kabwesere (Local), Manure, Row planting, Nutrient Management",
	    "Kabwesere (Local), TSP+Manure, Row planting",
	    "Kabwesere (Local), TSP+Manure, Row planting, Nutrient Management",
	    "Kabweseri (local), Manure, Row planting"
	  ),
	  "Kasidi (Local)" = c(
	    "Kasidi (Local), Control, Row planting",
	    "Kasidi (Local), TSP, Row planting"
	  ),
	  "Katuna (local)" = c(
	    "katuna (local) , TSP",
	    "katuna (local) , control",
	    "katuna (local) , manure+TSP"
	  ),
	  "Keta" = c(
	    "Keta, I",
	    "Keta, P",
	    "Keta, P+I",
	    "Keta, control"
	  ),
	  "Kibumbuli" = c(
	    "Kibumbuli, +NPK",
	    "Kibumbuli, +NPK + FYM",
	    "Kibumbuli, Control"
	  ),
	  "Kirkhouse" = c(
	    "Kirkhouse, control",
	    "kirkhouse, +P"
	  ),
	  "LN 8E" = c(
	    "LN 8E + DAP",
	    "LN 8E, + Control",
	    "LN 8E, + Inoculant",
	    "LN 8E, +Inoculant + DAP"
	  ),
	  "Local" = c(
	    "Local , Control",
	    "Local ,+ TSP",
	    "Local, + Control",
	    "Local, + DAP",
	    "Local, + DAP, sole",
	    "Local, + NPK",
	    "Local, + PK+ FYM",
	    "Local, +DAP+Urea, intercrop local maize",
	    "Local, +DAP+Urea, intercrop, maize Local",
	    "Local, +DAP, intercrop, maize Local",
	    "Local, +Inoculant+ TSP+ sole",
	    "Local, I",
	    "Local, Inoculant",
	    "Local, P",
	    "Local, P+I",
	    "Local, control"
	  ),
	  "Lyamungo 90" = c(
	    "Lyamungo 90,  + Inoculant (Legumefix), sole",
	    "Lyamungo 90, +N+P+K (NPK), sole",
	    "Lyamungo 90, +N+P+K+Inoculant (NPK Legumefix), sole",
	    "Lyamungo 90, +NPK",
	    "Lyamungo 90, +NPK +FYM",
	    "Lyamungo 90, +P+K (MKP), sole",
	    "Lyamungo 90, +P+K+Inoculant (MKP Legumefix), sole",
	    "Lyamungo 90, Control",
	    "Lyamungo 90, no fertilizer, sole",
	    "Lyamungu 90, + Control",
	    "Lyamungu 90, + Control, sole",
	    "Lyamungu 90, + DAP",
	    "Lyamungu 90, +NPK",
	    "Lyamungu 90, +PK+FYM"
	  ),
	  "MAC 44" = c(
	    "MAC 44, +FYM",
	    "MAC 44, +FYM +NPK +Inoculant"
	  ),
	  "Maize (variety unspecified)" = c(
	    "Maize, Biofix (Mayer)",
	    "Maize, Biofix (Mayer) + TSP",
	    "Maize, MakBiofixer"
	  ),
	  "MakSoy 2N" = c(
	    "MakSoy2N, +TSP",
	    "MakSoy2N, +TSP +Legumefix",
	    "MakSoy2N, +TSP +makbiofix",
	    "MakSoy2N, control",
	    "Maksoy 2N, Makbiofixer",
	    "Maksoy 2N, TSP",
	    "Maksoy 2N, TSP+Makbiofixer",
	    "Maksoy 2N, TSP+Makbiofixer, Beans Clean herbicide",
	    "Maksoy 2N, TSP+Makbiofixer, Farmer's weeding practice",
	    "Maksoy 2N, TSP+Makbiofixer, Recommended weeding",
	    "Maksoy 2N, control"
	  ),
	  "MakSoy 3N" = c(
	    "MakSoy 3N, +TSP",
	    "MakSoy 3N, +TSP +legumefix",
	    "MakSoy 3N, +TSP +makbiofix",
	    "MakSoy 3N, control",
	    "Maksoy 3N, Biofix (Mayer)",
	    "Maksoy 3N, Biofix (Mayer) + TSP",
	    "Maksoy 3N, Biofix(Mayer)",
	    "Maksoy 3N, Control",
	    "Maksoy 3N, Conventional rate",
	    "Maksoy 3N, Conventional rate + Biofix(Mayer)",
	    "Maksoy 3N, Conventional rate + Kinyzobium",
	    "Maksoy 3N, Conventional rate + MakBiofixer",
	    "Maksoy 3N, Grain Pulse rate",
	    "Maksoy 3N, Grain Pulse rate + Biofix(Mayer)",
	    "Maksoy 3N, Grain Pulse rate + Kinyzobium",
	    "Maksoy 3N, Grain Pulse rate + MakBbiofixer",
	    "Maksoy 3N, Kinyzobium",
	    "Maksoy 3N, Kinyzobium + TSP",
	    "Maksoy 3N, MakBiofixer",
	    "Maksoy 3N, Makbiofixer",
	    "Maksoy 3N, TSP",
	    "Maksoy 3N, TSP+Makbiofixer",
	    "Maksoy 3N, control"
	  ),
	  "MakSoy 4N" = c(
	    "MakSoy 4N, +TSP",
	    "MakSoy 4N, +TSP +legumefix",
	    "MakSoy 4N, +TSP +makbiofix",
	    "MakSoy 4N, control",
	    "Maksoy 4N, TSP",
	    "Maksoy 4N, TSP+Makbiofixer",
	    "Maksoy 4N, control"
	  ),
	  "MakSoy 5N" = c(
	    "MakSoy 5N, +TSP",
	    "MakSoy 5N, +TSP +legumefix",
	    "MakSoy 5N, +TSP +makbiofix",
	    "MakSoy 5N, control",
	    "Maksoy 5N, Makbiofixer",
	    "Maksoy 5N, TSP",
	    "Maksoy 5N, TSP+Makbiofixer",
	    "Maksoy 5N, control"
	  ),
	  "Makwacha" = c(
	    "Makwacha + inoculant",
	    "Makwacha minus inoculany"
	  ),
	  "Mkemwema" = c(
	    "Mkemwema, +NPK",
	    "Mkemwema, +NPK + FYM",
	    "Mkemwema, Control"
	  ),
	  "Mnanje" = c(
	    "Mnanje, +FYM",
	    "Mnanje, +P",
	    "Mnanje, +P +FYM",
	    "Mnanje, +P +FYM +Gypsum",
	    "Mnanje, Control"
	  ),
	  "Moti" = c(
	    "Moti, I",
	    "Moti, P",
	    "Moti, P+I",
	    "Moti, control"
	  ),
	  "NARO Bean 1" = c(
	    "NARO Bean 1, Control, Row planting",
	    "NARO Bean 1, TSP, Row planting"
	  ),
	  "NARO Bean 15" = c(
	    "NARO Bean 15, Control, Row planting",
	    "NARO Bean 15, TSP, Row planting"
	  ),
	  "NARO Bean 4" = c(
	    "NARO Bean 4, Control, Row planting",
	    "NARO Bean 4, TSP, Row planting"
	  ),
	  "NARO Bean 4C" = c(
	    "NARO Bean 4C, Control",
	    "NARO Bean 4C, Lime",
	    "NARO Bean 4C, Manure",
	    "NARO Bean 4C, TSP",
	    "NARO Bean 4C, TSP+Manure"
	  ),
	  "NARO Bean 5C" = c(
	    "NARO Bean 5C, Control",
	    "NARO Bean 5C, Lime",
	    "NARO Bean 5C, Manure",
	    "NARO Bean 5C, TSP",
	    "NARO Bean 5C, TSP+Manure"
	  ),
	  "Nabe 10C (local Kabale)" = c(
	    "Nabe 10C (local Kabale), manure+ TSP",
	    "nabe 10 (kabale local), +manure+TSP"
	  ),
	  "Nabe 12C" = c(
	    "Nabe 12C, Control",
	    "Nabe 12C, Control, Row planting",
	    "Nabe 12C, Control, Row planting, Participatory Variety Selection",
	    "Nabe 12C, DAP",
	    "Nabe 12C, DAP+ NPK",
	    "Nabe 12C, DAP+NPK, Row planting",
	    "Nabe 12C, DAP, Row planting",
	    "Nabe 12C, Manure, Row planting",
	    "Nabe 12C, Manure, Row planting, Nutrient Management",
	    "Nabe 12C, TSP",
	    "Nabe 12C, TSP + Sisal strings",
	    "Nabe 12C, TSP + Tripods",
	    "Nabe 12C, TSP , Row planting",
	    "Nabe 12C, TSP , Row planting+Tripods",
	    "Nabe 12C, TSP , Row planting+Tripods, Nutrient Management",
	    "Nabe 12C, TSP+Manure",
	    "Nabe 12C, TSP+Manure, Row planting",
	    "Nabe 12C, TSP+Manure, Row planting, Nutrient Management",
	    "Nabe 12C, TSP, Beans Clean herbicide",
	    "Nabe 12C, TSP, Farmer's weeding practice",
	    "Nabe 12C, TSP, Recommended weeding",
	    "Nabe 12C, TSP, Row planting+Sisal Strings",
	    "Nabe 12C, no fertliser, Beans Clean herbicide",
	    "Nabe 12C, no fertliser, Farmer's weeding practice",
	    "Nabe 12C, no fertliser, Recommended weeding",
	    "nabe 12 C, +Dap",
	    "nabe 12 C, +Dap +NPK",
	    "nabe 12 C, +TSP",
	    "nabe 12 C, +control",
	    "nabe 12 C, +manure +TSP",
	    "nabe 12C, Manure+TSP+sisal strings",
	    "nabe 12C, TSP",
	    "nabe 12C, control",
	    "nabe 12C, manure",
	    "nabe 12C, manure+TSP",
	    "nabe 12C, manure+TSP+de-topping",
	    "nabe 12C, manure+TSP+farmer practice"
	  ),
	  "Nabe 26C" = c(
	    "nabe 26 C, +manure +TSP",
	    "nabe 26C, control",
	    "nabe 26C, manure+TSP"
	  ),
	  "Nambale (Local)" = c(
	    "Nambale (Local) , Control, Row planting",
	    "Nambale (Local) , Control, Row planting, Nutrient Management",
	    "Nambale (Local), Control, Row planting",
	    "Nambale (Local), Control, Row planting, Participatory Variety Selection",
	    "Nambale (Local), TSP+Manure, Row planting",
	    "Nambale (Local), TSP+Manure, Row planting, Nutrient Management",
	    "Nambale (Local), TSP, Row planting",
	    "Nambale (Local), TSP, Row planting, Nutrient Management",
	    "Nambale (local), +TSP",
	    "Nambale (local), +TSP +manure",
	    "Nambale (local), +manure",
	    "Nambale (local), control"
	  ),
	  "Nasir" = c(
	    "Nasir, I",
	    "Nasir, P",
	    "Nasir, P+I",
	    "Nasir, control"
	  ),
	  "Njano Uyole" = c(
	    "Njano Uyole + DAP, intercrop (MBILI) maize DK 8031",
	    "Njano Uyole , + DAP, sole",
	    "Njano Uyole, + Control",
	    "Njano Uyole, + DAP",
	    "Njano Uyole, + DAP, intercrop (MBILI) maize DH 04",
	    "Njano Uyole, + DAP, intercrop maize DH 04",
	    "Njano Uyole, + DAP, intercrop maize DK 8031",
	    "Njano Uyole, + DAP, sole",
	    "Njano Uyole, +DAP+Urea, intercrop PAN 67 maize",
	    "Uyole njano, +NPK",
	    "Uyole njano, +NPK + FYM",
	    "Uyole njano, Control"
	  ),
	  "Nyiramuhondo (Iron enriched)" = c(
	    "Nyiramuhondo (Iron enriched), Control, Row planting",
	    "Nyiramuhondo (Iron enriched), Control, Row planting, Nutrient Management",
	    "Nyiramuhondo (Iron enriched), Control, Row planting, Participatory Variety Selection",
	    "Nyiramuhondo (Iron enriched), Manure, Row planting",
	    "Nyiramuhondo (Iron enriched), Manure, Row planting, Nutrient Management",
	    "Nyiramuhondo (Iron enriched), TSP +manure",
	    "Nyiramuhondo (Iron enriched), TSP+Manure, Row planting, Nutrient Management",
	    "Nyiramuhondo (Iron enriched), TSP, Row planting, Nutrient Management",
	    "iron enriched (Nyiramuhondo), TSP",
	    "iron enriched (Nyiramuhondo), control",
	    "iron enriched (Nyiramuhondo), manure+TSP",
	    "iron enriched, +manure +TSP"
	  ),
	  "Padituya" = c(
	    "Padituya +P",
	    "Padituya -P",
	    "Padituya, + P",
	    "Padituya, control"
	  ),
	  "Pendo" = c(
	    "Pendo,  +P +FYM +Gypsum",
	    "Pendo, +FYM",
	    "Pendo, +P",
	    "Pendo, +P +FYM",
	    "Pendo, Control",
	    "Pendo, Control + Aflasafe",
	    "Pendo, FYM",
	    "Pendo, FYM + Aflasafe",
	    "Pendo, FYM +Minjingu",
	    "Pendo, FYM+ Minjingu + Gypsum",
	    "Pendo, FYM+ Minjingu + Gypsum + Aflasafe",
	    "Pendo, FYM+ Minjingu+ Aflasafe",
	    "Pendo, Gypsum",
	    "Pendo, Gypsum + Aflasafe",
	    "Pendo, Minjingu",
	    "Pendo, Minjingu + Aflasafe"
	  ),
	  "ROBA 1" = c(
	    "ROBA 1, +TSP",
	    "ROBA 1, +TSP +manure",
	    "ROBA 1, control",
	    "ROBA 1, manure"
	  ),
	  "RWR 2154 (Iron enriched)" = c(
	    "RWR 2154  (Iron enriched), Control, Row planting",
	    "RWR 2154  (Iron enriched), Control, Row planting, Nutrient Management",
	    "RWR 2154  (Iron enriched), Control, Row planting, Participatory Variety Selection",
	    "RWR 2154  (Iron enriched), TSP+Manure, Row planting",
	    "RWR 2154  (Iron enriched), TSP+Manure, Row planting, Nutrient Management",
	    "RWR 2154  (Iron enriched), TSP, Row planting",
	    "RWR 2154  (Iron enriched), TSP, Row planting, Nutrient Management",
	    "RWR 2154, Control, Beans Clean herbicide",
	    "RWR 2154, Control, Farmer's weeding practice",
	    "RWR 2154, TSP+Manure, Beans Clean herbicide",
	    "RWR 2154, TSP+Manure, Recommended weeding",
	    "RWR 2154, TSP, Beans Clean herbicide",
	    "RWR 2154, TSP, Recommended weeding"
	  ),
	  "RWR 2245 (Iron enriched)" = c(
	    "RWR 2245 (Iron enriched), Control, Row planting",
	    "RWR 2245 (Iron enriched), Control, Row planting, Nutrient Management",
	    "RWR 2245 (Iron enriched), Control, Row planting, Participatory Variety Selection",
	    "RWR 2245 (Iron enriched), TSP+Manure, Row planting",
	    "RWR 2245 (Iron enriched), TSP+Manure, Row planting, Nutrient Management",
	    "RWR 2245 (Iron enriched), TSP, Row planting",
	    "RWR 2245 (Iron enriched), TSP, Row planting, Nutrient Management",
	    "RWR 2245, +TSP",
	    "RWR 2245, +TSP +manure",
	    "RWR 2245, +manure",
	    "RWR 2245, Control, Beans Clean herbicide",
	    "RWR 2245, Control, Farmer's weeding practice",
	    "RWR 2245, TSP+Manure, Beans Clean herbicide",
	    "RWR 2245, TSP+Manure, Recommended weeding",
	    "RWR 2245, TSP, Beans Clean herbicide",
	    "RWR 2245, TSP, Recommended weeding",
	    "RWR 2245, control"
	  ),
	  "Raha 1" = c(
	    "Raha 1, +N+P (DAP), sole",
	    "Raha 1, no fertilizer, sole",
	    "Raha1, + Control",
	    "Raha1, +DAP",
	    "Raha1, +DAP , Insecticide",
	    "Raha1, , Insecticide"
	  ),
	  "Red beauty" = c(
	    "Red beauty, +TSP",
	    "Red beauty, +TSP +gypsum",
	    "Red beauty, control"
	  ),
	  "SAMNUT 22" = c(
	    "Farmer variety (SAMNUT 22):10-cm intra-row spacing: Spacing: 75x10 cm",
	    "Farmer variety (SAMNUT 22):30-cm intra-row spacing: Spacing: 75x30 cm",
	    "Improved variety (SAMNUT 22):10-cm intra-row spacing: Spacing: 75x10 cm",
	    "Improved variety (SAMNUT 22):30-cm intra-row spacing: Spacing: 75x30 cm",
	    "SAMNUT 22, +P only",
	    "SAMNUT 22, +P,  10 x 75cm plant spacing",
	    "SAMNUT 22, +P, 20 x 75cm plant spacing",
	    "Samnut 22",
	    "Samnut 22, + P",
	    "Samnut 22, + TSP",
	    "Samnut 22, Spacing 75x10cm",
	    "Samnut 22, Spacing 75x30cm",
	    "Samnut 22, control",
	    "samnut 22, + P 60X20 cm"
	  ),
	  "SAMNUT 23" = c(
	    "SAMNUT 23 +P,  10 x 75cm plant spacing",
	    "SAMNUT 23, +P, 20 x 75cm plant spacing",
	    "Samnut 23",
	    "Samnut 23, + P",
	    "Samnut 23, + TSP",
	    "Samnut 23, +SSP +apron star, 20x75 2 seeds",
	    "Samnut 23, +SSP, 20x75 2 seeds",
	    "Samnut 23, control",
	    "samnut 23, + P, 60X20 cm",
	    "samnut 23, -  P, 60X20 cm",
	    "samnut 23, - P, Farmers' Practice"
	  ),
	  "SAMNUT 24" = c(
	    "Farmer variety (SAMNUT 24):10-cm intra-row spacing: Spacing: 75x10 cm",
	    "Farmer variety (SAMNUT 24):30-cm intra-row spacing: Spacing: 75x30 cm",
	    "G/nuts SAMNUT 24,+SSP,  plant spacing",
	    "G/nuts,SAMNUT 24+SSP+ plant spacing",
	    "Improved variety (SAMNUT 24):10-cm intra-row spacing: Spacing: 75x10 cm",
	    "Improved variety (SAMNUT 24):30-cm intra-row spacing: Spacing: 75x30 cm",
	    "SAMNUT 24 +P,  10 x 75cm plant spacing",
	    "SAMNUT 24, +P, 20 cm spacing + row spacing",
	    "SAMNUT 24, +P, 20 x 75cm plant spacing",
	    "maize +  SAMNUT 24, +SSP+NPK+N, 20cm plant spacing",
	    "maize + g/nuts, SAMNUT 24+SSP+NPK+N +20cm plant spacing",
	    "sorghum+SAMNUT 24+SSP+NPK+N+25cm plant spacing"
	  ),
	  "SAMNUT 25" = c(
	    "SAMNUT 25 +P,  10 x 75cm plant spacing",
	    "SAMNUT 25, +P, 20 x 75cm plant spacing"
	  ),
	  "SC-Safari" = c(
	    "SC-Safari, Control, sole",
	    "SC-Safari, Inoculant+ TSP, sole",
	    "SC-Safari, Inoculant, sole",
	    "SC-Safari, TSP, sole"
	  ),
	  "SC-Samba" = c(
	    "SC-Samba, Control, sole",
	    "SC-Samba, Inoculant+ TSP, sole",
	    "SC-Samba, Inoculant, sole",
	    "SC-Samba, TSP, sole"
	  ),
	  "SC-Semeki" = c(
	    "SC-Semeki,  Control, sole",
	    "SC-Semeki,  Inoculant+ TSP, sole",
	    "SC-Semeki,  Inoculant, sole",
	    "SC-Semeki,  TSP, sole"
	  ),
	  "SC-Spike" = c(
	    "SC-Spike,  Control, sole",
	    "SC-Spike,  Inoculant, sole",
	    "SC-Spike, Inoculant+ TSP, sole",
	    "SC-Spike, TSP, sole"
	  ),
	  "Selian 06" = c(
	    "selian 06, +FYM",
	    "selian 06, +FYM +NPK",
	    "selian 06, +FYM +NPK +Inoculant",
	    "selian 06, +Inoculant",
	    "selian 06, +NPK",
	    "selian 06, Control"
	  ),
	  "Serenut 11T" = c(
	    "Serenut 11T, +TSP",
	    "Serenut 11T, +TSP + gypsum",
	    "Serenut 11T, control"
	  ),
	  "Serenut 14R" = c(
	    "Serenut 14R, + gypsum",
	    "Serenut 14R, +TSP",
	    "Serenut 14R, +TSP + gypsum",
	    "Serenut 14R, control",
	    "serenut 14, control"
	  ),
	  "Serenut 5" = c("Serenut 5, +TSP","Serenut 5, +TSP +gypsum","Serenut 5, control"
	  ),
	  "Serenut 5R" = c(
	    "Serenut 5R, + gypsum",
	    "Serenut 5R, +TSP",
	    "Serenut 5R, +TSP+gypsum",
	    "Serenut 5R, control"
	  ),
	  "Shallo" = c("Shallo, I","Shallo, P","Shallo, P+I","Shallo, control"
	  ),
	  "Songotura" = c("Songotura +P","Songotura -P"
	  ),
	  "Sorghum (farmer variety)" = c(
	    "sorghum Farmer's var., +NPK+N, 25cm plant spacing",
	    "sorghum, Farmer's Var.+NPK+N+25cm plant spacing"
	  ),
	  "Sorghum (variety unspecified)" = c(
	    "Sorghum, Biofix (Mayer)",
	    "Sorghum, Biofix (Mayer) + TSP",
	    "Sorghum, Biofix(Mayer)",
	    "Sorghum, Control",
	    "Sorghum, Conventional rate",
	    "Sorghum, Conventional rate + Biofix(Mayer)",
	    "Sorghum, Conventional rate + Kinybium",
	    "Sorghum, Conventional rate + MakBiofixer",
	    "Sorghum, Grain Pulse rate",
	    "Sorghum, Grain Pulse rate + Biofix(Mayer)",
	    "Sorghum, Grain Pulse rate + Kinybium",
	    "Sorghum, Grain Pulse rate + MakBbiofixer",
	    "Sorghum, Kinybium",
	    "Sorghum, Kinybium + TSP",
	    "Sorghum, MakBiofixer",
	    "Sorghum, TSP"
	  ),
	  "Soungpoung" = c(
	    "Soungpoung",
	    "Soungpoung,  + Inoculant",
	    "Soungpoung,  + Inoculant, + TSP",
	    "Soungpoung, + Farmer's Practice",
	    "Soungpoung, + TSP",
	    "Soungpungu, + I+ P",
	    "Soungpungu, control",
	    "Sungpungu, +P +I"
	  ),
	  "TGX1448-2E" = c(
	    "Soybean,TGX 1448-2E+SSP+Inoculant+10cmplant spacing",
	    "TGX 1448-2E, +NM  +P",
	    "TGX 1448-2E, +NM  -P",
	    "TGX 1448-2E, -LF  +P",
	    "TGX 1448-2E, -NM  +P",
	    "TGX 1448-2E, -NM  -P (Control)",
	    "TGX1448-2E, + Inoculant (Nodumax) , Row Planting",
	    "TGX1448-2E, + P application, Row Planting",
	    "TGX1448-2E, +P application + Inoculant , Row Planting",
	    "TGX1448-2E, Control",
	    "TGX1448-2E, I only",
	    "TGX1448-2E, P only"
	  ),
	  "TGX1835-10E" = c(
	    "TGX 1835 - 10E ,+ I + P",
	    "TGX 1835 - 10E, control",
	    "TGX1835 -10E, +I+P, 10cm spacing + row spacing",
	    "TGX1835-10E, +I-P, 10cm spacing + row spacing",
	    "sorghum+soybean,TGX1835-10E+SSP+Inoculant+NPK+N+10cm plant spacing"
	  ),
	  "TGX1904-6F" = c(
	    "TGX 1904-6F  +LF +P",
	    "TGX 1904-6F  +NM +P",
	    "TGX 1904-6F (control)",
	    "TGX 1904-6F +LF -P",
	    "TGX 1904-6F +NM -P",
	    "TGX 1904-6F -LF  +P",
	    "TGX 1904-6F -NM  +P",
	    "TGX1904-6F +LF+P",
	    "TGX1904-6F +LF-P",
	    "TGX1904-6F +NM+P",
	    "TGX1904-6F +NM-P",
	    "TGX1904-6F, + P application, Row Planting",
	    "TGX1904-6F, +P application + Inoculant , Row Planting",
	    "TGX1904-6F, Control",
	    "TGX1904-6F, I + P",
	    "TGX1904-6F, I only",
	    "TGX1904-6F, P only"
	  ),
	  "TGX1951-3F" = c(
	    "TGX 1951-3F,  +LF  +P",
	    "TGX 1951-3F,  +NM  +P",
	    "TGX 1951-3F, +LF -P",
	    "TGX 1951-3F, +NM -P",
	    "TGX 1951-3F, -LF  +P",
	    "TGX 1951-3F, -LF -P (Control)",
	    "TGX 1951-3F, -NM  +P",
	    "TGX 1951-3F, -NM -P (Control)",
	    "TGX1951 -3F, +I+P, 10cm spacing + row spacing",
	    "TGX1951-3F (Control), no input , Row Planting",
	    "TGX1951-3F +LF+P",
	    "TGX1951-3F +LF-P",
	    "TGX1951-3F +NM+P",
	    "TGX1951-3F +NM-P",
	    "TGX1951-3F, + Inoculant (Nodumax) , Row Planting",
	    "TGX1951-3F, + P application, Row Planting",
	    "TGX1951-3F, +I+P, 5cm spacing + row spacing",
	    "TGX1951-3F, +I-P, 10cm spacing + row spacing",
	    "TGX1951-3F, +Organic manure , Row Planting",
	    "TGX1951-3F, +P application + Inoculant , Row Planting",
	    "TGX1951-3F, -I+P, 10cm spacing + row spacing",
	    "TGX1951-3F, -I-P, 10cm spacing + row spacing",
	    "TGX1951-3F, Farmer's practice"
	  ),
	  "TGX1955-4F" = c(
	    "TGX 1955-4F , +LF +P",
	    "TGX 1955-4F , +NM +P",
	    "TGX 1955-4F ,+LF -P",
	    "TGX 1955-4F ,+NM -P",
	    "TGX 1955-4F, -LF  +P",
	    "TGX 1955-4F, -LF  -P (Control)",
	    "TGX 1955-4F, -NM  +P",
	    "TGX 1955-4F, -NM  -P (Control)",
	    "TGX1955-4F +LF+P",
	    "TGX1955-4F +LF-P",
	    "TGX1955-4F +NM+P",
	    "TGX1955-4F +NM-P"
	  ),
	  "TZE99EVDT-STR-W maize" = c(
	    "EVDT 99-STR-W (sole)",
	    "Farmer_s Cowpea + EVDT 99- STR-W",
	    "Maize,EVDT 2009+NPK + N",
	    "Sole improved maize (2009 TZE-EVDT STR)",
	    "TZE99EVDT-STR-Wmaize, NPK + Urea, sole crop",
	    "maize EVDT 2009, +NPK+N+25cm, plant spacing"
	  ),
	  "Tikolore" = c("Tikolore + inoculant","Tikolore minus inoculant"
	  ),
	  "Tumaini" = c(
	    "Tumaini, + Control",
	    "Tumaini, +DAP",
	    "Tumaini, +DAP, Insecticide",
	    "Tumaini, +N+P (DAP), sole",
	    "Tumaini, , Insecticide",
	    "Tumaini, no fertilizer, sole"
	  ),
	  "UAM 1046-6-1" = c(
	    "Sole improved cowpea UAM 1046-6-1 (double row)"
	  ),
	  "UY Soya 2" = c(
	    "UY Soya 2,  Control, sole",
	    "UY Soya 2, Inoculant+ TSP, sole",
	    "UY Soya 2, Inoculant, sole",
	    "UY Soya 2, TSP, sole"
	  ),
	  "UY soya 3" = c(
	    "UY soya 3,  Control",
	    "UY soya 3, + Inoculant",
	    "UY soya 3, + Inoculant+ TSP",
	    "UY soya 3, + TSP"
	  ),
	  "UY soya 4" = c(
	    "UY soya 4,  + TSP",
	    "UY soya 4, + Inoculant",
	    "UY soya 4, + Inoculant+ TSP",
	    "UY soya 4, Control"
	  ),
	  "Uyole soya 2" = c(
	    "Uyole soya 2, + Control",
	    "Uyole soya 2, + DAP",
	    "Uyole soya 2, + Inoculant",
	    "Uyole soya 2, +Inoculant + DAP"
	  ),
	  "V1" = c("V1-PD1","V1-PD2","V1PD1","V1PD2")
	  ,
	  "V2" = c("V2-PD1","V2-PD2","V2PD1","V2PD2")
	  ,
	  "V3" = c("V3-PD1","V3-PD2","V3PD1","V3PD2")
	  ,
	  "V4" = c("V4-PD1","V4-PD2","V4PD1","V4PD2" )
	  ,
	  "Vuli 2" = c(
	    "Vuli 2, + DAP (cowpea) + Urea, intercrop (MBILI) maize SC 513",
	    "Vuli 2, + DAP (cowpea) + Urea, intercrop maize SC 513",
	    "Vuli 2, + DAP + Urea, intercrop (MBILI) maize SC 513",
	    "Vuli 2, + DAP + Urea, intercrop maize SC 513",
	    "Vuli 2, +Control, sole",
	    "Vuli 2, +DAP, sole"
	  ),
	  "Vuli AR1" = c("Vuli AR1, +N+P (DAP), sole","Vuli AR1, no fertilizer, sole")
	  ,
	  "Vuli R1" = c(
	    "Vuli R1, + Control",
	    "Vuli R1, +DAP",
	    "Vuli R1, +DAP , Insecticide",
	    "Vuli R1, , Insecticide"
	  ),
	  "Wang-Kae" = c("Wang - Kae, + P","Wang - Kae, , control")
	  ,
	  "Wolki" = c("Wolki, I","Wolki, P","Wolki, P+I","Wolki, control")
	  ,
	  "Yellow bean" = c("Yellow bean, TSP, Row planting, Nutrient Management")
	  ,
	  "Zaayera" = c("Zaayera +P")
	  ,
	  "DH 04" = c("maize DH 04, +DAP")
	  ,
	  "DK 8031" = c(
	    "maize DK 8031, + DAP + Urea",
	    "maize DK 8031, control",
	    "maize DK 8031,+DAP+Urea, sole maize DK 8031"
	  ),
	  "PAN 67" = c("maize PAN 67, +DAP+Urea, sole maize")
	  ,
	  "SC 513" = c("maize SC 513 ,+ DAP + Urea","maize SC 513,control")
	  ,
	  "SC 719" = c("maize SC 719,+Control, sole")
	)
	
	d$variety <- NA_character_
	for (v in names(variety_map)) {
	  d$variety[d$treatment %in% variety_map[[v]]] <- v
	}
	
	#intercrops
	intercrops_map <- list(
	  "bean_maize" = c(
	    "Local, +DAP+Urea, intercrop local maize",
	    "Local, +DAP+Urea, intercrop, maize Local",
	    "Local, +DAP, intercrop, maize Local"
	  ),
	  "cowpea_maize" = c(
	    "Farmer_s Cowpea + EVDT 99- STR-W",
	    "IT89KD-288, +SSP, intercrop (maize_NPK_UREA)",
	    "IT98KD-288  + EVDT 99-STR-W",
	    "Improved cowpea (IT98K-288) + Improved Maize (2009 TZE-EVDT STR)",
	    "farmer variety, +SSP, intercrop (maize_NPK_UREA)",
	    "maize+cowpea IT99K 573-1-1, +SSP+NPK+N, 20cm plant spacing",
	    "maize+cowpea,IT99K 573-1-1+SSP+NPK+N+20cm plant spacing"
	  ),
	  "cowpea_sorghum" = c(
	    "sorghum+IT573-1-1+SSP+NPK+N+20cm plant spacing"
	  ),
	  "groundnut_maize" = c(
	    "maize +  SAMNUT 24, +SSP+NPK+N, 20cm plant spacing",
	    "maize + g/nuts, SAMNUT 24+SSP+NPK+N +20cm plant spacing"
	  ),
	  "groundnut_sorghum" = c(
	    "sorghum+SAMNUT 24+SSP+NPK+N+25cm plant spacing"
	  ),
	  "maize_soybean" = c(
	    "Njano Uyole + DAP, intercrop (MBILI) maize DK 8031",
	    "Njano Uyole, + DAP, intercrop (MBILI) maize DH 04",
	    "Njano Uyole, + DAP, intercrop maize DH 04",
	    "Njano Uyole, + DAP, intercrop maize DK 8031",
	    "Njano Uyole, +DAP+Urea, intercrop PAN 67 maize",
	    "Vuli 2, + DAP (cowpea) + Urea, intercrop (MBILI) maize SC 513",
	    "Vuli 2, + DAP (cowpea) + Urea, intercrop maize SC 513",
	    "Vuli 2, + DAP + Urea, intercrop (MBILI) maize SC 513",
	    "Vuli 2, + DAP + Urea, intercrop maize SC 513"
	  ),
	  "sorghum_soybean" = c(
	    "sorghum+soybean,TGX1835-10E+SSP+Inoculant+NPK+N+10cm plant spacing"
	  )
	)
	
	d$intercrops <- "sole"
	for (combo in names(intercrops_map)) {
	  d$intercrops[d$treatment %in% intercrops_map[[combo]]] <- combo
	}
	
	d$crop[d$crop=="soya_bean"] <- "soybean"
	d$crop[d$crop=="pigeon_pea"] <- "pigeon pea"
	d$crop[d$crop %in% c("climbing_bean","bush_bean")] <- "common bean"
	d$crop[d$crop=="faba_bean"] <- "faba bean"
	
	d$trial_id <- as.character(1)
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$irrigated <- FALSE
  d$geo_from_source <- TRUE
  d$yield_isfresh <- NA
  d$yield_moisture <- NA
  d$yield_part <- "seed"
  d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- d$S_fertilizer <- NA
  
  #fix intercrops
  d$intercrops[d$intercrops == "bean_maize"] <- "common bean_maize"
  
  #fix previous_crop name
  pc_map <- c(
    pigeon_pea    = "pigeon pea",
    soyabean      = "soybean",
    sweet_potato  = "sweetpotato",
    irish_potato  = "potato",
    bush_bean     = "common bean",       
    climbing_bean = "common bean",       
    green_gram    = "mung bean",
    bambara_bean  = "bambara groundnut", 
    faba_bean     = "faba bean",         
    vegetables    = "vegetable",
    fallow        = "none",              
    ensete        = "enset"              
  )
  
  fix_prev <- function(x) {
    sapply(strsplit(x, " "), function(v) {
      if (all(is.na(v))) return(NA_character_)
      v <- ifelse(v %in% names(pc_map), pc_map[v], v)
      v <- unique(v[v != "other"])          
      if (length(v) == 0) NA_character_ else paste(v, collapse = "; ")
    }, USE.NAMES = FALSE)
  }
  
  d$previous_crop <- fix_prev(d$previous_crop)
  
  #fix disease names (for those that are not specified in terminag, resotrted to using scientic names)
  clean_dis <- function(x) {
    x <- gsub("([a-z])([A-Z])", "\\1 \\2", x)          # split run-together words (spotBacterial)
    x <- tolower(x)
    x <- gsub("\\s+", " ", x)
    x <- gsub("\\bbr?ights?\\b", "blight", x)           # bright / bight -> blight
    x <- gsub("lateb[rl]ights?", "late blight", x)
    x <- gsub("early and late blights?", "early blight, late blight", x)
    x <- gsub("late and early blights?", "late blight, early blight", x)
    x <- gsub("sport", "spot", x)
    x <- gsub("lesf", "leaf", x)
    x <- gsub("bacter(ia|ial|ail)\\b", "bacterial", x)
    x <- gsub("willt", "wilt", x)
    x <- gsub("\\broo?t ?roo?ts?\\b", "root rot", x)      # rootroot, rot rot, rootrot
    x <- gsub("dry root(?! rot)", "dry root rot", x, perl = TRUE)
    x <- gsub("\\brott\\b", "rot", x)
    x <- gsub("checolate", "chocolate", x)
    x <- gsub("milder\\b", "mildew", x)
    x <- gsub("hallow", "halo", x)
    x
  }
  
  # order -> specific terms first, generic ones (rust, leaf spot, blight, wilt) last
  # anything not matched (nil, none, not known, #NAME?, pests, weeds, vague symptoms) -> dropped
  
  dis_pat <- c(
    "Pseudocercospora griseola"           = "angular ?leaf ?(spot|blight)|\\bals\\b",
    "Xanthomonas axonopodis pv. phaseoli" = "common (bacterial |bean )?blight|\\bcbb\\b|ubacterial blight|bean (bacterial )?blight",
    "halo blight"                         = "halo blight",
    "Macrophomina phaseolina"             = "ashy stem blight|dry root rot",
    "Rhizoctonia solani"                  = "web blight|\\bwebs\\b",
    "potato late blight"                  = "late blight",
    "Alternaria solani"                   = "early blight",
    "bean common mosaic virus"            = "(bean )?common (bean )?mosaic( virus)?|bean mosaic",
    "bean yellow mosaic virus"            = "(bean )?yellow mosaic( virus)?",
    "Pseudomonas syringae pv. syringae"   = "bacterial brown spot",
    "Alternaria spp."                     = "alternaria leaf spot",
    "Mycovellosiella phaseoli"            = "floury leaf spot",
    "Olpidium viciae"                     = "faba ?bean gall",
    "Botrytis fabae"                      = "chocolate ?spot",
    "Colletotrichum spp."                 = "\\b(an|a|n)[ty]?h?r[ao]c[ao]?n[ao]?s?e?\\b",
    "Ascochyta spp."                      = "\\ban?sc[a-z]*ta( blight| leaf and pod spot)?\\b|anscocaitus",
    "Meloidogyne spp."                    = "root knot nematodes?",
    "bacterial wilt"                      = "(common )?bacterial wilt",
    "bacterial blight"                    = "bacterial blight",
    "groundnut rosette virus"             = "(groundnut )?rosette",
    "maize streak virus"                  = "maize streak",
    "sorghum streak"                      = "sorghum streak",
    "Sporisorium sorghi"                  = "sorghum sm[au]r?t",
    "cercospora leaf spot"                = "cercospora leaf (smut|spot)",
    "downy mildew"                        = "downy mildew",
    "powdery mildew"                      = "powdery mildew",
    "Uromyces appendiculatus"             = "bean rust",
    "Puccinia sorghi"                     = "common rust",
    "leaf rust"                           = "leaf rust",
    "rust"                                = "\\brusty?\\b",
    "stem rot"                            = "stem rot",
    "pod rot"                             = "pod rot",
    "damping off"                         = "damping off",
    "root rot"                            = "root rots?",
    "brush broom"                         = "bruch broom",
    "leaf spot"                           = "leaf ?spots?",
    "blight"                              = "\\bblights?\\b",
    "wilt"                                = "\\bwilt(ing|ed)?\\b",
    "fungal disease"                      = "\\bfungal( des?eases?| diseases?)?\\b",
    "viral disease"                       = "\\bvirus\\b"
  )
 
  fix_dis <- function(x, leftover = FALSE) {
    s <- clean_dis(x)
    res <- lapply(s, function(v) {
      if (is.na(v)) return(list(NA_character_, NA_character_))
      out <- character(0)
      for (i in seq_along(dis_pat)) {
        if (grepl(dis_pat[i], v, perl = TRUE)) {
          out <- c(out, names(dis_pat)[i])
          v <- gsub(dis_pat[i], " ", v, perl = TRUE)
        }
      }
      out <- unique(out)
      list(if (length(out) == 0) NA_character_ else paste(out, collapse = "; "),
           trimws(gsub("[[:punct:][:space:]]+", " ", v)))
    })
    if (leftover) sapply(res, `[[`, 2) else sapply(res, `[[`, 1)
  }
  
  ## option to see what gets dropped before overwriting
  # chk <- unique(data.frame(raw = d$disease, new = fix_dis(d$disease), dropped = fix_dis(d$disease, TRUE)))
  # chk[!is.na(chk$dropped) & chk$dropped != "", ]
  
  d$disease <- fix_dis(d$disease)
  
  #out of bounce values
  d$row_spacing[d$row_spacing < 1 | d$row_spacing > 180] <- NA
  d$plant_spacing[d$plant_spacing < 5 | d$plant_spacing > 100] <- NA
  
  d <- unique(d)
  
	carobiner::write_files(path, meta, d)
}


