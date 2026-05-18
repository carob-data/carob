# R script for "carob"
# license: GPL (>=3)

## ISSUES



carob_script <- function(path) {

"
Rice yield gaps and nitrogen-use efficiency in the Northwestern Indo-Gangetic Plains of India: Evidence based insights from heterogeneous farmers’ practices

A large database of individual farmer field data (n = 4,107) for rice production in the Northwestern Indo- Gangetic Plains of India was used to decompose rice yield gaps and to investigate the scope to reduce nitrogen (N) inputs without compromising yields. Stochastic frontier analysis was used to disentangle efficiency and resource yield gaps, whereas data on rice yield potential in the region were retrieved from the Global Yield Gap Atlas to estimate the technology yield gap. Rice yield gaps were small (ca. 2.7 t ha􀀀 1, or 20% of potential yield, Yp) and mostly attributed to the technology yield gap (ca. 1.8 t ha􀀀 1, or ca. 15% of Yp). Efficiency and resource yield gaps were negligible (less than 5% of Yp in most districts). Small yield gaps were associated with high input use, particularly irrigation water and N, for which small yield responses were observed. N partial factor productivity (PFP-N) was 45–50 kg grain kg􀀀 1 N for fields with efficient N management and approximately 20% lower for the fields with inefficient N management. Improving PFP-N appears to be best achieved through better matching of N rates to the variety types cultivated and by adjusting the amount of urea applied in the 3rd split in correspondance with the amount of diammonium-phosphate applied earlier in the season. Future studies should assess the potential to reduce irrigation water without compromising rice yield and to broaden the assessment presented here to other indicators and at the cropping systems level.
"

	uri <- "hdl:11529/10548774"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "ICAR;IARI; CIMMYT; CSSRI;BISA",#  BISA: Borlaug Institute for South Asia, #IARI: Indian Agricultural Research Institute #  CSSRI: Central Soil Salinity Research Institute
		publication = NA,
		project = NA,
		carob_date = "2026-05-15",
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "yield", 
		carob_contributor = "Cedric Ngakou",
		completion = 100,	
		notes = NA
	)
	

	f1 <- ff[basename(ff) == "Rice yield gaps and nitrogen-use efficiency in the Northwestern.xlsx"]

	r1 <-  suppressWarnings(carobiner::read.excel(f1, sheet="kk9 - Copy", na= "NA"))
	r2 <- carobiner::read.excel(f1, sheet="Sheet1")


	d <- data.frame(
	  expected_yield = r1$GY_kgpha,
	  latitude = r1$Latitude,
	  longitude = r1$Longitude,
	  #survey_date = r1$survey_date,
	  trial_id = paste(r1$data_collector,r1$uniqueID, sep = "-"),
	  country = r1$Main_Group.consent_yes.bg1.name_coun,
	  adm1 = r1$state,
	  adm2 = ifelse(grepl("Others",r1$Main_Group.consent_yes.bg1.location_select.block), r1$Main_Group.consent_yes.bg1.location_select.block_oth, r1$Main_Group.consent_yes.bg1.location_select.block),
	  location = r1$Main_Group.consent_yes.bg1.location_select.village,
	  season =  r1$Main_Group.consent_yes.bg1.farmer_info.name_crop,
	  date = as.character(r1$Main_Group.consent_yes.bg1.farmer_info.name_year),
	  yield_moisture = as.numeric(r1$Main_Group.consent_yes.bg2.quad_1.grain_moist1),
	  yield = r1$self_kgpha,
	  farmer_age = as.numeric(r1$Main_Group.consent_yes.bg3.hh_age),
	  farmer_education= r1$Main_Group.consent_yes.bg3.hh_edu,
	  #unit = r1$Main_Group.consent_yes.land_units.unit,
	  farmland_owned = r1$Main_Group.consent_yes.land_units.land_owned,
	  #r1$Main_Group.consent_yes.land_units.land_rented,
	  cropland_total = ifelse(grepl("Acre",r1$Main_Group.consent_yes.land_units.unit), r1$Main_Group.consent_yes.land_units.total_land *0.4047, NA),
	  cropland_used = ifelse(grepl("Acre",r1$Main_Group.consent_yes.land_units.unit), r1$Main_Group.consent_yes.land_units.total_culitland*0.4047, NA),
	  plot_area =  ifelse(grepl("Acre",r1$Main_Group.consent_yes.land_units.unit), r1$Main_Group.consent_yes.land_units.total_riceArea*0.4047, NA)*10000, #m2
	  #land_type = r1$Main_Group.consent_yes.land_info_1.land_type,
	  soil_type = r1$Main_Group.consent_yes.land_info_1.soil_type,
	  previous_crop = tolower(r1$Main_Group.consent_yes.land_info_1.crop_beforeRice),
	  variety = r1$name_variety,
	  variety_type = r1$Main_Group.consent_yes.bg5.type_variety,
	  planting_method = r1$Main_Group.consent_yes.bg5.crop_estab,
	  planting_date = ifelse(nchar(r1$Main_Group.consent_yes.bg5.date_tpr)>3, as.character(as.Date(r1$Main_Group.consent_yes.bg5.date_tpr, origin = "1899-12-31")), NA),
	  seed_rate = r1$total_seed,
	  seed_source =ifelse(grepl("Others",r1$Main_Group.consent_yes.bg5.seed_source), r1$Main_Group.consent_yes.bg5.seed_source_oth, r1$Main_Group.consent_yes.bg5.seed_source) ,
	  land_prep_method = r1$Main_Group.consent_yes.bg5.tillage_type,
	  harvest_date = as.character(r1$Main_Group.consent_yes.date_harvest),
	  harvest_days= r1$Main_Group.consent_yes.harvest_days_tpr,
	  #harvest_days = r1$harvest_datee,
	  OM_used = grepl("Yes",r1$Main_Group.consent_yes.manure_group.manure),
	  OM_type = ifelse(!is.na(r1$Main_Group.consent_yes.manure_group.type_manure), "farmyard manure", "none"),
	  OM_amount = as.numeric(r1$Main_Group.consent_yes.manure_group.total_fym),
	  fertilizer_used = grepl("yes",r1$Main_Group.consent_yes.inorganic_applied),
	  fertilizer_type = r1$Main_Group.consent_yes.fert_group.inorganic_name,
	  fertilizer_price = rowSums(sapply(r1[, c("price_urea", "price_dap", "price_mop", "price_ssp", "price_zinc")], as.numeric), na.rm = TRUE),
	  N_fertilizer = r1$total_nitrogen_i+ r1$total_urea*0.46 + r1$total_dap*0.18  ,
	  K_fertilizer = r1$tot_k,
	  P_fertilizer = r1$tot_p + r1$total_dap*0.201 + r1$total_SSP*0.0874,
	  fertilizer_amount = r1$total_fertilizer_perha,
	  Zn_fertilizer = r1$total_Zinc,
	  S_fertilizer = r1$total_SSP*0.12,
	  #r1$Main_Group.consent_yes.source_infoFert,
	  irrigated = grepl("Yes", r1$Main_Group.consent_yes.bg8.irrigation),
	  irrigation_source = r1$Main_Group.consent_yes.bg8.irrgation_grp.irri_source,
	  #weed_density = r1$weed_density,
	  weeding_method = gsub("not any", "none", tolower(r1$weed_conMethod)),
	  weeding_done = TRUE,
	  insecticide_used = grepl("yes", r1$Main_Group.consent_yes.bg9.insect_used),
	  insecticide_times = as.integer(r1$insecticide_times),
	  insecticide_amount = r1$insect_amount,
	  insecticide_product = tolower(r1$insect_name),
	  insecticide_price = r1$insecticide_costperha,
	  # r1$fungi_amount,
	  fungicide_used =  grepl("yes", r1$Main_Group.consent_yes.bg9.fungi_used),
	  fungicide_times = as.integer(r1$fungicide_times),
	  fungicide_product = tolower(r1$fungi_name),
	  fungicide_amount = r1$final_fungi_perha,
	  herbicide_times = as.integer(r1$herbicide_times),
	  herbicide_amount = r1$herbi_amount,
	  herbicide_product = r1$herbicide_name,
	  herbicide_price = r1$herbicideprice_perha,
	  soil_bd = r1$bd,
	  soil_SOC = r1$soc,
	  soil_texture = tolower(r1$Texture),
	  soil_pH = r1$Ph,
	  tmin = r1$min_temp,
	  tmax = r1$max_temp,
	  temp = r1$mean_temp,
	  #GDD = r1$gdd,
	  srad = r1$srad,
	  crop = "rice",
	  on_farm = FALSE, 
	  is_survey = TRUE, 
	  yield_part = "grain", 
	  geo_from_source = TRUE
	 
	)
	
	### Fixing fertilizer type 
	
	P <- carobiner::fix_name(d$fertilizer_type)
	P <- gsub(" ", ";", P)
	P <- gsub("dap", "DAP", P)
	P <- gsub("mop", "KCl", P)
	P <- gsub("others|zinc", "unknown", P)
	P <- gsub("npk", "NPK", P)
	P <- gsub("ssp", "SSP", P)
	P <- gsub("tsp", "TSP", P)
	d$fertilizer_type <- P
	
	### Fixing land prep method 
	P <- carobiner::fix_name(d$land_prep_method)
	P <- gsub(" ", ";", P)
	P <- gsub("4_wh_tractor_rotavator", "rotovating", P)
	P <- gsub("disc_harrow", "harrowing", P)
	P <- gsub("puddler_wet_tillage", "puddled", P)
	P <- gsub("tyne_cultivator", "mechanical puddling", P)
	P <- gsub("others", "unknown", P)
	P <- gsub("dsr_machine", "mechanical puddling", P)
	P <- gsub("2_wh_tractor_rotavator", "rotovating", P)
	P <- gsub("dsr_hand_broadcasted", "manual puddling", P)
	P <- gsub("strip_tillage", "strip tillage", P)
	P <- gsub("no_tillage", "none", P)
	d$land_prep_method <- P
	
	## fixing planting method
	d$planting_method <- ifelse(grepl("transplanted|Transplanted", d$planting_method), "transplanted", d$planting_method)
	d$planting_method <- gsub("DSR ", "", d$planting_method)
	d$planting_method <- gsub("broadcasted", "broadcasting", d$planting_method)

	### Fixing crop 
	d$previous_crop <- gsub("tamato", "tomato", d$previous_crop)
	d$previous_crop <- gsub("vegitable", "vegetable", d$previous_crop)
	d$previous_crop <- gsub("bajra", "pearl millet", d$previous_crop)
	d$previous_crop <- gsub("50|green manuring", "unknown", d$previous_crop)
	
	### fixing season
	d$season <- gsub("Boro/ Rabi", "rabi", d$season)
	d$season <- gsub("Kharif/ Aman", "kharif", d$season)
	
	
	### Fixing herbicide product
	P <- carobiner::fix_name(d$herbicide_product)
	P <- gsub("AVTAAR CRYSTAL", "clodinafop", P)
	P <- gsub("BASF VALOR-32|Pendimethalin", "pendimethalin", P)
	P <- gsub("Butachlore", " butachlor", P)
	P <- gsub("Glyphosate", "glyphosate", P)
	P <- gsub("Nominee gold", "bispyribac-sodium", P)
	P <- gsub("others", "unknown", P)
	P <- gsub("Pretilachlore|Rifit plus", "pretilachlor", P)
	P <- gsub("Sathi-Pyrazosulfuron Ethyl", "pyrazosulfuron-ethyl", P)
	P <- gsub("Thiobencarb", "thiobencarb", P)
	d$herbicide_product <- trimws(P)
	
	P <- carobiner::fix_name(d$insecticide_product)
	P <- gsub("actara-thiamethoxam", "thiamethoxam", P)
	P <- gsub("admire-imidacloprid 70 wg", "imidacloprid", P)
	P <- gsub("ampligo", "chlorantraniliprole", P)
	P <- gsub("bifenthrin 10% ec", "bifenthrin", P)
	P <- gsub("buprofezin 25% sc", "buprofezin", P)
	P <- gsub("capcadis-thiamethoxam 75% sg", "thiamethoxam", P)
	P <- gsub("chess-pymetrozine", "pymetrozine", P)
	P <- gsub("chloropyriphos", "Chlorpyrifos", P)
	P <- gsub("chloropyriphos\\+cypermethrin", "chloropyriphos;cypermethrin", P)
	P <- gsub("coragen", "chlorantraniliprole", P)
	P <- gsub("daman gold", "beauveria bassiana hf 23", P)
	P <- gsub("fame-flubendiamide", "flubendiamide", P)
	P <- gsub("fertera", "chlorantraniliprole", P)
	P <- gsub("force 15-tefluthrin", "tefluthrin", P)
	P <- gsub("furadan", "carbofuran", P)
	P <- gsub("glamour-imidacloprid\\+ethiprole", "imidacloprid", P)
	P <- gsub("imidachloprid", "imidacloprid", P)
	P <- gsub("lambda cyhalothrin", "lambda-cyhalothrin", P)
	P <- gsub("mortar", "cartab", P)
	P <- gsub("others", "unknown", P)
	P <- gsub("pexaloan-triflumezopyrim", "triflumezopyrim", P)
	P <- gsub("slayer-thiamethoxam", "thiamethoxam", P)
	P <- gsub("token-dinotefuran 20% sg", "dinotefuran", P)
	P <- gsub("vibrant-thiocyclam hydrogen oxalate", "thiocyclam", P)
	P <- gsub("virtako-thiamethoxam 1% \\+ chlorantraniliprole 0.5% gr", "thiamethoxam;thiamethoxam", P)
	d$insecticide_product <- P
	
	P <- carobiner::fix_name(d$fungicide_product)
	P <- gsub("amistar top-azoxystrobin+difenoconazole", "azoxystrobin", P)
	P <- gsub("bavistin", "carbendazim", P)
	P <- gsub("bhim-tricyclazole 75% w/w", "tricyclazole", P)
	P <- gsub("blue copper-copper oxychloride 50wp", "copper oxychloride", P)
	P <- gsub("cryzol-hexaconazole 5%sc", "hexaconazole", P)
	P <- gsub("custodia-azoxystrobin 11% \\+ tebuconazole 18.3%", "azoxystrobin", P)
	P <- gsub("folicur-tebuconazole", "tebuconazole", P)
	P <- gsub("galileo-picoxystrobin 7% \\+ propiconazole 12% sc", "propiconazole", P)
	P <- gsub("nativo-tebuconazole\\+trifloxystrobin", "tebuconazole", P)
	P <- gsub("others", "unknwon", P)
	P <- gsub("pramiconazole", "pramiconazole", P)
	P <- gsub("pulsor-thiflizamide-24%sc", "Thifluzamide", P)
	P <- gsub("saaf-carbendazim12%\\+mancozeb 63%wp", "mancozeb", P)
	P <- gsub("sheathmar-validamycin", "validamycin", P)
	P <- gsub("ultra", "mancozeb", P)
	P <- gsub("amistar top-azoxystrobin\\+difenoconazole", "azoxystrobin", P)
	d$fungicide_product <- P
	### soil texture
	d$soil_texture <- gsub("fine\\(moderate\\)", "fine", d$soil_texture)
	
	### keep only rows where the plot_area > 0
	
	d <- d[which(d$plot_area>0), ]
	
	carobiner::write_files(path, meta, d)
}

