# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Replication Data for: Incorporating male sterility increases hybrid maize yield in low input African farming systems

In sub-Saharan Africa, maize is a staple crop but yields remain sub-optimal. A novel hybrid seed technology offers the opportunity to reduce seed production costs and increase yields. This dataset contains data from on-farm and on-station trials collected in 2017 to 2019 in South Africa, Kenya and Zimbabwe to assess this hybrid seed production technology. The results of the analysis are presented in the accompanying article.
"


	uri <- "hdl:11529/10548515"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=0,
		data_organization = "CIMMYT;KALRO;ARC",
		publication = "doi:10.1038/s42003-022-03680-7",
		project = NA,
		carob_date = "2026-05-28",
		design = NA,
		data_type = "compilation",
		treatment_vars = "seed_treatment",
		response_vars = "yield", 
		carob_contributor = "Cedric Ngakou",
		completion = 100,	
		notes = NA
	)
	

	f1 <- ff[basename(ff) == "Table_1A_data.xlsx"]
	f2 <- ff[basename(ff) == "Table_1B_data.xlsx"]
	f3 <- ff[basename(ff) == "Table_1C_data.xlsx"]
	#f4 <- ff[basename(ff) == "Table_2_Maize_evaluation_scores_2017_and_2018.xlsx"]

	r1 <- carobiner::read.excel(f1)
	r2 <- carobiner::read.excel(f2)
	r3 <- carobiner::read.excel(f3, fix_names = TRUE)
	#r4a <- carobiner::read.excel(f4, sheet="Maize evaltn scores 2017-18")
	#r4b <- carobiner::read.excel(f4, sheet="Variables")
	#r4c <- carobiner::read.excel(f4, sheet="Labels")


	d1 <- data.frame(
		on_farm = grepl("OFT", r1$Type),
		country = gsub("RSA", "South Africa", r1$Country),
		planting_date = as.character(r1$Year),
		location = r1$`Location ID`,
		location_id = as.character(r1$Location),
		rep = as.integer(r1$Rep),
		plot_id = as.character(r1$Plot),
		seed_treatment = r1$Sterility,
		yield = r1$YLDKG,
		yield_moisture = r1$MST,
		trial_id = paste(r1$Type, r1$Key, sep = "-"), 
		is_survey = FALSE, 
		crop = "maize", 
		yield_part = "grain", 
		irrigated = NA
	)


	d2 <- data.frame(
	  location_id = as.character(r2$Location),
		plot_id = as.character(r2$PLOT),
		rep = as.integer(r2$Rep),
		seed_treatment = r2$Sterility,
		plant_height = r2$PLTHT*100,
		ear_height = r2$EARHT*100 # cm
	)
	
	### merge d1 and d2
	agg <- aggregate(. ~ location_id+ plot_id + seed_treatment +rep,d2, function(X) mean(X) )
	d <- merge(d1, agg, by = c("plot_id", "seed_treatment", "rep", "location_id"), all.x = TRUE)
	
  d3 <- data.frame(
    rep = as.integer(r3$Rep),
    location_id = r3$Location,
    plot_id = as.character(r3$PLOT),
    seed_treatment = r3$Sterility,
    seed_weight = r3$X100KWt*10
  )
  
  ### merge d and d3 
  agg1 <- aggregate(. ~ location_id+ plot_id + seed_treatment +rep,d3, function(X) mean(X) )
  d <- merge(d, agg1, by = c("plot_id", "seed_treatment", "rep", "location_id"), all.x = TRUE)
  
  ### drop rows with missing location 
  d <- d[!is.na(d$location),]
  
  d$seed_treatment <- ifelse(grepl("^PP$", d$seed_treatment), paste0(d$seed_treatment, " (" ,"pollen producing(100%)", ")"), paste0(d$seed_treatment, " (", "pollen producing(50%):non pollen producing(50%)", ")"))
  
  ### Adding long and lat coordinate
  
  geo <- data.frame(
    location = c("Gwebi", "Harare", "Chiredzi", "Makhathini E", "Vaalharts B", "Cedara J", "Karurumo", "Muranda", "Lubao", "Kabula","Ilingu", "Musoli", "Ejinja", "Ilala", "Bumula", "Mwitoti", "Malinya","Embu","Kiboko", "Kakamega", "Harare J", "Harare S", "Makhathini D", "Potch C", "Potch G_TB6", "Cedara I", "Potch F_B Blk", "Emakale", "Butere", "Khaimba", "Kapina", "Muraka", "Kitale","Harare K" ),
    longitude = c(30.632, 31.0458, 31.665, 32.084, 24.8049, 30.273, 37.655, 37.449, 34.813, 34.517, 37.516, 34.668, 34.7518, 34.757, 34.437, 34.5288, 34.729, 37.459, 37.722, 34.751, 31.0473, 31.0418, 30.918, 27.862, 27.0897, 29.582, 27.0706, 34.433, 34.492, 34.609, 34.339, 34.751, 35.002, 30.9194),
    latitude = c(-17.583, -17.827, -21.033, -27.560, -27.943, -29.533, -0.465, -0.5186, 0.334, 0.486, -2.0321, 0.203, 0.282, 0.222, 0.563, 0.326, 0.197, -0.538, -2.2102, 0.283, -17.831, -17.831, -29.959, -26.278, -26.70008, -28.939, -26.719, 0.3576, 0.219, 0.332, 0.5981, 0.242, 1.0191, -17.827 ),
    geo_from_source = FALSE
  )
  
  d <- merge(d, geo, by= "location", all.x = TRUE)
  
  d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
  
	carobiner::write_files(path, meta, d)
}

