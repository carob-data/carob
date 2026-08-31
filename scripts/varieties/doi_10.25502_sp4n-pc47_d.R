# R script for "carob"
# license: GPL (>=3)

## ISSUES

carob_script <- function(path) {

"
Validation trial of improved yam varieties in Nigeria in 2017

The average yam yields of local varieties is less than 25% of the yield of improved released varieties, which range from 30 to 40 tons/ha. Absence of a formal seed system has not encouraged the promotion and adoption of released improved yam varieties. To sensitize farmers on the superiority of improved varieties, seed tubers of selected released varietiesproduced by IITA were planted in comparison with local checks in 80 on-farm validation/demonstration trials in six states of Nigeria - Enugu, Benue, Nasarawa, Federal Capital Territory (FCT), Niger and Oyo. Three improved yam varieties [two Dioscorea rotundata (TDr 89/02665 and TDr 95/19177) and one D. alata (TDa 98/01176)], and one location specific farmers’ best variety were used to quantify the superiority of the improved varieties over the locals.
"

	uri <- "doi:10.25502/sp4n-pc47/d"
	group <- "varieties"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "variety",
	  response_vars = "yield", 
		notes = NA,
		carob_contributor = "Mitchelle Njukuya",
		carob_date = "2026-08-29",
		carob_completion = 100,	
		carob_effort = 1
	)
	
	f1 <- ff[basename(ff) == "seed-company-2017-data.csv"]

	r1 <- read.csv(f1)

	d <- data.frame(
	  country = "Nigeria",
	  adm1 =  r1$State,
	  adm2 = r1$LGA,
	  location = r1$Village,
	  crop = "yam", 
	  variety = r1$Variety,
	  farmer_gender = r1$Gender,
	  plot_area = r1$VarPlotSize,
	  yield = r1$Yield_t_ha * 1000,
	  tuber_density = r1$Total_No_Tuber,
	  pest_incidence = r1$ScaleIncid,
	  pest_species = "nematode;bettle;mealybug",
	  nematode_severity = r1$NemaSev,
	  nematode_incidence = r1$NemaIncid,
	  nematode_infected_percentage = r1$PERC_Nema,
	  bettle_severity = r1$BeetleSev,
	  bettle_incidence = r1$BeetleIncid,
	  bettle_infected_percentage = r1$PERC_Beetle,
	  mealybug_severity = r1$MBSev,
	  mealybug_incident = r1$MBIncid,
	  mealybug_infected_percentage = r1$PERC_MB
	)

	fill_by_id <- function(x, id) {
	  grp <- match(id, unique(id))
	  ave(x, grp, FUN = function(v) v[!is.na(v)][1])
	}
	
	d$farmer_gender <- fill_by_id(r1$Gender, r1$ID)
	
	d$farmer_gender[d$farmer_gender==1] <- "male"
	d$farmer_gender[d$farmer_gender==2] <- "female"
	d$plant_density <- (r1$Stand / r1$VarPlotSize) * 10000
	d$trial_id <- as.character(1)
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	d$geo_from_source <- FALSE
  d$planting_date <- NA
	d$harvest_date  <- NA
  d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- d$fertilizer_type <- NA
	d$yield_part <- "tubers"
	d$yield_moisture <- NA
  d$yield_isfresh <- NA
  
  #geolocation_data - longitude and latitude data for most of the villages were not found
  #resorted to a Fallback order village ->LGA (adm2) headquarters -> state (adm1) for co-ordinates that were not found
  #data accessed from https://www.geonames.org/ ,not all coordinates were available here hence
  #they were accessed from geodatos.net (https://www.geodatos.net/en/coordinates/nigeria/<place>) and https://www.google.com/maps/place/Nassarawa+Egon most adm2 locations
  
  geo <- data.frame(
    location = c(
      "Rafin Zurfi", "Angwar Dodo", "Chuiri","Shazhi", "Yaba", "Awawa",
      "Yanbabu", "Kpaduma", "Guto", "Yaupe", "Dorowa", "Sabongida",
      "Angida Gida", "Angwaan Kadaura", "Kantasakwa","Akunza Maralaba", "Obi", "Tundun Adabu",
      "Doma", "Kadarko", "Sarkin Loma","Bakinrijiya", "Adoyi", "Nassarawa Eggon",
      "Agunji", "Kpanga", "Egbanasara","Chachafu", "Maali", "Ndaabarshi",
      "Paiko Lugodan", "Popoi", "Badna","Kurmin Sheyi", "Gabi", "Ganamadi",
      "Cheche", "Nami", "Boku","Lambata/Gaiji", "Gwaape", "Mbanaka",
      "Garagboughul", "Mbasenku", "TseAkpongu Mbaleva","The Azati Mbatishi", "Anber", "Ugba",
      "Mbatie", "Abetise", "Tse Kaase","Mke", "Adum East", "Ochodu Upka","Aidegbe Ainu", "Lagbanda", "Lube",
      "Igboho", "Yemere", "Oloje","Otiri", "Tokunbo", "Kanko","Agunrege", "Kisii", "Kotogiri",
      "Araromi", "NgeneUgbo", "Nemwe","Ugbo", "EziokaMgbowo", "Itienyi Omudo Nemwe"),
    longitude = c(
      7.075, 7.075, 7.227, 7.227, 6.943, 7.018,7.018, 7.495, 7.380, 7.380, 7.661, 8.515,
      8.406, 8.515, 8.515, 8.515, 8.767, 8.767,8.355, 8.796, 8.796, 8.515, 8.515, 8.515,
      8.51532, 8.767, 6.312, 5.6, 5.6, 6.547,6.633, 6.516, 6.72, 6.72, 6.570, 6.570,
      6.57089, 6.318, 6.318, 6.547, 6.547, 9.001,9.204, 9.204, 9.204, 9.284, 9.284, 9.348,
      9.20455, 9.001, 8.885, 9.001, 8.25, 8.419,8.419, 3.756, 3.756, 3.756, 3.393, 3.424,
      3.59626, 3.424, 3.393, 3.421, 3.851, 3.756,3.757, 7.477, 7.498, 7.477, 7.477, 7.498),
    latitude = c(
      8.939, 8.939, 8.8795, 8.8795, 8.475, 8.8835,8.883, 9.0579, 9.279, 9.279, 9.00, 8.493,
      8.9107, 8.493, 8.4939, 8.4939, 8.367, 8.367,8.393, 8.147, 8.1472, 8.493, 8.4939, 8.493,
      8.493, 8.367, 8.7607, 9.2, 9.2, 9.615,9.436, 9.653, 9.859, 9.8591, 9.044, 9.0443,
      9.0443, 9.0085, 9.00, 9.6152, 9.615, 7.3227,7.4596, 7.459, 7.4596, 7.16, 7.169, 7.507,
      7.4596, 7.322, 7.5638, 7.3227, 7.0, 6.8452,6.84526, 8.837, 8.837, 8.837, 8.667, 7.539,
      7.9702, 7.5390, 8.6676, 8.5049, 9.0829, 8.8378,7.4710, 6.0727, 6.4413, 6.0727, 6.0727, 6.441),
    elevation = c(
      186, 186, 300, 300, 152, 197,197, 475, 567, 567, 390, 179,
      454, 179, 179, 179, 185, 185,157, 129, 129, 179, 179, 179,
      179, 185, 71, 207, 207, 243,312, 283, 322, 322, 169, 169,
      169, 120, 120, 243, 243, 226,93, 93, 93, 152, 152, 189,
      93, 226, 191, 226, 67, 158,158, 405, 405, 405, 472, 186,
      313, 186, 472, 333, 371, 405,214, 133, 192, 133, 133, 192
    ))
	
  d <- merge(d, geo, by = "location", all.x = TRUE)
	
	carobiner::write_files(path, meta, d)
}


