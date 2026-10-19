# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Enhancing farmer’s access to technology  for increased Sorghum productivity in the selected staple crop processing zones

The Agricultural Transformation Agenda Support Program Phase 1 (AT ASP-1) of the Federal Government of Nigeria was launched in 2015 as a follow up to the previous. Agricultural Transformation Agenda (ATA). It is expected to create in 120.000 jobs along the value chain of priority commodities and add additional 20 million metric tons of food. Project activities included thematic training, on-farm technology demonstrations, community seed production and formation of Innovation Platforms for market linkages. The project has made remarkable progress in enhancing access to quality seeds and other inputs to over 34.300 farmers while expanding knowledge of best-bet productron technologies in over 100 communities across three staple crop processing zones (SCPZ). During the 2016 cropping season, farmers produced over 70,268 Mt of grains valued at £i9.135billion (US$29M). The use of improved varieties increased yields by 32%, 42% and 64% in Bida-Badeggi. Kano-Jigawa and Sokoto-Kebbi SCPZ, respectively. Seed dressing increased yields by 38%. 27%. and 30% m the three SCPZs respectively, while tillage practices increased yields by 20% and 55% m Kano - Jigawa and Sokoto - Kebbi SCPZs. Through Innovation Platforms set up with other stakeholders and market linkages to large scale processors, 109.76 tons of seeds were procured and planted. Average yield obtained on the improved technologies was 1.5 tha compared to 11 t/ha by other farmers giving a 40% increase. A total of 1,093 women farmers comprising of about 34.2% of the total number of participating farmers benefited directly from the project. Seed fairs, rural radios and audio-visual broadcasts on improved sorghum production technologies were used to reach non-participating farmers within the zones.        Experiment location on Google Map-Department of Meteorology and Climate Science    

Experiment location on Google Maps-Federal University of Technology Akure(FUTA)
"


	uri <- "doi:10.21421/D2/ZORT8F"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "ICRISAT",
		publication = NA,
		project = "AT ASP-1",
		design = NA,
		data_type = NA,
		treatment_vars = "seed_treatment;variety",
		response_vars = "yield", 
		notes = NA,
		carob_contributor = "Cedric Ngakou",
		carob_date = "2026-09-30",
		carob_completion = 100,	
		carob_effort = 3
	)
	

	f1 <- ff[basename(ff) == "Data file of Icrisat on farm seed dressing demonstration.xlsx"]

	r1 <- carobiner::read.excel(f1)


#####
	d1 <- data.frame(
		adm1 = carobiner::fix_name(r1$State, "title"),
		adm2 = carobiner::fix_name(r1$LGA, "title"), 
		location = carobiner::fix_name(r1$Community, "title"),
		variety = r1$Variety,
		treatment = r1$Treatment,
		seed_treatment = tolower(r1$Treatment),
		yield = r1$GHvYld_C_kgha,
		trial_id = r1$SCPZ,
		country = "Nigeria",
		crop = "sorghum",
		planting_date = "2016"
	)
	
	
	### Adding geo_coordinate 
	
	geo <- data.frame(
	  location = c("Auyo", "Bebeji", "Bunkure", "Dawakin Kudu", "Ngaski", "Kware", "Rurum T/gari", "Rurum S/gari", "Saji", "Yalwa", "Zurgu", "Gafan", "Dalili", "Dan Hassan", "Gwarmai", "Unguwar Duniya", "Fagam", "Farin Dutse", "Dunari", "M/madori", "Tonikutara", "Gamsarka", "Ayama", "Yankoli", "Rumfa", "Gagulmari", "Kyangakwai", "Gorun Yamma", "Kamba", "Geza", "Libata", "Kambuwa", "Sawashi", "Kwakware", "Durbawa", "Shabalegbo", "G/mallam", "Kofa", "Wak", "Damau", "Atafi 1", "Buya", "Fana", "Gbangba", "Giron Masa", "Kila", "Kwandage", "Lafiyagi", "Lanle", "Magaji", "Makaddari", "Makusidi", "Nankokan", "Ruba", "Sara", "Sarawa", "Shayya", "Takalafiya", "Tamburawa Tambari", "Utono", "Zandam"),
	  longitude = c(9.9969, 8.2841, 8.5680, 8.6459, 4.7743, 5.3120, 8.4509, 8.4699, 8.5636, 8.5413, 8.5285, 8.4584, 8.3954, 8.5245, 8.2573, 8.6161, 9.9792, 8.9925, 9.8225, 9.8902, 9.8808, 9.8591, 9.8389, 10.0663, 10.0449, 10.0418, 3.7479, 3.7021, 3.6550, 8.5163, 4.5932, 4.9696, 4.6948, 4.2848, 5.3221, 6.0667, 8.3710, 8.2644, 8.3618, 8.4339, 10.0469, 8.7423, 3.934, 5.734, 4.717, 11.3077, 8.456, 5.404, 5.645, 7.439,  9.776, 6.145, 6.219, 6.3266, 9.655, 7.7111, 8.9387, 9.7581, 8.532, 4.5719, 7.2055),
	  latitude = c(12.3155, 11.4917, 11.6632, 11.7965, 10.5439, 13.1497, 11.4510, 11.5159, 11.4621, 11.4018, 11.5477, 11.6698, 11.7680, 11.7853, 11.5299, 11.8783, 11.0485, 11.3549, 12.4961, 12.5986, 12.5647, 12.3128, 12.3155, 12.4487, 12.4504, 12.4557, 11.9692, 11.9507, 11.8544, 12.1812, 10.1495, 10.9133, 11.0938, 12.9268, 13.0639, 9.4333, 11.6818, 11.5541, 11.5928, 10.9006, 12.4478, 10.8369, 11.7119, 9.1208, 11.096, 7.446, 12.830, 8.852, 9.228, 9.123, 12.474, 9.575, 9.3199, 8.90014, 11.3477, 8.5475, 12.5738, 12.154, 11.8709, 10.545, 13.0482),
	  geo_source = c(rep("GADM 4.1, adm2", 6), rep("Google Maps", 55)),
	  geo_uncertainty = c(32584, 24763, 18688, 17536, 62519, 27349, rep(NA, 55))
	  
	)
	
	d <- merge(d1, geo, by = "location", all.x = TRUE)
	
	### Use adm2 to fill lon and lat where the location is unknown 
	geo1 = data.frame(
    adm2 = c("Auyo", "Kafin Hausa", "Dawakin Kudu", "Bagudo", "Ngaski", "Suru", "Agaie", "Katcha", "Lavun", "Mokwa", "M/madori"),
	  lon = c( 9.9969, 10.0102, 8.6459, 3.9641, 4.7743, 4.1363, 6.4082, 6.2422,5.6833, 5.1464, 9.9821),
	  lat = c(12.3155, 12.1324, 11.7965, 11.3169, 10.5439, 11.7707, 8.9306, 9.1167, 9.2735 ,9.2437, 12.5263),
    geo_s = "GADM 4.1, adm2",
    geo_un = c(32584, 27090, 17536, 56649, 62519, 41146, 40982, 55993, 75197, 87356, 25498)
	)
	
	d <- merge(d, geo1, by = "adm2", all.x = TRUE)
	
	i <- is.na(d$longitude)|is.na(d$latitude)
	d$longitude[i] <- d$lon[i]
	d$latitude[i] <- d$lat[i]
	d$geo_uncertainty[i] <- d$geo_un[i]
	d$geo_source[i] <- d$geo_s[i]
	d$geo_s <- d$geo_un <- d$lon <- d$lat <- NULL
	
	
	d$is_survey <- FALSE
	d$on_farm <-  TRUE
	d$yield_moisture <- NA
	d$yield_part <- "grain"
	d$geo_from_source <- FALSE
	d$irrigated <- NA
	d$yield_isfresh <- NA
	d$harvest_date <- NA
	
	d$K_fertilizer <- d$N_fertilizer <- d$P_fertilizer <- as.numeric(NA) 
	
	
	carobiner::write_files(path, meta, d)
}


