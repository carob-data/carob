# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Enhancing farmer's access to technology  for tillage demonstration increased Sorghum productivity in the selected staple crop processing zones

The Agricultural Transformation Agenda Support Program Phase 1 (AT ASP-1) of the Federal Government of Nigeria was launched m 2015 as a follow up to the previous. Agricultural  Transformation Agenda (ATA). It is expected to create in 120.000jobs along the value chain of priority commodities and add additional 20 million metric tons of food. Project activities included thematic training, on-farm technology  demonstrations, community seed production and formation of Innovation Platforms for market linkages. The project has made remarkable progress in enhancing access to quality seeds and other inputs to over 34.300 farmers while expanding knowledge of best-bet productron technologies in over 100 communities across three staple crop processing zones (SCPZ). Duiing the 2016 cropping season, farmers produced over 70,268 Mt of grains valued at £i9.135billion (US$29M). The use of improved varieties increased yields by 32%. 42% and 64% in Bida-Badeggi. Kano-Jigawa and Sokoto-Kebbi SCPZ. respectively. Seed dressing increased yields by 38%. 27%. and 30% m the three SCPZs respectively, while tillage practices increased yields by 20% and 55% m Kano - Jigawa and Sokoto - Kebbi SCPZs. Through Innovation Platforms set up with other stakeholders and market linkages to large scale processors, 109.76 tons of seeds were procured and planted. Average yield obtained on the improved technologies was 1.5 tha compared to 1 1 t/ha by other farmers giving a 40% increase. A total of 1,093 women farmers comprising of about 34.2% of the total number of participating farmers benefited directly from the project. Seed fairs, rural radios and audio-visual broadcasts on improved sorghum production technologies were used to reach non-participating farmers within the zones.        Experiment location on Google Map-Department of Meteorology and Climate Science    

Experiment location on Google Maps-Federal University of Technology Akure(FUTA)
"

	uri <- "doi:10.21421/D2/RYVBEU"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "ICRISAT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "land_prep_method",
		response_vars = "yield", 
		notes = NA,
		carob_contributor = "Cedric Ngakou",
		carob_date = "2026-09-30",
		carob_completion = 100,	
		carob_effort = 3
	)
	

	f1 <- ff[basename(ff) == "Data file of ICRISAT on farm tillage demonstration.xlsx"]

	r1 <- carobiner::read.excel(f1)

	#### process
	
	d1 <- data.frame(
		adm1 = carobiner::fix_name(r1$State, "title"),
		adm2 = carobiner::fix_name(r1$LGA, "title"),
		location = carobiner::fix_name(r1$Community, "title"),
		variety = r1$Variety,
		treatment = r1$Treatment,
		land_prep_method = gsub("minimum","minimum tillage", tolower(r1$Treatment)),
		yield = r1$GHvYld_C_kgha,
		trial_id = r1$SCPZ,
		country = "Nigeria",
		crop = "sorghum",
		planting_date = "2016"
		
	)
	
	
	### Adding geo coordinate 
	
	geo <- data.frame(
	  location = c("Auyo", "Bebeji", "Dawakin Kudu", "Rurum T/gari", "Rurum S/gari", "Saji", "Yalwa", "Zurgu", "Gafan", "Dalili", "Dan Hassan", "Gwarmai", "Unguwar Duniya", "Fagam", "Farin Dutse", "Dunari", "M/madori", "Tonikutara", "Gamsarka", "Ayama", "Yankoli", "Rumfa", "Kyangakwai", "Gorun Yamma", "Kamba", "Geza", "Kwakware", "Durbawa", "Wak", "Damau", "Atafi 1", "Buya", "Fana", "Kila", "Kwandage", "Makaddari", "Ruba", "Sara", "Sarawa", "Shayya", "Takalafiya", "Tamburawa Tambari", "Zandam", "Fana Sabo", "Gidan Gangu","Hamma Ali", "Marbawa", "Shamakeri"),
	  longitude = c(9.9969, 8.2841, 8.6459, 8.4509, 8.4699, 8.5636, 8.5413, 8.5285, 8.4584, 8.3954, 8.5245, 8.2573, 8.6161, 9.9792, 8.9925, 9.8225, 9.8902, 9.8808, 9.8591, 9.8389, 10.0663, 10.0449, 3.7479, 3.7021, 3.6550, 8.5163, 4.2848, 5.3221, 8.3618, 8.4339, 10.0469, 8.7423, 3.9340, 11.3077, 8.4560, 9.7760, 6.3266, 9.6550, 7.7111, 8.9387, 9.7581, 8.5320, 7.2055, 3.933, 10.0117, 5.3155, 5.3067, 8.5474),
	  latitude = c(12.31550, 11.49170, 11.79650, 11.45100, 11.51590, 11.46210, 11.40180, 11.54770, 11.66980, 11.76800, 11.78530, 11.52990, 11.87830, 11.04850, 11.35490, 12.49610, 12.59860, 12.56470, 12.31280, 12.31550, 12.44870, 12.45040, 11.96920, 11.95070, 11.85440, 12.18120, 12.92680, 13.06390, 11.59280, 10.90060, 12.44780, 10.83690, 11.71190, 7.44600, 12.83000, 12.47400, 8.90014, 11.34770, 8.54750, 12.57380, 12.15400, 11.87090, 13.04820, 11.7119, 12.4274, 13.17007, 13.2166, 11.9560),
	  geo_source = c(rep("GADM 4.1, adm2", 3), rep("Google Maps", 45)),
	  geo_uncertainty = c(32584, 24763, 17536, rep(NA, 45))
	)
	
	d <- merge(d1, geo, by = "location", all.x = TRUE)
	
	### Use adm2 to fill lon and lat where the location is unknown 
	geo1 <- data.frame(
	  adm2 = c("Auyo", "Gwaram", "Hadejia", "Kafin Hausa", "Bebeji", "Bunkure", "Dawakin Kudu", "Kura", "Rano", "Bagudo", "Dandi", "Suru", "Kware", "G/mallam", "M/madori"),
	  lon = c(9.9969, 9.9918, 10.0311, 10.0102, 8.2841, 8.5680, 8.6459, 8.4700, 8.4988, 3.9641, 3.8375, 4.1363, 5.3120, 8.373, 9.9821),
	  lat = c(12.3155, 11.1783, 12.4271, 12.1324, 11.4917, 11.6632, 11.7965, 11.7708, 11.4665, 11.3169, 11.7948, 11.7707, 13.1497, 11.6469, 12.5263),
	  geo_s = "GADM 4.1, adm2",
	  geo_un = c(32584, 43960, 4913, 27090, 24763, 18688, 17536, 16798, 20758, 56649, 35224, 41146, 27349, 13222, 25498)
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


