# R script for "carob"
# license: GPL (>=3)

## ISSUES
# yield is NA for the plots in Allal Tazi and Kfarchakhna and some of the plots in Guranghe Ambra and Debrezeit (they still contain other info, so they were left in)
# i left the coordinates for Guranghe Ambra in Ethiopia as NA until further guidance. google lists the places as 2 different locations Gurage(8.3675, 38.0888) and Amhara Region(11.3494, 37.9785)

# New variable names
# - row_type: # of rows of the barley spike (6-row)

carob_script <- function(path) {

"
Barley Landraces multi-location field trial - Morocco_Tunisia_Lebanon_Ethiopia 2023-2024

Assessment of Barley Landraces: multi-location field trial conducted in Morocco, Tunisia, Lebanon and Ethiopia during 2023-2024 cropping season.
"
	uri <- "hdl:20.500.11766.1/FK2/RSCKQG"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=5, minor=0,
		data_organization = "ICARDA",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;dmy_total;heading_days;maturity_days;plant_height;spike_density", 
		notes = NA,
		carob_contributor = "Kudzaishe M. Muzata",
		carob_date = "2026-10-23",
		carob_completion = 80,	
		carob_effort = 6
	)
	

	f <- ff[basename(ff) == "Data.csv"]
	r <- read.csv(f, sep=";", na.strings="NA")

	# iso codes to country names
	iso_codes <- c(CHN = "China", MAR = "Morocco", IRN = "Iran", TUN = "Tunisia", YEM = "Yemen", SYR = "Syria", JOR = "Jordan",
						UZB = "Uzbekistan", TKM = "Turkmenistan", IRQ = "Iraq", ERI = "Eritrea", ETH = "Ethiopia", RUS = "Russia", EGY = "Egypt", LBN = "Lebanon")
	origin = ifelse(r$Origin %in% names(iso_codes), iso_codes[r$Origin], NA)
	
	# 3 grain_per_spike to mean grain_per_spike
	avg_grains_per_spike <- rowMeans(r[, c("Grain_per_Spike1", "Grain_per_Spike2", "Grain_per_Spike3")], na.rm=TRUE)
	avg_grains_per_spike[is.nan(avg_grains_per_spike)] <- NA

	# retaining check and local check 
	var_type <- tolower(r$Source23) 
	var_type[!(var_type %in% c("check", "local check"))] <- NA

	d <- data.frame(
		location = carobiner::fix_name(r$Location, "title"),
		plot_id = as.character(r$Plot_Number),
		variety_code = trimws(r$Entry24),
		variety = trimws(r$ID),
		variety_traits = r$Trait,
		variety_origin = trimws(origin),
		variety_type = trimws(var_type),
		row_type = r$Row_Type,
		plot_row = r$Row,
		plot_column = r$Column,
		heading_days = r$DTH,
		maturity_days = r$DTM,
		plant_height = r$PLH,
		spike_density = r$SN * 10000, # convert to spikes/ha from spikes/m2
		grains_per_spike = avg_grains_per_spike,
		dmy_total = r$Biomass_Yield,
		yield = r$GY,
		seed_weight = r$TKW,
		yellow_rust = r$YellowRust,
		leaf_rust = r$LeafRust
	)

	d$variety <- sub("^IG[: ]*([0-9]+)", "IG:\\1", d$variety)
	d$crop <- "barley"
	d$yield_part <- "grain"
	d$trial_id <- d$location
	
	d$on_farm <- NA
	d$is_survey <- FALSE
	d$irrigated <- FALSE

	d$location[d$location == "Sidi El Aydi"] <- "Sidi el Aïdi"
	geo <- data.frame(
		location = c("Mornag", "El Kef", "Kfarchakhna", "Tel Amara", "Melk Zhar", "Allal Tazi", "Marchouch", "Annoceur", "Sidi el Aïdi", "Guranghe Ambra", "Debrezeit"),
		country = c("Tunisia", "Tunisia", "Lebanon", "Lebanon", "Morocco", "Morocco", "Morocco", "Morocco", "Morocco", "Ethiopia", "Ethiopia"),
		longitude = c(10.2875, 8.7105, 35.8676, 35.9883, -9.5529, -6.3229, -6.7889, -6.6926, -7.6246, NA, 38.9798),
		latitude = c(36.6799, 36.1691, 34.36522, 33.8660, 30.0458, 34.5217, 33.8000, 33.5616, 33.1241, NA, 8.7530)
	)
    d <- merge(d, geo, by="location", all.x=TRUE)
	d$geo_from_source <- FALSE

	# coords sources
	# mornag - https://www.google.com/maps/place/Mornag,+Tunisia/@36.6798312,10.2809415,16z/data=!4m16!1m9!3m8!1s0x12fd47d87174c96b:0x5aed27dd68027353!2sMornag,+Tunisia!3b1!8m2!3d36.6795862!4d10.2879555!10e5!16s%2Fm%2F05b6w3c!3m5!1s0x12fd47d87174c96b:0x5aed27dd68027353!8m2!3d36.6795862!4d10.2879555!16s%2Fm%2F05b6w3c?entry=ttu&g_ep=EgoyMDI2MTAyOC4wIKXMDSoASAFQAw%3D%3D
	# el kef - https://www.google.com/maps/place/El+Kef,+Tunisia/@36.1668541,8.6611212,13z/data=!3m1!4b1!4m15!1m8!3m7!1s0x1521f7708d484f57:0x914ed926af1bf054!2sKfar+Chakhna,+Lebanon!3b1!8m2!3d34.3643742!4d35.8640401!16s%2Fg%2F1td8ngc3!3m5!1s0x12fba448bbb60817:0xd20dcc094b5debbf!8m2!3d36.1679648!4d8.7095789!16zL20vMGIxcXJm?entry=ttu&g_ep=EgoyMDI2MTAyOC4wIKXMDSoASAFQAw%3D%3D
	# kfarchakhna - https://www.google.com/maps/place/Kfar+Chakhna,+Lebanon/@34.3643737,35.8640401,15z/data=!3m1!4b1!4m6!3m5!1s0x1521f7708d484f57:0x914ed926af1bf054!8m2!3d34.3643742!4d35.8640401!16s%2Fg%2F1td8ngc3?entry=ttu&g_ep=EgoyMDI2MTAyOC4wIKXMDSoASAFQAw%3D%3D
	# tel Amara - https://www.google.com/maps/place/Tal+Al+Amara,+Lebanon/@33.8642585,35.9642448,14z/data=!3m1!4b1!4m6!3m5!1s0x1518b5ebcb1a12cd:0xc58ee82599fbc8f6!8m2!3d33.8642608!4d35.9848445!16s%2Fg%2F1tfdhlz_?entry=ttu&g_ep=EgoyMDI2MTAyOC4wIKXMDSoASAFQAw%3D%3D
	# melk Zhar - https://www.google.com/maps/place/Domaine+exp%C3%A9rimental+Melk+Zher+Belfaa+(INRA-Agadir)/@30.0455948,-9.5531694,17z/data=!3m1!4b1!4m6!3m5!1s0xdb39329b296214f:0xda67937b0e3c665b!8m2!3d30.0455948!4d-9.5531694!16s%2Fg%2F1tdvljjb?entry=ttu&g_ep=EgoyMDI2MTAyOC4wIKXMDSoASAFQAw%3D%3D
	# allal Tazi - https://www.google.com/maps/place/Allal+Tazi,+Morocco/@34.5235181,-6.3223883,15.53z/data=!4m15!1m8!3m7!1s0xd0a1b553a7d09a1:0xfa7c72949b3f7654!2sAllal+Tazi,+Morocco!3b1!8m2!3d34.5215145!4d-6.3238891!16s%2Fm%2F09rx2ds!3m5!1s0xd0a1b553a7d09a1:0xfa7c72949b3f7654!8m2!3d34.5215145!4d-6.3238891!16s%2Fm%2F09rx2ds?entry=ttu&g_ep=EgoyMDI2MTAyOC4wIKXMDSoASAFQAw%3D%3D
	# marchouch - https://www.google.com/maps/place/Merchouch,+Morocco/@33.5610113,-6.7024219,15z/data=!4m15!1m8!3m7!1s0xda6deeeefc37e23:0xc8556a56d1b33e65!2sMerchouch,+Morocco!3b1!8m2!3d33.5613189!4d-6.6918828!16s%2Fm%2F010f6hyq!3m5!1s0xda6deeeefc37e23:0xc8556a56d1b33e65!8m2!3d33.5613189!4d-6.6918828!16s%2Fm%2F010f6hyq?entry=ttu&g_ep=EgoyMDI2MTAyOC4wIKXMDSoASAFQAw%3D%3D
	# annoceur - https://www.google.com/maps/place/Annoceur,+Morocco/@33.6647046,-4.8604384,15.16z/data=!4m6!3m5!1s0xd9f77acf52a6f27:0x906fe72178a3271f!8m2!3d33.663741!4d-4.85336!16s%2Fg%2F1th7p7lh?entry=ttu&g_ep=EgoyMDI2MTAyOC4wIKXMDSoASAFQAw%3D%3D
	# sidi el aïdi - https://www.google.com/maps/place/Sidi+el+A%C3%AFdi,+Morocco/@33.1269699,-7.6286175,14.54z/data=!4m15!1m8!3m7!1s0xda619f00ccd7891:0xf9c4ff29ef933264!2sSidi+el+A%C3%AFdi,+Morocco!3b1!8m2!3d33.123732!4d-7.624978!16s%2Fm%2F0k0lg79!3m5!1s0xda619f00ccd7891:0xf9c4ff29ef933264!8m2!3d33.123732!4d-7.624978!16s%2Fm%2F0k0lg79?entry=ttu&g_ep=EgoyMDI2MTAyOC4wIKXMDSoASAFQAw%3D%3D
	# debrezeit - https://www.google.com/maps/place/Bishoftu,+Ethiopia/@8.7405559,38.9020729,12z/data=!4m15!1m8!3m7!1s0x164b72d3c27674db:0x9d2f48c1182510c6!2sBishoftu,+Ethiopia!3b1!8m2!3d8.7481497!4d38.9768471!16zL20vMDZyejBo!3m5!1s0x164b72d3c27674db:0x9d2f48c1182510c6!8m2!3d8.7481497!4d38.9768471!16zL20vMDZyejBo?entry=ttu&g_ep=EgoyMDI2MTAyOC4wIKXMDSoASAFQAw%3D%3D

	d$planting_date <- "2023"
	d$harvest_date  <- "2024"

	d$P_fertilizer <- NA
	d$K_fertilizer <- NA
	d$N_fertilizer <- NA

	d$yield_moisture <- NA
	d$yield_isfresh <- NA

	carobiner::write_files(path, meta, d)
}


