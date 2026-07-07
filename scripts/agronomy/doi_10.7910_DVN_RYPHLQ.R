# R script for "carob"
# license: GPL (>=3)

## ISSUES

carob_script <- function(path) {

"
Production of quality feeds from dual-purpose pearl millet in Niger

Data were collected from on-farm trials, conducted with farmers, aiming at developing a productive cropping systems. The main objective was to optimize the production of biomass to generate more residues of good quality as sources of feeds in crop-livestock farming systems.
"

	uri <- "doi:10.7910/DVN/RYPHLQ"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "ICRISAT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "fertilizer_used",
		response_vars = "yield", 
		notes = NA,
		carob_contributor = "Premrose Masunungure",
		carob_date = "2026-06-25",
		carob_completion = 100,	
		carob_effort = 5
	)
	
	f1 <- ff[basename(ff) == "Data on Integrated Production System 2019 & 2020-LSIL & Collaborating projects_ Niger.xlsx"]
	f2 <- ff[basename(ff) == "Data on Pearl millet production _2019&2020_LSIL zones_Niger.xls"]

	r1 <- carobiner::read.excel(f1, sheet="Integrated Pro. System 2019")
	r2 <- carobiner::read.excel(f2)

	d1 <- data.frame(
		trial_id = paste0("1_", r1$`N of farmer`),
		country = "Niger",
		adm1 = r1$Region,
		adm2 = r1$Departement,
		adm3 = r1$District,
		adm4 = r1$Village,
		field_size = as.numeric(r1$`Field area (ha)`),
		#treatment = r1$`Production System`,
		treatment = r1$`Treatment Groups`,
		fwy_residue = r1$`Pearl millet Biomass Yield (kg/ha)`,
		yield = r1$`Pearl millet grain yield (kg/ha)`,
		crop = "pearl millet",
		tree_density = r1$`Number of trees/ha`,	
		shrub_density = r1$`Number of shrubs/ha`
	)
	
  d2 <- data.frame(
	trial_id = paste0("2_", r2$`N of farmer`),
    country = "Niger",
    adm1 = r2$Region,
    adm2 = NA,
    adm3 = NA,
    adm4 = r2$Village,
    field_size = NA,
    treatment = r2$`Treatment Groups/ Production System`,
    fwy_residue = r2$`Pearl millet stover yield/ha`,
    yield = r2$`Pearl millet grain yield/ha`,
    crop = "pearl millet"
  )	

  d <- carobiner::bindr(d1, d2)
  
	d$planting_date <- "2019"
	d$harvest_date <- "2020"
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	
	d$adm1 <- gsub("Tillabéry", "Tillabéri",d$adm1)

	npk <- grepl("NPK", d$treatment)
	man <- grepl("Manure", d$treatment)
	d$fertilizer_used <- d$OM_used <- NA  #Farmers' practices were not specified
	d$fertilizer_used[man] <- FALSE 
	d$fertilizer_used[npk] <- TRUE 
	d$OM_used[npk] <- FALSE 
	d$OM_used[man] <- TRUE 


	d$adm1[d$adm1 == "Tillabéri"] <- "Tillabéry"
	d$adm3[d$adm3 == "Sarki Haoussa"] <- "Sarkin Haoussa" 
	d$adm3[d$adm3 == "Dantchiandou"] <- "Dantiandou" 
	d$adm4 <- carobiner::fix_name(d$adm4, "title")


## adm4 is unique, keeping other levels for checking. adm3 can be found with adm_pointRadius
## these need to be georefenced with Google Maps or similar, to the extent possible
	geo4 <- data.frame(
		#adm1 = c("Maradi", "Maradi", "Maradi", "Maradi", "Maradi", "Maradi", "Maradi", "Maradi", "Maradi", "Maradi", "Maradi", "Maradi", "Maradi", "Maradi", "Maradi", "Maradi", "Maradi", "Maradi", "Maradi", "Maradi", "Maradi", "Maradi", "Tillabéry", "Tillabéry", "Tillabéry", "Tillabéry", "Tillabéry", "Tillabéry", "Tillabéry", "Tillabéry", "Tillabéry", "Tillabéry", "Tillabéry", "Zinder", "Zinder"),
		#adm2 = c("Aguié", "Aguié", "Aguié", "Dakoro", "Dakoro", "Guidan Roumdji", "Guidan Roumdji", "Guidan Roumdji", "Guidan Roumdji", "Guidan Roumdji", "Madarounfa", "Madarounfa", "Madarounfa", "Madarounfa", "Madarounfa", "Madarounfa", "Madarounfa", "Madarounfa", "Mahahi", "Mahahi", "Mahahi", "Mayahi", "Balleyara", "Balleyara", "kollo", "kollo", "Torodi", "Torodi", "Torodi", "Torodi", "Torodi", NA, NA, "Magaria", "Mirriah"),
		#adm3 = c("Aguié", "Aguié", "Aguié", "Adjékoria", "Sabon Machi", "Guidan Roumdji", "Guidan Roumdji", "Guidan Sori", "Guidan Sori", "Guidan Sori", "Gabi", "Gabi", "Gabi", "Madarounfa", "Safo", "Sarkin Yamma", "Sarkin Yamma", "Sarkin Yamma", "Sarkin Haoussa", "Sarkin Haoussa", "Sarkin Haoussa", "Sarkin Haoussa", "Tagazar", "Tagazar", "Dantiandou", "Kouré", "Torodi", "Torodi", "Torodi", "Torodi", "Torodi", NA, NA, "Bandé", "Gouna"),
		adm4 = c("Baban Anné", "Grpment Peulh Hardo Harouna", "Kodaou", "Akora-Idi", "Baban Kori", "Karazomé", "Karo Sofoua", "Guidan Sori", "Koki", "Noualla", "Gabi", "Samiya Goma", "Sarkin Bindigua", "Bargaja", "Safo", "Guarin Guizo", "Guarin Makoyo", "Guidan Bawa", "Azzazala", "Guidan Tawayé Saboua", "Sarki Haoussa", "Azzazala", "Tabala", "Tabala Tondi T", "Kampa Tegui", "Tchioubi", "Djioga", "Koubo", "Seno Tiko", "Tchampanga", "Tiko", "Sirimbana", "Patty", "Gochalo", "Kassari H"),
		longitude = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
		latitude = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
	)

	# if we cannot find "adm4" we fall back to "adm3"
	geo3 <- data.frame(
		adm3 = c("Adjékoria", "Aguié", "Bandé", "Dantiandou", "Gabi", "Gouna", "Guidan Roumdji", "Guidan Sori", "Kouré", "Madarounfa", "Sabon Machi", "Safo", "Sarkin Haoussa", "Sarkin Yamma", "Tagazar", "Torodi"),
		#adm2 = c("Dakoro", "Aguié", "Magaria", "Kollo", "Madarounfa", "Mirriah", "Guidan Roumdji", "Guidan Roumdji", "Kollo", "Madarounfa", "Dakoro", "Madarounfa", "Mayahi", "Madarounfa", "Balleyara", "Torodi"),
		#adm1 = c("Maradi", "Maradi", "Zinder", "Tillabéry", "Maradi", "Zinder", "Maradi", "Maradi", "Tillabéry", "Maradi", "Maradi", "Maradi", "Maradi", "Maradi", "Tillabéry", "Tillabéry"),
		lon3 = c(6.6548, 7.693, 8.9152, 2.7069, 7.0223, 9.1546, 6.5346, 6.7179, 2.59, 7.1725, 6.9999, 6.9419, 7.4739, 6.9731, 2.875, 1.4754),
		lat3 = c(14.3012, 13.5321, 13.1895, 13.5327, 13.1214, 13.4741, 13.7152, 13.5794, 13.3438, 13.2385, 13.8999, 13.2913, 13.8175, 13.4321, 13.7285, 13.2097),
		unc3 = c(34663, 35648, 26588, 25699, 20739, 21872, 50045, 32186, 25195, 24139, 21047, 28456, 24590, 15603, 37088, 5533),
		src3 = "GADM 5, adm3" 
	)

	# replace the missing lon/lat in geo4 with geo3
	geo <- merge(geo4, geo3, all.x=TRUE)
	i <- is.na(geo$latitude) 
	geo$latitude[i] <- geo$lat3[i]
	geo$longitude[i] <- geo$lon3[i]
	geo$geo_uncertainty[i] <- geo$unc3[i]
	geo$geo_source[i] <- geo$src3[i]
	geo$lat3 <- geo$lon3 <- geo$unc3 <- geo$src3 <- NULL

	# merge by adm4 only because adm3 is missing in about half the cases. adm4 is unique within the other adm levels so that's OK
	d <- merge(d, geo[c("adm4", "longitude", "latitude", "geo_uncertainty", "geo_source")], by="adm4", all.x=TRUE)
	d$geo_from_source <- FALSE

	d$P_fertilizer <- d$K_fertilizer <-d$N_fertilizer <- as.numeric(NA)
    d$fertilizer_type <- "none"
	d$fertilizer_type[d$fertilizer_used] <- "NPK"

	d$yield_part <- "grain"
	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- TRUE

	carobiner::write_files(path, meta, d)
}


