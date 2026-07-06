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


	#u <- unique(d[, c("adm1", "adm2", "adm3", "adm4")])
	#u = u[!is.na(u$adm3), ]
	#u= u[order(u$adm1, u$adm2, u$adm3, u$adm4), ]
	#carobiner::dfput(u)

## adm4 is unique, keeping other levels for checking. adm3 can be found with adm_pointRadius
	geo <- data.frame(
		adm1 = c("Maradi", "Maradi", "Maradi", "Maradi", "Maradi", "Maradi", "Maradi", "Maradi", "Maradi", "Maradi", "Maradi", "Maradi", "Maradi", "Maradi", "Maradi", "Maradi", "Maradi", "Maradi", "Maradi", "Maradi", "Maradi", "Maradi", "Tillabéri", "Tillabéri", "Tillabéri", "Tillabéri", "Tillabéri", "Tillabéri", "Tillabéri", "Tillabéri", "Tillabéri", "Zinder", "Zinder"),
		adm2 = c("Aguié", "Aguié", "Aguié", "Dakoro", "Dakoro", "Guidan Roumdji", "Guidan Roumdji", "Guidan Roumdji", "Guidan Roumdji", "Guidan Roumdji", "Madarounfa", "Madarounfa", "Madarounfa", "Madarounfa", "Madarounfa", "Madarounfa", "Madarounfa", "Madarounfa", "Mahahi", "Mahahi", "Mahahi", "Mayahi", "Balleyara", "Balleyara", "kollo", "kollo", "Torodi", "Torodi", "Torodi", "Torodi", "Torodi", "Magaria", "Mirriah"),
		adm3 = c("Aguié", "Aguié", "Aguié", "Adjékoria", "Sabon Machi", "Guidan Roumdji", "Guidan Roumdji", "Guidan Sori", "Guidan Sori", "Guidan Sori", "Gabi", "Gabi", "Gabi", "Madarounfa", "Safo", "Sarkin Yamma", "Sarkin Yamma", "Sarkin Yamma", "Sarki Haoussa", "Sarki Haoussa", "Sarki Haoussa", "Sarki Haoussa", "Tagazar", "Tagazar", "Dantchiandou", "Kouré", "Torodi", "Torodi", "Torodi", "Torodi", "Torodi", "Bandé", "Gouna"),
		adm4 = c("Baban Anné", "Grpment Peulh Hardo Harouna", "Kodaou", "Akora-Idi", "Baban Kori", "Karazomé", "Karo Sofoua", "Guidan Sori", "Koki", "Noualla", "Gabi", "Samiya Goma", "Sarkin Bindigua", "Bargaja", "Safo", "Guarin Guizo", "Guarin Makoyo", "Guidan Bawa", "Azzazala", "Guidan Tawayé Saboua", "Sarki Haoussa", "Azzazala", "Tabala", "Tabala Tondi T", "kampa Tegui", "Tchioubi", "Djioga", "Koubo", "seno Tiko", "Tchampanga", "Tiko", "Gochalo", "Kassari H")
	)

## merge with adm4 should be sufficient
#    d <- merge(d, geo, by="adm4", all.x=TRUE)
 

#  location <- data.frame(
#    adm1 = c("Maradi", "Maradi", "Maradi", "Maradi", "Maradi", "Maradi", "Maradi", "Maradi", "Maradi", "Maradi", "Maradi", "Maradi",
#		"Maradi", "Maradi", "Tillabéri", "Maradi", "Maradi", "Maradi", "Tillabéri", "Tillabéri", "Maradi", "Maradi", "Tillabéri", "Tillabéri", "Tillabéri", "Tillabéri", "Zinder", "Zinder", "Maradi", "Maradi", "Tillabéri", "Tillabéri", "Tillabéri", "Tillabéri",
#	adm4 = c("Gabi", "Akora-Idi", "Guidan Tawayé Saboua", "Karazomé", "Koki", "Guarin Makoyo", "Guidan Bawa", "Safo", 
#         "Bargaja", "Sarki Haoussa", "Baban Kori", "Guarin Guizo", "Samiya Goma", "Noualla", "Tabala Tondi T", "Azzazala", "Guidan Sori", "Karo Sofoua", "seno Tiko", "kampa Tegui", "Sarkin Bindigua", "Grpment Peulh Hardo Harouna", "Koubo", "Tchioubi", "Tabala", "Tchampanga", "Gochalo", "Kassari H", "Baban Anné", "Kodaou", "Tiko", "Djioga", "Sirimbana", "Patty"),
#	latitude = c(13.235, 14.348, 13.81, 13.653, 13.573, 13.45, 13.87, 13.415, 14, 13.841, 13.933, 13.421, 13.266, 14.436, 12.73, 
#             13.819, 13.529, 13.63, 14.215, 13.443, 13.245, 13.491, 13.1,  13.3, 13.756, 14.212, 13.275, 13.801, 13.51, 13.465, 13.118, 
#             13.116, 13.098, 14.212),
#	longitude = c(7.068, 6.656, 7.494, 6.868, 6.788, 6.967, 7.285, 7.113, 7.85, 7.592, 6.966, 7.024, 7.033, 
#              6.92, 2.012, 7.617, 6.898, 6.619, 1.455, 2.649, 7.027, 7.096,    1.816, 2.628, 3.022, 1.455, 8.771, 8.985, 7.95, 7.768, 1.799, 1.75, 1.796, 1.453)
#	)
  
#    d <- merge(d,location,by=c("adm1", "adm4"),all.x=TRUE)
  
	d$geo_from_source <- FALSE

	d$P_fertilizer <- d$K_fertilizer <-d$N_fertilizer <- as.numeric(NA)
    d$fertilizer_type[d$fertilizer_used] <- "NPK"

	d$yield_part <- "grain"
	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- TRUE

	carobiner::write_files(path, meta, d)
}


