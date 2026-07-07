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

	loc <- data.frame(
	  adm3 = c("Guidan Roumdji", "Guidan Roumdji", "Guidan Sori", "Guidan Sori", "Guidan Sori", "Gabi", "Gabi", "Gabi", "Madarounfa", "Safo", "Sarkin Yamma", "Sarkin Yamma", "Sarkin Yamma", "Tagazar", "Tagazar", "Torodi", "Torodi", "Torodi", "Torodi", "Torodi", "Gouna"),
	  adm4 = c("Karazomé", "Karo Sofoua", "Guidan Sori", "Koki", "Noualla", "Gabi", "Samiya Goma", "Sarkin Bindigua", "Bargaja", "Safo", "Guarin Guizo", "Guarin Makoyo", "Guidan Bawa", "Tabala", "Tabala Tondi T", "Djioga", "Koubo", "seno Tiko", "Tchampanga", "Tiko", "Kassari H"),
	  longitude = c(6.5357, 6.5357, 6.7142, 6.7142, 6.7142, 7.0294, 7.0294, 7.0294, 7.3453, 6.943, 6.9758, 6.9758, 6.9758, 2.8641, 2.8641, 1.4951, 1.4951, 1.4951, 1.4951, 1.4951, 9.1986),
	  latitude = c(13.7266, 13.7266, 13.6021, 13.6021, 13.6021, 13.126, 13.126, 13.126, 13.2438, 13.2962, 13.4391, 13.4391, 13.4391, 13.7382, 13.7382, 13.1219, 13.1219, 13.1219, 13.1219, 13.1219, 13.4889),
	  geo_uncertainty = c(49890, 49890, 31921, 31921, 31921, 21031, 21031, 21031, 30085, 29379, 15836, 15836, 15836, 38040, 38040, 62102, 62102, 62102, 62102, 62102, 23555),
	  geo_source = c("GADM 4.1, adm3", "GADM 4.1, adm3", "GADM 4.1, adm3", "GADM 4.1, adm3", "GADM 4.1, adm3", "GADM 4.1, adm3", "GADM 4.1, adm3", "GADM 4.1, adm3", "GADM 4.1, adm3", "GADM 4.1, adm3", "GADM 4.1, adm3", "GADM 4.1, adm3", "GADM 4.1, adm3", "GADM 4.1, adm3", "GADM 4.1, adm3", "GADM 4.1, adm3", "GADM 4.1, adm3", "GADM 4.1, adm3", "GADM 4.1, adm3", "GADM 4.1, adm3", "GADM 4.1, adm3")
	)	
  d <- merge(d, loc, by=c("adm3","adm4"), all.x=TRUE)
 


	d$geo_from_source <- FALSE

	d$P_fertilizer <- d$K_fertilizer <-d$N_fertilizer <- as.numeric(NA)
    d$fertilizer_type[d$fertilizer_used] <- "NPK"

	d$yield_part <- "grain"
	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- TRUE

	carobiner::write_files(path, meta, d)
}


