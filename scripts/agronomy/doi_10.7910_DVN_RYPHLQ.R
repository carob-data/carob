# R script for "carob"
# license: GPL (>=3)

## ISSUES
# NA values on yield
#Locations and the geodata used, All the areas I used adm3 except, 
#areas highlighted below where I used adm1 and adm2.
#Akora-Idi adm1, used,
#Sirimbana adm1 used,
#Patty adm1 used,
#Azzazala adm1 used,
#Sarki Haoussa adm1 used,
#Guidan Tawayé Saboua adm2 used,
#Baban Kori adm2 used,
#kampa Tegui adm2 used,
#Baban Anné adm2 used,


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
		treatment_vars = "fertilizer_used;OM_used",
		response_vars = "yield", 
		carob_contributor = "Premrose Masunungure",
		carob_date = "2026-06-25",
		carob_completion = 50,
		carob_effort = 5
	)
	

	f1 <- ff[basename(ff) == "Data on Integrated Production System 2019 & 2020-LSIL & Collaborating projects_ Niger.xlsx"]
	f2 <- ff[basename(ff) == "Data on Pearl millet production _2019&2020_LSIL zones_Niger.xls"]

	r1a <- carobiner::read.excel(f1, sheet="Integrated Pro. System 2019")
	r1b <- carobiner::read.excel(f1, sheet="Sheet1")
	r2 <- carobiner::read.excel(f2)

	d1 <- data.frame(
	country = "Niger",
	adm1 = r1a$Region,
	adm2 = r1a$Departement,
	adm3 = r1a$District,
	adm4 = r1a$Village,
	field_size = as.numeric(r1a$`Field area (ha)`),
	treatment = r1a$`Production System`,
	dmy_residue = r1a$`Pearl millet Biomass Yield (kg/ha)`,
	yield = r1a$`Pearl millet grain yield (kg/ha)`,
	crop = "pearl millet"
	)
	
  d2 <- data.frame(
    country = "Niger",
    adm1 = r2$Region,
    adm2 = NA,
    adm3 = NA,
    adm4 = r2$Village,
    field_size = NA,
    treatment = r2$`Treatment Groups/ Production System`,
    dmy_residue = r2$`Pearl millet stover yield/ha`,
    yield = r2$`Pearl millet grain yield/ha`,
    crop = "pearl millet")	

  d <- rbind(d1, d2)
  d <- unique(d)
  
  d$planting_date <- "2019"
  d$harvest_date <- "2020"
  d$trial_id <- paste(d$adm4,d$planting_date,sep = "_")
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- NA
	
	
	d$adm1 <- gsub("Tillabéri", "Tillabéry",d$adm1)
	
  g <- unique(d[,c("country", "adm1", "adm4")])

 
  location <- data.frame(
    adm1 = c("Maradi",
             "Maradi", "Maradi", "Maradi", "Maradi", "Maradi", "Maradi", "Maradi",
             "Maradi", "Maradi", "Maradi", "Maradi", "Maradi", "Maradi", "Tillabéry",
             "Maradi", "Maradi", "Maradi", "Tillabéry", "Tillabéry", "Maradi",
             "Maradi", "Tillabéry", "Tillabéry", "Tillabéry", "Tillabéry",
             "Zinder", "Zinder", "Maradi", "Maradi", "Tillabéry", "Tillabéry",
             "Tillabéry", "Tillabéry"),
    adm4 = c("Gabi", "Akora-Idi", "Guidan Tawayé Saboua",
             "Karazomé", "Koki", "Guarin Makoyo", "Guidan Bawa", "Safo",
             "Bargaja", "Sarki Haoussa", "Baban Kori", "Guarin Guizo", "Samiya Goma",
             "Noualla", "Tabala Tondi T", "Azzazala", "Guidan Sori", "Karo Sofoua",
             "seno Tiko", "kampa Tegui", "Sarkin Bindigua", "Grpment Peulh Hardo Harouna",
             "Koubo", "Tchioubi", "Tabala", "Tchampanga", "Gochalo", "Kassari H",
             "Baban Anné", "Kodaou", "Tiko", "Djioga", "Sirimbana", "Patty"),
    latitude = c(13.126, 14.116, 14.118, 13.726, 13.602, 13.439,
                 13.439, 13.296, 13.243, 14.116, 14.617, 13.439, 13.126, 13.602, 13.738,
                 14.116, 13.602, 13.727, 13.122, 13.478, 13.126, 13.511, 13.122,
                 13.330, 13.738, 13.122, 13.188, 13.489, 14.618, 13.511, 13.122,
                 13.122, 14.184, 14.184),
    longitude = c(7.029, 7.299, 7.620, 6.535,
                  6.714, 6.975, 6.975, 6.943, 7.345, 7.299, 6.985, 6.975, 7.029,
                  6.714, 2.864, 7.299, 6.714, 6.536, 1.495, 2.298, 7.029, 7.625,
                  1.495, 2.553, 2.864, 1.495, 8.897, 9.199, 6.986, 7.625, 1.495,
                  1.495, 2.188, 2.188)
  )

  
  d <- merge(d,location,by=c("adm1","adm4"),all.x=TRUE)
  
  d$geo_from_source <- FALSE
  
  d$fertilizer_used <-NA
  d$fertilizer_used[d$treatment%in% c("Farmers practice","Use of manure","Manure","Farmers'practices")] <- FALSE
  d$fertilizer_used[d$treatment%in% c("Use of manure+Fertilizer (NPK)","Manure +Fertilizer (NPK)")] <- TRUE
  
  d$OM_used <-NA
  d$OM_used[d$treatment%in% c("Use of manure", "Manure", "Manure +Fertilizer (NPK)", "Use of manure+Fertilizer (NPK)")] <- TRUE
  d$OM_used[d$treatment%in% c("Farmers practice", "Farmers'practices")] <- FALSE
   
	d$geo_from_source <- FALSE

   d$P_fertilizer <- d$K_fertilizer <-d$N_fertilizer <- NA
   
	d$yield_part <- "grain"
	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- NA

	carobiner::write_files(path, meta, d)
}


