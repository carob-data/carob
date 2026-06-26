# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Missing values in the raw dataset for yield
# Some places i did not find exact coordinates for adm4 then had to use for adm1

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
		treatment_vars = "OM_used;fertilizer_used",
		response_vars = "yield", 
		notes = NA,
		carob_contributor = "Illiana kwenda",
		carob_date = "2026-06-25",
		carob_completion = 0,	
		carob_effort = 5
	)
	

	f1 <- ff[basename(ff) == "Data on Integrated Production System 2019 & 2020-LSIL & Collaborating projects_ Niger.xlsx"]
	f2 <- ff[basename(ff) == "Data on Pearl millet production _2019&2020_LSIL zones_Niger.xls"]

	r1a <- carobiner::read.excel(f1, sheet="Integrated Pro. System 2019")
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
	
d2<-data.frame(
  country = "Niger",
  adm1 = r2$Region,
  adm2 = NA,
  adm3 = NA,
  adm4 = r2$Village,
  field_size = NA,
  treatment = r2$`Treatment Groups/ Production System`,
  dmy_residue = r2$`Pearl millet stover yield/ha`,
  yield = r2$`Pearl millet grain yield/ha`,
  crop = "pearl millet"
  )	

d <-rbind(d1,d2)
d <- unique(d)

d$planting_date <-"2019"
d$harvest_date <- "2020"
d$trial_id <- paste(d$adm4,d$planting_date,sep = "_")
d$on_farm <- TRUE 
d$is_survey <- FALSE 
d$irrigated <- FALSE

d$adm1 <-	gsub("Tillabéry","Tillabéri",d$adm1)

d$fertilizer_used <- NA# farmer's was not clear if they had  fertilizer nor manure
d$fertilizer_used[d$treatment %in% c("Use of manure","Manure")] <- FALSE
d$fertilizer_used[d$treatment %in% c("Use of manure+Fertilizer (NPK)","Manure +Fertilizer (NPK)")] <- TRUE

d$OM_used[d$treatment %in% c("Farmers practice","Farmers'practices")] <- NA # not sure about farmers practices hence we cant put false
d$OM_used[d$treatment %in% c("Use of manure+Fertilizer (NPK)","Manure +Fertilizer (NPK)","use of manure")] <- TRUE

location <- data.frame(
  adm1 = c("Maradi", 
           "Maradi", "Maradi", "Maradi", "Maradi", "Maradi", "Maradi", "Maradi", 
           "Maradi", "Maradi", "Maradi", "Maradi", "Maradi", "Maradi", "Tillabéri", 
           "Maradi", "Maradi", "Maradi", "Tillabéri", "Tillabéri", "Maradi", 
           "Maradi", "Tillabéri", "Tillabéri", "Tillabéri", "Tillabéri", 
           "Zinder", "Zinder", "Maradi", "Maradi", "Tillabéri", "Tillabéri", 
           "Tillabéri", "Tillabéri"),
  adm4 = c("Gabi", "Akora-Idi", "Guidan Tawayé Saboua", 
           "Karazomé", "Koki", "Guarin Makoyo", "Guidan Bawa", "Safo", 
           "Bargaja", "Sarki Haoussa", "Baban Kori", "Guarin Guizo", "Samiya Goma", 
           "Noualla", "Tabala Tondi T", "Azzazala", "Guidan Sori", "Karo Sofoua", 
           "seno Tiko", "kampa Tegui", "Sarkin Bindigua", "Grpment Peulh Hardo Harouna", 
           "Koubo", "Tchioubi", "Tabala", "Tchampanga", "Gochalo", "Kassari H", 
           "Baban Anné", "Kodaou", "Tiko", "Djioga", "Sirimbana", "Patty"),
  latitude = c(13.235, 14.348, 13.81, 13.501, 14.35, 13.501, 
               13.871, 13.409, 14, 13.844, 13.933, 13.501, 13.267, 13.501, 14.212, 
               13.817, 13.501, 13.501, 14.212, 13.687, 13.501, 13.501, 13.1, 
               14.649, 14.212, 14.212, 13.278, 13.807, 13.501, 13.515, 17.611, 
               14.212, 13.098, 17.611),
  longitude = c(7.068, 6.656, 7.494, 7.104, 
                7.533, 7.104, 7.285, 7.119, 7.85, 7.589, 6.967, 7.104, 7.033, 
                7.104, 1.453, 7.617, 7.104, 7.104, 1.453, 2.308, 7.104, 7.104, 
                1.817, 2.145, 1.453, 1.453, 8.771, 8.988, 7.104, 7.916, 8.081, 
                1.453, 1.797, 8.081)
)
d <- merge(d,location,by = c("adm1","adm4"), all.x = TRUE)

d$geo_from_source <- FALSE

d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- as.numeric(NA)
d$fertilizer_type[d$treatment %in% c("Use of manure+Fertilizer (NPK)","Manure +Fertilizer (NPK)")] <- "NPK" 

d$yield_part <- "grain"
d$yield_moisture <- as.numeric(NA)
d$yield_isfresh <- TRUE

	carobiner::write_files(path, meta, d)
}



