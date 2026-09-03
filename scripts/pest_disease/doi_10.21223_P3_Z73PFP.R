# R script for "carob"
# license: GPL (>=3)

## ISSUES
##Yield was noit specified, it was divided into weight of small, medium and large tubers.
#Plant height for potatoes were out of bound with maximum value 75.5

carob_script <- function(path) {

"
Dataset for: Multi-location confined field trials (ML-CFTs) for dossier development of biotech late blight resistant potato in Uganda

Transgenic potato Vic.1 carries three resistance (R) genes from wild potato relatives that were introduced to confer resistance against potato late blight (LB) disease caused by Phytophthora infestans. Preliminary field trials show that the introduction of these R genes indeed confers extreme resistance to P. infestans. In this regulatory studies, we aim to confirm the effectiveness of the introduction of the R genes in Vic.1 by conducting confined field trials during two seasons at three locations.
"

	uri <- "doi:10.21223/P3/Z73PFP"
	group <- "pest_disease"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
	                                data_organization = "CIP; NARO",
	                                publication = NA,
	                                project = "3R potato ML-CFT",
	                                design = "RCBD",
	                                data_type = NA,
	                                treatment_vars = "variety",
	                                response_vars = "disease", 
	                                carob_contributor = "Premrose Masunungure",
	                                carob_date = "2026-08-26",
	                                carob_completion = 70,	
	                                carob_effort = 4
	)
	
	
	f1 <- ff[basename(ff) == "Data Dictionary.xlsx"]
	f2 <- ff[basename(ff) == "Data.xls"]
	
	r1 <- carobiner::read.excel(f1)
	r2a <- carobiner::read.excel(f2, sheet="CFT Data")
	r2b <- carobiner::read.excel(f2, sheet="RS 8 9 AGRO ERA-Plant Dev")
	r2c <- carobiner::read.excel(f2, sheet="RS9-ERA-NTO")
	r2d <- carobiner::read.excel(f2, sheet="RS4-LB data")    # no data
	r2e <- carobiner::read.excel(f2, sheet="Harvest data")
	r2f <- carobiner::read.excel(f2, sheet="Leaf samples")
	r2g <- carobiner::read.excel(f2, sheet="Tuber samples")
	
	
	d1 <- data.frame(
	  country = "Uganda",
	  plot_id = as.character(r2b$Plot),
	  variety = r2b$Genotype,
	  disease = "potato late blight",
	  disease_incidence = as.character(ifelse(r2b$`Incidence of LB (%)` == "No data", NA, r2b$`Incidence of LB (%)`)),
	  plant_height = as.numeric(ifelse(r2b$`Estimated average plant height (cm)` == "No data", NA, r2b$`Estimated average plant height (cm)`)),stringsAsFactors = FALSE)
	
	
	obs_lookup <- c(
	  "Incidence of early blight (%)"      = "early blight",
	  "Incidence of Rhizoctonia (%)"       = "Rhizoctonia",
	  "Incidence of bacterial wilt (%)"    = "bacterial wilt",
	  "Incidence of black leg (%)"         = "black leg",
	  "Incidence of virus (%)"             = "virus",
	  "Incidence of aphids (%)"            = "aphids",
	  "Incidence of white flies (%)"       = "white flies",
	  "Incidence of leaf miner flies (%)"  = "leaf miner flies",
	  "Incidence of moth (%)"              = "moth"
	)
	
	d2 <- do.call(rbind, lapply(names(obs_lookup), function(cn) {
	  data.frame(
	    plot_id = as.character(r2c$Plot),
	    variety = r2c$Genotype,
	    pest_species = obs_lookup[[cn]],
	    pest_incidence = as.integer(r2c[[cn]]),
	    stringsAsFactors = FALSE)}))
	
	
	d3 <- data.frame(
	  plot_id = as.character(r2e$Plot),
	  variety = r2e$Genotype,
	  flesh_color = r2e$`Flesh colour`)
	
	da <- merge(d1, d2, by = c("plot_id", "variety"), all = TRUE)
	
	d <- merge(da, d3, by = c("plot_id", "variety"), all = TRUE)
	
	
	d$trial_id <- as.character(as.integer(as.factor(1)))

	d$on_farm <- NA
	d$is_survey <- FALSE
	d$irrigated <- NA
	
	
	d$longitude <- 29.942
	d$latitude <- -1.254
	d$geo_from_source <- FALSE
	
	d$planting_date <-as.character(as.Date("27-11-2017", format = "%d-%m-%Y"))
	
	d$harvest_date <- as.character(as.Date("20-03-2018", format = "%d-%m-%Y"))
	
	
	d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- NA
	d$fertilizer_type <- NA
	d$yield <- NA
	d$yield_part <- "tubers"
	d$yield_moisture <- NA
	d$crop <- "potato"
	d$yield_isfresh <- TRUE
	
	d <-unique(d)
	
	carobiner::write_files(path, meta, d)
	
}

