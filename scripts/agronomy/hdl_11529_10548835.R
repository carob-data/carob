# R script for "carob"


carob_script <- function(path) {

"Climate change and soil fertility decline are major threats to smallholder farmers' food and nutrition security in southern Africa, and cropping systems that improve soil health are needed to address these challenges. Cropping systems that invest in soil organic matter, such as no-tillage (NT) with crop residue retention, have been proposed as potential solutions. However, a key challenge for assessing the sustainability of NT systems is that soil carbon (C) stocks develop over long timescales, and there is an urgent need to identify trajectory indicators of sustainability and crop productivity. Here we examined the effects of NT as compared with conventional tillage without residue retention on relationships between soil characteristics and maize (Zea mays L.) productivity in long-term on-farm and on-station trials in Zimbabwe. Our results show that relationships between soil characteristics and maize productivity, and the effects of management on these relationships, varied with soil type. Total soil nitrogen (N) and C were strong predictors of maize grain yield and above-ground biomass (i.e., stover) in the clayey soils, but not in the sandy soils, under both managements. This highlights context-specific benefits of management that fosters the accumulation of soil C and N stocks. Despite a strong effect of NT management on soil C and N in sandy soils, this accrual was not sufficient to support increased crop productivity in these soils. We suggest that sandy soils should be the priority target of NT with organic resource inputs interventions in southern Africa, as mineral fertilizer inputs alone will not halt the soil fertility decline. This will require a holistic management approach and input of C in various forms (e.g., biomass from cover crops and tree components, crop residues, in combination with mineral fertilizers). Clayey soils on the other hand have greater buffering capacity against detrimental effects of soil tillage and low C input."

	uri <- "hdl:11529/10548835"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIMMYT;JHU;UED;USH",
		publication = "doi:10.1017/S1742170521000442",
		project = NA,
		data_type = "on-farm experiment",
		response_vars = "yield",
		treatment_vars = "land_prep_method", 
		carob_contributor = "Blessing Dzuda",
		carob_date = "2024-07-02"
	)
	
	f <- ff[basename(ff) == "DATA 2006 - 2017.xlsx"]
	r <- carobiner::read.excel(f, na=c("", "*"))

	d <- data.frame(
		crop="maize", 
		country="Zimbabwe",
		location=r$`Site name`,
		planting_date="2004",
		treatment=r$Treatment,
		land_prep_method=tolower(r$Treatment),
		soil_pH=r$`pH (H2O) -repeat`,
		soil_EC=r$`Electrical conductivity (µS/cm)` / 100,
		soil_N=r$`N concentration (mg/g Soil)`,
		soil_C=r$`C concentration (mg/g Soil)`,
		soil_CEC=r$`Exchangeable cations (meq/100g)`,
		soil_ex_acidity=r$`Exchangeable acidity (meq/100g)`,
		fwy_total=r$`Mean biomass yield (kg/ha)`,
		yield_part="grain",
		yield=r$`Mean grain yield (kg/ha)`
	)

	d$land_prep_method <- gsub("direct seeding", "none", d$land_prep_method)
	d$land_prep_method <- gsub("ripper", "ripping", d$land_prep_method)
	
	
	d$trial_id <- as.character(as.integer(as.factor(d$location)))
	
	geo <- data.frame(
		location = c("Hereford", "Shamva", "Madziva"),
		longitude = c(31.4644, 31.6314, 31.6553),
		latitude = c(-17.4343, -17.1763, -17.062),
		geo_from_source = FALSE
	)  
	d <- merge(d, geo, by = "location", all.x = TRUE)
	
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <-FALSE
	
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
  
  
	carobiner::write_files(path, meta, d)
}

