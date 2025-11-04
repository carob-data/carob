# R script for "carob"

## EGB
# Related to scripts/fertilizer/doi_10.7910_DVN_1A6WMD.R

# need to add soil properties from 
# Table 1. Chemical and physical soil properties at six sites

# this is a crop rotation experiment but that is not captured.
# the paper also suggests that there are four years

carob_script <- function(path) {

"Retaining crop residues in fields is a pathway to build SOM on smallholder farmers. The quality of crop residues can be improved through integrating more legume residues. This experiment assess the effect of legume residues from a doubled up system as compared to maize residues on rotational maize and N dynamics in the short term. In the long term, this can increase both quantity and quality of SOM"


# Cropping systems are 

# groundnut-maize rotation (GN-MZ), 
# pigeonpea-maize rotation (PP-MZ), 
# groundnut/pigeonpea-maize rotation (GNPP-MZ), 
# continuous fully fertilized maize (MZ + F), 
# continuous maize/pigeonpea intercrop (MZPP), 
# continuous unfertilized maize (MZ-MZ).


	uri <- "doi:10.7910/DVN/UJIPSW"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "IITA",
		publication = "doi:10.1016/j.fcr.2021.108225",
		project = "Africa RISING",
		data_type = "experiment",
		carob_contributor= "Eduardo Garcia Bendito",
		carob_date="2024-05-07",
		treatment_vars=NA, 
		response_vars = "yield"
	)
	
	f1 <- ff[basename(ff) == "Old design_maize yields.csv"]
	r1 <- read.csv(f1)
	r1[r1 == ""] <- NA # Remove empty strings
	r1 <- r1[rowSums(is.na(r1)) != ncol(r1), ] # Remove empty rows

	f2 <- ff[basename(ff) == "Old design_gnut yields.csv"]
	r2 <- read.csv(f2)
	r2[r2 == ""] <- NA # Remove empty strings
	r2 <- r2[rowSums(is.na(r2)) != ncol(r2), ] # Remove empty rows

# "streamlite"; highcharts
## process file(s)

	d1 <- data.frame(
		crop = "maize",
		variety = "DKC 8033",
		planting_date = as.character(NA),  # 2016 or 2018?
		harvest_date = as.character(NA), # 2017 or 2019?
		trial_id = r1$Farmer.code,
		location = r1$EPA,
		tn = r1$Treatment.number,
		rep = r1$Rep,
		plot_area = r1$Net.plot.size..M2.,
		yield = r1$Grain.weight..kg.ha.,
		fwy_residue = r1$stover.dry.weight..kg.ha.,
		dmy_total = r1$Total.biomass..kg.ha.,
		row_spacing = r1$Distance.between.ridges..m.*100, # to cm
		treatment = r1$Treatment
	)
	
	d2 <- data.frame(
# data say 2016/2017 season; but paper says it is 2015/16 or 2017/18! 	
	  planting_date = "2016", 
	  harvest_date = "2017", 
	  variety = r2$Variety,
	  trial_id = r2$Farmer.code,
	  location = r2$EPA,
	  tn = r2$Treatment.number,
	  rep = r2$Rep,
	  plot_area = r2$Net.plot.size..m2.,
	  yield = r2$Grain.weight..kg.ha.,
	  fwy_residue = r2$stover.dry.weight..kg.ha.,
	  dmy_total = r2$Total.biomass..kg.ha.,
	  row_spacing = r2$Distance.between.ridges..m.*100, # to cm
	  treatment = r2$Treatment
	)
	
	# RH: 
	## we need to find out of the crop for which yield is reported is pigeon pea or groundnut 
	## the below assumption may be wrong. The previous assignment of "groundnut" may also be true
	## this could possibly be verified using the paper. 
	d2$crop <- "groundnut"
	d2$intercrops <- "none"
	i <- grep("Pigeonpea", d2$treatment)
	d2$crop[i] <- "pigeon pea"
	d2$intercrops[i] <- "groundnut"


	d <- carobiner::bindr(d1, d2)
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$country <- "Malawi"
	d$yield_part <- "grain"

##### Fertilizers #####
## From the publication experimental design
#Full rate fertilizer application (100%) = 69 kg/ha N, half-rate (50%) = 34.5 kg/ha N, and unfertilized maize had zero fertilizer (0%). For maize full rate fertilizer application, 100 kg/ha NP (23−21−0) was applied at planting, providing 23 kg/ha N and 9.2 kg/ha P. Four to six weeks after crop emergence, 100 kg/ha urea (46% N) was applied, providing 46 kg/ha N. Therefore, the full rate fertilizer regime provides about 9.2 kg/ha P and 69 kg/ha N. Doubled-up (an intercrop of two legumes that have different but compatible growth habits) or sole legumes were fertilized in the maize phase of the rotation at half the full rate.

	d$N_fertilizer <- ifelse(d$tn == 1, 69,
	                  ifelse(d$tn == 2, 0, 
					  ifelse(d$tn < 7, 34.5, 0))) # 1-6 is maize, >6 is groundnut
	d$P_fertilizer <- ifelse(d$tn == 1, 9.2,
	                  ifelse(d$tn == 2, 0, 
					  ifelse(d$tn < 7, 4.6, 0)))
	d$K_fertilizer <- 0
	i <- d$N_fertilizer > 0
	d$N_splits[i] <- as.integer(2)
	d$fertilizer_type[i] <- "NPK;urea"

## unique(d[, c("treatment", "tn", "N_fertilizer", "P_fertilizer", "N_splits")])

##### Location
# For Ntiya (EPA Mtiya of Malawi) => https://www.google.com/maps/place/Kwilembe,+Malawi
	geo <- data.frame(
		location = c("Nsanama", "Nyambi", "Ntiya"), 
		longitude = c(35.507,  35.573,  35.395), 
		latitude = c(-14.964, -14.689, -14.352),
		geo_from_source = FALSE
	)

	d <- merge(d, geo, by="location", all.x=TRUE)
	d$tn <- NULL
	d$irrigated <- FALSE	
	
	d$yield_moisture <- as.numeric(NA) #needs to be checked

	carobiner::write_files(path, meta, d)
}

