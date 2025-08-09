# R script for "carob"
# license: GPL (>=3)


carob_script <- function(path) {

"
How accurate are yield estimates from crop cuts? Evidence from smallholder maize farms in Ethiopia

Agricultural statistics and applied analyses have benefitted from moving from farmer estimates of yield to crop cut based estimates, now regarded as a gold standard. However, in practice, crop cuts and other sample-based protocols vary widely in the details of their implementations and little empirical work has documented how alternative yield estimation methods perform. Here, we undertake a well-measured experiment of multiple yield estimation methods on 237 smallholder maize plots in Amhara region, Ethiopia. We compare yield from a full plot harvest with farmer assessments and with estimates from a variety of field sampling protocols: W-walk, transect, random quadrant, random octant, center quadrant, and 3 diagonal quadrants. We find that protocol choices are important: alternative protocols vary considerably in their accuracy relative to the whole plot, with absolute mean errors ranging from 23 (farmer estimates) to 10.6 (random octant). Furthermore, while most methods approximate the sample mean reasonably well, the divergence of individual measures from true plot-level values can be considerable. We find that randomly positioned quadrants outperform systematic sampling schemes: the random octant had the best accuracy and was the most cost-effective. The nature of bias is non-classical: bias is correlated with plot size as well as with plot management characteristics. In summary, our results advocate that even 'gold standard' crop cut measures should be interpreted cautiously, and more empirical work should be carried out to validate and extend our conclusions.
"


	uri <- "doi:10.1016/j.foodpol.2021.102122"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group, files="https://ars.els-cdn.com/content/image/1-s2.0-S0306919221001019-mmc7.zip")

	meta <- data.frame(
		dataset_id = yuri::simpleURI(uri),
		uri = uri, 
		group = group,
		authors = "Frederic Kosmowski, Jordan Chamberlin, Hailemariam Ayalew, Tesfaye Sida, Kibrom Abay, Peter Craufurd",
		title = "How accurate are yield estimates from crop cuts? Evidence from smallholder maize farms in Ethiopia",
		description = "Agricultural statistics and applied analyses have benefitted from moving from farmer estimates of yield to crop cut based estimates, now regarded as a gold standard. However, in practice, crop cuts and other sample-based protocols vary widely in the details of their implementations and little empirical work has documented how alternative yield estimation methods perform. Here, we undertake a well-measured experiment of multiple yield estimation methods on 237 smallholder maize plots in Amhara region, Ethiopia. We compare yield from a full plot harvest with farmer assessments and with estimates from a variety of field sampling protocols: W-walk, transect, random quadrant, random octant, center quadrant, and 3 diagonal quadrants. We find that protocol choices are important: alternative protocols vary considerably in their accuracy relative to the whole plot, with absolute mean errors ranging from 23 (farmer estimates) to 10.6 (random octant). Furthermore, while most methods approximate the sample mean reasonably well, the divergence of individual measures from true plot-level values can be considerable. We find that randomly positioned quadrants outperform systematic sampling schemes: the random octant had the best accuracy and was the most cost-effective. The nature of bias is non-classical: bias is correlated with plot size as well as with plot management characteristics. In summary, our results advocate that even 'gold standard' crop cut measures should be interpreted cautiously, and more empirical work should be carried out to validate and extend our conclusions.",
		license = "CC BY 4.0",
		data_citation = "Frederic Kosmowski, Jordan Chamberlin, Hailemariam Ayalew, Tesfaye Sida, Kibrom Abay, Peter Craufurd, 2021. How accurate are yield estimates from crop cuts? Evidence from smallholder maize farms in Ethiopia. Food Policy 102:102122",
 		version = NA,
		data_organization = "SPIA;CIMMYT",
		publication = "doi:10.1016/j.foodpol.2021.102122",
		project = "TAMASA",
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none", 
		completion = 60,
		carob_contributor = "Robert Hijmans",
		carob_date = "2025-08-08",
		notes = NA,
		design = NA
	)
	
	
	r <- carobiner::read.RData(ff)

	d <- data.frame(
		crop= "maize",
		country = "Ethiopia",
		date = r$data$surveyDate,
		adm1 = r$data$note_demog.region,
		adm2 = r$data$note_demog.zone,
		adm3 = r$data$note_demog.woreda, 
		adm4 = trimws(r$data$note_demog.Kebele),
		location = r$data$note_demog.village,
		latitude = r$data$M3._M3.GPS_latitude,
		longitude = r$data$M3._M3.GPS_longitude,
		elevation = r$data$M3._M3.GPS_altitude,
		geo_uncertainty = r$data$M3._M3.GPS_precision,
		variety_type = tolower(r$data$S1b.S1.Q1), 
		variety = r$data$S1b.S1.Q2,
		insecticide_used = as.logical(r$data$S1b.S1.Q17),
		herbicide_used = as.logical(r$data$S1b.S1.Q18),
		irrigated = as.logical(r$data$S1b.S1.Q10),
		hhid = r$data$note_demog.hhid,
		urea = as.numeric(r$data$S1b.S1.Q7),
		npsdap = as.numeric(r$data$S1b.S1.Q8),
		seed_rate = r$data$S1b.S1.Q4b
	)

## labour_days is different than in the paper. The code from the paper had this error	
##	data [, c(65,66,67,69,70,71)] <- apply (data [, c(65,66,67,69,70,71)], 1, as.numeric)
## it should be (note the "t")
##	data [, c(65,66,67,69,70,71)] <- t(apply (data [, c(65,66,67,69,70,71)], 1, as.numeric))

	lab <- r$data[, c("S1b.S1.Q20_1", "S1b.S1.Q20_2", "S1b.S1.Q20_3", "S1b.S1.Q21_1", "S1b.S1.Q21_2", "S1b.S1.Q21_3")]
	d$labour_days <- apply(lab, 1, \(x) sum(as.numeric(x), na.rm=TRUE))

	d <- merge(d, r$GIS[,c("note_demog.hhid", "area_m2")], by.x="hhid", by.y="note_demog.hhid")

	d$trial_id <- as.character(1:nrow(d))
	d$planting_date <- as.character(NA)
	d$yield_part <- "grain"
	d$yield_moisture <- 12.5

	Wn <- paste0("M5.M5.Sub", 1:8, ".W")
	Mn <- gsub("W$", "Mo", Wn)
	for (i in 1:length(Wn)) {
		j <- r$data[,Wn[i]] > 400
		r$data[j, Wn[i]] <- r$data[j, Wn[i]] / 10
	}
	yield <- rowSums(r$data[, Wn] * ((100 - round(r$data[, Mn])) / 87.5))

	d$yield <- 10000 * yield / d$area_m2  # kg/ha
	d$seed_rate <- 10000 * d$seed_rate / d$area_m2 
	d$labour_days <- 10000 * d$labour_days / d$area_m2

	d$urea <- 10000 * d$urea / d$area_m2
	d$npsdap <- 10000 * d$npsdap / d$area_m2

	d$N_fertilizer <- d$urea * .46 + d$npsdap * .18
	d$P_fertilizer <- d$npsdap * .2
	d$K_fertilizer <- as.numeric(NA)
	d$urea <- d$npsdap <- NULL

  
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$geo_from_source <- TRUE
	d$crop_cut <- TRUE

	d$plot_size <- d$area_m2
	d$area_m2 <- NULL	
	carobiner::write_files(path, meta, d)
}

