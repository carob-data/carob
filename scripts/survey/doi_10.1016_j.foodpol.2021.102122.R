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
		insecticide_used = r$data$S1b.S1.Q17,
		herbicide_used = r$data$S1b.S1.Q18,
		irrigated = r$data$S1b.S1.Q10

	)

	d$trial_id <- as.character(1:nrow(d))
	d$planting_date <- as.character(NA)
	d$yield_part <- "grain"
	d$yield_moisture <- 12.5

	Wn <- paste0("M5.M5.Sub", 1:8, ".W")
	Mn <- gsub("W$", "Mo", Wn)
	for (i in 1:length(Wn)) {
		j <- data[,Wn[i]] > 400
		data[j, Wn[i]] <- data[j, Wn[i]] / 10
	}
	d$yield <- rowSums(data[, Wn] * ((100 - round(data[, Mn])) / 87.5))

	urea <- as.numeric(r$data$S1b.S1.Q7)
	npsdap <- as.numeric(r$data$S1b.S1.Q8)

	d$N_fertilizer <- urea * .46 + npsdap * .18
	d$P_fertilizer <- npsdap * .2
	d$K_fertilizer <- NA
	  
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$geo_from_source <- TRUE

   
	carobiner::write_files(path, meta, d)
}

