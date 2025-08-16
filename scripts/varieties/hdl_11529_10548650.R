# R script for "carob"


carob_script <- function(path) {

"A trial, International Grasspea Yield Trial-Low B-ODAP was conducted to screen grasspea lines in B-3 plot of El Batan Station. The trial included twenty four selections from promising materials and was compared with Vetch as check. The objective of this trial was selection of a legume crop that can be grown as a source of feed and fodder in order to diversify mono cropped maize systems, especially in highlands. The trial was planted on November 23, 2016. The experimental plot conducted had silty clay loam texture with medium in organic matter (1.95%), moderately high in available phosphorus (35.7 kg/Ha) and available potassium (425 kg/ha), with a pH of 7.19. The experimental crop was planted on residual moisture using a tractor operated plot planter. The crop was planted in two lines on raised beds (75 cm wide), keeping 25 seeds /m length of bed (333333 seeds/ha). Seeds were counted before planting. The net plot size for each entry was kept as 6 m2. Post seeding irrigation was given on December 16, 2016. The emergence of vetch and grasspea was observed 9-12 days after sowing, vetch emerged 1-3 days before grasspea. Observations recorded on January 25th, revealed that almost all of the lathyrus lines have more than 5 times higher biomass than vetch. Some of the lathyrus lines especially coming from Bangladesh has much higher biomass that those coming from Europe.
"

	uri <- "hdl:11529/10548650"
	group <- "varieties"

	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIMMYT",
		publication = NA,
		project = NA,
		data_type = "experiment",
		treatment_vars ="variety",
		response_vars ="yield", 
		carob_contributor = "Blessing Dzuda",
		carob_date = "2024-08-27",
		notes = "1. Yield in the original dataset is in kg/ha and has got some high values.
		         2. Coordinates extracted from CIMMYT station on google maps
		         3. Variety is only available as numbers without variety names"
	)
	
	f <- ff[basename(ff) == "DAT-BV105-OI2016-17-Grasspea-BiomassYield.xlsx"]
	r <- carobiner::read.excel(f, sheet = "Calculation")

	d <- data.frame(
		country = "Mexico",
		adm1 = "Mexico",
		adm2 = "Texcoco",
		location = "El Batán",
		elevation = 2200,
		
		trial_id="1",
		latitude=19.5293,
		longitude= -98.8492,
		planting_date="2016-11-23",
		crop="grass pea",
		rep=r$REP,
		variety=r$VAR,
		yield_moisture=r$`%Humidity` * 100,
		dmy_storage=r$`Yield_Dry/ plot`,
		yield_part="grain",
		yield=r$`yld kg /ha`,
		soil_texture="silty loam",
		soil_SOM=1.95,
		soil_pH=7.19,
		land_prep_method="raised beds",
		seed_density=333333)
		
	d$on_farm <- FALSE
	d$is_survey <- FALSE
  d$irrigated <- FALSE
  d$geo_from_source <- TRUE
  
  d$P_fertilizer <- d$K_fertilizer <-  d$N_fertilizer <- d$S_fertilizer <- as.numeric(NA)
  d$rep <- as.integer(d$rep)
  d$variety <- as.character(d$variety)
  
	carobiner::write_files(path, meta, d)
}
