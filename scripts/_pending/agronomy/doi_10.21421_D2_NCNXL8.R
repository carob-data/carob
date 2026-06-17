# R script for "carob"

#ISSUES 
# the description speaks of "eight different environments" but these do not appear in the standardized data
# also, if it is not the articile should provide info on N/P/K fertilization.

carob_script <- function(path) {

"
The data is on biofortification (delivery of micronutrients via micronutrient-dense crops) and this can be achieved through plant breeding and offers a cost-effective and sustainable approach to fighting micronutrient malnutrition. The present study was conducted to facilitate the initiation of a breeding programme to improve the concentration of iron (Fe) and zinc (Zn) in Peanut (Arachis hypogaea L.) seeds. The experiment was conducted with 64 diverse Peanut genotypes in eight different environments at the International Crops Research Institute for the Semi-Arid Tropics, Patancheru, India to assess the genetic variation for Fe and Zn concentrations in peanut seeds and their heritability and correlations with other traits. Significant differences were observed among the genotypes and environments for Fe, Zn, protein and oil concentration in seeds and their heritability was high, thus indicating the possibility of improving them through breeding. As seen in other plants, a significant positive association between concentrations of Fe and Zn was observed. Trade-offs between pod yield and Fe and Zn concentrations were not observed and the same was also true for oil content. Besides being high yielding, genotypes ICGV 06099 and ICGV 06040 had stable performance for Fe and Zn concentrations across environments. These are the ideal choices for use as parents in a breeding programme and in developing mapping populations.
"

	uri <- "doi:10.21421/D2/NCNXL8"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)
	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "ICRISAT",
		publication = "doi:10.1017/S0021859614000525",
		project = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "grain_Zn, grain_Fe,yield", 
		carob_contributor = "Blessing Dzuda",
		carob_effort = NA,
		carob_date = "2024-08-15",
		notes = "the irrigated trial had no irrigation data"
	)
	
	f1 <- ff[basename(ff) == "Post rainy 2009-10 Irrigated.xlsx"]
	f2 <- ff[basename(ff) == "Post rainy 2009-10 Rainfed.xlsx"]
	r1 <- carobiner::read.excel(f1)
	r2 <- carobiner::read.excel(f2)

	d1 <- data.frame(
	  trial_id="1",
		country = "India",
		crop="groundnut",
		planting_date="2009",
		variety= r1$Genotype,
		grain_Fe=r1$Iron,
		grain_Zn=r1$Zinc,
		shelling_percentage=r1$`Shelling outturn`,
		seed_weight=r1$`Hundred grain weight`,
		yield_part="grain",
		yield=r1$`Pod Yield`,
		grain_protein=r1$`Seed Protein Content`
	)
		
	d2 <- data.frame(
	  trial_id="2",
	  country = "India",
	  crop="groundnut",
	  planting_date="2009",
	  variety= r2$Genotype,
	  grain_Fe=r2$Iron,
	  grain_Zn=r2$Zinc,
	  shelling_percentage=r2$`Shelling outturn`,
	  seed_weight=r2$`Hundred grain weight`,
	  yield_part="grain",
	  yield=r2$`Pod Yield`,
	  grain_protein=r2$`Seed Protein Content`
	)
	
	d <- rbind(d1,d2)
	
	d$longitude=78.9629
	d$latitude= 20.5937
	d$elevation = 160
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$irrigated <- TRUE
	d$geo_from_source <- TRUE
    d$inoculated <- FALSE
    d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
  
# all scripts must end like this
	carobiner::write_files(path, meta, d)
}
