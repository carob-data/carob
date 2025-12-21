# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Data on extent of grain yield and plant growth enhancement by plant growth-promoting broad-spectrum Streptomyces sp. in Chickpea

The field trials were carried out in two consecutive post rainy seasons (2012−2013 and 2013–2014) at International Crops Research Institute for the Semi-Arid Tropics (ICRISAT), At 3 days before sowing, surface application and incorporation of 18 kg N per ha and 20 kg P per ha as diammonium phosphate (DAP) were carried out in both seasons. The field trial was laid out in a randomized complete block design (RCBD) with three replicates and subplot sizes of 4 m× 3 ridges. The five strains of Streptomyces (CAI-24, CAI-121, CAI-127, KAI-32, and KAI-90) were cultured individually on a starch casein broth (SCB) at 28 °C for 5 days. The field trial was conducted with a medium duration chickpea variety, ICCV 2, The crop was harvested manually during the two seasons, in both the 2012–13 and 2013-2014 seasons, at 30 days after sowing (DAS), the number of nodule, nodule weight, root weight, and shoot weight were noted and at 60 DAS, the plant height, number of pod, pod weight, leaf area, leaf weight, and stem weight were noted. At crop maturity, stover yield, grain yield, total dry matter (TDM), 1000-seed weight, pod weight, number of seed, and seed weight were noted in both seasons.         Experiment location on Google Map
"

	uri <- "doi:10.21421/D2/8RNMPO"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "ICRISAT",
		publication = "doi:10.1007/s13165-015-0099-1",
		project = NA,
		carob_date = "2025-12-20",
		design = NA,
		data_type = "on-station experiment",
		treatment_vars = "seed_treatment",
		response_vars = "yield", 
		carob_contributor = "Cedric Ngakou",
		completion = 100,	
		notes = NA
	)
	

	f1 <- ff[basename(ff) == "Response of microbial biomass carbob and dehydrogenase.xlsx"]
	f2 <- ff[basename(ff) == "Response of various plant of chickpea 30 DAS, 60 DAS.xlsx"]

	r1 <- carobiner::read.excel(f1, na= "NA")
	r2 <- carobiner::read.excel(f2, na= "NA")

	### process data 
	
	d1 <- data.frame(
		seed_treatment = r1$Treatment,
		rep = as.integer(r1$`Replication number`),
		#harvest_days = gsub(" days", "",  r1$Days),
		planting_date = substr(r1$Year, 1, 4),
		soil_SOC = r1$OC,
		soil_N = r1$Nitrogen/10000, ## from ppm to %
		soil_P = r1$Phosphorus,
		soil_MBC = r1$`Microbial boimassC`
	)

	agg <- aggregate(. ~ seed_treatment + rep+ planting_date , d1, function(x) mean(x))
	
	###
	d2 <- data.frame(
		seed_treatment = r2$Treatment,
		rep = as.integer(r2$`Replication number`),
		planting_date  = substr(r2$Year, 1, 4),
		DAP = as.integer(r2$Days),
		nodule_weight = r2$`Nodule weight`,
		plant_height = r2$`Plant height`,
		dmy_residue = r2$`Stover yield`,
		yield = r2$`Grain yield`*1000,
		dmy_stems = (r2$`Stem weight`/12)*10000,
		dmy_leaves = (r2$`Leaf weight`/12)*10000,
		plot_area = 4*3,
		seed_weight = r2$`Thousand seed weight`,
		N_fertilizer= 18,
		P_fertilizer = 20,
		K_fertilizer = 0,
		fertilizer_type = "DAP",
		adm1 = "Telangana",
		location = "Patancheru, ICRISAT",
		country = "India",
		crop = "chickpea",
		harvest_days = 60,
		longitude = 78.2666 , ### from publication
		latitude =  17.50,
		geo_from_source = TRUE,
		trial_id ="1", 
		on_farm = FALSE, 
		is_survey = FALSE, 
		yield_part = "grain", 
		yield_moisture = as.numeric(NA), 
		irrigated = NA,
		soil_clay = 52, ## from publication
		soil_sand = 26,
		soil_silt = 22
		
	)

 d <- merge(d2, agg, by=intersect(names(d1), names(d2)), all.x = TRUE)
	

carobiner::write_files(path, meta, d)

}


