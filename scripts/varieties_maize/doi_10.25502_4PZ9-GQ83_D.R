# R script for "carob"



carob_script <- function(path) {
   
"Landraces of maize (Zea mays L.) are invaluable sources of genetic variability for improving agronomic traits, and they hold great promise in developing new maize varieties with enhanced resilience to stresses. Even though phenotypic characterization is an inexpensive approach for elucidating variation hidden in genetic resources, information on the genetic diversity patterns in large collections of landraces remains limited and this constitutes a major impediment for their optimal utilization in modern maize breeding programs. We investigated the extent of phenotypic diversity among 196 maize landraces, representing gene pools from Burkina Faso, Ghana and Togo, and 14 improved populations/varieties from the Maize Improvement Program of International Institute of Tropical Agriculture (IITA-MIP). The germplasm was assessed for 26 agronomic traits. Highly significant differences (P < 0.001) were observed among the accessions for all measured traits.
Cluster analysis separated the maize germplasm into five major groups, differentiated largely by phenology and overall phenotypic appeal, enabling identification of outstanding genotypes for further screening for stress tolerance. Wide genetic diversity was observed between Burkinabe and improved gene pools, suggesting that the original Sahelian gene pool might not have contributed much to modern cultivars. This gene pool offers opportunities for pre-breeding by providing novel alleles for enriching elite maize germplasm. Shannon-Weaver diversity index (H’) revealed high genetic variability among the landraces (H’ = 0.73) and a narrow genetic base in the improved populations and varieties (H’ = 0.46). These results provide new insights into the potential of tropical maize landraces for genetic improvement of maize."

   
   uri <-  "doi:10.25502/4PZ9-GQ83/D"
   group <- "varieties_maize" 

   ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		publication= "doi:10.1080/15427528.2019.1674760",
		data_organization = "IITA",
		carob_contributor="Cedric Ngakou",
		data_type="experiment",
		response_vars = "yield",
		treatment_vars = "variety",
		project=NA,
		carob_date="2023-10-03"
   )
   
   r <- read.csv(ff[basename(ff)==tolower("Phenotypic-characterization-data.csv")])  
    
   d <- data.frame(
		variety = r$GENOTYPE,
		variety_code = as.character(r$ENTRY),
		planting_date = as.character(r$YEAR),
		rep = r$REP,
		dy_poll = r$POLLEN,
		silking_days = r$DYSK, 
		asi = r$ASI,
		plant_height= r$PLHT, 
		ear_height=r$EHT, 
		p_asp=r$PASP, 
		e_rot=r$EROT, 
		e_asp=r$EASP, 
		yield=r$YIELD, 
		husk=r$HUSK,
		crop="maize",
		yield_part="grain"
   )
   
	d$trial_id <- d$planting_date
	d$on_farm <- TRUE
	d$irrigated <- FALSE
	d$borer_trial <- FALSE
	d$striga_infected <- FALSE
	d$striga_trial <- FALSE


## from the paper 
	d$country <- "Nigeria"
	d$location <- "Ikenne"
	d$longitude <- 3.7
	d$latitude <- 6.88333
	d$elevation <- 60
	d$row_spacing <- 75
	d$plant_spacing <- 40
	d$plant_density <- 66666
	d$N_fertilizer <- 90
	d$P_fertilizer <- 60
	d$K_fertilizer <- 60
	d$fertilizer_type <- "NPK; urea"
	d$is_survey <- FALSE

	carobiner::write_files(meta, d, path=path)	
}


