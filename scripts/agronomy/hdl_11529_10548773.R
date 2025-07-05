# R script for "carob"


carob_script <- function(path) {

"A field study was framed in rice crop under conservation agriculture (CA) based rice-wheat system at experimental farm of Borlaug Institute for South Asia (BISA)-CIMMYT, Ladhowal, Punjab, India during kharif 2019. In the present study, nine treatments were imposed out of which four are CA-based treatments (ZT-N0, ZT-N50, ZT-N75 and ZT-N100), four are CA coupled with subsurface drip fertigation (CA+) based treatments (SSD-N0,SSD-N50, SSD-N75 and SSD-N100) and puddled transplanted rice (PTR) treatment as farmer’s practice. The findings of the study showed that PTR treatment out yielded in terms of yield attributing characters and biological yield than other treatments. CA+ treatment (SSD-N100) resulted higher biological yield (2.8%) than CA-based treatments (ZT-N100). SSD-N100 dominated ZT-N100 and PTR treatment in terms of plant N content (both grain and straw), total N uptake and N harvest index. PTR treatment resulted 22-33% higher ANUE than ZT-N100 and SSD-N100 treatments. (2020-12-01)"

	uri <- "hdl:11529/10548773"
	group <- "agronomy"

	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
		data_organization = "ICAR;CIMMYT",
		publication = "doi:10.56093/ijas.v91i4.112625",
		project = NA,
		data_type = "experiment",
		treatment_vars = "N_fertilizer;land_prep_method;irrigation_method;planting_method",
		response_vars = "yield;grain_N;residue_N", 
		carob_contributor = "Shumirai Manzvera",
		carob_date = "2024-08-01"
	)
	
	f <- ff[basename(ff) == "CIMMYT Data.xlsx"]
  h <- carobiner::read.excel(f, sheet= "IJAS-Rana")[1,]

  r <- carobiner::read.excel(f, sheet= "IJAS-Rana", skip=1 )
 
    rr <- r[-1,]
    r1 <- rr[, c(1, which(r[1,] == "R1"))]
    r2 <- rr[, c(1, which(r[1,] == "R2"))]
    r3 <- rr[, c(1, which(r[1,] == "R3"))]
	colnames(r2) <- colnames(r3) <- colnames(r1)
	r1$rep <- 1L
	r2$rep <- 2L
	r3$rep <- 3L
	x <- rbind(r1, r2, r3)

	d <- data.frame(
		treatment = x$Treatment,
		rep = x$rep,
		yield = as.numeric(x$`Biological yield (t/ha)`) * 1000,
		harvest_index = as.numeric(x$`Harvest index (%)`),
		grain_N = as.numeric(x$`N grain content (%) at harvest`) * 10,
		residue_N = as.numeric(x$`N straw content (%) at harvest`) * 10
	)
 
	d$variety <- "PR 126" 
	d$plot_length <- 25
	d$plot_width <- 5.4 
	
	d$N_fertilizer <- 0
	d$N_fertilizer[grep("N75", d$treatment)] <- 75
	d$N_fertilizer[grep("112.5", d$treatment)] <- 112.5
	d$N_fertilizer[grep("150", d$treatment)] <- 150
	d$N_fertilizer[grep("PTR", d$treatment)] <- 120
	d$N_splits <- 4L
	d$N_splits[grep("PTR", d$treatment)] <- 3L
	
	# A common dose of 60 kg P2O5 + 40 kg K2O + 25 kg ZnSO4/ha  was  applied  as  basal  in  all  the ZTDSR plots
	d$P_fertilizer <- 60 / 2.29
	d$K_fertilizer <- 40 / 1.2051
	d$Zn_fertilizer <- 25 / 2.74
	
	d$land_prep_method <- "none"
	d$land_prep_method[grep("PTR", d$treatment)] <- "puddling"

	d$irrigation_method <- "continuous flooding"
	d$irrigation_method[grep("SSD", d$treatment)] <- "sub-surface drip"

	d$planting_method <- "direct seeding"
	d$planting_method[grep("PTR", d$treatment)] <- "transplanting"
	 
	d$planting_date <- "2019"
	d$crop <- "rice" 
	
	# paper states location is at  "Borlaug Institute for South Asia (BISA)-CIMMYT, Ladhowal (30.99 °N latitude, 75.44 °E longitude, 229 m amsl), Punjab, India." 
	# But the BISA is at 30.99, 75.735
	d$country  <-  "India"
	d$adm1 <- "Punjab"
	d$location <- "Ladhowal"
	d$site <- "Borlaug Institute for South Asia"
	d$latitude <- 30.992
	d$longitude <- 75.735
	d$geo_uncertainty <- 1000
	d$elevation <- 229
	d$geo_from_source <- FALSE # ? TRUE because from article, but improved so it is FALSE
	d$is_survey <- FALSE 
	d$yield_part  <-  "grain"
	d$trial_id <- "1"
		
	d$on_farm <- TRUE
	d$irrigated <- TRUE

	carobiner::write_files(path, meta, d)
}

