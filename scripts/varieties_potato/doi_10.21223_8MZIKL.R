# R script for "carob"


carob_script <- function(path) {
    
"The LBHT x LTVR population came from crossing the two populations developed at CIP: LBHT for late blight resistance and LTVR for virus resistance in order to exploit heterosis for tuber yield, in 2013 under quarantine greenhouses at La Molina. 7200  genotypes (45 families with 160 seeds each) were planted. At harvest 258 clones were selected. Since 2015 until 2019, these clones were tested for late blight and PVX, PVY virus resistance, heat and drought tolerance,  marketable tuber yield, dry matter and quality for industrial processing, The experiments were planted in sites where environmental conditions are favorable to have high pressure biotic and abiotic stresses that allow us to select clones with resistance and / or tolerance to these factors. Thirty-nine clones were selected for late blight resistance, heat tolerance, some clones have drought tolerance, resistance to virus PVX and or PVY an good quality for industrtial processing."
    
    uri <- "doi:10.21223/8MZIKL"
    group <- "varieties_potato"
    ff  <- carobiner::get_data(uri, path, group)
    
    meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
        data_organization = "CIP",
        publication = NA,
        project = NA,
        data_type = "experiment",
        treatment_vars = "variety",
        response_vars = "yield;yield_marketable", 
        carob_contributor = "Henry Juarez",
        carob_effort = NA,
        carob_date = "2024-09-11",
        notes = NA
    )
    
    f <- ff[grep("Data.xls", basename(ff))]
    
	r <- carobiner::read.excel.hdr(f = f, sheet="Table", skip=2, hdr=2)

	d <- data.frame(
		variety = r$Clone,
		SRA_201_ = r$Marketable.tuber.Yield.tons.MTYNA._SRA,
		HYO_2017.2018 = r$X2017.2018_HYO,
		OXA_201_ = r$OXA,
		MAJ.NI_2018.2019 = r$X2018.2019_Majes.normal.irrigation.NI,
		MAJ.RI_2018.2019 = r$Majes.restricted.irrigation.RI,
		AUDPC = r$Resistance_LB.AUDPC.2017.2018 / 100
	)
	
   
	vc <- grep("_", colnames(d), value=TRUE)
	d <- reshape(d, direction = "long", idvar = "variety", varying = list(vc), 
						timevar = "trial_id", times = vc, v.names="yield_marketable")
	
	d$loc <- substr(d$trial_id,1, 3)
	d$irrigated <- d$loc == "MAJ"
	d$irrigation_desc <- "none"
	d$irrigation_desc[grep("NI", d$trial_id)] <- "normal"
	d$irrigation_desc[grep("RI", d$trial_id)] <- "restricted"
   
 
    d$yield_marketable <- as.numeric(d$yield_marketable) * 1000
    
    d$on_farm <- FALSE
    d$is_survey <- FALSE
    d$irrigated <- FALSE	
    d$crop <- "potato"
    d$pathogen <- "Phytophthora infestans"
    d$country <- "Peru"
    d$yield_part <- "tubers"

	geo <- data.frame(
		loc = c("HYO", "OXA", "SRA", "MAJ"),
		location=c("Huancayo", "Oxapampa", "San Ramón", "Majes"), 
		adm1 = c("Junin", "Pasco", "Junin", "Arequipa"), 
		adm2 = c("Huancayo", "Oxapampa", "Chanchamayo", "Majes"), 
		longitude = c(-75.2237, -75.3564, -75.35835, -75.3868), 
		latitude = c(-12.0093, -11.1286, -11.12787, -10.5954)
	)

	d <- merge(d, geo, by = "loc", all.x = TRUE)
	d$loc <- NULL
	d$geo_from_source <- FALSE
    d$yield <- as.numeric(NA)

	### how is this possible?
	# d$planting_date <- "2017-05-04"
	# d$harvest_date  <- "2017-11-17"
	d$planting_date <- as.character(NA)

    d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)

	d <- d[!is.na(d$yield_marketable), ]
 
    carobiner::write_files(path = path, metadata = meta, wide=d)
}
