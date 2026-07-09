# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Soybean response to cover crop and nitrogen fertilizer timing on sandy soil

The potential for nitrogen (N) fertilization to increase soybean yield is known to vary with environmental conditions, but the effects of N timing and rate remain unclear in sandy soils. We conducted a two-year irrigated field study on a Plainfield sandy soil (mixed, mesic Typic Udipsamments) in central Wisconsin to evaluate soybean growth and yield under varying N fertilizer treatments. Treatments included an unfertilized control; a starter application (34 kg N ha⁻¹); single applications of 101 kg N ha⁻¹ at 10, 30, 60, or 80 days after emergence (DAE); and split applications totaling 202 or 404 kg N ha⁻¹ applied at 30, 60, and 80 DAE. All fertilizer treatments were nested within a rye cover crop system, planted in the fall and chemically terminated in May prior to soybean planting. Soybean dry matter and N content were measured in June, July, and August, and yield was recorded at harvest. Fertilizer effects on yield varied by year. In 2019, the split404 treatment increased yield by 8% relative to the control, while in 2020, no fertilizer treatments significantly affected yield. Starter and 10DAE treatments increased August dry matter and N uptake, but only 10DAE avoided early-season reductions seen with starter. Cover cropping had no significant effect on soybean yield, dry matter, or N content. Split404 improved yield in 2019, but the yield gain was insufficient to justify the added fertilizer cost. Overall, we find no evidence that N fertilization improves yield in irrigated soybean grown on sandy soils in Wisconsin.  
"

	uri <- "doi:10.5061/dryad.866t1g22s"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=7, minor=NA,
		data_organization = "UWM",
		publication = "doi:10.1002/agj2.70396",
		project = NA,
		design = "RCSP",
		data_type = "experiment",
		treatment_vars = "cover_crop;N_fertilizer",
		response_vars = "yield", 
		notes = NA,
		carob_contributor = "Cedric Ngakou",
		carob_date = "2026-07-09",
		carob_completion = 100,	
		carob_effort = 1
	)
	

	f1 <- ff[basename(ff) == "DRYAD_DATA_FINAL.csv"]
	#f2 <- ff[basename(ff) == "README.md"]

	r1 <- read.csv(f1)
	
######
	d <- data.frame(
	  year = r1$Year,
		planting_date = ifelse(grepl("2020", r1$Year), "2020-05-29", "2019-05-20") ,
		plot_id = as.character(r1$Plot),
		rep = r1$Block,
		treatment = r1$Treatment,
		cover_crop = ifelse(grepl("Cover crop", r1$Cover.crop), "rye", "none"),
		yield = r1$Yield_13._moisture,
		yield_moisture = 13,
		dmy_residue_June = r1$June_DM_.kg.ha.,
		residue_N_june = r1$June_Ncontent,
		dmy_residue_July = r1$July_DM_.kg.ha.,
		residue_N_july = r1$July_Ncontent,
		dmy_residue_Aug = r1$Aug_DM_.kg.ha.,
		residue_N_Aug = r1$Aug._Ncontent,
		variety = "RS207NX, Renk",
		fertilizer_type = "urea",
		crop = "soybean",
		location = "University of Wisconsin Hancock Agricultural Research Station",
		longitude = -89.53903, ## from paper
		latitude = 44.11794,
		soil_texture = "sand",
		row_spacing = 76,
		country = "United States",
		is_survey = FALSE, 
		on_farm = TRUE, 
		trial_id = paste(r1$Block, r1$Plot, sep = "-"), 
		yield_part = "grain", 
		geo_from_source = TRUE, 
		irrigated = NA, 
		record_id = as.integer(1:nrow(r1))
	)
	
	d$N_fertilizer <- c(0, 34, 101, 101, 101, 101, 202, 404)[d$treatment]
	d$DAE <- as.integer(c(NA, NA, 10, 30, 60, 80, NA, NA)[d$treatment])
	d$treatment  <- c("control", "starter", "10DAE", "30DAE", "60DAE", "80DAE", "split(202)", "split(404)")[d$treatment]
	d$N_splits <- as.integer(ifelse(grepl("split", d$treatment), 3, 1))
	###  records dmy_residue in long format
	i <- grepl("^dmy_residue|residue|record_id|year", names(d))
	Nm <- names(d)[i]
	dm <- d[, Nm]
	dm_long <- reshape(dm, varying = c("dmy_residue_June", "dmy_residue_July", "dmy_residue_Aug", "residue_N_june", "residue_N_july", "residue_N_Aug"), v.names = "value",
	                   timevar = "date",
	                   direction = "long")
	dm_long$variable <- c(rep("dmy_residue", 3), rep("residue_N", 3))[dm_long$date]
	dm_long$date <- paste(dm_long$year, c("06", "07", "08", "06","07", "08")[dm_long$date], sep = "-")
	dm_long <- dm_long[!is.na(dm_long$value),]
	dm_long$year <- dm_long$id <- NULL
	i <- grepl("^dmy_residue|residue|year", names(d))
	d <- d[, !names(d) %in% names(d)[i]]
	
	d$K_fertilizer <- d$P_fertilizer <- as.numeric(NA)
	
	carobiner::write_files(path, meta, d, long = dm_long)
}


