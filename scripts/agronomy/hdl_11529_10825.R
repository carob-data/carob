0# R script for "carob"


carob_script <- function(path) {
  
"On-farm demonstration plots were set in Zambia to demonstrate the effects of conservation agriculture (CA) technologies as compared to the traditional farmers practice (ploughing with a mouldboard plough). The CA treatments included basins (BA), ripping (RI) and direct seeding with a direct seeder (DS) and direct seeding with a jab planter (JP). Also superimposed to the treatments are rotations and intercropping of maize with a grain legume (either soyabean or cowpea) and these are compared with continuous maize planting. The study is carried out in various communities of Zambia. Thus, the data set presents yields for maize and the legumes from these sites over 9 seasons (2006-2015). (2016-12-08)"
  
	uri <- "hdl:11529/10825"
	group <- "agronomy"
	ff	<- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=1,
		project=NA,
		publication=NA,
		data_organization = "CIMMYT",
		carob_contributor="Cedric Ngakou",
		carob_date="2023-08-02",
		data_type="on-station experiment",
		treatment_vars = "crop_rotation;planting_method;intercrops",
		response_vars = "yield"		
    )
  
  	f <- ff[basename(ff) == "Summary Zambia On-farm Demonstration 2006-2015.xls"]
	
	#read the data
	r1 <- carobiner::read.excel(f, sheet="Zambia all sites all maize", fix=TRUE)
	colnames(r1) <- tolower(colnames(r1))
	colnames(r1)[1:4] <- c("id", "year", "adm1", "village")
	r1[, 5:7] <- NULL
	colnames(r1) <- gsub(".11", "", colnames(r1))
	r1$crop.grown <- "maize"

	r2 <- carobiner::read.excel(f, sheet="Zambia all legume all years", fix=TRUE)
	colnames(r2) <- tolower(colnames(r2))
	colnames(r2)[1:3] <- c("id", "year", "adm1")

	r <- carobiner::bindr(r1, r2)
	
	d <- data.frame(
		adm1 = r$adm1, 
		location=r$village, 
		treatment=r$tmnt, 
		crop = tolower(r$crop.grown),
		fwy_residue = r$stalk.yield.kg.ha, 
		yield = r$grain.yield.kg.ha,
		rep = as.integer(r$site.rep),
		plant_density = r$final.stand.pl.ha,
		planting_date = as.character(r$year)
	)
	d$crop[d$crop=="cowpeas"] <- "cowpea"

	d$country <- "Zambia"
	
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	d$yield_part <- "grain"
	d$yield_part[d$crop != "maize"] = "seed"

	##really??? 
	###d$planting_date <- "2006"
	###d$harvest_date	<- "2015"

	d$trial_id <- paste0(d$crop, "_", d$location)
	
	p <- carobiner::fix_name(d$treatment)
	p <- gsub("DS", "direct seeder", p)
	p <- gsub("Control plot", "control", p)
	p <- gsub("check", "control", p)
	p <- gsub("dibble stick", "dibbling stick", p)
	p <- gsub("jabplanter", "jab planter", p)
	p <- tolower(gsub("^Direct$", "direct seeder",p))

	p[p=="basin"] <- "basins"

	d$treatment <- p

	pp <- sapply(strsplit(p, ","), \(i) i[1])

	d$planting_implement <- pp
	d$planting_implement[!(d$planting_implement %in% c("direct seeder", "jab planter", "dibbling stick"))] <- NA

	d$land_prep_method <- "none"
	d$land_prep_method[pp=="control"] <- "ploughing"
	d$land_prep_method[pp=="ripper"] <- "ripping"
	d$land_prep_method[pp=="basins"] <- "basins"

	d$land_prep_implement <- "none"
	d$land_prep_implement[pp=="control"] <- "mouldboard plough" 
	d$land_prep_implement[pp=="ripper"] <- "ripper"

	d$intercrops <- ifelse(grepl("maize/cowpea int", p), "cowpea", "none")
	
	d$crop_rotation <- ifelse(grepl("maize-cowpea rotation", p), "maize; cowpea",
			ifelse(grepl("maize-soybean rotation", p), "maize; soybean",
			ifelse(grepl("soybean-maize rotation", p), "maize; soybean", "none")))
													 
	# add longitude and	latitude
	geo <- data.frame(
		adm1=c("Monze","Kabwe","Chipata","Chibombo","Lundazi","Katete"),
		longitude=c(27.4763925, 28.3992336, 32.6324569, 27.6530228, 33.3487457, 32.04272),
		latitude=c(-16.2759563, -14.4571147, -13.7478246, -14.8569383, -12.4137188, -14.060241),
		geo_from_source=FALSE
	) 
	
	d <- merge(d, geo, by="adm1", all.x=TRUE)

	# fixes fwy_residue and plant_density out of the bounds
	d$fwy_residue[d$fwy_residue > 100000] <- NA
	d$plant_density[d$plant_density == 0] <- NA
	d <- d[!is.na(d$yield), ]
	d <- unique(d)
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
	
	
	d$yield_moisture <- as.numeric(NA) #needs to be checked

	carobiner::write_files(meta, d, path=path)
}



