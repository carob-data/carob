
## ISSUES
# list processing issues here so that an editor can look at them
##### the dataset does not have data dictionery to refer to eg units are missing 
# some variables such as tmax and tmin do not reflect realistic conditions.

carob_script <- function(path) {

"
A Soil Bulk Density Pedotransfer Function Based on Machine Learning: A Case Study With The Kellogg Soil Survey Laboratory Database

Soils data from 41,878 horizons were extracted from the Kellogg Soil Survey Laboratory (KSSL) database and used to calibrate and validate the PTF. Environmental datasets included terrain attributes (elevation, slope, aspect, landform), national land cover classification, hierarchical ecosystem land classifications, and 19 bioclimatic indicators. The results of a 5‒fold cross-validation scheme showed that average root mean squared prediction error (RMSPE) was 0.13 g cm-3, and mean prediction error (MPE) was -0.001 g cm-3.
"

	uri <- "doi:10.7910/DVN/Y7KGDK"
	group <- "soil_samples"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "IASU",
		publication = NA,
		project = NA,
		carob_date = "2026-07-28",
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none", 
		carob_contributor = "Illiana Kwenda",
		carob_completion = 80,	
		carob_effort = 5
	)
	

##	f1 <- ff[basename(ff) == "valid_10242016.csv"] not usable. See Readme.rft
	f2 <- ff[basename(ff) == "Geo_Peds06132016.Rda"]
	#f3 <- ff[basename(ff) == "Readme.rtf"]
	#f4 <- ff[basename(ff) == "rfsrc_bd.R"]
	

#	r1 <- data.frame(read.csv(f1))
	r2 <- carobiner::read.RData(f2)$Geo_Peds	

	d <- data.frame(
	  sample_id = as.character(r2$pedon_key),
	  adm1 = as.character(r2$state),
	  #location = r2$province,
	  location_id = as.character(r2$county_cod),
	  date = as.character(r2$site_obsyear),
	  land_slope = atan(r2$slope / 100) * (180 / pi), # % to degrees
	  latitude = r2$lat,
	  longitude = r2$long,
	  elevation = r2$elevation,
	  ##tmax = r2$tmax / 10, #not original observations
	  ##tmin = r2$tmin / 10, #not original observations
	  soil_pH = r2$ph_h2o,
	  soil_pH_CaCl2 = r2$ph_cacl2,
	  #soil_structure_grade = r2$structgrade,
	  soil_sand = r2$sand,
	  soil_clay = r2$clay,
	  soil_silt = r2$silt,
	  #surface_geology = r2$surf_geology,
	  soil_texture = as.character(r2$text_class),
	  #soil_structure = r2$structtype,
	  depth_top = r2$hzn_top,
	  depth_bottom = r2$hzn_bot,
	  soil_bd = r2$bd,
	  soil_SOC = r2$soc,
	  soil_EC = r2$ecec,
	  soil_Ca = r2$ca,
	  soil_K = r2$k,
	  soil_Mg = r2$mg,
	  soil_Na = r2$na,
	  soil_N_total = r2$n_tot,
	  soil_drainage = as.character(r2$drainagecl),
	  geo_source = as.character(r2$geocoordsource),
	  #CN_ratio = r2$cn_ratio,
	  soil_ex_acidity = r2$pot_acidity,
	  soil_stones = r2$rock_percent,
	  soil_depth = as.numeric(r2$depth)
	)
	
	d$soil_drainage <- tolower(d$soil_drainage)
	d$soil_drainage[d$soil_drainage == ""] <- NA
	
	soil_texture_class <- c(sil = "silt", c = "clay", sicl = "silty clay loam", sic ="silty clay",l = "loam", cl ="clay loam", fsl = "fine sandy loam", lfs = "very fine loamy sand", lcos = "coarse loamy sand", scl = "sandy clay", fs = "fine sand" , s = "sand", vfsl = "very fine sandy loam", cos = "coarse sand", sc = "sandy clay", cosl = "coarse sandy loam", sl = "sandy loam", lvfs = "very fine loamy sand", ls = "loamy sand", si = "silt", vfs = "very fine sand")
	d$soil_texture <- soil_texture_class[d$soil_texture]
	
##	d_unique <- d[!duplicated(d$location), ]
##	dup_cols <- setdiff(intersect(names(d1), names(d_unique)), "location")
##	d_clean <- d_unique[, !(names(d_unique) %in% dup_cols)]

  
    i <- d$soil_depth > 300
	d$soil_depth[i] <- d$soil_depth[i] / 10
	d$depth_top[i] <- d$depth_top[i] / 10
	d$depth_bottom[i] <- d$depth_bottom[i] / 10

	d$soil_depth[d$soil_depth <= 0] <- NA
	d$depth_bottom[d$depth_bottom == 0] <- NA
	# guesswork, but these seem to be in mm (unlikely to have 3000 cm)

	d$soil_pH_CaCl2[d$soil_pH_CaCl2 == 0] <- NA
	d$soil_pH[d$soil_pH <= 1] <- NA
	d$soil_SOC[d$soil_SOC == 0] <- NA
	d$soil_Ca[d$soil_Ca == 0] <- NA
	d$soil_K[d$soil_K == 0] <- NA
	d$soil_Mg[d$soil_Mg == 0] <- NA
	d$soil_Na[d$soil_Na == 0] <- NA

	d$country <- "United States"
	d$on_farm <- NA
	d$is_survey <- TRUE
	d$geo_from_source <- TRUE
	
	carobiner::write_files(path, meta, d)
}
