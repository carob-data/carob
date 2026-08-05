# REJECTED 
# Reason: no source columns matched draft() terminag heuristics (only empty data.frame() stubs or no tabular sheets)
# R script for "carob"
# license: GPL (>=3)

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
	

	f1 <- ff[basename(ff) == "valid_10242016.csv"]
	f2 <- ff[basename(ff) == "Geo_Peds06132016.Rda"]
	#f3 <- ff[basename(ff) == "Readme.rtf"]
	#f4 <- ff[basename(ff) == "rfsrc_bd.R"]
	
	

	r1 <- data.frame(read.csv(f1))
	load(f2)
	r2 = Geo_Peds
	
	
	d1 <- data.frame(
	  location = as.character(r1$province),
	  sample_id = as.character(r1$X.1),
	  soil_bd = as.numeric(r1$bd),
	  soil_SOC = as.numeric(r1$soc),
	  soil_pH = as.numeric(r1$ph_h2o),
	  depth = as.numeric(r1$depth),
	  soil_sand = as.numeric(r1$sand),
	  soil_clay = as.numeric(r1$clay),
	  #surface_geology = as.character(r1$surf_geology),
	  soil_texture = as.character(r1$structsize)
	  #soil_structure =as.character(r1$structtype)
	)
	
	d1[nrow(d1) + 1, "location"] <- "ozark broadleaf forest - meadow province"
	d1 <- d1[d1$location != "", ]
	
	###### For soil_texture which comprises  a mixture or range eg fine and medium the highest textural class was recorded ###### I had to ignore this structsize which also translated to soil_texture 
	d1$soil_texture[d1$soil_texture %in% c("very fine", "very fine and fine")] <- "fine"
	d1$soil_texture[d1$soil_texture == "fine and medium"] <- "medium"
	d1$soil_texture[d1$soil_texture %in% c("medium and coarse","very coarse","extremely coarse","fine to coarse","coarse and very coarse")] <- "coarse"
	d1$soil_texture[d1$soil_texture %in% c("thin","thick","very thick","very thin")] <- "unknown"
	

	d2 <- data.frame(
	  sample_id = as.character(r2$pedon_key),
	  adm1 = as.character(r2$state),
	  location = as.character(r2$province),
	  location_id = as.character(r2$county_cod),
	  date = as.character(r2$site_obsyear),
	  plot_slope = as.numeric(r2$slope),
	  latitude = as.numeric(r2$lat),
	  longitude = as.numeric(r2$long),
	  elevation = as.numeric(r2$elevation),
	  tmax = as.numeric(r2$tmax),
	  tmin = as.numeric(r2$tmin),
	  soil_pH = as.numeric(r2$ph_h2o),
	  soil_pH_CaCl2 = as.numeric(r2$ph_cacl2),
	  depth = as.numeric(r2$depth),
	  #soil_structure_grade = as.character(r2$structgrade),
	  soil_sand = as.numeric(r2$sand),
	  soil_clay = as.numeric(r2$clay),
	  soil_silt = as.numeric(r2$silt),
	  #surface_geology = as.character(r2$surf_geology),
	  soil_texture = as.character(r2$text_class),
	  #soil_structure = as.character(r2$structtype),
	  depth_top = as.numeric(r2$hzn_top),
	  depth_bottom = as.numeric(r2$hzn_bot),
	  soil_bd = as.numeric(r2$bd),
	  soil_SOC = as.numeric(r2$soc),
	  soil_EC = as.numeric(r2$ecec),
	  soil_Ca = as.numeric(r2$ca),
	  soil_K = as.numeric(r2$k),
	  soil_Mg = as.numeric(r2$mg),
	  soil_Na = as.numeric(r2$na),
	  soil_N_total = as.numeric(r2$n_tot),
	  soil_drainage = as.character(r2$drainagecl),
	  geo_source = as.character(r2$geocoordsource),
	  #CN_ratio = as.numeric(r2$cn_ratio),
	  soil_ex_acidity = as.numeric(r2$pot_acidity),
	  soil_stones = as.numeric(r2$rock_percent),
	  soil_depth = as.numeric(r2$hzn_thick) #### this case reffers to the layer thickness of the horizon
	)
	
	

	d2 <- d2[d2$location != "", ]
	
	d2$soil_drainage[d2$soil_drainage == "Excessively drained"] <- "excessively drained"
	d2$soil_drainage[d2$soil_drainage == "Well drained"] <- "well drained"
	d2$soil_drainage[d2$soil_drainage == "Moderately well drained"] <- "moderately well drained"
	d2$soil_drainage[d2$soil_drainage == "Somewhat excessively drained"] <- "somewhat excessively drained"
	d2$soil_drainage[d2$soil_drainage == "Somewhat poorly drained"] <- "somewhat poorly drained"
	d2$soil_drainage[d2$soil_drainage == "Very poorly drained"] <- "very poorly drained"
	d2$soil_drainage[d2$soil_drainage == "Poorly drained"] <- "poorly drained"
	d2$soil_drainage[d2$soil_drainage == ""] <- NA #### there are empty  spaces within this column
	
	
	soil_texture_class <- c(sil = "silt", c = "clay", sicl = "silty clay loam", sic ="silty clay",l = "loam", cl ="clay loam", fsl = "fine sandy loam", lfs = "very fine loamy sand", lcos = "coarse loamy sand", scl = "sandy clay", fs = "fine sand" , s = "sand", vfsl = "very fine sandy loam", cos = "coarse sand", sc = "sandy clay", cosl = "coarse sandy loam", sl = "sandy loam", lvfs = "very fine loamy sand", ls = "loamy sand", si = "silt", vfs = "very fine sand")
	d2$soil_texture <- soil_texture_class[d2$soil_texture]
	
	d2_unique <- d2[!duplicated(d2$location), ]
	dup_cols <- setdiff(intersect(names(d1), names(d2_unique)), "location")
	d2_clean <- d2_unique[, !(names(d2_unique) %in% dup_cols)]
	d <- merge(d1, d2_clean, by = "location", all.x = TRUE)
	
	d$soil_SOC[d$soil_SOC == 0] <- NA
	d$soil_Ca[d$soil_Ca == 0] <- NA
	d$soil_K[d$soil_K == 0] <- NA
	d$soil_Mg[d$soil_Mg == 0] <- NA
	d$soil_Na[d$soil_Na == 0] <- NA
	d$soil_pH[d$soil_pH == 10.8] <- NA
	d$tmax[d$tmax == 381] <- NA
	d$tmin[d$tmin == -138] <- NA
	
	
	code_map <- setNames(rep("United States", length(unique(d$location_id))),
	                     unique(d$location_id))
	
	d$country <- code_map[as.character(d$location_id)]
	d$on_farm <- NA
	d$is_survey <- TRUE
	d$geo_from_source <- TRUE
	
	
	carobiner::write_files(path, meta, d)
}
