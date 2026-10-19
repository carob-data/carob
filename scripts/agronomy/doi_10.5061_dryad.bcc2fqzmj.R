# R script for "carob"
# license: GPL (>=3)

## ISSUES

### Treatment: 
 # - Conventional management only
# - Combined conventional and organic management
# - Cover cropping


carob_script <- function(path) {

"
Data from: Early impacts of cover crop selection on soil biological parameters during a transition to organic agriculture

Although Texas is not widely known for organic production, it is responsible for most organic peanut production in the US. When managed effectively, this can be a lucrative practice due to consumer demands. However, farmers pursuing organic management must undergo a three-year transition period to obtain United States Department of Agriculture (USDA) certification, which is often associated with depressed yields. The objective of this study was to measure the impact of organic management and cover crop selection on biological indicators of soil health during the second year of transitioning to an organic cotton-peanut rotation in West Texas. We conducted a field study at 2 locations, and soil health indicators including in situ respiration, carbon mineralization, enzyme activities, and phospholipid fatty acids (PLFAs) were measured. The use of a cover crop resulted in generally greater biological activities compared to the fallow after only one cover crop rotation, representing early elevated biological activities under plots treated with a cover crop. However, cover crop species selection typically did not affect soil biological parameters. Cover crop use also did not contribute to yield losses when compared to a fallow treatment. When comparing organic management to conventional, both sites experienced elevated PLFA counts under organic management. Importantly, results indicated that although reduced peanut yields could be expected under transitional organic management, this was not cover crop species-specific, nor was it a guarantee. Given the economic risk of organic management for farmers, this study will benefit producers deciding whether to pursue organic production and cover cropping.  
"

	uri <- "doi:10.5061/dryad.bcc2fqzmj"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=NA,
		data_organization = "TAMU",
		publication = "doi:10.1002/agg2.20532",
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "land_prep_method;cover_crop",
		response_vars = "yield;emission_CO2;flux_CO2;soil_PLFA", 
		notes = NA,
		carob_contributor = "Cedric Ngakou",
		carob_date = "2026-10-13",
		carob_completion = 90,	
		carob_effort = 3
	)
	

	f1 <- ff[basename(ff) == "dataset_organic_and_conventional_management.xlsx"]
	f2 <- ff[basename(ff) == "README.md"]
	
	r1 <- carobiner::read.excel(f1, sheet="ConventionalOnly", na = ".")
	r2 <- carobiner::read.excel(f1, sheet="ConventionalOnly_PLFA", na = ".")
	r3 <- carobiner::read.excel(f1, sheet="ConventionalOnly_CumulativeCO2", na= ".")
	#r4 <- carobiner::read.excel(f1, sheet="ConventionalOnly_Nodules")
	r5 <- carobiner::read.excel(f1, sheet="OrgCon", na = ".")
	r6 <- carobiner::read.excel(f1, sheet="OrgCon_PLFA", na = ".")
	r7 <- carobiner::read.excel(f1, sheet="OrgCon_CumulativeCO2", na = ".")
	#r8 <- carobiner::read.excel(f1, sheet="OrgCon_Nodules")
	
	#### process
	
	#### Conventional Only
	d1 <- data.frame(
	  location = r1$location,
	  depth = r1$depth,
	  plot_id = r1$plot_no,
	  rep = as.integer(r1$rep),
	  treatment = r1$trt,
	  land_prep_method = r1$orgcon,
	  cover_crop = r1$cover,
	  flux_CO2 = r1$CO2,
	  soil_temp = r1$soiltemp, # soil temperature (Celsius)
	  minC = r1$MinC, ## mineral carbon (mg CO2 kg-1 soil)
	  soil_moisture = r1$soilmoist, # %
	  residue_prevcrop = r1$cc_biomass,
	  trial_id  = paste0("onlyorg", "-", r1$event)
	  
	)
	
	d2 <- data.frame(
	  location = r2$location,
	  plot_id = r2$plot_no,
	  treatment = r2$trt,
	  depth = r2$depth,
	  land_prep_method = r2$orgcon,
	  cover_crop = r2$cover,
	  soil_PLFA = r2$TotalPLFA, #  phospholipid fatty acids (pmol/g)
	  yield = r2$yield
	  
	)
	
	### merge d1 and d2
	dd <- merge(d1, d2, by = c("location", "plot_id", "treatment", "depth", "land_prep_method", "cover_crop"), all.x  = TRUE)	
	
	d3 <- data.frame(
	  location = r3$location,
	  plot_id = r3$plot_no,
	  rep = as.integer(r3$rep),
	  depth = r3$depth,
	  treatment = r3$trt,
	  land_prep_method = r3$orgcon,
	  cover_crop = r3$cover,
	  emission_CO2 = r3$CumulativeCO2
	)
	
	### merge dd and d3
	
	dd <- merge(dd, d3, by = c("location", "plot_id", "rep", "treatment", "depth", "land_prep_method", "cover_crop"), all = TRUE)	
	
	
	##  Conventional and organic
	
	d5 <- data.frame(
	  location = r5$location,
	  plot_id = r5$plot_no,
	  rep = as.integer(r5$rep),
	  depth = r5$depth,
	  treatment = r5$trt,
	  land_prep_method = r5$orgcon,
	  cover_crop = r5$cover,
	  soil_moisture = r5$soilmoist, # %
	  soil_temp = r5$soiltemp, # soil temperature (Celsius)
	  minC = r5$MinC, ## mineral carbon (mg CO2 kg-1 soil)
	  flux_CO2 = r5$CO2,
	  residue_prevcrop = r5$cc_biomass,
	  trial_id  = paste0("OrgCon", "-", r5$event)
	)
	
	
	d6 <- data.frame(
	  location = r6$location,
	  plot_id = r6$plot_no,
	  treatment = r6$trt,
	  depth = r6$depth,
	  land_prep_method = r6$orgcon,
	  cover_crop = r6$cover,
	  soil_PLFA = r6$TotalPLFA,#  phospholipid fatty acids (pmol/g)
	  yield = r6$yield
	)
	
	### merge d5 and d6
	dd1 <- merge(d5, d6, by = c("location", "plot_id", "treatment", "depth", "land_prep_method", "cover_crop"), all.x  = TRUE)	
	
	
	d7 <- data.frame(
	  location = r7$location,
	  plot_id = r7$plot_no,
	  rep = as.integer(r7$rep),
	  depth = r7$depth,
	  treatment = r7$trt,
	  land_prep_method = r7$orgcon,
	  cover_crop = r7$cover,
	  emission_CO2 = r7$CumulativeCO2
	)
	
	### merge dd1 and d7
	dd1 <- merge(dd1, d7, by = c("location", "plot_id", "rep", "treatment", "depth", "land_prep_method", "cover_crop"), all = TRUE)	
	
	
	#### joint dd and dd1
	
	d <- carobiner::bindr(dd, dd1)
	
	d$treatment <-  c(NA, "organic, rye cover crop", NA,"organic, radish cover crop", NA, "organic, rye/vetch cover crop", NA, "organic, rye/vetch/radish cover crop", NA, "conventional, fallow", "conventional, rye cover crop", "conventional, radish cover crop", "conventional, rye/vetch cover crop", "conventional, rye/vetch/radish cover crop")[d$treatment]
	d$cover_crop <- c("rvr" = "rye;vetch;radish" ,  "f" = "none" , "rad" = "radish" ,  "rv" = "rye;vetch" ,   "rye" = "rye")[d$cover_crop]
	d$depth_top <- c(0, 10)[d$depth]
	d$depth_bottom <- c(10, 20)[d$depth]
	d$depth <- NULL
	d$OM_used <- grepl("o", d$land_prep_method)
	d$OM_type <- ifelse(grepl("o", d$land_prep_method), "compost", "none") 
	d$land_prep_method <- c("c" = "conventional", "o" = "none")[d$land_prep_method]
	d$crop <- "groundnut"
	d$crop_rotation <- "cotton;groundnut"
	d$location <- c("LUBB" = "Lubbock" ,  "VERN" = "Vernon")[d$location]
	##### Adding lon and lat
	# from publication
	geo <- data.frame(
	  location = c("Lubbock", "Vernon"),
	  longitude = c(-101.828, -99.365),
	  latitude = c(33.693, 34.091),
	  irrigation_amount = c(51, 32)*10,# mm
	  irrigation_number = c(5L, 9L),
	  geo_from_source = TRUE
	)
	
	d <- merge(d, geo, by= "location", all.x = TRUE)
	
	
	d$is_survey <- FALSE
	d$on_farm <- TRUE
	d$yield_moisture <- NA
	d$yield_part <- "grain"
	d$country <- "United States"
	d$irrigated <- NA
	d$planting_date <- "2020"
	d$harvest_date <- NA
	d$yield_isfresh <- NA
	
	d$K_fertilizer <- d$N_fertilizer <- d$P_fertilizer <- as.numeric(NA)
	
	
	carobiner::write_files(path, meta, d)
}

