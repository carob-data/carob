# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
A non-relational database of over a million plant populations,  aggregated into over 6,000 field trials, grown across a decade over the  extent of Australian grain-growing regions. All data required to rebuild  the database is included, however, this step is extremely time  consuming and not at all necessary when using the pre-constructed data  or the imputed datasets. 

These latter datasets are analysis-ready, and contain millions of  phenotypes with associated weather, remote sensing, agronomic  management, species, variety, location and year data. These data are  extensively detailed in the associated manuscript, which should be cited  upon use:  Newman & Furbank (2021) 'A multiple species, continent-wide,  million-phenotype agronomic plant dataset' Sci. Data.   And further detailed in an analytic machine learning pipleline in:   Newman & Furbank (2021) 'Explainable machine learning models of  major crop traits from satellite-monitored continent-wide field trial  data' https://www.nature.com/articles/s41477-021-01001-0 (Preprint at https://www.biorxiv.org/content/10.1101/2021.03.08.434495v1 )  Individual flat files of species are detailed throughout, as compressed csv files bearing the species name. Site-aggregated phenotypes, environment, and management data are available in raw format as the 'site_frame.csv' and as datasets with missing data imputed through random resampling (random_sample_imputed1 and random_sample_imputed_rescaled) and random forest imputation (RF_imputed1 and RF_imputed_rescaled).

A further 25 imputations are availble on request. Barley data are only available on the understanding they will not be used for pro-alcohol research, or imporving barley yields, but only for aiding predictions in other species. 

Some of these data are derived from the Grains Research and  Development Corporation (GRDC). All site-variety aggregated data are  published on the current and cached  versions of the Grains Research and Development Corporation (GRDC)  website https://grdc.com.au/ under a CC BY-NC 3.0 AU license.

Oh and here's a legal disclaimer (which does NOT restrict your use or reuse of this database in any way beyond the CC license above) which we are including per the GRDC's request: 

Addendum

The dataset which forms the basis (in whole or part) for this paper is based predominantly on data sourced from the Grains Research & Development Corporation (GRDC) and GRDC's extensive investment in the collection, development and presentation of that dataset is acknowledged.

GRDC did not authorise the reproduction, publication or communication of the dataset and the dataset has not been subject to GRDC's quality control processes and does not include updates and corrections that have been made to the dataset and as such may be unreliable. Results of research based on the dataset should not be relied on for any purpose.

Any person wishing to conduct research using National Variety Trials (NVT) data must approach GRDC directly with a research proposal, noting that terms and conditions may apply.
"

	uri <- "doi:10.6084/m9.figshare.c.5296369"
	group <- "varieties"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=4, minor=NA,
		data_organization = "ANU",
		publication = "doi:10.1038/s41597-021-00898-8; doi:10.1038/s41477-021-01001-0",
		project = NA,
		carob_date = "2025-07-01",
		design = NA,	
		data_type = "on-farm experiment",
		treatment_vars = "variety",
		response_vars = "yield", 
		carob_contributor = "Robert Hijmans",
		completion = 25,	
		notes = NA
	)

	
	f <- ff[basename(ff) == "variety_frame.rds"]
	r <- readRDS(f)

	d <- data.frame(
		trial_id = as.character(r[["BOMDom_Trial_ID"]]),
		country = "Australia",
		adm1 = as.character(r[["PHENDom_State"]]),
		latitude = r[["BOMDom_Latitude"]],
		longitude = r[["BOMDom_Longitude"]],

		crop = tolower(r[["PHENDom_Crop"]]),
		yield = as.character(r[["PHENDom_yield_t_ha"]]),

		N_fertilizer = r[["METADom_Nitrogen_fert"]],
		P_fertilizer = r[["METADom_Phos_fert"]],
		S_fertilizer = r[["METADom_Sulfur_fert"]], #!!

		# = r[["PHENDom_Rows_per_Plot"]],
		planting_date = as.character(as.Date("1970-01-01") + r[["BOMDom_Sowing_date_numeric"]]),
		harvest_date = as.character(as.Date("1970-01-01") + r[["PHENDom_Harvest_date_numeric"]])
		#year = r[["METADom_Year"]],
	)

	d$adm1 <- carobiner::replace_values(d$adm1,
			c("NSW", "QLD", "SA", "TAS", "VIC", "WA"),
			c("New South Wales", "Queensland", "South Australia", "Tasmania", "Victoria", "West Australia"))

	d$crop <- carobiner::replace_values(d$crop,
			c("canola", "oat", "field pea"),
			c("rapeseed", "oats", "pea"))

	d$yield[d$yield == "-"] <- NA
	d$yield <- as.numeric(d$yield) * 1000

	vars <- grep("METADom_Variety_", names(r), value=TRUE)
	v <- sapply(r[, vars], as.logical)
	i <- apply(v, 1, \(i)which(i)[1])
	d$variety <- tolower(gsub("METADom_Variety_", "", vars)[i])


	#fn <- grep("METADom_Fertiliser_", names(r), value=TRUE)
	#f <- r[, fn]
	#names(f) <- gsub("METADom_Fertiliser_", "", names(f))
	#products <- unique(gsub("time_|_repeat_.", "", names(f)))
	
	#	soil_texture = tolower(r[["METADom_texture_class_10cm.test_10cm"]])

#"METADom_texture_class_10cm.test_10cm"
#"METADom_total_N_10cm.test_10cm"
#"METADom_total_P_10cm.test_10cm"                                     
#"METADom_Organic_C_10cm.test_10cm"                                   
#"METADom_pH_water_10cm.test_10cm"
#"METADom_pH_CaCl2_10cm.test_10cm"                                    
#"METADom_Conductivity_10cm.test_10cm"
#"METADom_ESP_10cm.test_10cm"                                         
#"METADom_Date_soil_test_10cm_numeric.test_10cm"
#"METADom_texture_class_10cm.test_10cm_repeat_1"                      
# _repeat_1
# _60cm
# _60cm_repeat_1

# "METADom_Crop_rotation_minus_0" _6  



	d$geo_from_source <- TRUE
	d$on_farm <- TRUE
	d$is_survey <- FALSE

	d <- unique(d[!is.na(d$yield), ])

	carobiner::write_files(path, meta, d)
}

