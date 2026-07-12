# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Chilimo litter-bag decomposition experiment, litter AWEN chemistry, soil
# temperature loggers, and monthly weather normals not processed: different
# observational unit (small elevation-gradient trial, not the 98-SU national grid).
# Left for a future contribution.
# biome, stoniness_pct, SOC_stock_tha, litter_C_stock_tha not in terminag (only the
# concentration soil_SOC, %, is standardized): kept under a reasonable name so they
# are flagged by the vocabulary check and can be considered for terminag.
# Source "LAT"/"LON" columns are swapped (LAT values fall in Ethiopia's longitude
# range 34-42, LON in its latitude range 5-14.5); corrected here.
# adm1 "Benishangul_Gumuz" -> "Benishangul-Gumuz"; "Dembi Dolo" (a town, not a
# region) -> "Oromia".
# location_id values are lab/field codes (e.g. "T-2"), not place names.
# SU O-348 has 2 litter measurements in the source with no replicate id; averaged.
# One extreme soil_bd (2.53 g/cm3, site AM-62, 20-30cm) kept as-is from source.

carob_script <- function(path) {

"Soil carbon stock, litter decomposition, and weather data from Ethiopian forests

100 sampling units (SU) were selected from the 631 SUs of the Forest Reference Level
submission 2017 (FRL 2017), stratified to be unbiased for total growing stock,
altitude and mean litter depth per SU; fieldwork succeeded on 98 of them. Soil was
sampled Nov 2017-mid Jan 2018 from undisturbed profiles at 0-10, 10-20 and 20-30 cm
below the organic layer with a 107.5 cm3 corer, composited from two parallel
profiles per SU, and analyzed at the EEFRI soil laboratory (Addis Ababa) for organic
C (wet oxidation), bulk density and particle size (laser diffraction). A subset of
28 samples was cross-checked by dry combustion at the Natural Resources Institute
Finland (LUKE), giving a correction factor (1.165) applied to all wet-oxidation OC%
values. SOC stock was calculated from OC%, fine-earth bulk density and a stoniness
correction (FAO VS-FAST). Litter stock (dry mass and depth of the surface litter
layer, converted to a C density assuming 50% C content) was measured at a subset of
the same SUs."

	uri <- "doi:10.5281/zenodo.6566752"
	group <- "soil_samples"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=6, minor=NA,
		data_organization = "Ethiopian Environment and Forest Research Institute; Natural Resources Institute Finland; FAO", #EEFRI; Luke; FAO
		publication = "doi:10.1002/ldr.3647",
		project = "Assessment of the Forest Carbon Content in Soil and Litter in Ethiopia",
		design = "balanced sample, 98 NFI SUs, 3 depths (0-10, 10-20, 20-30 cm) per SU",
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none",
		notes = NA,
		carob_contributor = "Oscar Bautista",
		carob_effort = 1,
		carob_date = "2026-07-11",
		carob_completion = 80
	)

	f1 <- ff[basename(ff) == "SOC_Ethiopia_2017-2018.csv"]
	r1 <- read.csv(f1)

	f2 <- ff[basename(ff) == "OC%20_PSA_12.2.2018.xlsx"]
	r2 <- carobiner::read.excel(f2, sheet="Sheet2", skip=2)
	colnames(r2) <- c("lab_no", "field_code", "depth_range", "net_sample_weight_g",
		"water_loss_105C", "weight_gt2mm_g", "weight_lt2mm_g", "OC_WB_pct",
		"coarse_fragment_pct", "soil_sand", "soil_silt", "soil_clay", "soil_texture")

	f3 <- ff[basename(ff) == "Litter_Ethiopia_25.1.2018.xlsx"]
	r3 <- carobiner::read.excel(f3)

	r1$field_code <- gsub(" +", "", trimws(r1$FieldCode))
	r1$depth_range <- gsub(" *-+ *", "-", trimws(r1$DepthRange_cm))
	r1$key <- paste(r1$field_code, r1$depth_range)

	r2$field_code <- gsub(" +", "", trimws(r2$field_code))
	r2$depth_range <- gsub(" *-+ *", "-", trimws(r2$depth_range))
	r2$key <- paste(r2$field_code, r2$depth_range)
	r2$soil_texture <- tolower(trimws(gsub("'", "", r2$soil_texture)))
	r2$soil_texture[r2$soil_texture == "silt loam"] <- "silty loam"
## a few SU codes (e.g. B-122) appear twice here with no counterpart in r1;
## drop the extra copy, it is unused by the merge below either way
	r2 <- r2[!duplicated(r2$key), ]

## SU O-348 has 2 litter measurements with no replicate id (see ISSUES); average them
## to one value per SU so the merge below doesn't fan out the unrelated SOC depth rows
	r3$field_code <- gsub(" +", "", trimws(r3$FieldCode))
	r3 <- aggregate(cbind(LitterCStock_tha) ~ field_code, data=r3, FUN=mean)

	r <- merge(r1, r2[, c("key", "soil_sand", "soil_silt", "soil_clay", "soil_texture")], by="key", all.x=TRUE)
	r <- merge(r, r3, by="field_code", all.x=TRUE)

	r$adm1 <- gsub("_", "-", r$Region)
	r$adm1[r$Region == "Dembi Dolo"] <- "Oromia"
## no PSA match for a few SUs (e.g. B-151, sampled twice for SOC but not for texture)
	r$soil_texture[is.na(r$soil_texture)] <- "unknown"

	depth <- do.call(rbind, strsplit(r$depth_range, "-"))

	d <- data.frame(
		country = "Ethiopia",
		adm1 = r$adm1,
		location_id = r$field_code,
		longitude = r$LAT, #swapped
		latitude = r$LON, #swapped
		geo_from_source = TRUE,
		depth_top = as.numeric(depth[,1]),
		depth_bottom = as.numeric(depth[,2]),
		soil_SOC = r$OC_adj,
		soil_bd = r$BDfe,
		soil_sand = r$soil_sand,
		soil_silt = r$soil_silt,
		soil_clay = r$soil_clay,
		soil_texture = r$soil_texture
	)
## not in terminag, see ISSUES
	d$biome <- r$BiomeSimplified
	d$stoniness_pct <- r$StoninessVSFAST
	d$SOC_stock_tha <- r$SOCfe_stoniness
	d$litter_C_stock_tha <- r$LitterCStock_tha

	carobiner::write_files(path, meta, d)
}
