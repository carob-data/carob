# R script for "carob"
# license: GPL (>=3)

## NOTES
# Cassava Monitoring Survey, Nigeria. One row per household x variety from cassava_variety_all-regions.csv.
# Area is converted only for Unit_Area_cass_pltd Hectare, Acre (x 0.404686) and Meter square (/ 10000). Other units (plot, football field, igba, chain, rope) stay in area_reported_ / area_unit_.
# Root harvest is converted only for Bag(50kg) and Bag(100kg). Yield (kg/ha) = bag kilograms / field_size when both exist. Other harvest units (pickup, wheelbarrow, basin, basket) are not converted.
# GPS from gps_data_offset.csv, joined on HouseID (first row per household). Points outside Nigeria (latitude 4-14, longitude 2.5-15) are set to NA. The file name says "offset".
# State, LGA and zone are numeric codes with no label table in the files that were read. They are kept as state_code_, lga_code_ and zone_code_.

## ISSUES
# Confirm whether gps_data_offset.csv coordinates are true positions or a privacy offset. Many points fall outside Nigeria (latitude up to hundreds of thousands, longitude up to 99) and were dropped. In-range points may still be shifted.
# Most area and harvest units are local and were not converted, so yield is missing for those rows. A bag-weight assumption for "Pickup" or "Wheelbarrow" would be a guess.
# StateID / LgaID / ZoneID need the CMS codebook before adm1/adm2 can be filled. Nigeria is set from the survey title (the files do not spell the country).
# Variety names are farmer names (AGRIC, TME 419, vitamin A, local descriptions), not a controlled list.
# dna.csv is leaf/DNA material, not a yield table.
# No publication DOI in the deposit.
# write_files: missing carob_contributor; unknown area_reported_, area_unit_, harvest_reported_, harvest_unit_, state_code_, lga_code_, zone_code_; yield 0 to 4e6 and high cassava yield 4e6; duplicate records; NA yield, latitude, longitude.

## NEW VARIABLES
# area_reported_: Area_cass_pltd as recorded
# area_unit_: Unit_Area_cass_pltd
# harvest_reported_: cassv_root_harvest as recorded
# harvest_unit_: cassv_root_harvstUnit
# state_code_, lga_code_, zone_code_: numeric location codes

## NOT PROCESSED
# association_all-regions.csv, household_assets, household_composition, institution_variable, livestock_owned, processing_machines, production_asset: household and asset modules.
# cassava_ppi_all-regions.csv, cassava_traits-preference, farmer_preference1 to 4: preference scores, not harvest.
# land_category_all-regions.csv: land holding by category. Not the variety-level area (that is Area_cass_pltd).
# keys_land_category_all_region.csv: land-category labels.
# dna.csv and metadata_cassava_dna.csv.
# cassava_variety columns on traits, product use, stem source, months to harvest, storage and root colour.
# All metadata_*.csv files (descriptions used only to confirm that area and harvest units are separate columns).
# cassava_traits-preference_all-regions.csv: not read. columns: ID, ZoneID, StateID, LgaID, LocalityID, EnumerationID, HouseID, Trait1, Trait2, Trait3, hhd_apply_fert, per_hh_inc_cassav, .... Observation records are missing from d until this file is standardized.
# farmer_preference1_all-regions.csv: not read. columns: ID, ZoneID, StateID, LgaID, LocalityID, EnumerationID, HouseID, Trait_Id, Trait_Id_remark, B01_Rank. Observation records are missing from d until this file is standardized.
# farmer_preference2_all-regions.csv: not read. columns: ID, ZoneID, StateID, LgaID, LocalityID, EnumerationID, HouseID, Traits_id, Rank, Remark. Observation records are missing from d until this file is standardized.
# farmer_preference3_all-regions.csv: not read. columns: ID, ZoneID, StateID, LgaID, LocalityID, EnumerationID, HouseID, trait, B01_3_Ranking, Remark. Observation records are missing from d until this file is standardized.
# farmer_preference4_all-regions.csv: not read. columns: ID, ZoneID, StateID, LgaID, LocalityID, EnumerationID, HouseID, Varietal_traits, Rank_traits, Remark. Observation records are missing from d until this file is standardized.
# household_assets_all-regions.csv: not read. columns: ID, ZoneID, StateID, LgaID, LocalityID, EnumerationID, HouseID, Household_asset, asset_own,  UnitValue_Naira , Total_Number, Women_portion, .... Observation records are missing from d until this file is standardized.
# household_composition_all-regions.csv: not read. columns: ID, ZoneID, StateID, LgaID, LocalityID, EnumerationID, HouseID, Individual_family_code, Name_hhd_member, Sex, Age, Age_checked, .... Observation records are missing from d until this file is standardized.
# institution_variable_all-regions.csv: not read. columns: ID, ZoneID, StateID, LgaID, LocalityID, EnumerationID, HouseID, Institution_Id, Ecntact, time_mem, FemaleContact_No, MaleContact_No, .... Observation records are missing from d until this file is standardized.
# livestock_owned_all-regions.csv: not read. columns: ID, ZoneID, StateID, LgaID, LocalityID, EnumerationID, HouseID, Livestock_owned, livestock_ownership, Total_Number_of_livestk_owned, Number_of_male_livestk_owned, Number_of_female_livestk_owned, .... Observation records are missing from d until this file is standardized.
# metadata_association_all_region.csv: documentation or a codebook, not an observation table.
# metadata_cass_ppi_all_region.csv: documentation or a codebook, not an observation table.
# metadata_cass_trait_prefernc_all_region.csv: documentation or a codebook, not an observation table.
# metadata_cass_variety_all_region.csv: documentation or a codebook, not an observation table.
# metadata_farmer_preference1.csv: documentation or a codebook, not an observation table.
# metadata_farmer_preference2.csv: documentation or a codebook, not an observation table.
# metadata_farmer_preference3.csv: documentation or a codebook, not an observation table.
# metadata_farmer_preference4.csv: documentation or a codebook, not an observation table.
# metadata_gps_data_offset.csv: documentation or a codebook, not an observation table.
# metadata_hhcomposition_all_region.csv: documentation or a codebook, not an observation table.
# metadata_house_asset_all-region.csv: documentation or a codebook, not an observation table.
# metadata_input_use.csv: documentation or a codebook, not an observation table.
# metadata_institution_variable_all-regions.csv: documentation or a codebook, not an observation table.
# metadata_land_category_all_region.csv: documentation or a codebook, not an observation table.
# metadata_livestockowned_allregion.csv: documentation or a codebook, not an observation table.
# metadata_processing_machine_allregion.csv: documentation or a codebook, not an observation table.
# metadata_production_asset_all-regions.csv: documentation or a codebook, not an observation table.
# processing_machines_all-regions.csv: not read. columns: ID, ZoneID, StateID, LgaID, LocalityID, EnumerationID, HouseID, ProcessingID, Access, Male_or_Female, OtherProcessing_ID, Process_Other_Name, .... Observation records are missing from d until this file is standardized.
# production_asset_all-regions.csv: not read. columns: ID, ZoneID, StateID, LgaID, LocalityID, EnumerationID, HouseID, production_asset, asset_ownership, Value_Naira, Total_portion, women_portion, .... Observation records are missing from d until this file is standardized.


carob_script <- function(path) {

"
The Cassava Monitoring Survey (CMS) in Nigeria household and plot level data

The Cassava Monitoring Survey (CMS) in Nigeria household and plot level data.
"

	uri <- "doi:10.25502/20180627/0915/at"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = NA,
		response_vars = "yield",
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 35,
		carob_effort = 0.5
	)

	f1 <- ff[basename(ff) == "cassava_variety_all-regions.csv"]
	r <- read.csv(f1)
	f2 <- ff[basename(ff) == "gps_data_offset.csv"]
	r1 <- read.csv(f2)

	unit <- trimws(r$Unit_Area_cass_pltd)
	area <- r$Area_cass_pltd
	field <- rep(NA_real_, nrow(r))
	field[unit == "Hectare"] <- area[unit == "Hectare"]
	field[unit == "Acre"] <- area[unit == "Acre"] * 0.404686
	field[unit == "Meter square"] <- area[unit == "Meter square"] / 10000

	hunit <- trimws(r$cassv_root_harvstUnit)
	hqty <- r$cassv_root_harvest
	hkg <- rep(NA_real_, nrow(r))
	hkg[hunit == "Bag(50kg)"] <- hqty[hunit == "Bag(50kg)"] * 50
	hkg[hunit == "Bag(100kg)"] <- hqty[hunit == "Bag(100kg)"] * 100
	yield <- ifelse(!is.na(field) & field > 0 & !is.na(hkg), hkg / field, NA_real_)

	variety <- trimws(r$Imp_cassv_var_name)
	variety[variety %in% c("", "NA")] <- NA

	r1 <- r1[!duplicated(r1$HouseID), ]
	lat <- r1$Lat
	lon <- r1$Long
	ok <- !is.na(lat) & !is.na(lon) & lat >= 4 & lat <= 14 & lon >= 2.5 & lon <= 15
	lat[!ok] <- NA
	lon[!ok] <- NA
	gps <- data.frame(
		HouseID = r1$HouseID,
		longitude = lon,
		latitude = lat
	)

	d <- data.frame(
		hhid = as.character(r$HouseID),
		crop = "cassava",
		variety = variety,
		field_size = field,
		yield = yield,
		yield_part = "roots",
		area_reported_ = area,
		area_unit_ = unit,
		harvest_reported_ = hqty,
		harvest_unit_ = hunit,
		state_code_ = as.character(r$StateID),
		lga_code_ = as.character(r$LgaID),
		zone_code_ = as.character(r$ZoneID),
		country = "Nigeria"
	)
	d$area_unit_[d$area_unit_ == ""] <- NA
	d$harvest_unit_[d$harvest_unit_ == ""] <- NA
	d$HouseID <- r$HouseID
	d <- merge(d, gps, by="HouseID", all.x=TRUE)
	d$HouseID <- NULL
	d <- d[!is.na(d$hhid), ]
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$geo_from_source <- !is.na(d$longitude) & !is.na(d$latitude)
	d$trial_id <- d$hhid

	carobiner::write_files(path, meta, d)
}
