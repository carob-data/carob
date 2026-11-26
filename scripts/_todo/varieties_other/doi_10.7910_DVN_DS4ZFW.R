# R script for "carob"
# license: GPL (>=3)

## NOTES
# 2017 rainfed-upland multi-environment trial. Each BLUP workbook is a genotype-by-site matrix, not plot records.
# One row per genotype and site column. Yield is kg/ha, flowering is days, height is cm: site means are about 900-6400, 60-110 and 89-111, so the figures are not t/ha.
# Combined-G1 is Nigeria-Ibadan and Nigeria-Sabon Daga. Combined-G2 is Benin-Kpakpazounme and Benin-Sowe. Combined-G3 is Burkina faso-Banfora and Burkina Faso-Farakoba.
# Flowering is blank for Nigeria-Mokwa. Height is blank for Benin-Kpakpazounme and Burkina Faso-Lesso. The margin note lists Guinea sites as "Bordo, Bordo, Diankana" and does not name Banakalan; locations follow the column headers.

## ISSUES
# No coordinates, planting or harvest dates, fertilizer rates, or yield moisture. Dataverse productionDate is 2016-12-31; the trial year is the 2017 title.
# on_farm is FALSE because the 2017 text says the farmer adoption trial was removed. The sheets do not flag each hub.
# Check varieties are yellow cell fill only. That fill was not read.
# Statistics sheets say 3 replicates where a site was analysed. Flowering at Nigeria-Mokwa and height at Kpakpazounme and Lesso have no replicate count. Those were not expanded into plot rows.
# Some genotype-by-site cells are blank, so yield is missing where only flowering or height was published.
# The flowering footer labels the third statistic LCS rather than LSD.
# write_files: missing carob_contributor; missing yield_moisture, geo_from_source, latitude, longitude, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date.

## NOT PROCESSED
# BLUP-Flowering, BLUP-Height and BLUP-Yield footer rows Heritability, Mean and LSD (LCS on the flowering sheet): the same summaries as the statistics workbooks, not genotypes.
# BLUP columns Combined-G1, Combined-G2, Combined-G3 and Combined-All: multi-site aggregates, not a location.
# Margin notes (site counts and "Checks are highlighted in yellow"): not records.
# Statistics-Flowering-2017-Rainfed-Upland.xls, Statistics-Height-2017-Rainfed-Upland.xls, Statistics-Yield-2017-Rainfed-Upland.xls: Heritability, Mean, LSD, Replicates, P-value (Genotype), Variance (Genotype), Variance (Residual) by site. ANOVA only.
# Supplementary Materials - Rainfed Upland 2017.pdf: font-encoded print of the tables, not a separate data table.

carob_script <- function(path) {

"
2017 Africa-wide Breeding Task Force Trials for Rainfed Upland

Objective and Requirement of MET by Breeding Task Force:

In 2010, Rice Breeding Task Force in Africa was established aimed at accelerating the breeding process, especially in the later phase of evaluation of promising breeding lines in multi-locations throughout Africa. Breeding lines included in the multi-environment trials (MET) are developed by various institutions such as IRRI, CIAT, NARS in Africa and AfricaRice. The objective of the MET is to identify lines that are suitable for cultivation under an ecological environment in the target region in Africa. To reduce noise and to acquire real genetic differences among test lines through MET, the following conditions must be met for the MET: 1. Experimental field is uniform in soil fertility; 2. Experimental field is very well leveled before seeding or transplanting, to facilitate uniform water and fertilizer management; 3. Fertilizer is evenly applied to every plot, and ideally every plant within a plot; 4. Weed control, either by applying herbicide or hand weeding, is carried out uniformly across the whole trial, and finished in one day per time; 5. Any other operations, whatever necessary for a trial, are uniformly done across the whole trial.

The MET serves as a part of a national testing program. In other words, the MET conducted by BTF is integrated into the corresponding national testing system. Data collected from the MET will be recognized and used by the Varietal Testing and Release Committee of a country where the MET is conducted. This is a measure to shorten breeding cycle and to increase genetic gains. Structure of varietal evaluation series has been changed since the season of 2017.

Explanations of the changes

1. The number of entries is reduced to ensure better conduct of the trials and higher quality data. 1.1 For each sub-region (WCA and ESA), a maximum of 29 new entries per production system (irrigated lowland, rainfed lowland, rainfed upland, mangrove and high-elevation) is considered to enter the BTF (Phase I, former MET) each year. 1.2 After evaluation and data analysis, a maximum of 10 entries is selected for further evaluation (Phase II, former PET). 1.3 After evaluation and data analysis, a maximum of 3 entries is selected for further evaluation (Phase III, former PAT). 2. Materials from Phase I, II and III are combined into a single trial (up to 42 test entries every year). 3. Participatory varietal selection (PVS) may be conducted in the single trial (one replication may be chosen for the PVS). 4. The single trial is conducted in three different locations in each participating country and in priority in the Hubs. 5. The Farmers Adoption Trial (FAT) and Validation AfricaRice Trial (VAT) is removed from the BTF scheme. Where required, further testing of selected entries in farmers' fields will be ideally handled by NARS with the assistance of Rice Agronomy Task Force.
"

	uri <- "doi:10.7910/DVN/DS4ZFW"
	group <- "varieties_other"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "AfricaRice",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;flowering_days;plant_height",
		notes = "Variety-by-location BLUPs from the 2017 rainfed upland trial, not plot replicates.",
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 75,
		carob_effort = 0.4
	)

	process <- carobiner::get_function("process_africarice_blup", path, group)
	d <- process(ff, year = 2017, irrigated = FALSE)

	carobiner::write_files(path, meta, d)
}
