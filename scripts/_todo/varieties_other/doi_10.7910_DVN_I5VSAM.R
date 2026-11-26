# R script for "carob"
# license: GPL (>=3)

## NOTES
# 2018 rainfed-lowland Breeding Task Force release. The workbooks are genotype-by-station BLUPs, not plot-level country files.
# One row per genotype and station. Yield, flowering and plant height come from the three BLUP sheets. Plot replicates are not in the files and are not rebuilt.
# The statistics sheets report 3 replicates where a trait was analysed. That is left as a note; there is no replicate column.
# Units are not printed. Yield is stored as recorded (about 1,100-10,400, kg/ha scale). Flowering values are days. Height values are cm.
# Flowering and height sheets spell Banfora as "banfora"; the yield sheet uses "Banfora". Both are stored as Banfora.
# The rainfed sheets spell Rokupr as "Rokupur". Stored as Rokupr.

## ISSUES
# No coordinates, planting or harvest dates, fertilizer rates, or yield moisture in the workbooks. Georeference the stations.
# Check genotypes are marked only by yellow cell highlighting, which is not a value, so they are not flagged.
# Some genotype-station cells are blank. Mali-Longorolla has no yield BLUP. Mali-Samogossoni has no height BLUP. Rows are kept when another trait is present, so yield is NA on those rows.
# Station names follow the column headers (Sokwae, Samogossoni, Longorolla, Edozhigi, Jima). Confirm spellings when georeferencing.
# The files do not say which stations were on-farm hot spots. All rows are coded on_farm FALSE from the station names.
# write_files: ; yield_moisture, geo_from_source, latitude, longitude, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date. NA detected: yield.

## NOT PROCESSED
# Statistics-Flowering-2018-Rainfed-Lowland.xls, Statistics-Height-2018-Rainfed-Lowland.xls, Statistics-Yield-2018-Rainfed-Lowland.xls: heritability, mean, LSD, replicates, genotype p-value, genotype variance and residual variance. ANOVA summaries, not variety rows. The same summary rows are repeated at the bottom of each BLUP sheet and are skipped.
# BLUP-Yield columns Combined-G1 (Samogossoni and Kilissi), Combined-G2 (Ndiaye and Ibadan) and Combined-All (all stations except Longorolla). Multi-station BLUPs, not a location. Grouping is described in Supplementary Materials - Rainfed Lowland 2018.pdf.
# 2018-Rainfed Lowland-protocol.pdf and the supplementary PDF (documents, not data tables).


carob_script <- function(path) {

"
2018 Africa-wide Breeding Task Force Trials for Rainfed Lowland

In 2010, the Africa-wide Rice Breeding Task Force was launched by AfricaRice involving National Agricultural Research System (NARS) from about 30 countries. The objectives of the network are to evaluate the stability of traits incorporated in breeding processes and to identify varieties best fit to growth conditions in target regions and to markets. The Task Force also accumulates data on the performance of new elite lines, thereby facilitating varietal release procedures. Furthermore, by exposing breeders from NARS and farmers to these elite lines during the testing phase, dissemination will be facilitated. The activities conducted by the Task Force consists of a series of consecutive trials. Promising breeding lines developed by AfricaRice or by national and international partners, such as IRRI, CIAT and the NARS are nominated for evaluation in one or several rice cultivation environments: rainfed lowland, irrigated lowland, rainfed upland, high elevation and mangrove. All nominated lines should be fixed and accompanied by supporting data on traits incorporated during the breeding process and with information on yield performance. These characteristics are checked at AfricaRice before incorporation into the network. Further details are in the '2018-Rainfed Lowland-protocol.pdf'
"

	uri <- "doi:10.7910/DVN/I5VSAM"
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
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 80,
		carob_effort = 0
	)

	process <- carobiner::get_function("process_africarice_blup", path, group)
	d <- process(ff, year = 2018, irrigated = FALSE)

	carobiner::write_files(path, meta, d)
}
