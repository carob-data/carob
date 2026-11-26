# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 1. The "Installation" sheet gives 10 plants per plot (one row) and a plot size of 2.7 m2,
#    but 30 tubers were planted per plot (NTP; "nº tub." in the field sheets "original" and
#    "Sheet1"), and up to 28 plants emerged. Each plot was one row of 30 plants at 0.3 x 0.9 m,
#    that is 8.1 m2 (confirmed by the contributor). Yield is computed with 8.1 m2
#    (TTWP * 10000 / 8.1); the TTYNA and TTYA columns of the fieldbook use 2.7 m2 and are not used.
# 2. Clones CIP302068 and CIP302264 have only 2 replications (also in the field sheets).
# 3. 5 plots have more plants harvested (NPH) than emerged (NPE), also in the field sheets.
# 4. The "Vine cutting / killing" date (2005-06-01) is before planting (2005-06-09); not used.
# 5. Soil texture is given as "Clay Loam", but the particle size (68% sand, 18% silt, 14% clay)
#    corresponds to a sandy loam; the texture class is not used.

## NOTES
# 1. This was a screening for resistance to potato leafroll virus (PLRV) ("Evaluation of PLRV ..."
#    in "Sheet1"; "2nd infection" in the "Minimal" sheet), with infector rows between the clones.
#    The PLRV infection (% plants positive by ELISA, 83-100%) recorded in "Sheet1" is not processed.
# 2. Yield is the non-adjusted total tuber yield; marketable yield is not reported.
#    plant_density is computed from the number of plants emerged (NPE).
# 3. Coordinates, elevation and dates from the "Minimal" sheet (CIP experiment station San Ramon).
#    It is not stated whether the trial was irrigated; no information on fertilizer use.
# 4. Soil properties from the "Soil_analysis" sheet (one sample, 2005-06-03).
#    Not processed: plant uniformity and vigor, tuber appearance and uniformity (1-9 scales).

carob_script <- function(path) {

"
Dataset for: Evaluation of Potato clones in San Ramon, Junin,  2005

The objective of this study was to assess the yield of 29 potato clones during the winter of 2005 in San Ramon, Junin.
"

	uri <- "doi:10.21223/NFNPO8"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = "RCBD",
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield",
		notes = NA,
		carob_contributor = "Vilma Hualla",
		carob_LLM = "Claude Opus 5.5",
		carob_date = "2026-11-26",
		carob_completion = 80,
		carob_effort = 1
	)

	process <- carobiner::get_function("process_cip_lbvars", path, group)
	f <- ff[grep("^PTYL.*\\.xls$", basename(ff))]
	d <- process(f)

	## plot size (ISSUES 1): process_cip_lbvars uses 2.7 m2 from "Installation";
	## the plots were one row of 30 plants (8.1 m2)
	adj <- d$plot_area / 8.1
	d$yield <- d$yield * adj
	d$plant_density <- d$plant_density * adj
	d$plot_area <- 8.1
	d$seed_density <- 30 * 10000 / 8.1

	## not stated in the data (NOTES 3)
	d$on_farm <- FALSE
	d$irrigated <- NA

	## soil (NOTES 4)
	s <- carobiner::read.excel(f, sheet="Soil_analysis")
	sv <- function(abb) as.numeric(s$Data1[trimws(s$Abbreviture) == abb])
	d$soil_pH <- sv("pH")
	d$soil_EC <- sv("EC")
	d$soil_SOM <- sv("MO")
	d$soil_P <- sv("P")
	d$soil_K <- sv("K")
	d$soil_sand <- sv("Sand")
	d$soil_silt <- sv("Silt")
	d$soil_clay <- sv("Clay")
	d$soil_CEC <- sv("CEC")
	d$soil_Ca_exch <- sv("ExCa2")
	d$soil_Mg_exch <- sv("ExMg2")
	d$soil_K_exch <- sv("ExK")
	d$soil_Na_exch <- sv("ExNa")
	d$soil_ex_acidity <- sv("ExAl3_H")
	d$soil_BS <- sv("BS")

	carobiner::write_files(path, meta, d)
}
