# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 1. No linked publication in the dataset record. Related-but-different CIP/Malawi papers were
#    found (Longwe et al. 2023 FARA report 7(62):800-809, sweetpotato-pigeonpea only; a 2025
#    Frontiers in Agronomy paper, sweetpotato-soybean at research stations) but neither matches
#    this on-farm Salima+Mulanje 8-treatment set, and the FARA report was Cloudflare-blocked, so
#    publication is left NA rather than guessed.
# 2. Season-1 file has non-NA "Soybean grain weight" values on "Sole SP" rows (no soybean planted
#    there), while "Sole SB"/"Sole PP" rows correctly show all-NA for inapplicable crops. These
#    values don't match the root-weight columns or anything else in the row and are never used
#    here, since a legume row is only built when trt$legume is non-NA, which excludes Sole SP.
# 3. plot_area back-calculated as kg/plot / t/ha * 10: exactly 18 m2 in season 1 (from the legume
#    kg/plot columns) and 24 m2 in season 2 (from the sweetpotato-root kg/plot columns); assumed
#    shared by all 8 co-located treatments per season (required for LER to be meaningful), but not
#    cross-checked within a season since neither file reports kg/plot for both root and legume.
# 4. Season-2 file has no vine-weight column at all, so fwy_residue is NA for all season-2 rows.
# 5. Planting/harvest dates, varieties, fertilizer, and spacing are not reported; planting_date is
#    set to the season's first calendar year only (Malawi's single Nov-Apr rainy season); harvest_
#    date is left NA.
# 6. intercrop_type is set to "row" for all intercropped treatments (1:1, 2:1, and "Row") per the
#    dataset abstract's own framing of these as row-based variants; the exact ratio under test is
#    kept verbatim in the non-standard `row_ratio` field.
# 7. Season-2 EPA "Thuchira" corrected to "Thuchila" (season-1 spelling; confirmed real EPA name).
# 8. No GPS in source; district-level GADM centroids used instead (adm_pointRadius), so
#    geo_uncertainty is tens of km, not farm-level.

carob_script <- function(path) {

"On-farm Sweetpotato-Legume Intercropping Trial Data (Malawi, 2022-2024)

On-farm trial dataset evaluating sweetpotato intercropped with soybean and pigeonpea under
eight spatial-arrangement treatments (sole crop and 1:1 / 2:1 / row intercrop combinations),
conducted with smallholder farmers in Salima and Mulanje districts, Malawi, across the
2022/2023 and 2023/2024 growing seasons. Variables include sweetpotato root count and weight
(marketable and non-marketable), vine weight, soybean and pigeonpea grain yield, and
partial/total Land Equivalent Ratio (LER)."

	uri <- "doi:10.21223/P3/3AIN78"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIP",
		publication = NA,
		# CIP Dataverse lists "Funder: CGIAR Research Program on Roots, Tubers and Bananas
		# (RTB)" as a Contributor, but RTB was a CGIAR-wide funding umbrella (not a specific
		# coordinated trial network like ACAI/N2Africa/GAIA) that formally ended ~2021, before
		# this 2022-2024 trial - a funder acknowledgment, not a carob "project".
		project = NA,
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "intercropped;intercrops;row_ratio",
		response_vars = "yield;yield_marketable;fwy_residue;LER",
		carob_contributor = "AI agronomy writer agent",
		carob_date = "2026-07-27",
		carob_completion = 85,
		# measured from session start to PR creation (scratchpad-dir ctime 10:29:01 to
		# PR-creation timestamp 10:58:23, both 2026-07-27): ~29 minutes, rounded up slightly
		carob_effort = 0.5
	)

## read data
	f1 <- ff[basename(ff) == "01_data_sweetpotato-legume 2022 2023 on farm for analysis_ANONYMIZED.xlsx"]
	f2 <- ff[basename(ff) == "02_data_sweetpotato-Legumes 2023-2024 on farm for analysis_ANONYMIZED.xlsx"]
	r1 <- carobiner::read.excel(f1, fix_names=TRUE, lower=TRUE)
	r2 <- carobiner::read.excel(f2, fix_names=TRUE, lower=TRUE)

# season 1 file has no explicit season column (season 2 does)
	r1$season <- "2022/2023"

# season 2 misspells this Mulanje EPA; correct to match season 1 / standard spelling
	r2$site.epa[r2$site.epa == "Thuchira"] <- "Thuchila"

# the two files punctuate the same 8 treatments slightly differently; harmonize to one code
	harmonize_treatment <- function(x) {
		x <- gsub("SP:PP-Row", "SP+PP Row", x, fixed=TRUE)
		x <- gsub("SP+PP-Row", "SP+PP Row", x, fixed=TRUE)
		x <- gsub("SP+PP-1:1", "SP+PP 1:1", x, fixed=TRUE)
		x
	}
	r1$treatment <- harmonize_treatment(r1$treatment)
	r2$treatment <- harmonize_treatment(r2$treatment)

# lookup table for the 8 spatial-arrangement treatments (avoids nested ifelse): which legume
# (if any) sweetpotato is intercropped with, the row ratio under test, and intercropped status
	trt <- data.frame(
		treatment    = c("Sole SP", "Sole SB", "Sole PP", "SP+SB 1:1", "SP+SB 2:1", "SP+PP 1:1", "SP+PP 2:1", "SP+PP Row"),
		legume       = c(NA, "soybean", "pigeon pea", "soybean", "soybean", "pigeon pea", "pigeon pea", "pigeon pea"),
		row_ratio    = c("sole", "sole", "sole", "1:1", "2:1", "1:1", "2:1", "row"),
		has_sp       = c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE),
		intercropped = c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE)
	)
	r1 <- merge(r1, trt, by="treatment")
	r2 <- merge(r2, trt, by="treatment")

# combine both seasons into one common schema (h1/h2), referencing each file's own raw
# column names directly; season 1 and 2 use different raw names for the same concepts
	h1 <- data.frame(
		trial_year = r1$season,
		district = r1$district,
		epa = r1$epa,
		farmer_id = r1$farmer.id,
		plot_no = as.integer(r1$treat.no),
		treatment = r1$treatment,
		legume = r1$legume,
		row_ratio = r1$row_ratio,
		has_sp = r1$has_sp,
		intercropped = r1$intercropped,
		vine_tha = r1$vine.wt.t.ha,
		root_mkt_density = r1$x.of.marketable.roots.ha,
		root_nonmkt_density = r1$x.of.non.marketable.roots.ha,
		root_mkt_tha = r1$weight.of.marketable.roots.t.ha,
		root_total_tha = r1$total.root.weight.t.ha,
		soybean_tha = r1$soybean.grain.weight.t.ha,
		pigeonpea_tha = r1$pieonpea.grain.weight.t.ha,
		pler_sp = r1$pler.sp,
		pler_sb = r1$pler.sb,
		pler_pp = r1$pler.pp,
		ler = r1$ler,
		plot_area = 18   # m2; derived from legume kg/plot / t/ha, see ISSUES 3
	)

	h2 <- data.frame(
		trial_year = r2$season,
		district = r2$district,
		epa = r2$site.epa,
		farmer_id = r2$farmer.id,
		plot_no = as.integer(r2$plot),
		treatment = r2$treatment,
		legume = r2$legume,
		row_ratio = r2$row_ratio,
		has_sp = r2$has_sp,
		intercropped = r2$intercropped,
		vine_tha = as.numeric(NA),  # not reported in season 2, see ISSUES 4
		root_mkt_density = r2$number.of.marketable.roots.ha,
		root_nonmkt_density = r2$number.of.non.marketable.roots.ha,
		root_mkt_tha = r2$weight.of.marketable.roots.t.ha,
		root_total_tha = r2$total.root.weight.t.ha,
		soybean_tha = r2$soybean.grain.yield.t.ha,
		pigeonpea_tha = r2$pigeonpea.grain.yield.t.ha,
		pler_sp = r2$pler.sp,
		pler_sb = r2$pler.sb,
		pler_pp = r2$pler.pp,
		ler = r2$ler,
		plot_area = 24   # m2; derived from sweetpotato-root kg/plot / t/ha, see ISSUES 3
	)

	h <- rbind(h1, h2)

# sweetpotato component: one row per plot that actually grew sweetpotato (all treatments
# except the sole-legume check plots "Sole SB"/"Sole PP")
	sp <- h[h$has_sp, ]
	sp_df <- data.frame(
		trial_year = sp$trial_year,
		district = sp$district,
		epa = sp$epa,
		farmer_id = sp$farmer_id,
		plot_no = sp$plot_no,
		treatment = sp$treatment,
		crop = "sweetpotato",
		intercropped = sp$intercropped,
		intercrops = ifelse(is.na(sp$legume), "none", sp$legume),
		intercrop_type = ifelse(sp$intercropped, "row", "monocrop"),
		row_ratio = sp$row_ratio,
		plot_area = sp$plot_area,
		yield = sp$root_total_tha * 1000,               # t/ha -> kg/ha, fresh weight, all grades
		yield_marketable = sp$root_mkt_tha * 1000,      # kg/ha, fresh weight
		yield_isfresh = TRUE,
		yield_part = "roots",
		fwy_residue = sp$vine_tha * 1000,               # kg/ha fresh vine (foliage) weight
		root_density_marketable = sp$root_mkt_density,       # sellable-grade roots/ha
		root_density_nonmarketable = sp$root_nonmkt_density, # below quality/size grade roots/ha
		LER = sp$ler,
		pLER = sp$pler_sp
	)

# legume component: one row per plot that actually grew soybean or pigeon pea (the sole-legume
# check plots, and the legume side of every intercrop plot)
	lg <- h[!is.na(h$legume), ]
	legume_df <- data.frame(
		trial_year = lg$trial_year,
		district = lg$district,
		epa = lg$epa,
		farmer_id = lg$farmer_id,
		plot_no = lg$plot_no,
		treatment = lg$treatment,
		crop = lg$legume,
		intercropped = lg$intercropped,
		intercrops = ifelse(lg$has_sp, "sweetpotato", "none"),
		intercrop_type = ifelse(lg$intercropped, "row", "monocrop"),
		row_ratio = lg$row_ratio,
		plot_area = lg$plot_area,
		yield = ifelse(lg$legume == "soybean", lg$soybean_tha, lg$pigeonpea_tha) * 1000, # kg/ha grain
		yield_marketable = as.numeric(NA),
		yield_isfresh = as.logical(NA),  # unclear if legume grain weight is fresh or air-dried
		yield_part = "seed",             # pulses use "seed", not "grain" (terminag convention)
		fwy_residue = as.numeric(NA),
		root_density_marketable = as.numeric(NA),
		root_density_nonmarketable = as.numeric(NA),
		LER = lg$ler,
		pLER = ifelse(lg$legume == "soybean", lg$pler_sb, lg$pler_pp)
	)

	d <- rbind(sp_df, legume_df)

## housekeeping variables
	d$country <- "Malawi"
# Malawi's 3 regions are not in the source; both districts' region is well documented
	region <- c(Salima="Central", Mulanje="Southern")
	d$adm1 <- region[d$district]
	d$adm2 <- carobiner::fix_name(d$district, "title")
	d$location <- carobiner::fix_name(d$epa, "title")

# no GPS in the source; district-level GADM centroids used instead (see ISSUES 8)
	g <- carobiner::adm_pointRadius("Malawi", adm=1, cache_path=file.path(path, "data", "gadm"))
	g <- g[g$adm1 %in% c("Salima", "Mulanje"), c("adm1", "longitude", "latitude", "geo_uncertainty", "geo_source")]
	d <- merge(d, g, by.x="adm2", by.y="adm1", all.x=TRUE)
	d$geo_from_source <- FALSE

# one trial = one farmer's field in one season, hosting all 8 treatments
	d$trial_id <- paste(d$farmer_id, d$trial_year)
	d$plot_id <- paste0(d$trial_id, "_", d$plot_no)
# each farmer hosted a single instance of the 8 treatments (farmer = block/replicate)
	d$rep <- 1L

	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- NA          # not stated in source, see ISSUES 5

# Malawi has a single rainy/wet season (Nov-Apr) in which these crops are grown
	d$season <- "wet"
	d$planting_date <- substr(d$trial_year, 1, 4)  # year only, see ISSUES 5
	d$harvest_date <- as.character(NA)

	d$N_fertilizer <- as.numeric(NA)
	d$P_fertilizer <- as.numeric(NA)
	d$K_fertilizer <- as.numeric(NA)

	d$yield_moisture <- as.numeric(NA)

	d$record_id <- 1:nrow(d)

# drop intermediate columns now fully captured (losslessly) by trial_id/plot_id/adm2/location
	d$trial_year <- d$district <- d$epa <- d$farmer_id <- d$plot_no <- NULL

	carobiner::write_files(path, meta, d)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# carob_script(path=".")
