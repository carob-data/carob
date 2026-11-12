# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 1. Year: the title and the Dataverse time period say 2009 (and the file name is PTPV200908), but the
#    fieldbook has begin date 2013-03-20, harvest evaluation on 2013-09-29 and storage on 2013-10-02.
#    planting_date is the begin date (2013-03-20) as in the fieldbook. Which year is correct?
# 2. Baby trials (3 farmers, 1 plot per clone): TTWP gives up to 75 t/ha (10.6-74.8) with 25-75% of plants
#    harvested, much higher than the mother trial (2.5-29 t/ha). Kept as reported. MTWP has the same
#    values as NMTPL (marketable tubers per plant), so yield_marketable is NA for the baby trials.
#    The baby plots have the same size as the mother plots (2.8 m2, Installation sheet), so the high
#    yields are not due to a different plot size.
# 3. Texture in the organoleptic test is scored 3 (soggy), 6 (intermediate) or 9 (mealy), but
#    tuber_texture is defined on a 1-5 scale. Stored as reported (out of bounds warning).
# 4. The organoleptic tests were done with tubers of the mother trial and of the baby trial (sheets
#    "Data_organoleptic_Mother" and "Data_organoleptic_Baby"); the selection criteria (F1) are from the
#    mother trial (confirmed by the contributor). They are by genotype, not by plot, and have no date,
#    so they are in the long table linked by trial_id and variety_code (hence the plot_id and duplicate
#    variety_code warnings).

## NOTES
# 1. Mother & Baby participatory variety selection (PVS) trial in Bumthang, Bhutan. Mother trial: RCBD
#    with 4 replications, 6 genotypes (5 clones of population B3C1 and Desiree, CIP800048). Baby trials:
#    3 farmers (Baby 1-3), each with 1 plot per genotype; each farmer is one replication of the baby
#    trial (REP 1-3). trial_type_ is "mother" or "baby"; the long records are linked to them by
#    trial_id. All plots of 20 plants (2.8 m2; 0.7 m between rows, 0.2 m between
#    plants).
# 2. The fieldbook (Minimal sheet) has coordinates and elevation but no country; country and adm1 are
#    from the Dataverse geographic coverage and the site name (BUMTHANG). The baby trials have the same
#    coordinates (farm locations not given).
# 3. yield is TTWP (kg/plot) * 10000 / 2.8 m2, not adjusted for missing plants. plant_density is the
#    number of plants harvested (NPH) per ha; many plants were not harvested (PPH 20-85%).
# 4. Fertilizer, irrigation and harvest date are not reported (the Crop_management sheet is empty
#    except for the participatory evaluations); irrigated is set to NA.
# 5. Parents from 09_Material_list.xlsx; the male parent of Desiree (DEPESCHE) is from the "Material
#    List" sheet of the fieldbook.
# 6. Long table, per plot and sex (mother plots first, then baby plots): farmer votes (pvs_votes_) at
#    flowering, at harvest and after storage (pvs_phase_), by men (sex = "male", corn kernels) and
#    women (sex = "female", beans). The same format (sex, pvs_votes_) is used for the selection
#    criteria (NOTES 9); the global score (MSGLO, BSGLO, SGLOB) is the sum. MSM/MSWM are for the mother plots
#    and BSM/BSWM for the baby plots (reps 1-3 = baby farmers); storage votes (F9, 18 plots) are for
#    the baby plots.
# 7. Long table, per panelist: organoleptic test of boiled tubers (5 genotypes, CIP393085.5 not
#    tested), 10 panelists (5 men, 5 women). Appearance and taste: 1 (poor), 3 (fair), 5 (excellent).
#    In the mother sheet, PanelNo is always 1; the panelist number is from the order of the records
#    (5 genotypes per panelist).
# 8. Not processed: percentages of votes (can be computed from the votes), adjusted
#    yields (TTYA, MTYA; plants planted and harvested are recorded instead), counts and weights per
#    plant and non-marketable tubers (can be computed), empty columns (plant vigor, growth habit, flowering,
#    senescence, stolons, tuber appearance, uniformity and size) and the summary and PCA sheets.
# 9. Long table, per trial: selection criteria identified by the farmers at flowering, harvest and
#    storage (rank_variable), with the order of importance (rank_score; OR_M, OR_WM) and the number of
#    votes (pvs_votes_; SCMEN, SCWM), one record per sex (F1_Selec_Crit). The global score (SGLOB) is
#    the sum of both. Not linked to genotypes or plots.
# 10. Baby plots: number of marketable tubers per ha (tuber_density_marketable_, from NMTP) and the
#    initial weight of the tubers used for the dormancy test (dormancy_tuber_weight_, kg, F8_Dormancy;
#    the sprouting evaluations are empty). Storage votes include the storage type (storage_type_).

carob_script <- function(path) {

"
Dataset for: Evaluation of Potato Clones Through Farmer Selection in Buthhang

A select group of 8 potato clones will be evaluated through a Farmer Participatory Variety Selection (PVS) in Buthhang during the 2009 campaign. Yield components and post-harvest traits will be determined
"

	uri <- "doi:10.21223/M97ZQS"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = "RCBD",
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;yield_marketable;tuber_flavor;tuber_texture;pvs_votes_;rank_score",
		notes = NA,
		carob_contributor = "Vilma Hualla",
		carob_LLM = "Claude Opus 5.5",
		carob_date = "2026-11-11",
		carob_completion = 90,
		carob_effort = 2
	)

	f <- ff[basename(ff) == "PTPV200908_BUMTHANG.xls"]
	trial <- "PTPV200908_BUMTHANG"
	padj <- 10000 / 2.8  # plot size 2.8 m2

	## mother trial
	process <- carobiner::get_function("process_cip_lbvars", path, group)
	d1 <- process(f)
	d1$on_farm <- NA
	d1$trial_type_ <- "mother"
	if (is.null(d1$variety_code)) d1$variety_code <- d1$variety
	# plants harvested per ha (NPH), the same for mother and baby plots
	r1 <- carobiner::read.excel(f, sheet="F4_ Harvest_Mother")
	d1$plant_density <- as.numeric(r1$NPH)[match(d1$plot_id, as.character(r1$PLOT))] * padj
	d1$yield_adjusted <- NULL

	## baby trials (ISSUES 2)
	r2 <- carobiner::read.excel(f, sheet="F5_harvest_Baby")
	d2 <- data.frame(
		plot_id = as.character(r2$PLOT),
		# each baby (farmer) is one replication of the baby trial
		rep = as.integer(r2$REP),
		variety_code = r2$INSTN,
		trial_id = paste0(trial, "_baby"),
		yield = as.numeric(r2$TTWP) * padj,
		yield_marketable = NA,
		plant_density = as.numeric(r2$NPH) * padj,
		tuber_density = as.numeric(r2$TNTP) * padj,
		tuber_density_marketable_ = as.numeric(r2$NMTP) * padj,
		on_farm = TRUE,
		trial_type_ = "baby"
	)
	# initial weight (kg) of the tubers of each baby plot used for the dormancy test (NOTES 10)
	r8 <- carobiner::read.excel(f, sheet="F8_Dormancy")
	d2$dormancy_tuber_weight_ <- as.numeric(r8$ITW)[match(d2$plot_id, as.character(r8$PLOT))]
	# variables that are the same for all plots
	same <- setdiff(names(d1), c(names(d2), "variety", "female_parent", "male_parent",
		"variety_pedigree", "block_id", "yield_moisture"))
	d2 <- data.frame(d2, d1[rep(1, nrow(d2)), same], row.names=NULL)
	d <- carobiner::bindr(d1, d2)

	## variety names and parents from the material list (NOTES 5)
	m <- carobiner::read.excel(ff[basename(ff) == "09_Material_list.xlsx"])
	i <- match(d$variety_code, m[["Institutional number"]])
	nm <- m[["Clone or variety name"]][i]
	d$variety <- ifelse(is.na(nm), d$variety_code, nm)
	fcode <- m[["Female code"]][i]
	d$female_parent <- ifelse(is.na(fcode), m[["Female Institutional number"]][i], fcode)
	d$male_parent <- m[["Male Institutional number"]][i]
	d$male_parent[d$variety_code == "CIP800048"] <- "DEPESCHE"
	d$variety_pedigree <- paste(d$female_parent, d$male_parent, sep=" x ")

	d$country <- "Bhutan"
	d$adm1 <- "Bumthang"
	d$irrigated <- NA

	## long table 1: farmer votes per plot and sex (NOTES 6); mother plots first, then baby plots
	# SM = score men (corn kernels), SWM = score women (beans); the global score is their sum
	votes <- function(r, sm, swm, mother, phase) {
		data.frame(
			trial_id = if (mother) trial else paste0(trial, "_baby"),
			plot_id = rep(as.character(r$PLOT), 2),
			pvs_phase_ = phase,
			sex = rep(c("male", "female"), each=nrow(r)),
			pvs_votes_ = as.numeric(c(r[[sm]], r[[swm]]))
		)
	}
	fl <- carobiner::read.excel(f, sheet="F2 Clones_Select_Flowering")
	hv <- carobiner::read.excel(f, sheet="F3_Clones_Selec_Harvest")
	st <- carobiner::read.excel(f, sheet="F9_Clones_Select_Storage")
	b <- function(r) r[r$REP %in% 1:3, ]
	lv <- carobiner::bindr(
		# mother
		votes(fl, "MSM", "MSWM", TRUE, "flowering"),
		votes(hv, "MSM", "MSWM", TRUE, "harvest"),
		# baby
		votes(b(fl), "BSM", "BSWM", FALSE, "flowering"),
		votes(b(hv), "BSM", "BSWM", FALSE, "harvest"),
		votes(st, "SCMEN", "SCWM", FALSE, "storage")
	)
	# storage type of the storage selection (F9), e.g. "rustic"
	lv$storage_type_ <- NA
	i <- lv$pvs_phase_ == "storage"
	lv$storage_type_[i] <- st$STYPE[match(lv$plot_id[i], as.character(st$PLOT))]

	## long table 2: organoleptic test per panelist (NOTES 7, ISSUES 3, 4)
	org <- function(sheet, trial_id) {
		r <- carobiner::read.excel(f, sheet=sheet)
		data.frame(
			trial_id = trial_id,
			variety_code = r$INSTN,
			panelist_ = rep(1:10, each=5),
			sex = r$Sex,
			tuber_appearance_ = as.numeric(r$appearance),
			tuber_flavor = as.numeric(r$taste),
			tuber_texture = as.numeric(r$texture)
		)
	}
	lo <- carobiner::bindr(
		org("Data_organoleptic_Mother", trial),
		org("Data_organoleptic_Baby", paste0(trial, "_baby"))
	)

	## long table 3: selection criteria identified by the farmers, per phase (NOTES 9)
	cr <- carobiner::read.excel(f, sheet="F1_Selec_Crit")
	lc <- data.frame(
		trial_id = trial,
		pvs_phase_ = tolower(cr$PHASE),
		rank_variable = tolower(cr[["Identified criteria"]]),
		sex = rep(c("male", "female"), each=nrow(cr)),
		rank_score = as.integer(c(cr$OR_M, cr$OR_WM)),
		pvs_votes_ = as.numeric(c(cr$SCMEN, cr$SCWM))
	)

	long <- carobiner::bindr(lv, lo, lc)

	carobiner::write_files(path, meta, d, long=long)
}
