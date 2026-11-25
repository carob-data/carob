# R script for "carob"
# license: GPL (>=3)

## NOTES
# Open sheets donini_Ngong (108 rows, 18 farmer fields x 2 blocks x 3 treatments) and
# donini_gashiga (102 rows, 17 farmer fields x 2 blocks x 3 treatments) from
# "operations culturales ensemble parcelles.xlsx".
# Associated publication (Guibert et al. 2016, Cahiers Agricultures 25(6):65006,
# doi:10.1051/cagri/2016048) and the three protocol .doc files provide:
#   - Design: RCBD ("blocs de Fischer") with 2 blocks per farmer field, 3 treatments:
#     NP (farmer practice), NI1 (extension recommendations), NI2 (high intensification).
#   - Elementary plot: 128 m2 (20 m x 8 rows x 0.80 m); harvested area: 64 m2 (4 central rows).
#   - Variety: improved variety CMS 2019 in NI1 and NI2; farmer varieties in NP.
#   - Spacing: 80 cm x 25 cm in NI1 and NI2 (NP spacing chosen by farmer).
#   - Site coordinates (Guibert et al. 2016): Ngong (villages Normandi/Normandie & Kawtal)
#     at 09d03'N, 13d27'E (9.05, 13.45); Gashiga (village Ngabala) at 09d34'N, 13d21'E
#     (9.5667, 13.35). Soils are ferruginous soils ("sols ferrugineux").
#   - Complex fertilizer: NPKSB (22-10-15-5-2, i.e., 22% N, 10% P2O5, 15% K2O, 5% S, 2% B)
#     plus urea (46% N). NI1 also received 2 kg/ha ZnSO4 and NI2 received 6 kg/ha ZnSO4,
#     4 t/ha farmyard manure, and Standak Top (fipronil) seed treatment.
#   - Harvested ears and stover ("fanes") were dried before weighing. BIOM is dry stover
#     (dmy_residue), in t/ha in Ngong and kg/ha in Gashiga, verified to machine precision
#     via PCGBI = 100 * RDGRA / (dmy_residue + RDEPI).
#   - NBEPI (cob_density) is cobs/ha in Ngong but raw cob counts per 64 m2 harvested plot
#     in Gashiga (61-270); Gashiga NBEPI is converted to cobs/ha via * 10000 / 64.
#   - Plant height was measured at 20 DAP (HAUT1) and 40 DAP (HAUT2) per protocol and is
#     stored in the long table together with repeated CEB (1-10) weed infestation scores.
#   - Daily weather from Garoua synoptic station (IRAD Bocklé/Sanguéré, aggregated from
#     hourly observations) and daily rainfall from Bibol (Ngong) are passed via wth.

## ISSUES
# - Soil analysis workbooks ("analyses sols essai intensification maïs 2013 Nord Cameroun.xlsx"
#   and "saisie_ph_sols_parcelles_securite_alim.xlsx") are restricted on Dataverse and unavailable.
# - In donini_gashiga, village is spelled "Ngalaba" on all 34 NP rows and "Ngabala" on all
#   68 NI1/NI2 rows of the same 17 fields; standardized to "Ngabala" (as in Guibert et al. 2016).
# - In donini_Ngong, field N°parcelle 40 (degraded soil, Classe fertilité étude = 1) is coded
#   "NgD20" on NP rows and "NgF20" on NI1/NI2 rows; standardized to "NgD20".
# - In donini_gashiga, 6 NP rows (GaF8, GaD13, GaD16, blocks 1-2) have blank Dose urée,
#   no urea application date, and Dose totale engrais == Dose NPK; treated as 0 kg/ha urea.
# - Zinc sulfate hydration state is not specified in the protocol/paper; elemental Zn (40.5%)
#   and S (19.9%) are calculated using terminag's ZnSO4 table values (if heptahydrate 22.7% Zn
#   or monohydrate 36% Zn was used, Zn_fertilizer and S_fertilizer would be lower).
# - Two Ngong NP fields failed before harvest (NgD16 blocks 1-2: zero emergence, all traits 0;
#   NgF17 blocks 1-2: emerged but 0 harvested plants and 0 yield). Zero values are kept as
#   recorded, causing expected out-of-bounds messages on plant_height (0), plant_density (0),
#   and cob_density (0). Shelling percentage (RDEGR = 0 when RDEPI = 0) is set to NA on those
#   4 rows because 0/0 is undefined; one Gashiga row has shelling_percentage = 90.54 (> 90).
# - Grain yield is dry weight after drying ears, but exact moisture % is not reported, so
#   yield_moisture is omitted (triggers expected "missing variables: yield_moisture" message).
# - In donini_Ngong, 4 failed NP plots have harvest_date = NA; 6 NP plots with OM_used = TRUE
#   have no recorded OM rate (OM_amount = NA).
# - Weather station coordinates for Garoua IRAD Bocklé (9.3014, 13.4136) and Bibol (9.1541,
#   13.4061) are estimated from gazetteer/literature (geo_from_source = FALSE).

## NOT PROCESSED
# - analyses sols essai intensification maïs 2013 Nord Cameroun.xlsx: restricted on Dataverse.
# - saisie_ph_sols_parcelles_securite_alim.xlsx: restricted on Dataverse.
# - pluviometrie_cameroun_2013_gashiga_djalingo.xls, sheet "Gashiga 2013": only decadal
#   (10-day) rainfall totals are available (800 mm annual total), not daily records.
# - operations culturales ensemble parcelles.xlsx (donini_Ngong & donini_gashiga):
#   "Délais semis" (days after 2013-06-15, derivable from planting_date),
#   "Date apport organique" / "Délais apport organique" (OM application date/DAP),
#   "Date buttage" / "Délai buttage" (hilling date/DAP),
#   "Délais premier désherbage" (derivable from weeding_dates - planting_date),
#   "Dose totale engrais" (redundant total with +50 kg/ha in NI2 and +2..+4 in some Gashiga NP rows).

## NEW VARIABLES
# - ear_yield_: RDEPI, dry maize ear yield (kg/ha).
# - emerged_density_: DENL, maize plant density after emergence/thinning (plants/ha).
# - lodged_density_: NPVER, density of lodged/broken maize plants at maturity (plants/ha).
# - striga_density_: NPSTR, density of maize plants attacked by Striga spp. (plants/ha).
# - soil_fertility_class_: Classe fertilité étude, study soil fertility stratum ("degraded", "fertile").
# - years_since_clearing_: Age défriche, years since the field was cleared for cultivation (years).
# - replanted_: Resemis ?, logical indicating whether gap-filling/replanting was performed.


carob_script <- function(path) {

"
Essai intensification du maïs au Nord Cameroun (2013)

Essais mis en place chez des producteurs dans deux grappes de villages du Nord Cameroun en 2013 dans l'objectif d'étudier la faisabilité de l'intensification de la culture du maïs. Des résultats d'analyses de sols sur chaque essai sont parvenus après ces valorisations qui peuvent donner lieu à d'autres interprétations
"

	uri <- "doi:10.18167/DVN1/TWZLIS"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIRAD",
		publication = "doi:10.1051/cagri/2016048",
		project = NA,
		design = "RCBD",
		data_type = "on-farm experiment",
		treatment_vars = "N_fertilizer;P_fertilizer;K_fertilizer;S_fertilizer;B_fertilizer;Zn_fertilizer;OM_used;land_prep_method;herbicide_used;seed_treatment",
		response_vars = "yield;dmy_residue;dmy_total;plant_height;cob_density;plant_density;harvest_index;shelling_percentage",
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 85,
		carob_effort = 0.5
	)

	f <- ff[basename(ff) == "operations culturales ensemble parcelles.xlsx"]
	r1 <- carobiner::read.excel(f, sheet = "donini_Ngong", na = c("", "NA", "Néant"))
	r2 <- carobiner::read.excel(f, sheet = "donini_gashiga", na = c("", "NA", "Néant"))

	as_day <- function(x) {
		if (inherits(x, "POSIXt")) return(format(x, "%Y-%m-%d"))
		if (inherits(x, "Date")) return(as.character(x))
		if (is.numeric(x)) return(as.character(as.Date(x, origin = "1899-12-30")))
		as.character(x)
	}

	# Align column differences between Ngong and Gashiga sheets before combining:
	# - In Gashiga, "Délais épandage urée" holds Excel dates and "Délais  urée" holds DAP.
	# - Ngong has "Cotation enherbement 15 JAL" & "COT2"; Gashiga has "COT2" & "COT3".
	# - Ngong BIOM is in t/ha; Gashiga BIOM is in kg/ha.
	# - Ngong NBEPI is in cobs/ha; Gashiga NBEPI is raw cobs per 64 m2 harvested subplot.
	norm_sheet <- function(x, cluster) {
		is_ga <- cluster == "Gashiga"
		field_id <- trimws(as.character(x[["code parcelle"]]))
		# Field 40 in Ngong is degraded (Classe fertilité étude = 1), coded NgD20 on NP and NgF20 on NI1/NI2
		field_id[field_id == "NgF20"] <- "NgD20"
		village <- trimws(as.character(x$village))
		# Typo on all 34 Gashiga NP rows ("Ngalaba" for "Ngabala", cf. Guibert et al. 2016)
		village[village == "Ngalaba"] <- "Ngabala"

		urea_dap1 <- if (is_ga) as.numeric(x[["Délais  urée"]]) else as.numeric(x[["Délais épandage urée"]])
		biom_kg <- as.numeric(x$BIOM) * (if (is_ga) 1 else 1000)
		cob_ha <- as.numeric(x$NBEPI) * (if (is_ga) 10000 / 64 else 1)

		data.frame(
			cluster = cluster,
			field_id = field_id,
			village = village,
			block = as.integer(x$BLOCS),
			modalite = trimws(as.character(x[["Modalités"]])),
			labour = x[["labour ?"]] == "oui",
			buttage = x[["Buttage ?"]] == "oui",
			planting_date = as_day(x[["Date semis"]]),
			harvest_date = as_day(x[["Date récolte"]]),
			harvest_days = as.numeric(x$cycle),
			herbicide_used = x[["Herbicidage ?"]] == "oui",
			herbicide_dap = as.numeric(x[["Délais herbicidage"]]),
			npk_dap = as.numeric(x[["Délais application NPK"]]),
			urea_dap1 = urea_dap1,
			urea_dap2 = as.numeric(x[["Délais urée 2"]]),
			urea_dap3 = as.numeric(x[["Délais urée 3"]]),
			seed_treated = x[["Traitement des semences ?"]] == "oui",
			replanted = x[["Resemis ?"]] == "oui",
			weeding_times = as.integer(x[["Nombre herbicidage sarclages et buttage"]]),
			first_weed_date = as_day(x[["Date premier désherbage (mécanique ou chimique)"]]),
			OM_used = x[["Apports organiques"]] == "oui",
			npk_kg = as.numeric(x[["Dose NPK"]]),
			urea_kg = as.numeric(x[["Dose urée"]]),
			tot_fert_raw = as.numeric(x[["Dose totale engrais"]]),
			fert_farmer = as.integer(x[["Jugement fertilité producteur"]]),
			fert_study = as.integer(x[["Classe fertilité étude"]]),
			age_clear = as.numeric(x[["Age défriche"]]),
			cot1 = if (is_ga) NA_integer_ else as.integer(x[["Cotation enherbement 15 JAL"]]),
			cot2 = as.integer(x$COT2),
			cot3 = if (is_ga) as.integer(x$COT3) else NA_integer_,
			lodged_ha = as.numeric(x$NPVER),
			haut1 = as.numeric(x$HAUT1),
			haut2 = as.numeric(x$HAUT2),
			denl = as.numeric(x$DENL),
			denr = as.numeric(x$DENR),
			striga_ha = as.numeric(x$NPSTR),
			cob_ha = cob_ha,
			dmy_residue = biom_kg,
			ear_yield = as.numeric(x$RDEPI),
			yield = as.numeric(x$RDGRA),
			shelling = as.numeric(x$RDEGR),
			harvest_index = as.numeric(x$PCGBI),
			stringsAsFactors = FALSE
		)
	}

	r <- rbind(norm_sheet(r1, "Ngong"), norm_sheet(r2, "Gashiga"))

	# In Gashiga NP, 6 rows have blank Dose urée, no urea date/DAP, and Dose totale engrais == Dose NPK -> 0 urea
	urea <- r$urea_kg
	urea[is.na(urea) & is.na(r$urea_dap1) & !is.na(r$tot_fert_raw) & r$tot_fert_raw == r$npk_kg] <- 0
	npk <- r$npk_kg

	# Protocol & Guibert et al. (2016): NPKSB (22-10-15-5-2), urea (46% N), and ZnSO4 (2 kg/ha NI1, 6 kg/ha NI2)
	znso4 <- ifelse(r$modalite == "NI1", 2, ifelse(r$modalite == "NI2", 6, 0))

	ftype <- mapply(function(n_p, u_p, z_p) {
		prods <- c(if (!is.na(n_p) && n_p > 0) "NPK",
			if (!is.na(u_p) && u_p > 0) "urea",
			if (!is.na(z_p) && z_p > 0) "ZnSO4")
		if (length(prods) == 0) "none" else paste(prods, collapse = ";")
	}, npk, urea, znso4, USE.NAMES = FALSE)

	# Combine inorganic fertilizer application DAPs (NPK + urea splits)
	fert_dap <- apply(r[, c("npk_dap", "urea_dap1", "urea_dap2", "urea_dap3")], 1, function(v) {
		v <- v[!is.na(v)]
		if (length(v) == 0) NA_character_ else paste(as.integer(v), collapse = ";")
	})

	n_splits <- as.integer(
		(!is.na(npk) & npk > 0) +
		(!is.na(urea) & urea > 0) +
		!is.na(r$urea_dap2) +
		!is.na(r$urea_dap3)
	)

	# Land preparation + post-emergence hilling (buttage)
	lprep <- ifelse(r$labour & r$buttage, "ploughing; hilling",
		ifelse(r$labour, "ploughing",
		ifelse(r$buttage, "hilling", "none")))

	# Seed treatment: Standak Top (fipronil 250 g/L) in NI2 per protocol & paper; unknown product where NP treated
	streat <- ifelse(!r$seed_treated, "none",
		ifelse(r$modalite == "NI2", "fipronil", "unknown"))

	# Coordinates from Guibert et al. (2016): Ngong 09d03'N 13d27'E; Gashiga 09d34'N 13d21'E
	is_ng <- r$cluster == "Ngong"

	d <- data.frame(
		record_id = seq_len(nrow(r)),
		trial_id = r$field_id,
		plot_id = paste(r$field_id, r$block, r$modalite, sep = "_"),
		block_id = as.character(r$block),
		rep = r$block,
		country = "Cameroon",
		adm1 = "Nord",
		adm2 = "Bénoué",
		adm3 = ifelse(is_ng, "Tchéboa", "Demsa"),
		location = r$cluster,
		site = r$village,
		longitude = ifelse(is_ng, 13.45, 13.35),
		latitude = ifelse(is_ng, 9.05, 9.5667),
		geo_from_source = TRUE,
		on_farm = TRUE,
		is_survey = FALSE,
		irrigated = FALSE,
		soil_type = "ferruginous",
		soil_quality = c("poor", "medium", "good")[r$fert_farmer],
		soil_fertility_class_ = c("degraded", "fertile")[r$fert_study],
		years_since_clearing_ = r$age_clear,
		crop = "maize",
		variety = ifelse(r$modalite %in% c("NI1", "NI2"), "CMS 2019", NA_character_),
		treatment = r$modalite,
		plot_area = 128,
		row_spacing = ifelse(r$modalite %in% c("NI1", "NI2"), 80, NA_real_),
		plant_spacing = ifelse(r$modalite %in% c("NI1", "NI2"), 25, NA_real_),
		planting_date = r$planting_date,
		harvest_date = r$harvest_date,
		harvest_days = r$harvest_days,
		land_prep_method = lprep,
		seed_treatment = streat,
		replanted_ = r$replanted,
		herbicide_used = r$herbicide_used,
		herbicide_timing = ifelse(is.na(r$herbicide_dap), NA_character_, as.character(as.integer(r$herbicide_dap))),
		weeding_done = r$weeding_times > 0,
		weeding_times = r$weeding_times,
		# Date of first weed-control operation (mechanical or chemical)
		weeding_dates = r$first_weed_date,
		fertilizer_used = (npk > 0) | (urea > 0) | (znso4 > 0),
		fertilizer_type = ftype,
		fertilizer_amount = npk + urea + znso4,
		fertilizer_dap = fert_dap,
		N_splits = n_splits,
		N_fertilizer = round(npk * 0.22 + urea * 0.46, 2),
		P_fertilizer = round(npk * 0.10 / 2.29, 2),
		K_fertilizer = round(npk * 0.15 / 1.2051, 2),
		# Elemental S from NPKSB (5% S) and ZnSO4 (19.9% S per terminag ZnSO4 table)
		S_fertilizer = round(npk * 0.05 + znso4 * 0.199, 2),
		B_fertilizer = round(npk * 0.02, 2),
		# Elemental Zn from ZnSO4 (40.5% Zn per terminag ZnSO4 table)
		Zn_fertilizer = round(znso4 * 0.405, 2),
		OM_used = r$OM_used,
		OM_type = ifelse(r$OM_used, "farmyard manure", "none"),
		# Protocol: 4 t/ha manure in NI2; 0 where none applied; rate unknown for 6 NP rows with OM
		OM_amount = ifelse(r$modalite == "NI2" & r$OM_used, 4000, ifelse(!r$OM_used, 0, NA_real_)),
		emerged_density_ = r$denl,
		plant_density = r$denr,
		cob_density = r$cob_ha,
		lodged_density_ = r$lodged_ha,
		striga_infected = r$striga_ha > 0,
		striga_density_ = r$striga_ha,
		dmy_residue = r$dmy_residue,
		ear_yield_ = r$ear_yield,
		dmy_total = r$dmy_residue + r$ear_yield,
		yield = r$yield,
		yield_part = "grain",
		yield_isfresh = FALSE,
		# Shelling percentage is undefined (0/0) on the 4 failed plots where ear_yield == 0
		shelling_percentage = ifelse(r$ear_yield == 0, NA_real_, round(r$shelling, 2)),
		harvest_index = round(r$harvest_index, 2),
		stringsAsFactors = FALSE
	)

	# Long table: repeated plant height (20 and 40 DAP per protocol) and CEB (1-10) weed scores
	dl_h1 <- data.frame(
		record_id = d$record_id,
		DAP = 20L,
		order = 1L,
		timing = "20 DAP",
		plant_height = r$haut1,
		weed_severity = NA_character_,
		severity_scale = NA_character_,
		stringsAsFactors = FALSE
	)
	dl_h2 <- data.frame(
		record_id = d$record_id,
		DAP = 40L,
		order = 2L,
		timing = "40 DAP",
		plant_height = r$haut2,
		weed_severity = NA_character_,
		severity_scale = NA_character_,
		stringsAsFactors = FALSE
	)
	dl_w1 <- data.frame(
		record_id = d$record_id[!is.na(r$cot1)],
		DAP = NA_integer_,
		order = 1L,
		timing = "15 DAE",
		plant_height = NA_real_,
		weed_severity = as.character(r$cot1[!is.na(r$cot1)]),
		severity_scale = "1-10",
		stringsAsFactors = FALSE
	)
	dl_w2 <- data.frame(
		record_id = d$record_id[!is.na(r$cot2)],
		DAP = NA_integer_,
		order = 2L,
		timing = "weed assessment 2",
		plant_height = NA_real_,
		weed_severity = as.character(r$cot2[!is.na(r$cot2)]),
		severity_scale = "1-10",
		stringsAsFactors = FALSE
	)
	dl_w3 <- data.frame(
		record_id = d$record_id[!is.na(r$cot3)],
		DAP = NA_integer_,
		order = 3L,
		timing = "weed assessment 3",
		plant_height = NA_real_,
		weed_severity = as.character(r$cot3[!is.na(r$cot3)]),
		severity_scale = "1-10",
		stringsAsFactors = FALSE
	)
	dlong <- rbind(dl_h1, dl_h2, dl_w1, dl_w2, dl_w3)

	# Daily weather table:
	# 1) Hourly synoptic data from Station IRAD de Bocklé (Sanguéré), Garoua -> aggregated to daily
	f_met <- ff[basename(ff) == "météo_station_synoptique_Garoua_2013.xlsx"]
	rmet <- carobiner::read.excel(f_met, sheet = 1, skip = 5, col_names = FALSE)
	names(rmet) <- c("datetime", "irad", "prec", "wspd", "wspdmx", "temp", "tmin", "tmax", "rhum", "dewp", "dewp_min")
	rmet$date <- format(rmet$datetime, "%Y-%m-%d")

	safe_mean <- function(x) if (all(is.na(x))) NA_real_ else round(mean(x, na.rm = TRUE), 2)
	safe_min <- function(x) if (all(is.na(x))) NA_real_ else round(min(x, na.rm = TRUE), 2)
	safe_max <- function(x) if (all(is.na(x))) NA_real_ else round(max(x, na.rm = TRUE), 2)
	safe_sum <- function(x) if (all(is.na(x))) NA_real_ else round(sum(x, na.rm = TRUE), 2)

	u_dates <- sort(unique(rmet$date))
	# Coordinates of IRAD Garoua-Bocklé station estimated from literature/gazetteer
	w1 <- data.frame(
		station_name = "Station IRAD de Bocklé (Sanguéré), Garoua",
		country = "Cameroon",
		adm1 = "Nord",
		adm2 = "Bénoué",
		adm3 = NA_character_,
		location = "Garoua",
		site = "Bocklé",
		longitude = 13.4136,
		latitude = 9.3014,
		geo_from_source = FALSE,
		geo_uncertainty = 5000,
		date = u_dates,
		prec = as.numeric(tapply(rmet$prec, rmet$date, safe_sum)[u_dates]),
		temp = as.numeric(tapply(rmet$temp, rmet$date, safe_mean)[u_dates]),
		tmin = as.numeric(tapply(rmet$tmin, rmet$date, safe_min)[u_dates]),
		tmax = as.numeric(tapply(rmet$tmax, rmet$date, safe_max)[u_dates]),
		rhum = as.numeric(tapply(rmet$rhum, rmet$date, safe_mean)[u_dates]),
		dewp = as.numeric(tapply(rmet$dewp, rmet$date, safe_mean)[u_dates]),
		irad = as.numeric(tapply(rmet$irad, rmet$date, safe_mean)[u_dates]),
		wspd = as.numeric(tapply(rmet$wspd, rmet$date, safe_mean)[u_dates]),
		wspdmx = as.numeric(tapply(rmet$wspdmx, rmet$date, safe_max)[u_dates]),
		stringsAsFactors = FALSE
	)

	# 2) Daily rainfall from Ngong 2013 sheet (Village ou CPE: Bibol, total 691 mm)
	f_plu <- ff[basename(ff) == "pluviometrie_cameroun_2013_gashiga_djalingo.xls"]
	rplu <- carobiner::read.excel(f_plu, sheet = "Ngong 2013", skip = 6, col_names = FALSE)
	rplu <- rplu[as.character(rplu[[1]]) %in% as.character(1:31), 1:13]
	plu_list <- lapply(1:12, function(m) {
		dday <- as.integer(rplu[[1]])
		dt <- sprintf("2013-%02d-%02d", m, dday)
		ok <- !is.na(as.Date(dt, format = "%Y-%m-%d"))
		val <- as.numeric(rplu[[m + 1]][ok])
		val[is.na(val)] <- 0
		data.frame(date = dt[ok], prec = val, stringsAsFactors = FALSE)
	})
	p_ng <- do.call(rbind, plu_list)

	# Coordinates of Bibol (Ngong area) estimated from gazetteer
	w2 <- data.frame(
		station_name = "Bibol (Ngong)",
		country = "Cameroon",
		adm1 = "Nord",
		adm2 = "Bénoué",
		adm3 = "Tchéboa",
		location = "Ngong",
		site = "Bibol",
		longitude = 13.4061,
		latitude = 9.1541,
		geo_from_source = FALSE,
		geo_uncertainty = 5000,
		date = p_ng$date,
		prec = p_ng$prec,
		temp = NA_real_,
		tmin = NA_real_,
		tmax = NA_real_,
		rhum = NA_real_,
		dewp = NA_real_,
		irad = NA_real_,
		wspd = NA_real_,
		wspdmx = NA_real_,
		stringsAsFactors = FALSE
	)

	wth <- rbind(w1, w2)

	carobiner::write_files(path, meta, d, long = dlong, wth = wth)
}
