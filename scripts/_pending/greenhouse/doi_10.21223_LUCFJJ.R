# R script for "carob"

## ISSUES
# 1. Raw file is named "3rd_exp_data" ("3rd experiment") but the linked publication
#    (Makokha et al. 2020, Journal of Crop Improvement) only describes a single greenhouse
#    trial with this exact design (4 varieties x 2 systems x 4 blocks x 5 reps = 160 pots,
#    RCB split-plot, KEPHIS-PQBS Muguga, Jun 2018-Mar 2019); the row count (160) and design
#    match the paper exactly, so this is treated as the data underlying that publication
#    -- evidence: 160 rows, 40 pots/variety, 80 pots/system, matching the paper's stated
#    "five replications across four blocks" per variety x system combination.
# 2. No true biomass "yield" was measured in this trial: the response variables are counts
#    of vine nodes produced per pot (planting-material multiplication) and production cost
#    per node, not a harvested crop weight. yield/yield_moisture/yield_part are left NA/none
#    because a fresh/dry weight yield genuinely does not exist for this dataset.
# 3. This is a screenhouse pot-propagation trial (pre-basic seed multiplication), not a field
#    trial: plot_area is the real pot footprint (18 cm diameter pot -> 0.0254 m2), which is
#    far below terminag's valid_min (1 m2) for plot_area, and the resulting plant_density /
#    node_density (scaled to a hypothetical hectare of identical pots) are far above the
#    normal field valid_max. These out-of-bounds warnings are left visible intentionally:
#    the pot size is real (from the paper), and the per-ha scaling is only kept so the raw
#    per-pot node count remains recoverable (per repo convention), not because this
#    technology is ever deployed at field/hectare scale.
# 4. N/P/K_fertilizer are left NA: the conventional substrate was a 5:2:1 (topsoil:cow
#    manure:gravel) mix and the sandponics substrate was fertigated with a nutrient solution,
#    but the paper does not report quantified nutrient application rates for either.
# 5. Raw production costs (cost_per_pot, cost_per_node) are in Kenya Shillings (KES), not
#    US$: the paper states "1 US$ equivalent to 100 KSH" and its abstract's US$ figures are
#    a currency conversion of the underlying KES data -- confirmed since cost_per_node/100
#    (e.g. 3.513 KSH/100 = $0.035) matches the paper's reported per-system average cost per
#    node. The raw cost_per_pot column (rounded to a whole KES value, constant per system)
#    is dropped as redundant with cost_per_node_ (a more precise, already-computed value);
#    it can be approximately recovered as cost_per_node_ * node count.
# 6. Exact planting/harvest days are not reported, only the months (Methods: "conducted
#    between June 2018 and March 2019"); planting_date/harvest_date are therefore recorded
#    at year-month precision rather than a fabricated exact date.
# 7. "University of Eldoret" (co-author affiliation, Matasyoh & Kiplagat) triggers an
#    "invalid terms" warning for data_organization: it is a real author institution but is
#    not (yet) in terminag's values_organization.csv (which has no Kenyan-university entry
#    at all, only CIP and KALRO). Left in rather than dropped, since it is genuine
#    information about who produced the data.

carob_script <- function(path) {

"'Sandponics' could reduce the cost of producing sweetpotato planting material and improve yields. In Sub-Saharan Africa (SSA), conventionally, sweetpotato (Ipomoea batatas (L.) Lam) pre-basic seed is multiplied in screenhouses using sterilized soil substrate. This makes pre-basic seed expensive for basic seed multipliers and thereby a challenge in delivering an economically viable option for further multiplication for root producers to access low cost quality planting materials. Sandponics is postulated to be an alternative to the conventional sweetpotato pre-basic seed multiplication, but its cost-effectiveness is unknown compared with the conventional approach. A study was conducted at Kenya Plant Health Inspectorate Service - Plant Quarantine and Biosecurity Station, Muguga, Kenya to determine the cost-effectiveness for sweetpotato pre-basic seed generation in the two distinct vine propagation systems using genotypes Irene, Kabode, Ejumula and Gweri. Results showed a significant increase in the vine multiplication rate in the sandponics by 21.8% compared with the conventional vine propagation. The cost of producing one sweetpotato node in sandponics was significantly lower by 0.009 US$ compared with conventional system. However, the cost-effectiveness of producing vines in sandponics varied among the genotypes with Ejumula being the most cost-effective and Gweri the least cost-effective. Therefore, sandponics technology can be more cost-effective for specific genotypes."

	uri <- "doi:10.21223/LUCFJJ"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIP; KALRO; University of Eldoret",
		publication = "doi:10.1080/15427528.2019.1674758",
		project = NA,
		data_type = "experiment",
		design = "RCB split-plot (propagation system = whole-plot factor, variety = sub-plot factor); 4 blocks x 5 reps",
		treatment_vars = "propagation_system_;variety",
		response_vars = "node_density_;cost_per_node_",
		carob_contributor = "AI agronomy writer agent",
		carob_date = "2026-07-24",
		carob_completion = 90,
		carob_effort = 0.6
	)

## the main data file already has readable variety names and a numeric system code;
## the companion "cumulative data" file (not read here) carries the same records plus
## an appended legend confirming system 1 = sandponic, 2 = soil (conventional)
	f <- ff[basename(ff) == "3rd_exp_data.dta"]
	r <- as.data.frame(haven::read_dta(f))

## pot footprint, from the paper's Methods: "Plastic pots measuring 18 cm diameter ..."
## used here as plot_area (m2) -- see ISSUES 3 for why this is far below the usual
## field-plot range
	pot_area <- pi * (0.18 / 2)^2   # m2

## growth-habit description of each variety, from the paper's Methods
## (a lookup table, indexed by match(), rather than a nested ifelse())
	habit <- data.frame(
		variety = c("Irene", "Kabode", "Ejumula", "Gweri"),
		variety_traits = c("erect growth habit", "semi-erect growth habit", "spreading growth habit", "extremely spreading growth habit")
	)

	d <- data.frame(
		record_id = as.integer(r$pot_no),
		plot_id = as.character(r$pot_no),
		block_id = as.character(r$block),
		rep = as.integer(r$rep),
		country = "Kenya",
		adm1 = "Kiambu",
		location = "Muguga",
		elevation = 1950,                      # m asl, from the paper's Methods
		crop = "sweetpotato",
		variety = r$variety,
		variety_traits = habit$variety_traits[match(r$variety, habit$variety)],
		## system 1 = sandponic, 2 = soil (conventional); from the "cumulative data" file's legend
		propagation_system_ = c("sandponics", "conventional")[r$system],
		plot_area = pot_area,                  # m2; see ISSUES 3
		## cost figures in the raw data are Kenya Shillings (KES); see ISSUES 5
		cost_per_node_ = r$cost_per_node,       # KES per vine node produced
		currency = "KES"
	)

	d$treatment <- paste(d$propagation_system_, d$variety, sep = "_")

## one trial: single site (KEPHIS-PQBS Muguga), single experimental run (Jun 2018 - Mar 2019)
	d$trial_id <- as.character(as.integer(as.factor(paste(d$location, "2018_2019"))))

	d$on_farm <- FALSE      # screenhouse trial at a plant quarantine research station
	d$is_survey <- FALSE
	d$irrigated <- TRUE     # both systems were drip-irrigated/fertigated from elevated tanks (Methods)
	d$irrigation_method <- "drip"

	d$longitude <- 36.65        # 36 deg 39' E, from the paper's Methods
	d$latitude <- -1.18333      # 1 deg 11' S, from the paper's Methods
	d$geo_from_source <- TRUE

## coarse (year-month) precision only: the paper reports the trial period as months, not days
	d$planting_date <- "2018-06"
	d$harvest_date <- "2019-03"

## node counts are a screenhouse propagation-system output, not a field yield; see ISSUES 2 and 3
	d$node_density_ <- (r$node_per_pot / pot_area) * 10000    # nodes/ha equivalent; see ISSUES 3
	d$plant_density <- (10 / pot_area) * 10000                # 10 plants/pot; see ISSUES 3

	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)   # see ISSUES 4

	d$yield <- as.numeric(NA)          # see ISSUES 2
	d$yield_part <- "none"
	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- as.logical(NA)

	carobiner::write_files(path, meta, d)
}
