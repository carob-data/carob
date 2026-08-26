# R script for "carob"
# license: GPL (>=3)

## REJECTED -- in vitro tissue-culture experiment, not a field/pot trial
## 1. All data are per-plantlet observations inside sterile in vitro culture
##    (liquid MS medium in vials, laminar-flow hood, 25C/4000 lux growth room)
##    -- "unit" = an individual plantlet in a vial, not a field/pot plot.
## 2. Treatments are plant growth regulators added to the culture medium
##    (GA, JA, NAA, uniconazole-p at uM concentrations; control = sterile
##    water) at the single-node/primary-nodal-complex micropropagation
##    stages -- there is no planting/harvest, soil, or field/greenhouse
##    management to record, and no meaningful land-area basis for yield.
## 3. Confirmed against the linked publication: Balogun et al. (2014)
##    "Relative Effects of Uniconazole-p and other growth regulators on
##    Micropropagation ratio of yam (Dioscorea spp.) plantlets at different
##    growth phases", Journal of Root Crops 40(2):5-11 (no DOI found for the
##    article; ISSN 0378-2409) -- Materials and Methods describe genotypes
##    "established in vitro at the tissue culture laboratory of IITA,
##    Ibadan" and PGR regimes applied to single-node cuttings in liquid MS
##    medium; no field/greenhouse component anywhere in the paper.
## Per AGENTS.md Section 12, data collected in a lab rather than in the
## field are rejected (the "greenhouse experiments for variety traits are
## OK" exception does not apply: this is not a variety trial, it is a PGR
## dose-response study on in vitro-cultured plantlets).

carob_script <- function(path) {

"
In vitro microtuber production and dormancy as affected by some cultural factors

Scarcity of planting materials, pest and diseases are major constraints to yam
production. The tissue culture techniques, meristem culture combined with heat
therapy has been used to produce high yielding virus-tested plantlets of root
crops, which are later multiplied through micropropagation. This procedure,
however has low regedaration and multiplication rates in yam. We investigated
effects of different plant growth regulators on rate of growth of yam plantlets
when applied at different growth phases. Three concentrations each of
uniconazole-p (UP), gibberellic acid (GA3), jasmonic acid(JA) and naphthalene
acetic (NAA) acids were applied at single node and primary nodal complex (PNC)
formation phases in a split-plot design using two genotypes each of white and
water yam. Number of nodes per plantlet was recorded after 16 weeks. Results
showed that number of new nodes varied significantly with genotype by growth
phase interaction and the main effect of PGRs. After 16 weeks, the control, 1.7
uM UP and 0.03 uM JA showed highest means of 7, 9 and 8 nodes per plantlet
respectively compared to 2-3 nodes in the GA treatments. We demonstrated that
4900, 8100 and 6400 plantlets could be obtained in one year using 100 nodes by
sub-culturing them four times. Addition of PGRs at PNC doubled the number of
nodes per plantlet. However the effect of stage of treatment varied with
genotypes. The GA and its inhibitor, UP influenced the multiplication rate of
yam.
"

	uri <- "doi:10.25502/mbk7-f218/d"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

## data/raw files (all inspected):
## - microtuberization.csv    : per-plantlet node/root/microtuber counts
##                               under 13 PGR regimes, in vitro (4132 rows)
## - microtuber-dormancy.csv  : per-microtuber sprouting counts under the
##                               same PGR regimes, in vitro (6498 rows)
## - definition-of-acronyms.csv: PGR regime codes -> concentration/compound
## - metadata.csv             : column dictionary only, no location/geo

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,          # no DOI found for the linked paper
		project = NA,
		design = "split-plot (growth phase = whole-plot; PGR regime = subplot)",
		data_type = "experiment",
		treatment_vars = "",
		response_vars = "",
		carob_contributor = "Oscar Bautista",
		carob_LLM = "Claude Sonnet 5",
		carob_date = "2026-08-26",
		carob_completion = 0,     # in vitro data does not fit the schema
		carob_effort = 0.3
	)
	meta$dataset_id <- paste0(meta$dataset_id, "_nodata")

	carobiner::write_files(path, meta)
}
