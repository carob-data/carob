# R script for "carob"
# license: GPL (>=3)

## NOTES
# 2015 rainfed-lowland participatory evaluation trials (PET). One row per genotype at each site.
# Dictionnary.txt: YIELD is paddy at 14% moisture, t/ha (stored as kg/ha). FLWR and MATURITY are days. PLTHGT and PNL are cm. GW1000 is g per 1000 grains.
# No replicate column. Values are fractional, so they are not single-plot records. Replicates were not filled in.
# Every row is production system RL, so irrigated is FALSE. Trial is PET on every row.
# Cote d'Ivoire is stored as Côte d'Ivoire. Guinea Conakry is stored as Guinea.
# Seredou has two columns both named Trial. The values are RL and PET, the same codes as PS and Trial in the other files.
# Edozhigi labels ARICA 2 and NERICA-L19 with a check tag. That tag was removed from the variety name. LOCAL CHECK is kept as the name and marked variety_type "check".
# Named-site trials, so on_farm is FALSE.

## ISSUES
# No coordinates in the workbooks or the supplementary PDF. Sites still need georeferencing.
# No fertilizer rates. Year is 2015, but planting and harvest dates are not reported.
# Karfiguela PNL is 148-277, which is not a plausible panicle length in cm. Left as recorded in panicle_length_.
# Mali (Longorola) reports maturity days and panicle length, and does not report flowering. Grain weight is only in Gagnoa and Seredou. Panicle length is absent for Banfora and Edozhigi.
# Edozhigi genotype strings omit spaces used in the other country files (for example ARC1-20-B-1 versus ARC 1-20-B-1), use NERICA-L19 rather than NERICA-L-19, and one WAB name is missing a hyphen. They were not rewritten.
# write_files: ; missing variables: geo_from_source, latitude, longitude, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date; unknown variables: panicle_length_.

## NOT PROCESSED
# Dictionnary.txt TILL, BIOM, FERT: those columns are not in the workbooks.
# 2015-RL-PET-Suppleentary information.pdf: LSD, heritability, and p-value. Trial statistics, not plot or genotype rows.
# PS and Trial, including the two Trial columns in the Seredou file: constant production system RL and trial type PET.
# Year: the trial year (2015), stored as planting_date.


carob_script <- function(path) {

"
2015 Africa-wide Breeding Task Force Trials for Rainfed Lowland - PET

In 2010, the Africa-wide Rice Breeding Task Force was launched by AfricaRice involving National Agricultural Research System (NARS) from about 30 countries. The objectives of the network are to evaluate the stability of traits incorporated in breeding processes and to identify varieties best fit to growth conditions in target regions and to markets. The Task Force also accumulates data on performance of new elite lines, thereby facilitating varietal release procedures. Furthermore, by exposing breeders from NARS and farmers to these elite lines during the testing phase, dissemination will be facilitated.

The activities conducted by the Task Force consists of a series of consecutive trials. Promising breeding lines developed by AfricaRice or by national and international partners, such as IRRI, CIAT and the NARS are nominated for evaluation in one or several rice cultivation environments: rainfed lowland, irrigated lowland, rainfed upland, high elevation and mangrove. All nominated lines should be fixed and accompanied by supporting data on traits incorporated during the breeding process and with information on yield performance. These characteristics are checked at AfricaRice before incorporation into the network.

The first phase (MET, Multi-Environment Testing) consists of an initial evaluation of about 100 lines selected from the nominated lines. Each national partner evaluates these lines at sites in his/her country. Such sites may be at an experimental station under optimal management to evaluate yield potential, or may be 'hot spots' to check the performance of the nominations in a stressed growth environment. Trials are replicated three times and include at least a common and a local check.

The second phase (PET, Participatory Evaluation Trial) serves to evaluate and confirm the performance of the selected lines. These lines are cultivated using the same experimental design with 3 replications. An important feature of PET is that farmer and other stakeholders such as miller and traders are invited to participate in varietal selection and their opinion on the performance of all entries (i.e. participatory varietal selection, PVS) collected. Based on the data collected, observations by the breeders and the opinion of stakeholder groups, NARS partners select up to 10 lines.

Further, NARS evaluated these lines in at least three sites per country and during one or more growing seasons, depending on varietal release requirements. All stakeholders are again invited to get acquainted with the new lines and voice their opinion to help select lines for further advancement. Among the 10 lines, farmers are invited to select three lines and cultivate these in their own fields, together with a common check and their own variety.
"

	uri <- "doi:10.7910/DVN/PJRTEA"
	group <- "varieties_other"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "AfricaRice",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;flowering_days;plant_height;seed_weight;maturity_days;panicle_length",
		notes = NA,
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 85,
		carob_effort = 0
	)

	process <- carobiner::get_function("process_africarice_btf", path, group)
	d <- process(ff, year = 2015, irrigated = FALSE)

	carobiner::write_files(path, meta, d)
}
