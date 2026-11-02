# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Rejected. The three data files are laboratory ant-mortality bioassays (Anoplolepis tenella, Pheidole, Camponotus, Myrmicaria): treatment, dose, week, and number of dead ants.
# There is no cassava field, no scale-insect count, and no yield. The abstract describes a field experiment and a 16% yield difference, but those observations are not in the files.
# Publication cited in the metadata, not processed: doi:10.1016/j.cropro.2015.04.016 (Hanna et al. 2015, Crop Protection 74:131-137).

## NOT PROCESSED
# ant_control_database.csv, myrmicaria.csv, preliminary.csv: all columns. Laboratory toxicology, no crop.
# metadata.csv: data dictionary for those bioassays.


carob_script <- function(path) {

"
Boric acid for suppression of the ant Anoplolepis tenella and effects on an associated scale insect pest Stictococcus vayssierei in cassava fields in the Congo Basin

The scale insect, Stictococcus vayssierei, is one of the major pests and constraints to cassava production in the Congo Basin. It is frequently found associated with the ground nesting ant, Anoplolepis tenella, under cassava plant. Amdro fire ant bait granules (contains 7.3 g/kg Hydramethylnon), borax and boric acid solution were evaluated in the laboratory for toxicity against A. tenella colonies. Workers were exposed to bait for ten days during primary screening and colonies for six weeks during secondary screening. Mortality was recorded daily for each toxicant. In a field experiment, the effect of a 1% boric acid-sucrose solution was evaluated on A. tenella and its impact on the density of S. vayssierei on cassava. Each of the three toxicants displayed delayed toxicity with less than 15% mortality after the first day. In comparison to Amdro and borax, the 1% boric acid-sucrose solution was more effective in killing ant workers. The boric acid-sucrose solution reduced the numbers of workers and brood by 90% within 6 weeks. In the field experiment, monthly ant and scale sampling showed significant reductions in ant density in treated fields which resulted in a reduction of scale infestation on cassava. We recorded a 16% yield increase in treated plots compared to control plots. These results outline the dependence of S. vayssierei survival on A. tenella attendance and demonstrate that control of the ant in cassava fields can help in developing control options for the scale insect pest in the Congo Basin.
"

	uri <- "doi:10.25502/tdhw-m205/d"
	group <- "pest_disease"
	# Lab bioassay only. Not processed. See ISSUES.
	invisible(uri)
	invisible(path)
	invisible(group)
}
