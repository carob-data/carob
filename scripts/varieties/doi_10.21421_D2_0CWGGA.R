# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Selections data of high-oil-yielding stables Groundnut genotypes based on genotype X environment interactions

Peanut (Arachis hypogaea L.) genotypes with superior and stable agronomic performance and high oil content were identified from testing of 160 advanced breeding lines over six seasons (three rainy and three post rainy seasons). The study revealed significant genotype and genotype X environment interaction determining oil and protein content; shelling out-turn; and pod, kernel, and oil yield in peanut. The variability among genotypes was high across the environments for pod yield (546- 7382 kg per ha), oil yield (301-2742 kg per ha), oil content (37-60%), 100-seed weight (21-127 g), and protein content (19-31%). The GGE bi-plot technique revealed that ICGV 05155 is a stable genotype for oil yield with an average oil yield of 1886 kg per ha. ICGV 05155 recorded highest average pod yield of 4928 kg per ha, kernel yield of 3420 kg per ha, and oil content of 55%. ICGV 06049, ICGV 06041, ICGV 06420, and ICGV 03043 were other genotypes with stable oil yield. Simple regression showed significant contributions of oil content (18-54%), and kernel yield (92-99%) to oil yield across the environments. Simultaneous improvement of kernel yield and oil content is feasible in breeding programs, as kernel yield had no negative association with oil content. The high oil content genotypes, ICGV 05155, ICGV 06049, ICGV 06041, ICGV 06420, and ICGV 03043, with stable oil yield were promoted to multilocation adaptive trials required for their release for cultivation and used as parents in breeding programs and development of mapping population to identify quantitative trait loci (QTL) governing oil content.
"

	uri <- "doi:10.21421/D2/0CWGGA"
	group <- "varieties"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=3,
		data_organization = "ICRISAT",
		publication = "doi:10.2135/cropsci2016.01.0005",
		project = NA,
		carob_date = "2025-07-29",
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield", 
		carob_contributor = "Cedric Ngakou",
		completion = 100,	
		notes = NA
	)
	
    ff <- ff[grepl("Groundnut", basename(ff))] 
	

	process <- function(f){
	   
	   r <- carobiner::read.excel(f, fix_names = TRUE)
	   names(r) <- gsub("X.Seed.oil.content", "Seed.oil.content", names(r))
	   names(r) <- gsub("Pod.Yield", "Pod.yield", names(r))
	   if(is.null(r$Hundred.seed.weight)) r$Hundred.seed.weight <- NA
	   if(is.null(r$Seed.protein.content)) r$Seed.protein.content <- NA
	   
	   d1 <- data.frame(
	      variety = r$Genotype,
	      yield = r$Pod.yield, 
	      seed_weight = r$Hundred.seed.weight,
	      grain_protein = r$Seed.protein.content,
	      #grain_oil = r$Seed.oil.content,
	      trial_id = trimws(gsub(".xlsx|Groundnut genotypes based on genotype x environment interactions", "", basename(f))),
	      crop = "groundnut",
	      irrigated = grepl("post rainy", basename(f)),
	      row_spacing = 30,
	      plant_spacing = 10,
	      P_fertilizer = 60/2.29,
	      K_fertilizer = 0,
	      N_fertilizer = 0,
	      seed_treatment = "mancozeb",
	      gypsum = 400,
	      country = "India",
	      adm1 = "Telangana",
	      adm2 = "Sangareddy District",
	      adm3 = "Patancheru",
	      location = "ICRISAT, Patancheru",
	      longitude = 78.45 , # from paper
	      latitude = 17.88,
	      elevation = 545,
	      geo_from_source = TRUE,
	      on_farm = TRUE, 
	      is_survey = FALSE, 
	      yield_part = "pod", 
	      yield_moisture = as.numeric(NA)
	      
	   )
	   
	}

	dd <- lapply(ff, process)
	d <- do.call(rbind, dd)	
	d$planting_date <- ifelse(grepl("post", d$trial_id), paste0(substr(gsub("post rainy ", "", d$trial_id), 1, 4), "-11"), paste0(substr(gsub("rainy ", "", d$trial_id), 1, 4), "-06") )
 

carobiner::write_files(path, meta, d)

}

