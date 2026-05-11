# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Data from: Phosphorus budgets of intensively managed row crops at a long-term agroecosystem research site in the upper US Midwest

Phosphorus (P) budgets for cropping systems provide insights for keeping soil P at optimal levels for crops while avoiding excess inputs. We quantified 12 years of P inputs (fertilizer and atmospheric deposition) and outputs (harvest and leaching losses) for replicated maize (Zea mays L.)—soybean (Glycine max L.)—wheat (Triticum aestivum) crop rotations under conventional, no-till, reduced input, and biologically based (organic without compost or manure) management systems at the Kellogg Biological Station LTAR site in southwest Michigan. Conventional, no-till, and reduced input systems were fertilized between 13 and 50 kg P ha−1 depending on year. Soil test phosphorus (STP) was measured at 0- to 25-cm depth every autumn. Leached P was measured as dissolved P in the soil solution sampled beneath the rooting depth and combined with modeled percolation. Fertilization and harvest were the predominant P fluxes in the fertilized systems, whereas only harvest dominated P flux in the unfertilized organic system. Leaching losses were minor terms in the budgets, but leachate concentrations were nevertheless close to the range of concern for downstream eutrophication. Over the 12-year study period, the organic system exhibited a negative P balance (−82.0 kg P ha−1), coinciding with suboptimal STP levels, suggesting a need for P supplementation. In contrast, the fertilized systems showed positive P balances (mean: 70.1 kg P ha−1) with STP levels well above agronomic optima. Results underscore the importance of tailored P management strategies to sustain crop productivity while mitigating environmental impacts.
"

	uri <- "doi:10.5061/dryad.d7wm37q93"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)
	
	
	meta <- carobiner::get_metadata(uri, path, group, major=4, minor=NA,
		data_organization = "MSU",
		publication = "doi:10.1002/jeq2.70000",
		project = NA,
		carob_date = "2026-05-11",
		design = NA,
		data_type = "experiment",
		treatment_vars = "P_fertilizer;land_prep_method",
		response_vars = "yield", 
		carob_contributor = "Cedric Ngakou",
		completion = 100,	
		notes = NA
	)
	

	f1 <- ff[basename(ff) == "1._Nitrogen_and_phosphorus_fertilization_no_terms.csv"]
	f2 <- ff[basename(ff) == "2._Grain_yield_of_maize__wheat_and_soybean_no_terms.csv"]
	f3 <- ff[basename(ff) == "3._stover_yield_of_wheat_no_terms.csv"]
	f4 <- ff[basename(ff) == "4._Plant_tissue_phosphorus_concentration.csv"]
	f5 <- ff[basename(ff) == "5._drainage.csv"]
	f6 <- ff[basename(ff) == "6._Dissolved_P_concentration.csv"]
	f7 <- ff[basename(ff) == "7._Soil_test_phosphorus_concentration_no_terms.csv"]
	f8 <- ff[basename(ff) == "README.md"]

	
	r1 <- read.csv(f1, na="")
	r2 <- read.csv(f2)
	r3 <- read.csv(f3)
	r4 <- read.csv(f4)
	r5 <- read.csv(f5)
	r6 <- read.csv(f6)
	r7 <- read.csv(f7)
	
## process
	names(r1) <- r1[17,]
	r1 <- r1[-c(1:17),]
	
	d1 <- data.frame(
	  date = as.character(as.Date(r1$obs_date, "%m/%d/%Y")),
	  treatment = r1$treatment,
	  rep = as.integer(substr(r1$replicate, 2, 2)),
	  trial_id = r1$plot,
	  fertilizer_type = r1$material,
	  fertilizer_amount = as.numeric(r1$rate_kg_ha),
	  N_fertilizer = as.numeric(r1$n_rate_kg_ha),
	  P_fertilizer = as.numeric(r1$p_rate_kg_ha),
	  K_fertilizer = as.numeric(r1$k_rate_kg_ha),
	  planting_date = substr(as.Date(r1$obs_date, "%m/%d/%Y"), 1, 4)
	)
	
	names(r2) <- r2[21,]
	r2 <- r2[-c(1:22),]
	d2 <- data.frame(
	  harvest_date =  as.character(as.Date(r2$Date, "%m/%d/%Y")),
	  treatment = r2$Treatment,
	  rep = as.integer(substr(r2$Replicate, 2, 2)),
	  crop = ifelse(grepl("Triticum aestivum L.",  r2$Crop), "wheat", 
	                ifelse(grepl("Zea mays L.",  r2$Crop), "maize", "soybean")),
	  yield = as.numeric(r2$crop_only_yield_kg_ha),
	  dmy_total = as.numeric(r2$whole_plot_yield_kg_ha),
	  yield_moisture = 15.5,
	  planting_date = r2$Year
	)
	
	### merge d1 and d2
	
	d <- merge(d1, d2, by= intersect(names(d1), names(d2)), all = TRUE)
	
	
	names(r3) <- r3[12,]
	r3 <- r3[-c(1:13),]
	d3 <- data.frame(
	  harvest_date = as.character(as.Date(r3$date, "%m/%d/%Y")),
	  treatment = r3$treatment,
	  #description = r3$description,
	  rep = as.integer(substr(r3$replicate, 2, 2)),
	  crop = "wheat",
	  dmy_residue = as.numeric(r3$yield_kg_ha),
	  planting_date = r3$year
	)
	
	### merge d and d3
	
	d <- merge(d, d3, by = intersect(names(d), names(d3)), all = TRUE)  
	
	names(r4) <- r4[1,]
	r4 <- r4[-c(1),]
	d41 <- data.frame(
	  planting_date = r4$year,
	  treatment = r4$treatment,
	  crop=trimws(gsub("corn", "maize", tolower(r4$Crop))),
	  rep = as.integer(substr(r4$replicate, 2, 2)),
	  grain_P = ifelse(grepl("grain", r4$fraction), as.numeric(r4$`P (ppm)`), NA)/1000 ## to mg/g
	)
	
	d42 <- data.frame(
	  planting_date = r4$year,
	  treatment = r4$treatment,
	  crop=trimws(gsub("corn", "maize", tolower(r4$Crop))),
	  rep = as.integer(substr(r4$replicate, 2, 2)),
	  residue_P = ifelse(grepl("stover", r4$fraction), as.numeric(r4$`P (ppm)`), NA)/1000  ## to mg/g
	)
	
	d4 <- merge(d41[!is.na(d41$grain_P),], d42[!is.na(d42$residue_P),], by= intersect(names(d41), names(d42)), all=TRUE)
	### merge d and d4
	
	d <- merge(d, d4, by = intersect(names(d), names(d4)), all = TRUE)  
	
	names(r7) <- r7[10,]
	r7 <- r7[-c(1:10),]
	d5 <- data.frame(
	  planting_date = substr(as.Date(r7$Sample_Date, "%m/%d/%Y"), 1, 4),
	  treatment = r7$Treatment,
	  crop = r7$crop,
	  rep = as.integer(substr(r7$Replicate, 2, 2)),
	  depth_top = 0,
	  depth_bottom = 25,
	  soil_P = as.numeric(r7$phosphorus_milligramsPerGram),
	  soil_P_method = "Bray I"
	  
	)

	### merge d and d5
	
	d <- merge(d, d5, by = intersect(names(d), names(d5)), all = TRUE)  
  
	d <- d[!is.na(d$yield),]
	### Fixing fertilizer type
	
	d$fertilizer_type <- ifelse(grepl("Fertilizer 19-17-0|0-46-0", d$fertilizer_type), "NPK",
	                     ifelse(grepl("UAN", d$fertilizer_type), "AN",
	                     ifelse(grepl("Ammonium sulfate", d$fertilizer_type), "AS", 
	                     ifelse(grepl("monoammonium phosphate", d$fertilizer_type), "MAP",
	                     ifelse(grepl("potash fertilizer", d$fertilizer_type), "KCl", 
	                     ifelse(grepl("lime", d$fertilizer_type), "lime",
	                     ifelse(grepl("Fertilizer|Sulfur|Herbicide|H2O|Dual|custom mix", d$fertilizer_type),"unknown", d$fertilizer_type)))))))
	
	d$land_prep_method <- ifelse(grepl("reduced input", d$treatment), "reduced tillage",
	                             ifelse(grepl("no-till", d$treatment), "none", 
	                             ifelse(grepl("biologically based", d$treatment), "unknown", d$treatment)))
	d$P_fertilizer <- ifelse(is.na(d$P_fertilizer), 0, d$P_fertilizer)
	
	d$country <- "United States" 
	d$adm1 <- "Michigan"
	d$location <- "KBS"
	d$longitude <- - 85.3749
	d$latitude  <- 42.3956
	d$on_farm <- TRUE 
	d$is_survey <- FALSE
	d$yield_part <- "grain" 
	d$irrigated <- NA 
	d$geo_from_source <- TRUE # From publication
	
	### remove duplicate rows
	
	d <- unique(d)
	
	
	carobiner::write_files(path, meta, d)
}


