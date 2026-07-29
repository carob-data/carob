# R script for "carob"
# license: GPL (>=3)

## ISSUES

carob_script <- function(path) {

"
Evaluation data of Pearl millet hybrids for grain Iron and Zinc during 2010 rainy season
Experimental Materials and Field Trials  Genotypes were evaluated in two replications in randomized complete block design in Alfisols at Patancheru during the 2010 rainy season. The plot size was two row of 4m, with rows spaced 75 cm apart.  Within-row plant-to-plant spacing was 10 cm. At crop maturity, open-pollinated main panicles of  6-8 standing-plants (panicles without soil contact) in each plot  were harvested, placed in paper bags, sundried for 10 to 15 days, and threshed in single head machine thresher (Wintersteiger-129 ID780ST4, Ried, Austria). About 30 gm grain samples were collected for each plot and stored for 1-2 months in clean and non-metal fold paper bags at room temperatures normally above 32oC, and then used for Fe and Zn density analysis.   Micronutrient Analysis  Open-pollinated grain samples produced from each were divided into two sub-samples.  One sub-sample was sent to the Waite Analytical Services Laboratory, Adelaide, Australia, for Inductively Coupled Plasma Optical Emission Spectrometer (ICP), hereafter referred to as ICP  analysis; and the other sample was analyzed at the ICRISAT using Energy-Dispersive X-ray Fluorescence (XRF) Spectrometry, hereafter referred to as XRF.  The ICP analysis for Fe and Zn density was done following the method described by Wheal et al. (2011). Analysis of Aluminum (Al) density as an index element for monitoring dust contamination was also done. Grain samples were oven-dried overnight at 85ºC prior to digestion, grounded enough to pass through 1 mm stainless steel sieve using Christie and Norris hammer mill and stored in screw-top polycarbonate vials.   The samples were digested with di-acid (Nitric / Perchloric acid) mixture. After digestion, the volume of the digest was made to 25 mL using distilled water; and the content was agitated for 1 minute by vortex mixer. The digests were filtered and the Fe concentration was read at 259.94 nm and Zn concentration at 213.86 nm using ICP-OES and these micronutrient were expressed as mg kg-1. Care was taken at each step to avoid any contamination of the grains with dust particles and any other extraneous matter (Stangoulis and Sison 2008).  For XRF analysis, the calibration of Oxford Instruments X-Supreme 8000 fitted with a 10 place auto-sampler was done at Flinders University, Adelaide, Australia (Paltridge et al. 2012).  For this, 20 reference pearl millet whole grain samples that had ICP-determined Fe (29-163 mg kg-1) and Zn (35-100 mg kg-1) density were used to calibrate XRF method. Thus, ICP concentrations used as reference value were entered into the machine before each sample was scanned. Clean Poly-4 film was used for each sample. According to the manufacturer, the X-Supreme 8000 scans a circle of 21 mm diameter with the sample spinner on. All scans in this study were performed in this mode, so the scanned area was 346 mm2 (Paltridge et al 2012). So background scans fixed uniform emission toward sampling compartment with 60 s acquisition times for each sample cup. The relationship between X-ray fluorescence and reference values was then established using the XRF calibrates function and a simple linear model. Calibration results showed very high correlation coefficients (R2 = 0.97; P20 g of grain weight capacity, combined with polypropylene inner cups sealed at one end with 4 μm Poly-4 XRF sample film.  Cups in a batch of 10 were filled with 8-12 g of grain samples to a depth of >22 mm higher than >6 mm depth and > 4 g suggested by Paltridge et al. (2012).  The cups were shaken to evenly distribute grain in the cups, which were loaded in the XRF instrument holder. It takes 13 minutes to complete the analysis and display the Fe and Zn density on the monitor attached to XRF instrument.  After the analysis, the cups were removed and cleaned to prepare for the next batch of analysis.        Experiment location on Google Map
"
  
	uri <- "doi:10.21421/D2/PMNQEO"
	group <- "varieties" 
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
		data_organization = "ICRISAT",
		publication = NA,
		project = "Harvest Plus",
		design = "RCB",
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "grain_Fe;grain_Zn", 
		notes = NA,
		carob_contributor = "Blessing Dzuda",
		carob_date = "2026-07-24",
		carob_completion = 100,	
		carob_effort = 5
	)
	
	f1 <- ff[basename(ff) == "Pearl millet commercial hybrid trial ICP and XRF data.xlsx"]
	f2 <- ff[basename(ff) == "Pearl millet Hybrid trial 26 entries ICP and XRF data.xlsx"]
	f3 <- ff[basename(ff) == "Pearl millet Hybrid trial 28 entries ICP and XRF data.xlsx"]
	f4 <- ff[basename(ff) == "Pearl millet hybrid trial 39 entries ICP and XRF data.xlsx"]

	r1 <- carobiner::read.excel(f1)
	r2 <- trimws(carobiner::read.excel(f2))
	r3 <- carobiner::read.excel(f3)
	r4 <- carobiner::read.excel(f4)

	r <- rbind(r1,r2,r3,r4)
	
	d <- data.frame(
	  date="2010",
	  country="India",
	  location="ICRISAT HQ, Patancheru" 
	  adm1="Telangana",
	  adm2="Sangareddy",
	  longitude = 78.27265,
	  latitude = 17.50466,
	  geo_uncertainty = 100, # approx length of field
	  geo_from_source = TRUE,
	  crop="pearl millet",
	  yield=as.numeric(NA),
	  yield_part="grain",
	  yield_moisture=NA,
	  yield_isfresh=NA,
	  season="wet",
	  #same genotype name despite description mentioning commercial, hybrid 1,2 and 4
	  variety= paste(r$`Genotype ID`,"RP 07B/2010",sep ="_"),
	  rep=r$`Replication number`,
	  grain_Fe=r$Iron,
	  grain_Zn=r$Zinc,
	  grain_analysis_method=r$Method,
	  planting_date=NA,
	  harvest_date=NA
	)

	d$trial_id <- paste(d$variety,1:nrow(d),sep = "_")
	
	d$on_farm <- FALSE
	d$is_survey <- FALSE 
	d$irrigated <-FALSE
	d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- as.numeric(NA)
  
  #fixing row(s) where data is concatenated within a cell
  row_i <- which(is_concat)
  
  # columns holding a pure deparsed vector
  vec_cols <- c("rep", "grain_Fe", "grain_Zn", "analysis_method")
  
  # columns holding a deparsed vector with extra text stuck on the end
  vec_suffix_cols <- c("variety", "trial_id")
  
  parse_vec <- function(x) eval(parse(text = x))
  
  split_vec_suffix <- function(x) {
    close_pos <- regexpr(")", x, fixed = TRUE)   # end of the c(...) call
    vec  <- eval(parse(text = substr(x, 1, close_pos)))
    suff <- substr(x, close_pos + 1, nchar(x))
    paste0(vec, suff)
  }
  
  row <- d[row_i, , drop = FALSE]
  n   <- length(parse_vec(row$rep[1]))     
  
  new_rows <- row[rep(1, n), , drop = FALSE]
  rownames(new_rows) <- NULL
  
  for (col in vec_cols)         new_rows[[col]] <- parse_vec(row[[col]][1])
  for (col in vec_suffix_cols)  new_rows[[col]] <- split_vec_suffix(row[[col]][1])
  
  d <- rbind(d[-row_i, ], new_rows)
  rownames(d) <- NULL
  
  #improving quality
  d$rep <- ifelse(d$rep=="1",1,2)
  d$rep <- as.integer(d$rep)
  d$grain_Fe <- as.numeric(d$grain_Fe)
  d$grain_Zn <- as.numeric(d$grain_Zn)

  d <- unique(d)#2 records removed, i assume they originated from the expanded concatenate
  
  carobiner::write_files(path, meta, d)
}
