# R script for "carob"
# license: GPL (>=3)

## NOTES
# Tanzania 2015 household survey, plot-level crop module. Geographic coverage is Tanzania, United Republic of. The blurred file has no coordinates and every plot location name is empty.
# Sub-plot area is acres, converted with 0.404686 ha per acre. Dry harvest is labeled kg. Yield is that weight divided by the crop area. yield_isfresh is FALSE.
# An intercrop area share, when present, scales the subplot. A share of 0 is treated as missing, so the full subplot area is used.
# Long rains (Maskia 2015) is stored as long rains. Maskia is the file's spelling of Masika. Short rains (Vuli 2014/1015) is stored as short rains.
# Haricot bean is common bean. Greengram is mung bean. Soyabean and soya lishe are soybean. Ngano is wheat. Linseed is flax. Cardamon is cardamom. Sweet potato is sweetpotato. Yams is yam. Gabbage is cabbage. Pepper and capsicum are chili pepper. African egg plant and nyanya chungu are eggplant. Desi chickpea is chickpea. Field pea is pea. Finger millet, pearl millet, and pigeonpea keep their usual names. Local vegetables is vegetable.
# Bale, ngwala, ngwara, and rosera are kept as recorded.
# The same household, plot, season, and crop slot occurs twice in a few rows. Both rows are kept, with a suffix on trial_id.

## ISSUES
# write_files: missing variables: latitude, longitude. out of bounds: yield (0, 308881.454). invalid terms: crop: bale, ngwala, ngwara, rosera. low yield (tons not kg?): cabbage, chili pepper, cotton, eggplant, flax, mango, ngwara, orange, pineapple, rosera, tobacco, watermelon, yam. NA detected: yield, yield_part.
# Dry harvest is labeled kg and is stored that way. The low-yield warning is kept because the label says kilograms.
# Plot location names are empty in the blurred file.

## NOT PROCESSED
# Fresh or green harvest. The dry weight is the yield.
# Fertilizer, manure, seed, herbicide, and pesticide quantities. The urea and DAP labels say "e.g." and the column names do not match those labels.
# Labour, costs, tenure, soil classes, and stress scores.
# Trees and fruit trees, and rows coded none.
# The other questionnaire modules.


carob_script <- function(path) {

"
Pathways to sustainable intensification in Eastern and Southern Africa - Tanzania 2015

The Adoption Pathways project was part of a portfolio of projects that has contributed to the broader theme of sustainable intensification research led by the International Maize and Wheat Improvement Center (CIMMYT) and made possible by the contribution of several teams from national and international research groups brought together by funding from the Australian Centre for International Agricultural Research (ACIAR). The project was undertaken in Ethiopia, Kenya, Malawi, Mozambique and Tanzania. This file is the Tanzania 2015 plot-level crop production module.
"

	uri <- "hdl:11529/11128"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=3,
		data_organization = "CIMMYT",
		publication = NA,
		project = "Adoption Pathways",
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "yield",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 40,
		carob_effort = 0.6
	)

	f <- ff[grepl("Plot level", basename(ff)) & grepl("\\.sav$", basename(ff))]
	r <- haven::read_sav(f)

	txt <- function(x) {
		z <- trimws(as.character(haven::as_factor(x)))
		z[z %in% c("", "NA")] <- NA_character_
		z
	}
	num <- function(x) {
		z <- haven::zap_labels(x)
		z <- trimws(as.character(z))
		z[z %in% c("", "NA", "-99", "-88", ".")] <- NA_character_
		as.numeric(z)
	}
	known <- c(
		"banana"="banana", "barley"="barley", "cardamon"="cardamom",
		"cassava"="cassava", "cocoa"="cocoa", "cotton"="cotton",
		"cowpea"="cowpea", "desi chickpea"="chickpea", "field pea"="pea",
		"finger millet"="finger millet", "greengram"="mung bean",
		"groundnut"="groundnut", "haricot bean"="common bean",
		"linseed"="flax", "local vegetables"="vegetable", "maize"="maize",
		"mango"="mango", "onion"="onion", "orange"="orange",
		"pearl millet"="pearl millet", "pepper"="chili pepper",
		"pigeonpea"="pigeon pea", "pineapple"="pineapple", "pumpkin"="pumpkin",
		"rice"="rice", "sesame"="sesame", "sorghum"="sorghum",
		"soyabean"="soybean", "sugarcane"="sugarcane", "sunflower"="sunflower",
		"sweet potato"="sweetpotato", "tomato"="tomato", "watermelon"="watermelon",
		"wheat"="wheat", "yams"="yam", "capsicum"="chili pepper",
		"gabbage"="cabbage", "african egg plant"="eggplant",
		"nyanya chungu"="eggplant", "coconut tree"="coconut",
		"coconut"="coconut", "coconut trees"="coconut", "ngano"="wheat",
		"pinapple"="pineapple", "soya lishe"="soybean", "tobacco"="tobacco",
		"bale"="bale", "ngwala"="ngwala", "ngwara"="ngwara", "rosera"="rosera"
	)
	part_of <- c(
		banana="fruit", barley="grain", cardamom="seed", cassava="roots",
		cocoa="seed", cotton="lint", cowpea="seed", chickpea="seed",
		pea="seed", "finger millet"="grain", "mung bean"="seed",
		groundnut="seed", "common bean"="seed", flax="seed",
		vegetable=NA_character_, maize="grain", mango="fruit",
		onion=NA_character_, orange="fruit", "pearl millet"="grain",
		"chili pepper"="fruit", "pigeon pea"="seed", pineapple="fruit",
		pumpkin="fruit", rice="grain", sesame="seed", sorghum="grain",
		soybean="seed", sugarcane="stems", sunflower="seed",
		sweetpotato="roots", tomato="fruit", watermelon="fruit",
		wheat="seed", yam="tubers", cabbage=NA_character_, eggplant="fruit",
		coconut="seed", tobacco="leaves", bale=NA_character_,
		ngwala=NA_character_, ngwara=NA_character_, rosera=NA_character_
	)
	map_crop <- function(x) {
		key <- tolower(x)
		out <- unname(known[key])
		out[is.na(key)] <- NA_character_
		out
	}

	hh <- trimws(as.character(haven::zap_labels(r$hhldid)))
	acres <- num(r$subpltsize)
	season <- txt(r$season)
	season[season == "Long rains (Maskia 2015)"] <- "long rains"
	season[season == "Short rains (Vuli 2014/1015)"] <- "short rains"
	plot <- txt(r$plotcode)
	subp <- txt(r$subplot)
	irr <- txt(r$irrigation)
	irrigated <- rep(NA, length(irr))
	irrigated[which(irr == "Yes")] <- TRUE
	irrigated[which(irr == "No")] <- FALSE

	slot <- function(crop, other, variety, othvar, share, dry, slot_id) {
		stated <- txt(crop)
		alt <- txt(other)
		use_alt <- !is.na(stated) & tolower(stated) == "other,specify"
		stated[use_alt] <- alt[use_alt]
		cr <- map_crop(stated)
		ok <- !is.na(cr)
		sh <- num(share)[ok]
		sh[which(sh == 0)] <- NA_real_
		ha <- acres[ok] * 0.404686
		fs <- ha
		got <- !is.na(sh)
		fs[got] <- ha[got] * sh[got] / 100
		vy <- txt(variety)
		if (!is.null(othvar)) {
			ov <- txt(othvar)
			vy_alt <- !is.na(vy) & tolower(vy) == "other,specify"
			vy[vy_alt] <- ov[vy_alt]
		}
		vy[which(tolower(vy) %in% c("none", "i dont know", "other,specify"))] <- NA_character_
		w <- num(dry)[ok]
		yld <- w / fs
		yld[which(is.na(fs) | fs == 0)] <- NA_real_
		data.frame(
			country = "Tanzania",
			hhid = hh[ok],
			trial_id = paste(hh[ok], plot[ok], subp[ok], season[ok], slot_id),
			plot_id = paste(plot[ok], subp[ok]),
			season = season[ok],
			field_size = fs,
			crop = cr[ok],
			variety = vy[ok],
			yield = yld,
			yield_part = unname(part_of[cr[ok]]),
			yield_isfresh = FALSE,
			irrigated = irrigated[ok],
			is_survey = TRUE,
			on_farm = TRUE,
			geo_from_source = FALSE
		)
	}

	d <- rbind(
		slot(r$cropgrwn1, r$othcropgrwn1, r$cropvar1, r$othcropvar1, r$areashare1, r$amtdry1, 1),
		slot(r$cropgrwn2, r$othcropgrwn2, r$cropvar2, NULL, r$areashare2, r$amtdry2, 2),
		slot(r$cropgrwn3, r$othcropgrwn3, r$cropvar3, NULL, r$areashare3, r$amtdry3, 3)
	)
	d <- d[!is.na(d$crop), ]
	# The same household, plot, season, and crop slot can occur twice. Keep both.
	id <- d$trial_id
	n <- ave(seq_along(id), id, FUN=length)
	seq <- ave(seq_along(id), id, FUN=seq_along)
	id[n > 1] <- paste(id[n > 1], seq[n > 1], sep="_")
	d$trial_id <- id
	carobiner::write_files(path, meta, d)
}
