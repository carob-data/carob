# Instructions for an AI agent: writing Carob data-processing scripts

This document tells an AI coding agent how to turn a published dataset into a
Carob processing script, following the conventions used in this repository. Read it fully before starting.

Do NOT guess silently. If you get stuck on *what* a value should be (not *how* to code it), leave a clear
`# comment` in the code, and add a line to the `## NOTE:` or `## ISSUES` block at the top of the scripts



---

## 1. What a Carob script is

Each script is a single R file that defines one function:

```r
carob_script <- function(path) {
    ...
    carobiner::write_files(path, meta, d)
}
```

It downloads one dataset, reshapes it into a **tidy `data.frame` `d`** whose column
names and units follow the **terminag** vocabulary, attaches **metadata** (`meta`),
and writes standardized output with `carobiner::write_files(path, meta, d)`. There may be an additional data.frame with "long" records for example to record multiple observations in time for each experimental plot.

- One dataset = one script = (ideally) one pull request.
- The file name is the dataset id: `yuri::simpleURI(uri)` with slashes replaced by
  underscores, e.g. `doi:10.7910/DVN/SMGA6L` → `doi_10.7910_DVN_SMGA6L.R`.
- The script lives in `scripts/<group>/` where `<group>` is the thematic group
  (`agronomy`, `varieties`, `survey`, `soil_samples`, `pest_disease`, ...).

Use `scripts/_template.R` as the canonical skeleton and any recent file in
`scripts/varieties/` (e.g. `doi_10.7910_DVN_SMGA6L.R`) as a concrete example.

`path` passed to `carob_script()` is the carob repo working dir, e.g.
`"C:/github/carob/carob"`. Downloads land in `data/raw/<group>/<dataset_id>/`.

---

## 2. vocabulary

``
- The **vocabulary** (which variable names and which categorical values are
  allowed) lives in **terminag**. 
  
- **Which variables are *required*** (and which may not be `NA`) is **not** in the 
  vocabulary; it lives in the R package "caroboner" `terms/required_variables.csv`. This is
  what `carobiner::write_files()` checks.


---

## 3. Workflow

1. **bootstrap** with `carobiner::draft(uri, path, group)`. This
   downloads the data and writes a starter into `scripts/_draft/<group>/`. 
2. **Inspect the raw data**: list `ff`, read each relevant sheet/file, and look at
   column names, codebooks, and the dataset's description/abstract. Codebooks
   (often extra `.csv`/`.xlsx`/`.pdf`) tell you units and category codes.
3. **Map columns** to terminag names and correct units (Section 5–6).
4. **Fill metadata** (Section 4).
5. **Build `d`**, then set the required "housekeeping" variables (trial_id, geo,
   on_farm/is_survey, yield_part, ...).
6. **Test** in a clean session and resolve every `write_files()` message
   (Section 8).
7. **Place** the finished file in `scripts/<group>/` (Section 9).

---

## 4. Metadata: `carobiner::get_metadata(...)`

```r
meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
    data_organization = "PURDUE",     # data provider and/or author institutes
    publication = NA,                 # paper DOI if any (add matching RIS in references/); use NA, never ""
    project = NA,
    design = NA,                      # experimental/survey design if known
    data_type = "on-farm experiment", # "experiment", "on-farm experiment", "survey", "compilation", ...
    treatment_vars = "variety",       # ";"-separated variables that ARE the treatments
    response_vars = "yield;plant_height;maturity_days", # ";"-separated measured responses
    carob_contributor = "Your Name",
    carob_date = "2026-07-17",        # date first written (YYYY-MM-DD)
    carob_completion = 80,            # % of relevant variables standardized (0-100)
    carob_effort = 0.1               # hours spent
)
```

Rules and gotchas:

- Prefer `NA` (not `""`) for absent `publication`, `project`, `design`, etc. An
  empty string triggers warnings from `write_files()`.
- `treatment_vars` must be actual column names present in `d`, and each must have
  >1 non-missing value (there must be variation). `response_vars` are the measured
  outcomes of interest — **not** management variables applied to all plots.
- `data_type` "survey" (and the `survey`/`soil_samples` groups) relax some
  crop/agronomy requirements — see the required-variables logic below.
- Copy the dataset **title and abstract** verbatim into the quoted string near the
  top of the function (see the template) so reviewers have context.

---

## 5. Building `d`: the tidy data.frame

Assign source columns to terminag names, coercing types and units as you go:

```r
f <- ff[basename(ff) == "B lines observation nursery at Mieso 2014.xlsx"]
r <- carobiner::read.excel(f, na="NA")   # or read.csv(f) / haven::read_dta(f)

d <- data.frame(
    crop = "sorghum",
    variety = r$Genotype,
    treatment = r$Genotype,
    yield = r$`YieldKg/Ha`,                 # kg/ha
    plant_height = r$PHTMean,
    maturity_days = r$DTM,
    plant_density = 10000 * as.numeric(r$StandAtHarv) / as.numeric(r$PlotArea),
    country = "Ethiopia",
    location = r$Site
)
```

Then set the "housekeeping" variables that most scripts need:

```r
## one trial = one location x season (NOT one per treatment/replicate).
## For a survey, each row/household gets a unique trial_id.
d$trial_id <- as.character(as.integer(as.factor(paste(d$location, year))))

d$on_farm  <- TRUE   # or FALSE / NA
d$is_survey <- FALSE
d$irrigated <- NA

## geography (see Section 7)
d$longitude <- 40.5638
d$latitude  <- 9.1779
d$geo_from_source <- FALSE            # TRUE if coords came from the data/paper

## crop production
d$yield_part <- "grain"               # what the yield refers to
d$yield_moisture <- as.numeric(NA)    # % moisture if known
```

General rules:

- **variables** all variables should be processed unless they are redundant (used to compute a variable of interest, or derived thereof) or cannot be interpreted. Write a comment for each variable that is not processed.
- **Variable (Column) names** should match a variable name from terminag. Where that is not possible because terminag
  does not have a matching variable name propose an appropriate new variable name, that ends in an 
  underscore (e.g. `annual_income_`). 
- **Categorical values, and units must match terminag.** Check
  `variables_*.csv` (names, `valid_min`/`valid_max`) and `values_*.csv` (accepted
  category values, e.g. crop names, country names).  
- **Coerce explicitly.** `read.excel`/`read.csv` may read a column as character;
  wrap numeric math in `as.numeric(...)` (e.g. density calculations) and integers
  in `as.integer(...)`. This avoids "bad datatype" warnings.
- **Normalize names**: `carobiner::fix_name(x, "title")` for admin/location names;
  `trimws()` to remove stray whitespace (untrimmed values are flagged).
- **`crop`** and other controlled values must be lowercase accepted terms
  (`tolower(...)` where appropriate). Intercrops use an underscore: `"maize_bean"`.

---

## 6. Units and common conversions

- **Yield**: kg/ha, as **fresh weight** of `yield_part`. Convert t/ha → kg/ha
  (`* 1000`). Set `yield_moisture` (%) when known; if all yields are dry or
  moisture is unknown, consider `yield_isfresh`.
- **Area**: hectares. 1 acre = 0.4046863 ha.
- **Fertilizer**: report elemental **N, P, K** in kg/ha — **not** oxides.
  `P = P2O5 / 2.29`, `K = K2O / 1.2051`. Also `S_fertilizer`, `lime` when present.
  Compute nutrient amounts from product rate × nutrient fraction (e.g. urea 46% N).
- **Dates**: character strings, one of `"2023"` (year), `"2023-07"` (year-month),
  or `"2023-07-21"` (full date). Use `as.character(as.Date(...))` for full dates.
- **Prices**: include `currency` whenever `crop_price` is present (a price without
  a currency is flagged).

Add a short `#` comment whenever a computation relies on the codebook, the paper,
or a non-obvious assumption (e.g. basket→kg conversions, nutrient fractions).

---

## 7. Georeferencing

Every distinct site needs `longitude`/`latitude`.

- If the data/publication provide coordinates, use them and set
  `d$geo_from_source <- TRUE`.
- If not, estimate them from admin units / place names and set
  `d$geo_from_source <- FALSE`. Useful helpers:
  - `carobiner::geocode(...)` for place names.
  - `carobiner::adm_pointRadius(country, level)` to get admin-unit centroids plus a
    `geo_uncertainty` (meters) and a `geo_source` string (e.g. `"GADM 4.1, adm3"`).
    See the "Georeferencing" contribute page for the worked example.
- When you estimate from an admin unit, also set `d$geo_uncertainty` and
  `d$geo_source` to document the estimate.
- Fill `adm1`/`adm2`/`adm3` (title-cased) and `location`/`site` when available;
  use `location` before `site` (a `site` column is not allowed without `location`).

---

## 8. Testing and interpreting `write_files()`

Run in a **clean** R session (no stray objects), e.g.:

```r
devtools::load_all("<root>/carobiner")
carob_script <- NULL; source("scripts/<group>/<dataset_id>.R")
carob_script(path = "<root>/carob/carob")
```

`carobiner::write_files(path, meta, d)` prints messages you must resolve:

- **`missing variables` / `missing metadata`**: a required variable/metadata field
  is absent. Add it (see `carobiner/inst/terms/required_variables.csv`). Some are
  conditional on the group (e.g. `crop`, `yield`, `N/P/K_fertilizer`, `irrigated`
  are not required for `survey`/`soil_samples`).
- **`unknown variables`**: a column name is not in the vocabulary. Rename it to a
  terminag name, or if it is legitimately non-standard, keep it. 
- **`out of bounds`**: a numeric value is outside `valid_min`/`valid_max`. Consider fixing the
  units or the value.
- **`bad datatype`**: coerce the column (`as.numeric`, `as.integer`,
  `as.character`).
- **`NA detected`**: a variable that may not be `NA` (per `required_variables.csv`,
  `NAok=no`) contains `NA`. Provide values or reconsider the mapping.
- **`empty character values` / `untrimmed characters`**: clean strings with
  `trimws()` and replace `""` with a real value or `NA`.
- **`invalid terms`**: a categorical value is not in the accepted `values_*.csv`
  list (e.g. a crop or country spelled differently). Map it to the accepted term.

Keep iterating until the only remaining output is the contributor line / `TRUE`.

Do **not** force data into a one-row-per-unit shape when that loses information.
If a dataset has several record types (e.g. a household survey with per-variety
records *and* a separate "largest plot" block), capture each in its own
`data.frame` with interpretable, standardized names rather than deduplicating away
real observations. Decide how to write extra tables (e.g. as "long" records)
separately.

---

## 9. Where scripts go, and PR conventions

- Work-in-progress from `draft()`: `scripts/_draft/<group>/`.
- Hard/auto-rejected drafts: `scripts/_AI/_rejected/` (a review queue; skipped by
  the build).
- **Finished** script: `scripts/<group>/<dataset_id>.R`.
- Confirmed "do not process": `scripts/_rejected/`.
- The build (`make_carob()` / `process_carob()`) **skips** `_draft`, `_AI`,
  `_pending`, and `_rejected`. Only files under a real `scripts/<group>/` folder are
  compiled.
- Prefer **one script per pull request**. Use branch with same name as script file name. 

---

## 10. Group-specific notes

### `varieties`
- Core treatment is `variety` (also set `treatment = variety`). Include `variety_pedigree` when given.
- Set `variety_type` — often derivable from the dataset description (e.g. "advanced drought tolerant hybrid"). Read the abstract.
- Identify and flag **check** varieties when the data/description indicate them (e.g. a `Check` column, or named hybrid/OPV/parent checks). Follow the pattern in existing `scripts/varieties/` files.
- Density variables: `plant_density`, `spike_density` = `10000 * count / plot_area`  (per ha); coerce `count` and `plot_area` with `as.numeric` first.

### `survey`
- `data_type = "survey"`, `d$is_survey <- TRUE`. Each surveyed unit (household) gets a unique `hh_id`. `crop`, `yield`, and management vars are not required. 
- Multi-module surveys often need to create several `data.frame`s (Section 8) that can be merged.

### `soil_samples`
- `crop`/`management` requirements are relaxed; focus on soil variables and
  `sample_id`. Still set `is_survey`, `on_farm`, and geography.

---

## 11. Pre-submission checklist

- [ ] File named `<dataset_id>.R` and placed in `scripts/<group>/`.
- [ ] Title + abstract copied into the script; `## ISSUES` notes any caveats.
- [ ] `uri`, `group`, `get_data`, `get_metadata`, `write_files` all present.
- [ ] Metadata: real `data_organization`, `data_type`, `treatment_vars`,
      `response_vars`, `carob_contributor`, `carob_date`, `carob_completion`,
      `carob_effort`; `NA` (not `""`) for absent fields.
- [ ] All column names and categorical values match terminag; units correct
      (kg/ha yields, elemental N/P/K, ha areas, proper date formats).
- [ ] `trial_id`, `on_farm`, `is_survey`, `longitude`, `latitude`,
      `geo_from_source`, `yield_part` set appropriately.
- [ ] `carob_script(path)` runs clean in a fresh session with no unresolved
      `write_files()` messages.
- [ ] No information silently dropped to fit a single table.

---

## Related references

- `scripts/_template.R` — canonical skeleton (note the template typo:
  `fertlizer_type` should be `fertilizer_type`).
- terminag: <https://github.com/controvoc/terminag> (variables and accepted values).
- Carob contribute docs: <https://carob-data.org/contribute/index.html>
  (see the Example, Guidelines, and Georeferencing pages).
