# Instructions for an AI agent: writing Carob data-processing scripts

This document tells an AI coding agent how to turn a published dataset into a Carob processing script, following the conventions used in this repository. Read it fully before starting.

**Also read these two official Carob pages first:**
- Guidelines: <https://carob-data.org/contribute/guidelines.html> — the authoritative rules ("Before you start", Scripts, Standardization, R coding style).
- Example script: <https://carob-data.org/contribute/example.html> — a full worked example (doi:10.21421/D2/STACVA) walking through metadata, reading a sheet, and building `d`.

This document supplements those pages with practical, agent-specific detail. If they ever disagree, follow the official guidelines.

**Before writing, also read a few existing scripts in `scripts/<group>/`** — especially the same group you are targeting — to see the conventions applied to real datasets (e.g. recent files in `scripts/varieties/` such as `doi_10.7910_DVN_SMGA6L.R`, or any script in `scripts/agronomy/` or `scripts/survey/`). They are the best templates for structure, naming, units, and how metadata and geo/housekeeping variables are set.

Do NOT guess silently. If you get stuck on *what* a value should be (not *how* to code it), leave a clear`# comment` in the code, and add a line to the `## NOTE:` or `## ISSUES` block at the top of the scripts

---

## 1. What a Carob script is

Each script is a single R file that defines one function:

```r
carob_script <- function(path) {
    ...
    carobiner::write_files(path, meta, d)
}
```

It downloads one dataset, reshapes it into a **tidy `data.frame` `d`** whose column names and units follow the **terminag** vocabulary, attaches **metadata** (`meta`), and writes standardized output with `carobiner::write_files(path, meta, d)`. There may be an additional data.frame with "long" records for example to record multiple observations in time for each experimental plot.

- One dataset = one script = (ideally) one pull request.
- The file name is the dataset id: `yuri::simpleURI(uri)` with slashes replaced by underscores, e.g. `doi:10.7910/DVN/SMGA6L` → `doi_10.7910_DVN_SMGA6L.R`.
- The script lives in `scripts/<group>/` where `<group>` is the thematic group (`agronomy`, `varieties`, `survey`, `soil_samples`, `pest_disease`, ...).

Use `scripts/_template.R` as the canonical skeleton and any recent file in `scripts/varieties/` (e.g. `doi_10.7910_DVN_SMGA6L.R`) as a concrete example.

`path` passed to `carob_script()` is the carob repo working dir, e.g. `"C:/github/carob/carob"`. Downloads land in `data/raw/<group>/<dataset_id>/`.

---

## 2. vocabulary

- The **vocabulary** (which variable names and which categorical values are allowed) lives in **terminag**.

### Where to find the terminag files

Do **not** assume terminag is a git clone sitting next to the `carob` repo. That is only true in a maintainer's development setup; it is **not** the general case.

`carobiner` reads the vocabulary through the **`vocal`** package. By default the vocabulary source is the GitHub repo, addressed as the string **`"github:controvoc/terminag"`** (see `carobiner::carob_vocabulary()`). `vocal` downloads a **snapshot** of that repo (via the GitHub API) and stores it in a per-user cache, then refreshes it when the upstream `sha` changes. So on a typical machine the terminag files are here, not in a project folder:

```
<rappdirs::user_data_dir()>/.vocal/controvoc/terminag/
    variables/variables_<group>.csv   # variable names, types, valid_min/valid_max, vocabulary key
    values/values_<name>.csv          # accepted categorical values (crops, countries, ...)
    sha.txt                           # cached upstream commit id
```

`rappdirs::user_data_dir()` is platform-specific, e.g.:
- Windows: `C:\Users\<you>\AppData\Local\.vocal\controvoc\terminag`
- macOS:   `~/Library/Application Support/.vocal/controvoc/terminag`
- Linux:   `~/.local/share/.vocal/controvoc/terminag`

To locate/inspect them portably, resolve the path in R rather than hard-coding it:

```r
file.path(rappdirs::user_data_dir(), ".vocal", "controvoc", "terminag")
```

**For an agent, reading the cached CSVs directly is usually the most efficient** way to inspect the vocabulary (with your normal file-read/grep tools) — no R session needed. Just make sure you read the *active cache* at the path above, not a random clone. The `vocal` API reads the very same files, so use it only when you are already in R (e.g. to double-check the resolved values):

```r
vocal::accepted_variables(include = "varieties")  # allowed variable names (optionally by group)
vocal::accepted_values("crop")                    # allowed values for a categorical variable
carobiner::carob_vocabulary()                     # the current vocabulary source string
```

Whichever you use, be aware the cache is only refreshed by `vocal` (on the GitHub `sha`) — if in doubt about freshness, compare against GitHub. You can also browse the same files on GitHub: <https://github.com/controvoc/terminag>. The carob-side pointer to the chosen vocabulary is cached in `<rappdirs::user_data_dir()>/.carob/voc`.

- **Which variables are *required*** (and which may not be `NA`) is **not** in the vocabulary; it lives in the R package "carobiner" `terms/required_variables.csv`. This is what `carobiner::write_files()` checks.

---

## 3. Workflow

0. **Study a few finished scripts** in `scripts/<group>/` (ideally the target group) to learn the expected structure and conventions before you begin.
1. **bootstrap** with `carobiner::draft(uri, path, group)`. This downloads the data and writes a starter into `scripts/_draft/<group>/`.
2. **Inspect the raw data**: list `ff`, read each relevant sheet/file, and look at column names, codebooks, and the dataset's description/abstract. Codebooks (often extra `.csv`/`.xlsx`/`.pdf`) tell you units and category codes. **Also read the associated publication's Methods section** (Section 8) for location, management, design, and unit information not in the data files.
3. **Map columns** to terminag names and correct units (Section 5–6).
4. **Fill metadata** (Section 4).
5. **Build `d`**, then set the required "housekeeping" variables (trial_id, geo, on_farm/is_survey, yield_part, ...).
6. **Test** in a clean session and resolve every `write_files()` message (Section 9).
7. **Place** the finished file in `scripts/<group>/` (Section 10).

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

- Prefer `NA` (not `""`) for absent `publication`, `project`, `design`, etc. An empty string triggers warnings from `write_files()`.
- `treatment_vars` must be actual column names present in `d`, and each must have
  >1 non-missing value (there must be variation). `response_vars` are the measured
  outcomes of interest — **not** management variables applied to all plots.
- `data_type` "survey" (and the `survey`/`soil_samples` groups) relax some crop/agronomy requirements — see the required-variables logic below.
- Copy the dataset **title and abstract** verbatim into the quoted string near the top of the function (see the template) so reviewers have context.

---

## 5. Building `d`: the tidy data.frame

### Naming and style convention

- Read each **input** file into **`r`**. If a script reads more than one file (or sheet), name them **`r1`, `r2`, ...** (or descriptive names like `rA`, `rD`).
- Transform each into an **output** `data.frame` named **`d`**, or **`d1`, `d2`, ...** when there are several, then combine as needed.
- Do the mapping **as directly as possible inside the `data.frame(...)` call** — one line per variable, using a plain column reference or a simple in-line transformation (e.g. a unit multiplication, `tolower()`, `as.numeric()`):

  ```r
  d <- data.frame(
      crop = r$Crop,
      longitude = r$lon,
      yield = r$yield_tha * 1000       # t/ha -> kg/ha
  )
  ```

- **Avoid** re-opening `d` afterwards to assign columns that could have been mapped directly. That is, do **not** write later:

  ```r
  d$country <- r$Country   # avoid: this belongs inside the data.frame() above
  ```

  Keep the direct `r$... -> d$...` mappings together in the `data.frame()` call. Reserve later `d$<name> <- ...` assignments for values that genuinely cannot go there: constants set for the whole table, "housekeeping" variables, values derived from several columns or from another `data.frame`, or post-hoc cleanup.

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
- **Variable (Column) names** should match a variable name from terminag. Where that is not possible because terminag does not have a matching variable name propose an appropriate new variable name, that ends in an underscore (e.g. `annual_income_`).
- **Categorical values, and units must match terminag.** Check `variables_*.csv` (names, `valid_min`/`valid_max`) and `values_*.csv` (accepted category values, e.g. crop names, country names).
- **Coerce explicitly.** `read.excel`/`read.csv` may read a column as character; wrap numeric math in `as.numeric(...)` (e.g. density calculations) and integers in `as.integer(...)`. This avoids "bad datatype" warnings.
- **Normalize names**: `carobiner::fix_name(x, "title")` for admin/location names; `trimws()` to remove stray whitespace (untrimmed values are flagged).
- **`crop`** and other controlled values must be lowercase accepted terms (`tolower(...)` where appropriate). Intercrops use an underscore: `"maize_bean"`.

---

## 6. Units and common conversions

- **Yield**: kg/ha, as **fresh weight** of `yield_part`. Convert t/ha → kg/ha (`* 1000`). Set `yield_moisture` (%) when known; if all yields are dry or moisture is unknown, consider `yield_isfresh`.
- **Area**: hectares. 1 acre = 0.4046863 ha.
- **Fertilizer**: report `P_fertilizer` and `K_fertilizer` as the weight of the **elements P and K**, *not* the weight of the oxides P2O5 and K2O. Convert with `P = P2O5 / 2.29` and `K = K2O / 1.2051`. Likewise report elemental **N** (and `S_fertilizer`, `lime`) in kg/ha. Compute nutrient amounts from product rate × nutrient fraction (e.g. urea 46% N).
- **Dates**: character strings, one of `"2023"` (year), `"2023-07"` (year-month), or `"2023-07-21"` (full date). Use `as.character(as.Date(...))` for full dates.
- **Prices**: include `currency` whenever `crop_price` is present (a price without a currency is flagged).

Add a short `#` comment whenever a computation relies on the codebook, the paper, or a non-obvious assumption (e.g. basket→kg conversions, nutrient fractions).

---

## 7. Georeferencing

Every distinct site needs `longitude`/`latitude`.

- If the data/publication provide coordinates, use them and set `d$geo_from_source <- TRUE`.
- If not, estimate them from admin units / place names and set `d$geo_from_source <- FALSE`. Useful helpers:
  - `carobiner::geocode(...)` for place names.
  - `carobiner::adm_pointRadius(country, level)` to get admin-unit centroids plus a `geo_uncertainty` (meters) and a `geo_source` string (e.g. `"GADM 4.1, adm3"`). See the "Georeferencing" contribute page for the worked example.
- When you estimate from an admin unit, also set `d$geo_uncertainty` and `d$geo_source` to document the estimate.
- Fill `adm1`/`adm2`/`adm3` (title-cased) and `location`/`site` when available; use `location` before `site` (a `site` column is not allowed without `location`).

---

## 8. Filling gaps from the associated publication

Repository files rarely contain everything. When the dataset links to a paper (a DOI in the metadata, a `publication`, or a citation in the dataset description), read its **Methods / Materials and Methods** (and often the study-area and supplementary sections) to recover information that is missing from, or only coded in, the data files. Treat the publication as an authoritative source and cite it in a `#` comment wherever you use it.

Common things the Methods section supplies:

- **Location / geography**: study site names, region/district, and often explicit **coordinates** or a map. Use these to set `location`/`adm*` and `longitude`/`latitude`. If the paper gives the coordinates, set `d$geo_from_source <- TRUE`; if you only get a place name and must estimate, keep `FALSE` and record `geo_uncertainty`/`geo_source` (Section 7).
- **Management**: planting/harvest **dates** or seasons, **fertilizer** rates and products (convert to elemental N/P/K, Section 6), **irrigation**, plant spacing / **plant_density**, tillage, variety details, and whether trials were `on_farm`. These are frequently described once in prose and applied to all plots, so they won't appear as columns in the data.
- **Design and treatments**: the experimental/survey **design** (→ `design`), replication, and what the **treatments** and measured **responses** actually were (→ `treatment_vars` / `response_vars`).
- **Units and definitions**: what a yield figure refers to (`yield_part`), whether it is fresh or dry (`yield_moisture` / `yield_isfresh`), moisture basis, plot sizes, and the meaning of coded columns in a codebook.

Guidance:

- Add a RIS file for the paper in the `references/` folder (matching the `publication` DOI) as noted in Section 4.
- When a value is stated in the paper but not the data, hard-code it with a comment, e.g. `d$N_fertilizer <- 120   # Methods: 120 kg N/ha as urea, all plots`.
- If the value applies to some rows only, be careful to assign it to the right subset rather than the whole table.
- If the paper and data disagree, prefer the data for measured values, note the discrepancy in `## ISSUES`, and use the paper for context/management.
- If there is no accessible paper, or the Methods do not resolve a gap, leave the variable `NA` and record the open question in `## ISSUES`.

## 9. Testing and interpreting `write_files()`

Run in a **clean** R session (no stray objects), e.g.:

```r
devtools::load_all("<root>/carobiner")
carob_script <- NULL; source("scripts/<group>/<dataset_id>.R")
carob_script(path = "<root>/carob/carob")
```

`carobiner::write_files(path, meta, d)` prints messages you must resolve:

- **`missing variables` / `missing metadata`**: a required variable/metadata field is absent. Add it (see `carobiner/inst/terms/required_variables.csv`). Some are conditional on the group (e.g. `crop`, `yield`, `N/P/K_fertilizer`, `irrigated` are not required for `survey`/`soil_samples`).
- **`unknown variables`**: a column name is not in the vocabulary. Rename it to a terminag name, or if it is legitimately non-standard, keep it.
- **`out of bounds`**: a numeric value is outside `valid_min`/`valid_max`. Consider fixing the units or the value.
- **`bad datatype`**: coerce the column (`as.numeric`, `as.integer`, `as.character`).
- **`NA detected`**: a variable that may not be `NA` (per `required_variables.csv`, `NAok=no`) contains `NA`. Provide values or reconsider the mapping.
- **`empty character values` / `untrimmed characters`**: clean strings with `trimws()` and replace `""` with a real value or `NA`.
- **`invalid terms`**: a categorical value is not in the accepted `values_*.csv` list (e.g. a crop or country spelled differently). Map it to the accepted term.

Keep iterating until the only remaining output is the contributor line / `TRUE`.

### Never suppress or work around warnings

Warnings (from `write_files()` or from R itself) are signals, not noise. For each one there are only two acceptable outcomes:

1. **Truly fix it** — you identified a concrete problem and solved it (corrected a unit, coerced a type, mapped a value to an accepted term, fixed a name, etc.).
2. **Leave it visible** — if it reflects a genuine limitation of the source data that you cannot resolve, let the warning stand so a reviewer can inspect it, and add a `#` comment (and/or a `## ISSUES` note) explaining **why** it remains.

Do **not**:

- use `suppressWarnings()`, `suppressMessages()`, `options(warn=-1)`, `try()`/ `tryCatch()` swallowing, or similar, to hide a warning;
- filter/drop rows, coerce blindly, or tweak values **just to silence** a message without understanding it;
- delete or comment out a variable only to make a warning disappear.

The goal is that every remaining warning is either intentional and explained, or gone because the underlying problem was actually fixed — never merely hidden.

Do **not** force data into a one-row-per-unit shape when that loses information. If a dataset has several record types (e.g. a household survey with per-variety records *and* a separate "largest plot" block), capture each in its own `data.frame` with interpretable, standardized names rather than deduplicating away real observations. Decide how to write extra tables (e.g. as "long" records) separately.

---

## 10. Where scripts go, and PR conventions

- Work-in-progress from `draft()`: `scripts/_draft/<group>/`.
- Hard/auto-rejected drafts: `scripts/_AI/_rejected/` (a review queue; skipped by the build).
- **Finished** script: `scripts/<group>/<dataset_id>.R`.
- Confirmed "do not process": `scripts/_rejected/`.
- The build (`make_carob()` / `process_carob()`) **skips** `_draft`, `_AI`, `_pending`, and `_rejected`. Only files under a real `scripts/<group>/` folder are compiled.
- Prefer **one script per pull request**. Use branch with same name as script file name.

---

## 11. Group-specific notes

### `varieties`
- Core treatment is `variety` (also set `treatment = variety`). Include `variety_pedigree` when given.
- Set `variety_type` — often derivable from the dataset description (e.g. "advanced drought tolerant hybrid"). Read the abstract.
- Identify and flag **check** varieties when the data/description indicate them (e.g. a `Check` column, or named hybrid/OPV/parent checks). Follow the pattern in existing `scripts/varieties/` files.
- Density variables: `plant_density`, `spike_density` = `10000 * count / plot_area`  (per ha); coerce `count` and `plot_area` with `as.numeric` first.

### `survey`
- `data_type = "survey"`, `d$is_survey <- TRUE`. Each surveyed unit (household) gets a unique `hh_id`. `crop`, `yield`, and management vars are not required.
- Multi-module surveys often need to create several `data.frame`s (Section 9) that can be merged.

### `soil_samples`
- `crop`/`management` requirements are relaxed; focus on soil variables and `sample_id`. Still set `is_survey`, `on_farm`, and geography.

---

## 12. Pre-submission checklist

- [ ] File named `<dataset_id>.R` and placed in `scripts/<group>/`.
- [ ] Title + abstract copied into the script; `## ISSUES` notes any caveats.
- [ ] `uri`, `group`, `get_data`, `get_metadata`, `write_files` all present.
- [ ] Metadata: real `data_organization`, `data_type`, `treatment_vars`, `response_vars`, `carob_contributor`, `carob_date`, `carob_completion`, `carob_effort`; `NA` (not `""`) for absent fields.
- [ ] Associated publication's Methods checked for location/management/design/units; values taken from it are commented and (if a paper exists) a RIS added to `references/`.
- [ ] All column names and categorical values match terminag; units correct (kg/ha yields, elemental N/P/K, ha areas, proper date formats).
- [ ] `trial_id`, `on_farm`, `is_survey`, `longitude`, `latitude`, `geo_from_source`, `yield_part` set appropriately.
- [ ] `carob_script(path)` runs clean in a fresh session with no unresolved `write_files()` messages.
- [ ] No `suppressWarnings()`/`suppressMessages()`/`options(warn=-1)` used; every remaining warning is either fixed or left with a `#` comment explaining why.
- [ ] No information silently dropped to fit a single table.

---

## Related references

- **Official contributor guidelines (read this): <https://carob-data.org/contribute/guidelines.html>**
- **Official example script walkthrough (read this): <https://carob-data.org/contribute/example.html>**
- `scripts/_template.R` — canonical skeleton (note the template typo: `fertlizer_type` should be `fertilizer_type`).
- terminag: <https://github.com/controvoc/terminag> (variables and accepted values).
- Carob contribute docs: <https://carob-data.org/contribute/index.html> (see the Example, Guidelines, and Georeferencing pages).
