# GPS Tracking Data Cleaning Protocol

**Version:** 1.0 (draft for team review)  
**Last updated:** 2026-04-29  
**Status:** ⚠️ Draft — please review and add comments before implementation

---

## Overview

This protocol covers preprocessing steps between receipt of raw GPS tracking data from collaborators and the start of analyses for different subprojects. Each level produces a versioned output dataset. All decisions are logged in the cleaning metadata spreadsheet.

**Responsibility note:** Verification of data integrity — correct species, individual identity, deployment dates, tag behavior — is the responsibility of the data collector/owner. Flagged issues must be resolved with the data provider before proceeding.

---

## Level 0 — Data Standardization

Each study is processed into a standardized flat file saved as `Study_name.csv` in the `Cleaned_data_L0/` folder. Completely duplicated datasets (same species, same study, redundant submissions from different collaborators) are not recorded in the cleaning metadata tab but are retained in the data provider contact sheet for reference.

**Naming convention:** `Study.name` must be all lowercase, no spaces, underscores only (e.g., `mule_deer_utah`). For Movebank-sourced data, retain the original Movebank study ID/name. Confirm that names are unique and consistent across all tracking sheets before proceeding.

### Required columns

Retain all fields if provided. Contact the data owner if mandatory fields are absent.

| Field | Mandatory | Notes |
|---|---|---|
| `Study.name` | Yes | Binomial or species name + study ID/name/location. Assigned by data cleaner if not from Movebank. Must be unique and consistent across sheets. |
| `Binomial` | Yes | Species-level identification. Contact data owner if missing. |
| `trackID.original` | Yes | Original individual ID from the source dataset. Retained as-is; a new unique ID is assigned in Level 1. |
| `TimestampUTC` | Yes | Verify time zone conversion to UTC. Format: `YYYY-MM-DD HH:MM:SS`. |
| `Location.long` | Yes | Decimal degrees, EPSG:4326. |
| `Location.lat` | Yes | Decimal degrees, EPSG:4326. |
| `movebank.id` | If applicable | For Movebank-sourced data only. |
| `Bodymass.kg` | If provided | Confirm units are kg. |
| `Sex` | If provided | |
| `Age` | If provided | Record as numerical age or life stage (e.g., adult, juvenile, calf). |
| `DOP` | If provided | Note whether HDOP, PDOP, or number of satellites — retain original field name and document in metadata. |
| `Group.ID` | If provided | e.g., wolf packs, social groups. |
| `Temperature` | If provided | |
| Additional attributes | If provided | Retain any other fields supplied by the data owner. |

**CRS note:** Standardize all coordinates to EPSG:4326 (WGS84) at this level, before any distance or speed calculations in subsequent steps. Speed is computed using `distHaversine()` (R package `geosphere`), which calculates great-circle distances in meters directly from decimal-degree coordinates — no intermediate reprojection is required.

---

## Level 1 — Rough Data Filtering and Deduplication

Each study is filtered individually and saved as `Study_name_L1.csv`. The output file retains only the following six columns: `Study.name`, `Binomial`, `trackID.unique`, `TimestampUTC`, `Location.long`, `Location.lat`.

> **Important:** Apply steps in the order listed. Visualize the output after each step before proceeding (Gupte et al. 2022, §4.2). Do not thin or aggregate data before completing all filtering steps — thinning before error removal can bias speed estimates and preserve artifacts (Gupte et al. 2022, §6.2).

### Step 1.0 — Standardize study names

Convert all `Study.name` values to lowercase, replace spaces with underscores, remove special characters.

### Step 1.1 — Remove invalid locations and timestamps

Filter out:

- Records with `NA` in `Location.long`, `Location.lat`, or `TimestampUTC`
- Records where coordinates are exactly `[0, 0]` (common GPS artifact indicating failed fix)
- Exact duplicate records (identical timestamp and coordinates for the same individual)

### Step 1.2 — Filter implausible timestamps

Remove records with timestamps outside a biologically and logistically plausible range (e.g., future dates, dates preceding deployment, obvious data entry errors such as year = 2028). Document the bounds used in the cleaning metadata.

### Step 1.3 — Flag and filter unrealistic movement speeds

Compute step-level instantaneous speed for each individual's track using `distHaversine()` (R package `geosphere`), which returns distances in meters from decimal-degree coordinates. Divide by the elapsed time in seconds to obtain speed in m/s.

Flag positions where **both** incoming and outgoing speed exceed a study-level threshold — targeting both directions avoids removing valid fast-transit segments (Bjørneraas et al. 2010; Gupte et al. 2022, §5.2). The threshold should be based on the known or published maximum movement speed for the focal species. If no species-specific value is available, use the 99th percentile of the study's own speed distribution as a data-driven threshold, starting with a liberal (high) value to minimize over-filtering. Document the threshold used for each study in the cleaning metadata.

Remove flagged positions with caution — review before deleting.

### Step 1.4 — Assign unique individual IDs

Create `trackID.unique` as a globally unique identifier within each species across all studies (e.g., `Binomial_StudyName_originalID`). The original ID is retained in `trackID.original` at Level 0.

### Step 1.5 — Resolve duplicate datasets

Identify cases where the same species in the same country appears in more than one submitted dataset (potential overlapping submissions). When confirmed duplicates exist, retain the dataset with the longer temporal coverage. Document the decision in the cleaning metadata and consult the data provider contact sheet before removing any dataset.

### Step 1.6 — Save Level 1 output

Save each study as `Study_name_L1.csv` with only the six required columns listed above.

### Step 1.7 — Update cleaning metadata

Record results in the **"Level 1 Study Metadata"** tab:

- Study name
- Number of individuals before and after filtering
- Date range
- Fix rate
- Total records before and after each filter step
- Filter thresholds applied
- Any issues flagged

---

## Level 2 — Visual Filtering (Secondary Outlier Removal)

This level applies human-in-the-loop visual inspection to catch errors that automated filters miss — particularly biologically implausible track segments, water incursion errors, and device artifacts such as prolonged displacement errors (consecutive erroneous positions that cannot be detected by point-level speed filters; Gupte et al. 2022, §5.2, Figure 3).

> **⚠️ All flagging decisions require independent review by at least two team members before any deletion is made.**

### Step 2.1 — Plot individual tracks

For each individual, generate a map of the movement track. The plot title must include: `Binomial`, `trackID.unique`, and the median fix interval. Retain all plots for documentation.

### Step 2.2 — Flag individuals with issues

Reviewers independently flag individuals with visible problems: positional outliers, implausible track segments, prolonged displacement errors, or geographic anomalies. Use the shared screenshots document to record and annotate complicated cases.

### Step 2.3 — Process by case type

#### Straightforward cases (both reviewers agree)

1. Export the individual track as a GeoPackage (`.gpkg`) to the designated GeoPackage folder.
2. In QGIS, delete obvious outliers manually.
3. Check all speed-flagged positions from Level 1 — delete with caution; retain points where the correct decision is ambiguous and note in cleaning metadata.
4. **Water point issue:** Overlay points against permanent water body layers to determine whether a point is genuinely in permanent water. If the classification is uncertain, retain the point and note it in the cleaning metadata.

#### Ambiguous cases (reviewers disagree, or additional data needed)

- **DOP issue:** Return to the original database to retrieve DOP values if not included in the Level 0 file. Re-evaluate the flagged point with this information.
- **Timestamp issue:** Return to the original database to verify the timestamp.
- Document all ambiguous cases with screenshots and notes in the shared annotation document.

### Step 2.4 — Export and archive

Export cleaned GeoPackages as CSV files with `_L2` appended to the filename. Upload to the `L2/` folder. Update the cleaning metadata record with the final individual count, number of records removed, and decision rationale.

### Step 2.5 — Column selection for L2 output

Retain the core six columns from Level 1, plus any ancillary fields needed for downstream analyses (e.g., `DOP`, `Sex`, `Age` where available).

---

## Study-Specific Filters

### Subproject - Settlement Configuration (Xu et al.)

Apply the following filter after Level 2 cleaning, as a final eligibility criterion before analysis:

**Minimum tracking duration and displacement filter** (Tucker et al. 2018): Exclude individuals with fewer than **60 days** of tracking data or fewer than **50 displacements**. Document all exclusions in the cleaning metadata.

### Subproject - [TO BE UPDATED]

---

## Cleaning Metadata and Contact Sheet

### Cleaning metadata tab

Updated at each level. Records for each study:

- Study name and data source
- Cleaning level and date processed
- Personnel
- Filter thresholds applied
- Records before and after each filter
- Flagged issues and resolutions

### Data provider contact sheet

Retains: study name, final number of individuals included, approximate study centroid (lat/lon), contact name and email. Updated when datasets are excluded or issues require provider consultation. Completely duplicated datasets are logged here even if not in the cleaning metadata tab.

---

## Implementation Notes

- Filters are applied sequentially, not simultaneously, with a visual sanity check after each step.
- Consecutive erroneous positions that appear plausible individually — cannot be caught by point-level speed filters and are the primary target of Level 2 visual review.
- Some study locations are sensitive (e.g., endangered or poaching-risk species) and individual centroid location will not be made public.

---

## References

Bjørneraas, K., Van Moorter, B., Rolandsen, C. M., & Herfindal, I. (2010). Screening GPS location data for errors using animal movement characteristics. *Journal of Wildlife Management*, 74, 1361–1366.

Gupte, P. R., Beardsworth, C. E., Spiegel, O., Lourie, E., Toledo, S., Nathan, R., & Bijleveld, A. I. (2022). A guide to pre-processing high-throughput animal tracking data. *Journal of Animal Ecology*, 91, 287–307. https://doi.org/10.1111/1365-2656.13610

Langley, L. P., Lang, S. D. J., Ozsanlav-Harris, L., & Trevail, A. M. (2024). ExMove: An open-source toolkit for processing and exploring animal-tracking data in R. *Journal of Animal Ecology*, 93, 1–12. https://doi.org/10.1111/1365-2656.14111

Tucker, M. A., et al. (2018). Moving in the Anthropocene: Global reductions in terrestrial mammalian movements. *Science*, 359, 466–469. https://doi.org/10.1126/science.aam9712
