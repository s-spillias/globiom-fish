# Fish-GLOBIOM — analysis and figure pipeline

Code accompanying *"Integrated assessment of food-system and land-based environmental
impacts under blue growth scenarios"* (Spillias et al.).

The figures are built from GLOBIOM model output (run 3945). Because the raw model
output is a multi-hundred-MB GAMS `.gdx` file, the repository commits small per-figure
data tables extracted from it, plus the R code that turns those tables into the
manuscript figures. The figures can be regenerated with R alone; reproducing the
committed tables from the `.gdx` additionally needs Python and the model output.

## Pipeline

```
output_3945_merged.gdx  (GLOBIOM run 3945; not tracked — see Data availability)
        │  R/extract_figure_data.py   (Python + gams.transfer)
        ▼
data/figure_data/*.csv  (committed per-figure tables)
        │  R/make_figures.R           (R)
        ▼
figures/Figure_*.png
```

`R/make_figures.R` reads only the committed CSVs, so the figures reproduce without the
`.gdx`, GAMS, or Python. `R/extract_figure_data.py` documents and reproduces the CSV
extraction from the model output.

## Repository layout

```
R/
  make_figures.R          builds every figure from data/figure_data/ into figures/
  extract_figure_data.py  extracts data/figure_data/*.csv from the run-3945 .gdx
  functions/
    glob_plot.R           stacked delta-from-baseline bars (Figure 4)
data/
  figure_data/            per-figure tables extracted from run 3945 (committed)
  lookup_table.csv        model ITEM -> production system (SYST) lookup (Figure 2)
  fao_capture_vs_aquaculture.csv   FAO capture/aquaculture history (Figure 2b)
  regions/                Region-37 shapefile (Reg37_aggregate.*) for Figure 5
figures/                  output PNGs, one per manuscript figure
```

## Running

Regenerate all figures (needs R and ImageMagick `convert`; run from the repo root):

```
Rscript R/make_figures.R
```

R packages: `dplyr`, `tidyr`, `ggplot2`, `stringr`, `sf`, `tmap`. Multi-panel figures
(2, 3, 4) are assembled with ImageMagick.

Re-extract the per-figure tables from the model output (optional; needs the `.gdx`):

```
pip install gamspy-base gamsapi pandas
python R/extract_figure_data.py path/to/output_3945_merged.gdx
```

## Scenarios

The model runs an 18-cell grid `SCEN_DIET{d}_CULTURE{c}_CAPTURE{w}` with
`d ∈ {-10, 0, +10}`, `c ∈ {0, +50}`, `w ∈ {-10, 0, +10}`. Three cells are the headline
scenarios:

| Manuscript name          | Scenario code                        |
|--------------------------|--------------------------------------|
| BAU (Business as Usual)  | `SCEN_DIET0_CULTURE0_CAPTURE0`       |
| Blue Transformation      | `SCEN_DIET+10_CULTURE+50_CAPTURE+10` |
| Barriers to Blue Growth  | `SCEN_DIET-10_CULTURE0_CAPTURE-10`   |

`Reference 2020` is the 2020 slice of BAU.

## Figure → code map

| Manuscript figure | Data (`data/figure_data/`) | Output file |
|---|---|---|
| **Fig 1** — Demand (fish & livestock) | `fig1_demand.csv` | `figures/Figure_1_demand.png` |
| **Fig 2** — production by system (a), trends (b) | `fig2a_production.csv`, `fig2b_trends.csv` | `figures/Figure_2_production.png` |
| **Fig 3** — feed crops (a), aquafeed 2050 (b) | `fig3a_feed.csv`, `fig3b_aquafeed.csv` | `figures/Figure_3_feed.png` |
| **Fig 4** — land / emissions / water / N impacts | `fig4_impacts.csv` (+ `functions/glob_plot.R`) | `figures/Figure_4_impacts.png` |
| **Fig 5** — regional maps | `fig5_land.csv`, `fig5_fish.csv`, `regions/` | `figures/Figure_5_<scenario>_map.png` |
| **Fig 6** — conceptual diagram | not generated in R | — |
| **Fig 7** — FMFO source shift | `fig7_s2_fmfo_sources.csv` | `figures/Figure_7_fmfo_sources.png` |
| **Fig S1** — regional seafood demand | `figS1_demand_regional.csv` | `figures/Figure_S1_demand_regional.png` |
| **Fig S2** — FMFO sources (stacked) | `fig7_s2_fmfo_sources.csv` | `figures/Figure_S2_fmfo_sources.png` |
| **Fig S3** — FMFO inclusion by species | `figS3_fmfo_inclusion.csv` | `figures/Figure_S3_fmfo_inclusion.png` |

The Figure 7 / S2 / S3 tables are exogenous FMFO assumptions (not model output) and are
committed directly; `extract_figure_data.py` reproduces the other tables from the `.gdx`.

## Data availability

The GLOBIOM run-3945 output `output_3945_merged.gdx` (~700 MB) is the data of record for
the extracted tables. It exceeds GitHub's file-size limit and is not tracked; obtain it
from the corresponding author or the deposited archive to re-run
`R/extract_figure_data.py`. The committed `data/figure_data/*.csv` are sufficient to
reproduce every figure without it.

## Notes

- Figures use a colourblind-safe (Okabe-Ito) palette and distinguish scenarios by point
  shape and line style as well as colour.
- Figure S1 was regenerated from run 3945 for consistency with the other figures; the
  originally submitted manuscript showed the earlier run's version of this panel.
- The R pipeline that produced the originally submitted run (the `.gdx` → `.RData` loader
  and its figure script) is retained under `_archive/` for provenance.
