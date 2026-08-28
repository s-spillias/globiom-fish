#!/usr/bin/env python3
"""
Extract the per-figure data tables in data/figure_data/ from the GLOBIOM run-3945
output (output_3945_merged.gdx). This is the provenance step for the figure
pipeline: it turns the large model output into the small committed CSVs that
R/make_figures.R plots.

The GDX is the data of record. It is far too large for version control and is not
tracked here (see README "Data availability"); place it at the path in GDX below,
or pass a path as the first argument.

Requires: gamspy-base, gamsapi (gams.transfer), pandas.  Read the GDX with
    pip install gamspy-base gamsapi pandas
The Fig 7 / S2 / S3 tables are exogenous FMFO assumptions (not model output); they
are committed directly and are not produced here.

Usage:  python R/extract_figure_data.py [path/to/output_3945_merged.gdx] [out_dir]
"""
import os, sys
import pandas as pd
import gamspy_base
import gams.transfer as gt

GDX = sys.argv[1] if len(sys.argv) > 1 else "data/output_3945_merged.gdx"
OUT = sys.argv[2] if len(sys.argv) > 2 else "data/figure_data"
os.makedirs(OUT, exist_ok=True)

# run-3945 scenario names -> publication labels / scenario codes
PUB = {"Scen_ScottBAU_core_FCAP0": "BAU",
       "Scen_ScottBarriers_core_FCAP0": "Barriers to Blue Growth",
       "Scen_ScottBlueTrans_core_FCAP0": "Blue Transformation"}
CODE = {"BAU": "SCEN_DIET0_CULTURE0_CAPTURE0",
        "Barriers to Blue Growth": "SCEN_DIET-10_CULTURE0_CAPTURE-10",
        "Blue Transformation": "SCEN_DIET+10_CULTURE+50_CAPTURE+10"}
LIVE = ["BVMEAT", "SGMEAT", "PGMEAT", "PTMEAT", "ALMILK", "PTEGGS"]
FISH = ["DMRSF", "SALMF", "TUNAF", "SHRIF", "MLSCF", "MARNF", "FRSHF", "CEPHF", "PELGF", "CRSTF"]

m = gt.Container(GDX, system_directory=gamspy_base.directory)

# OUTPUT: file/VAR/UNIT/REGION/ITEM/SSP/SPA/SCEN/YEAR/value
o = m.data["OUTPUT"].records.rename(columns={
    "uni_1": "VAR", "uni_2": "UNIT", "uni_3": "REGION", "uni_4": "ITEM", "uni_7": "SCEN", "uni_8": "YEAR"})
o = o[o.SCEN.isin(PUB) & (o.REGION != "World")].copy()
o["P"] = o.SCEN.map(PUB); o["value"] = o.value.astype(float)

# FISH_VAR_COMPARE: file/COUNTRY/FNF/FREG/PRIM/TYPE/MSC/BSC/SCEN/YEAR/value
fv = m.data["FISH_VAR_COMPARE"].records
fv.columns = ["file", "COUNTRY", "FNF", "FREG", "PRIM", "TYPE", "MSC", "BSC", "IEA", "YEAR", "v"]
fv = fv[fv.IEA.isin(PUB)].copy(); fv["P"] = fv.IEA.map(PUB); fv["v"] = fv.v.astype(float)


def ref_and_2050(df, key, valcol):
    """2020 BAU reference row + 2050 rows for the three scenarios."""
    rows = []
    ref = df[(df.P == "BAU") & (df.YEAR == "2020")].groupby(key)[valcol].sum()
    for it, v in ref.items():
        rows.append(("Reference 2020", it, v))
    for lab in ["BAU", "Barriers to Blue Growth", "Blue Transformation"]:
        g = df[(df.P == lab) & (df.YEAR == "2050")].groupby(key)[valcol].sum()
        for it, v in g.items():
            rows.append((lab, it, v))
    return rows


# ---- Fig 1: global demand (fish supply, livestock production), kt ----
fish = fv.groupby(["P", "YEAR"])["v"].sum().reset_index().rename(columns={"v": "value", "P": "PUBSCEN"})
fish["product_type"] = "Fish"
lp = o[(o.VAR == "Prod") & (o.ITEM.isin(LIVE))].groupby(["P", "YEAR"])["value"].sum().reset_index()
lp["product_type"] = "Livestock"; lp = lp.rename(columns={"P": "PUBSCEN"})
cols = ["product_type", "YEAR", "PUBSCEN", "value"]
pd.concat([fish[cols], lp[cols]]).to_csv(f"{OUT}/fig1_demand.csv", index=False)

# ---- Fig 2a: production by system ----
items2a = ["BVMEAT", "SGMEAT", "PGMEAT", "PTMEAT", "FRSHF", "SALMF", "MARNF", "CEPHF", "CRSTF", "DMRSF",
           "MLSCF", "PELGF", "SHRIF", "TUNAF", "FRSHW", "SALMW", "MARNW", "CEPHW", "CRSTW", "DMRSW",
           "MLSCW", "PELGW", "SHRIW", "TUNAW"]
p = o[o.VAR == "Prod"].copy(); p["ITEM"] = p.ITEM.str.upper(); p = p[p.ITEM.isin(items2a)]
pd.DataFrame(ref_and_2050(p, "ITEM", "value"),
             columns=["ALLSCEN3", "ITEM", "OUTPUT"]).to_csv(f"{OUT}/fig2a_production.csv", index=False)

# ---- Fig 2b: CATCH/CULTURE trends (model) + FAO history ----
fv["TYPE2"] = fv.TYPE.apply(lambda t: "CATCH" if t == "CATCH" else "CULTURE")
tr = fv.groupby(["P", "YEAR", "TYPE2"])["v"].sum().reset_index().rename(
    columns={"P": "PUBSCEN", "TYPE2": "TYPE", "v": "output"})
tr["Source"] = "This Study"
fao = pd.read_csv("data/fao_capture_vs_aquaculture.csv")
faorows = []
for _, r in fao.iterrows():
    faorows.append(("FAO", str(int(r.year)), "CULTURE", r.Aquaculture * 1000, "FAO"))
    faorows.append(("FAO", str(int(r.year)), "CATCH", r.Capture * 1000, "FAO"))
faodf = pd.DataFrame(faorows, columns=["PUBSCEN", "YEAR", "TYPE", "output", "Source"])
cols = ["PUBSCEN", "YEAR", "TYPE", "output", "Source"]
pd.concat([tr[cols], faodf[cols]]).to_csv(f"{OUT}/fig2b_trends.csv", index=False)

# ---- Fig 3a: feed crop usage ----
fe = o[o.VAR == "FEED"].copy(); fe["ITEM"] = fe.ITEM.str.upper()
pd.DataFrame(ref_and_2050(fe, "ITEM", "value"),
             columns=["ALLSCEN3", "ITEM", "OUTPUT"]).to_csv(f"{OUT}/fig3a_feed.csv", index=False)

# ---- Fig 3b: aquafeed in 2050 by ingredient ----
af = m.data["FISH_AQUAFEED_QUANTITY_COMPARE"].records
af.columns = ["f", "REG", "PROD", "MSC", "BSC", "SCEN", "YEAR", "v"]
af = af[af.SCEN.isin(PUB)].copy(); af["P"] = af.SCEN.map(PUB); af["v"] = af.v.astype(float)
pd.DataFrame(ref_and_2050(af, "PROD", "v"),
            columns=["PUBSCEN", "PRODUCT", "value"]).to_csv(f"{OUT}/fig3b_aquafeed.csv", index=False)

# ---- Fig 4: land / emissions / water / N impacts by region, 2050 ----
# LAND keeps four cover types; EMIS relabels to glob.plot's codes; WATR and FRTN
# are each summed to a single indicator row per region/scenario (glob.plot collapses
# them anyway).
EMIS_LAB = {"CROP": "CRP", "LIVE": "LSP", "LUC": "LUC"}
UNIT_FIX = {"1000 Ha": "1000 ha", "Mt CO2eq/yr": "Mt CO2e/yr"}  # match manuscript axis labels
COLS4 = ["VAR_ID", "VAR_UNIT", "REGION", "ITEM", "ALLSCEN3", "YEAR", "OUTPUT"]
o50 = o[o.YEAR == "2050"].copy()
o50["ALLSCEN3"] = o50.P.map(CODE)


def sel4(df):
    df = df.rename(columns={"VAR": "VAR_ID", "UNIT": "VAR_UNIT", "value": "OUTPUT"}).copy()
    df["VAR_UNIT"] = df["VAR_UNIT"].astype(str).replace(UNIT_FIX)
    return df[COLS4]


def collapse(var, unit_filter=None):
    d = o50[o50.VAR == var].copy()
    if unit_filter:
        d = d[d.UNIT == unit_filter]
    g = d.groupby(["REGION", "ALLSCEN3", "UNIT", "YEAR"], as_index=False)["value"].sum()
    g["VAR"] = var; g["ITEM"] = var
    return g


lnd = o50[(o50.VAR == "LAND") & (o50.ITEM.isin(["CrpLnd", "Forest", "GrsLnd", "NatLnd"]))].copy()
ems = o50[(o50.VAR == "EMIS") & (o50.ITEM.isin(EMIS_LAB))].copy(); ems["ITEM"] = ems.ITEM.map(EMIS_LAB)
fig4 = pd.concat([sel4(lnd), sel4(ems), sel4(collapse("WATR")), sel4(collapse("FRTN", "1000 t"))])
fig4.to_csv(f"{OUT}/fig4_impacts.csv", index=False)

# ---- Fig 5 land: cultivated + natural by region, 2050 (old-style item labels) ----
LAND5 = {"CrpLnd": "Managed Land", "Forest": "FOREST", "NatLnd": "OTHNATVEG"}
l5 = o50[(o50.VAR == "LAND") & (o50.ITEM.isin(LAND5))].copy()
l5["ITEM"] = l5.ITEM.map(LAND5); l5["REGION"] = l5.REGION.str.upper()
sel4(l5).to_csv(f"{OUT}/fig5_land.csv", index=False)

# ---- Fig 5 fish + Fig S1: fish production by region (country -> region via REGION_MAP) ----
rm = m.data["REGION_MAP"].records.iloc[:, [1, 2]]; rm.columns = ["REGION", "COUNTRY"]; rm = rm.drop_duplicates()
fvr = fv.merge(rm, on="COUNTRY", how="left")
fvr["ALLSCEN3"] = fvr.P.map(CODE); fvr["REGION"] = fvr.REGION.str.upper()
# Fig 5 fish: 2050 only
f5 = fvr[fvr.YEAR == "2050"].groupby(["REGION", "ALLSCEN3"], as_index=False)["v"].sum().rename(columns={"v": "OUTPUT"})
f5.dropna(subset=["REGION"]).to_csv(f"{OUT}/fig5_fish.csv", index=False)
# Fig S1: full time series, publication scenario labels
s1 = fvr.rename(columns={"P": "PUBSCEN"}).groupby(["REGION", "YEAR", "PUBSCEN"], as_index=False)["v"].sum()
s1 = s1.rename(columns={"v": "value"}).dropna(subset=["REGION"])
s1.to_csv(f"{OUT}/figS1_demand_regional.csv", index=False)

print("wrote figure-data CSVs to", OUT)
