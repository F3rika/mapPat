The `GeoAssociationGeneration` folder collects the workflow, scripts and configuration files that allow to generate the `allADM_CountryRegion_AssocTab.txt` table that summarises countries/regions association and corrects inconsistencies in geographic metada annotation. `allADM_CountryRegion_AssocTab.txt` is a configuration file required to correctly generate mapPat input tables.

Currently, the `mapPat_CouRegAssTabGen_WF.py` workflow can process:

+ SARS-CoV-2 metadata from *GISAID*.
+ mPox or other pathogens (excluding SARS-CoV-2) metadata from *Nextstrain* or produced through the *Nextstrain workflows*.

The workflow accepts as inputs:

+ A metadata table downloaded from either *GISAID* or *Nextstrain*. Flagged as `-i` or `--input_file`.
+ The path to the `Config` folder, which collects all configuration files required by the workflow. Flagged as `-pc` or `--path-config`. Defaults to `./Config`.
+ The path to the `Scripts` folder, which collects all scripts required by the workflow. Flagged as `-ps` or `--path-scripts`. Defaults to `./Scripts`.
+ The name of the database from which metadata is obtained. Can be either *GISAID* or *Nextstrain*. Flagged as `-db` or `--database`.

`mapPat_CouRegAssTabGen_WF.py` produces as final outputs the `allADM_CountryRegion_AssocTab.txt` table and a compressed directory named `CouRegAssTab_InterOut` that collects the intermediate files produced while running the workflow.
