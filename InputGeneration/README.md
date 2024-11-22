The `InputGeneration` folder collects the workflow, scripts and configuration files that allow to generate the input tables required by mapPat starting from custom data. 

Currently, the `mapPat_inTabGen_WF.py` workflow can process:

+ SARS-CoV-2 metadata from *GISAID* or *Nextstrain*.
+ mPox or other pathogens (excluding SARS-CoV-2) metadata from *Nextstrain* or produced through the *Nextstrain workflows*. In this case metadata should be accompanied by matched genomic sequences. Incomplete or low quality sequences should be excluded.

The workflow accepts as inputs:

+ A metadata table downloaded from either *GISAID* or *Nextstrain*. Flagged as `-i` or `--input_file`.
+ The path to the `Config` folder, which collects all configuration files required by the workflow. Flagged as `-pc` or `--path-config`. Defaults to `./Config`.
+ The path to the `Scripts` folder, which collects all scripts required by the workflow. Flagged as `-ps` or `--path-scripts`. Defaults to `./Scripts`.
+ Sequencing data in *FASTA* format. Flagged as `-s` or `--seq`. Required only to analyse data from *Nextstrain*.
+ The reference sequence for the pathogen of interest in *FASTA* format. Flagged as `-rs` or `--refSeq`. Required only to analyse data from *Nextstrain*.
+ The name of the pathogen of interest. Flagged as `-p` or `--pathogen`.
+ The name of the database from which metadata is obtained. Can be either *GISAID* or *Nextstrain*. Flagged as `-db` or `--database`.
+ A string indicating the *BASENAME* of the final output. Flagged as `-o` or `--output_file`.

`mapPat_inTabGen_WF.py` produces as final outputs two compressed directories named `BASENAME_mapPatOut` and `BASENAME_mapPatInterOut` that respectively collect the tables used as mapPat input and the intermediate files produced while running the workflow. *BASENAME* is chosen by the user.

Please refer to the [mapPat quick customisation guide](https://mappat.readthedocs.io/en/latest/customGuide.html) for further information.
