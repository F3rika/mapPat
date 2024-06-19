Counts tables
-------------

| Counts tables summarise the number of genomes associated with a specific variant, lineage or mutation on a weekly or monthly basis. Counts are recorded at both national and regional level. Time is always represented as an offset with respect to a set date, for example for SARS-CoV-2 offsets are computed from 2019-12-30. The preferred time unit (weeks or months) for counts tables reflects data availability and sampling. mapPat counts SARS-CoV-2 sequencing data weekly, while data for other pathogens, such as mPox, are recorded on a monthly basis.
| Counts tables are space delimited and have a common layout. Weeks or months are represented on the columns, and data points for either variants, lineages or mutations are recorded in the rows. Some of these tables also collect other additional information (i. e. regions or locales names, lineages names and such) that are required to correctly group and represent data.
| A complete set of counts tables is generated for every country that meets the minimum requirements to be included in mapPat.

Briefly counts tables collect, from available data, information regarding:

+ The type of variants, lineages and/or mutations identified.
+ The country and region where they were observed.
+ The time points when they were observed.
+ The number of times they were registered at each time point calculated both nationally and regionally.

These data are essential to visualise and represent the circulation of pathogens variants, lineages and mutations using mapPat.

The following counts table are required/used by mapPat:

#. ``Epiweek.Var.COUNTRY.csv``
	| Collects the number of sequenced genomes for each named variant calculated per week/month at national level.

	+------------+-------+-------+-------+-------+
	|            | 1     | 2     | ...   | n     |
	+============+=======+=======+=======+=======+
	| Delta      | 4     | 120   | ...   | 50673 |
	+------------+-------+-------+-------+-------+
	| Omicron    | 12450 | 33810 | ...   | 96551 |
	+------------+-------+-------+-------+-------+
	| ...        | ...   | ...   | ...   | ...   |
	+------------+-------+-------+-------+-------+
	| VarN       | 4980  | 21695 | ...   | 34216 |
	+------------+-------+-------+-------+-------+

#. ``Epiweek.COUNTRY.csv``
	| Collects the number of sequenced genomes for each named lineage calculated per week/month at national level.

	+--------------+-------+-------+-------+-------+
	|              | 1     | 2     | ...   | n     |
	+==============+=======+=======+=======+=======+
	| "AY.20"      | 12    | 120   | ...   | 8467  |
	+--------------+-------+-------+-------+-------+
	| "BA.5.7"     | 0     | 1587  | ...   | 17478 |
	+--------------+-------+-------+-------+-------+
	| ...          | ...   | ...   | ...   | ...   |
	+--------------+-------+-------+-------+-------+
	| "LinN"       | 345   | 3400  | ...   | 10000 |
	+--------------+-------+-------+-------+-------+

#. ``HeatmapRegLin_COUNTRY.csv``
	| Collects the number of sequenced genomes for each named lineage calculated per week/month at regional level.

	+--------------+--------------+-------+-------+-------+-------+
	| lin          | reg          | 1     | 2     | ...   | n     |
	+==============+==============+=======+=======+=======+=======+
	| "B.1.1.7"    | "Puglia"     | 779   | 8576  | ...   | 14085 |
	+--------------+--------------+-------+-------+-------+-------+
	| "B.1.1.7"    | "Veneto"     | 242   | 69590 | ...   | 7189  |
	+--------------+--------------+-------+-------+-------+-------+
	| ...          | ...          | ...   | ...   | ...   | ...   |
	+--------------+--------------+-------+-------+-------+-------+
	| "B.1.1.7"    | "RegN"       | 1275  | 28900 | ...   | 950   |
	+--------------+--------------+-------+-------+-------+-------+
	| "AY.25"      | "Puglia"     | 6359  | 576   | ...   | 83573 |
	+--------------+--------------+-------+-------+-------+-------+
	| "AY.25"      | "Veneto"     | 494   | 7001  | ...   | 27348 |
	+--------------+--------------+-------+-------+-------+-------+
	| ...          | ...          | ...   | ...   | ...   | ...   |
	+--------------+--------------+-------+-------+-------+-------+
	| "AY.25"      | "RegN"       | 6323  | 57107 | ...   | 704   |
	+--------------+--------------+-------+-------+-------+-------+
	| ...          | ...          | ...   | ...   | ...   | ...   |
	+--------------+--------------+-------+-------+-------+-------+
	| "LinN"       | "Puglia"     | 33748 | 8606  | ...   | 539   |
	+--------------+--------------+-------+-------+-------+-------+
	| "LinN"       | "Veneto"     | 6174  | 65    | ...   | 76743 |
	+--------------+--------------+-------+-------+-------+-------+
	| ...          | ...          | ...   | ...   | ...   | ...   |
	+--------------+--------------+-------+-------+-------+-------+
	| "LinN"       | "RegN"       | 747   | 2720  | ...   | 29309 |
	+--------------+--------------+-------+-------+-------+-------+

#. ``COUNTRY_muts_perLin.csv``
	| Collects, for every lineage, the list of associated non-defining mutations (characterised by frequency >=1%, but <50%, in a specific country or locale for more than a week) and the number of sequenced genomes that present them, calculated per week/month at both national and regional level. Mutations are considered defining for a lineage when they are observed in the majority (>50%) of the genomes associated with that lineage.

	+--------------+--------------+--------------+-------+-------+-------+-------+
	| lineage      | mutation     | region       | 1     | 2     | ...   | n     |
	+==============+==============+==============+=======+=======+=======+=======+
	| "BA.2"       | "Spike_T732I"| "ITA"        | 9196  | 77381 | ...   | 508111|
	+--------------+--------------+--------------+-------+-------+-------+-------+
	| "BA.2"       | "Spike_T732I"| "Puglia"     | 981   | 2963  | ...   | 24954 |
	+--------------+--------------+--------------+-------+-------+-------+-------+
	| "BA.2"       | "Spike_T732I"| "Veneto"     | 39    | 3605  | ...   | 33421 |
	+--------------+--------------+--------------+-------+-------+-------+-------+
	| ...          | ...          | ...          | ...   | ...   | ...   | ...   |
	+--------------+--------------+--------------+-------+-------+-------+-------+
	| "BA.2"       | "Spike_T732I"| "RegN"       | 882   | 1787  | ...   | 29086 |
	+--------------+--------------+--------------+-------+-------+-------+-------+
	| "BA.2"       | "Spike_T95I" | "ITA"        | 69    | 7206  | ...   | 81526 |
	+--------------+--------------+--------------+-------+-------+-------+-------+
	| "BA.2"       | "Spike_T95I" | "Puglia"     | 905   | 5641  | ...   | 90163 |
	+--------------+--------------+--------------+-------+-------+-------+-------+
	| "BA.2"       | "Spike_T95I" | "Veneto"     | 2662  | 884   | ...   | 38115 |
	+--------------+--------------+--------------+-------+-------+-------+-------+
	| ...          | ...          | ...          | ...   | ...   | ...   | ...   |
	+--------------+--------------+--------------+-------+-------+-------+-------+
	| "BA.2"       | "Spike_T95I" | "RegN"       | 5559  | 5485  | ...   | 7434  |
	+--------------+--------------+--------------+-------+-------+-------+-------+
	| "AY.4"       | "Spike_F175L"| "ITA"        | 48776 | 97983 | ...   | 749833|
	+--------------+--------------+--------------+-------+-------+-------+-------+
	| "AY.4"       | "Spike_F175L"| "Puglia"     | 349   | 8516  | ...   | 35344 |
	+--------------+--------------+--------------+-------+-------+-------+-------+
	| "AY.4"       | "Spike_F175L"| "Veneto"     | 103   | 1673  | ...   | 73486 |
	+--------------+--------------+--------------+-------+-------+-------+-------+
	| ...          | ...          | ...          | ...   | ...   | ...   | ...   |
	+--------------+--------------+--------------+-------+-------+-------+-------+
	| "AY.4"       | "Spike_F175L"| "RegN"       | 147   | 3664  | ...   | 25991 |
	+--------------+--------------+--------------+-------+-------+-------+-------+
	| ...          | ...          | ...          | ...   | ...   | ...   | ...   |
	+--------------+--------------+--------------+-------+-------+-------+-------+
	| "LinN"       | "MutN"       | "ITA"        | 9750  | 13164 | ...   | 931399|
	+--------------+--------------+--------------+-------+-------+-------+-------+
	| "LinN"       | "MutN"       | "Puglia"     | 964   | 6455  | ...   | 81803 |
	+--------------+--------------+--------------+-------+-------+-------+-------+
	| "LinN"       | "MutN"       | "Veneto"     | 3676  | 799   | ...   | 48666 |
	+--------------+--------------+--------------+-------+-------+-------+-------+
	| ...          | ...          | ...          | ...   | ...   | ...   | ...   |
	+--------------+--------------+--------------+-------+-------+-------+-------+
	| "LinN"       | "MutN"       | "RegN"       | 223   | 8926  | ...   | 11090 |
	+--------------+--------------+--------------+-------+-------+-------+-------+

#. ``Total_COUNTRY_regions.csv``
	| Collects the total number of sequenced genomes calculated per week/month at regional level.

	+--------------+-------+-------+-------+-------+
	|              | 1     | 2     | ...   | n     |
	+==============+=======+=======+=======+=======+
	| "Puglia"     | 833   | 6317  | ...   | 28821 |
	+--------------+-------+-------+-------+-------+
	| "Veneto"     | 40404 | 2824  | ...   | 158   |
	+--------------+-------+-------+-------+-------+
	| ...          | ...   | ...   | ...   | ...   |
	+--------------+-------+-------+-------+-------+
	| "RegN"       | 4572  | 17902 | ...   | 62    |
	+--------------+-------+-------+-------+-------+

Where ``COUNTRY`` is a placeholder for countries ISO-3 codes.