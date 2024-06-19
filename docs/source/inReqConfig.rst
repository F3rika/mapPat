Configuration tables
--------------------

Configuration tables summarise available data and information for every country and are used to set-up and specify the correct configuration of widgets and plots. These tables are tab delimited, with the features of interest represented by the columns and their values reported in the rows. Some configuration tables are used to set the general layout (general), while others describe data available for a specific pathogen (pathogen specific).

Configuration tables collect information regarding data availability and key characteristics for data association and mapPat configuration, including:

+ The complete list of pathogens that can be analysed and their associated country default, time unit default and reference date for the computation of time offsets.
+ The complete list of countries that can be analysed.
+ The complete list of available input tables for each of the analysable countries.
+ The complete list of ISO-3 codes and associated administrative level (called ADM and required to draw choropleth maps).
+ The association between variants, lineages and their WHO classification.

More in detail the collection of configuration tables consists of:

#. ``PathogenSelection_ConfigTab.txt``
	| Sets country defaults (ISO-3 codes), time unit defaults and the reference date for the computation of time offsets for all the pathogens available in mapPat.
	
	+--------------+--------------+------------+--------------+------------+
	| PathogenName | PathogenAbbr | CountryDef | TimeUn       | refDate    |
	+==============+==============+============+==============+============+
	| SARS-CoV-2   | SARS-CoV-2   | ITA        | Weeks        | 2019-12-30 |
	+--------------+--------------+------------+--------------+------------+
	| mPox         | mPox         | USA        | Months       | 2022-01-01 |
	+--------------+--------------+------------+--------------+------------+
	| ...          | ...          | ...        | ...          | ...        |
	+--------------+--------------+------------+--------------+------------+
	| PathN        | PathAbbN     | ISO-3      | Weeks/Months | yyyy-mm-dd |
	+--------------+--------------+------------+--------------+------------+

#. ``countriesListTracker.txt``
	| Collects the complete list of the ISO-3 codes for all the countries with available data to be visualised using mapPat. A distinct table is generated for every pathogen for which data is available.
	
	+-------+
	|  ITA  |
	+-------+
	|  JPN  |
	+-------+
	|  GBR  |
	+-------+
	|  ...  |
	+-------+
	| ISO-3 |
	+-------+

#. ``inTab_avCheck.txt``
	| Associates ISO-3 country codes with the corresponding list of counts tables. In this table columns represent input files categories and rows represent countries. When available, file names are reported, otherwise NA (Not Available) is indicated. A specific table is produced for every pathogen included in mapPat.

	+---------+-----------------+---------------------+-----------------------+-----------------------+---------------------+
	| Country | Lin             | Mut                 | HeatChoroMap          | TotReg                | Var                 |
	+=========+=================+=====================+=======================+=======================+=====================+
	| ITA     | Epiweek.ITA.csv | ITA_muts_perLin.csv | HeatmapRegLin_ITA.csv | Total_ITA_regions.csv | Epiweek.Var.ITA.csv |
	+---------+-----------------+---------------------+-----------------------+-----------------------+---------------------+
	| JPN     | Epiweek.JPN.csv | JPN_muts_perLin.csv | HeatmapRegLin_JPN.csv | Total_JPN_regions.csv | NA                  |
	+---------+-----------------+---------------------+-----------------------+-----------------------+---------------------+
	| ...     | ...             | ...                 | ...                   | ...                   | ...                 |
	+---------+-----------------+---------------------+-----------------------+-----------------------+---------------------+
	| ISO-3   | FileName/NA     | FileName/NA         | FileName/NA           | FileName/NA           | FileName/NA         |
	+---------+-----------------+---------------------+-----------------------+-----------------------+---------------------+

#. ``CountryISOADM_AssocTab.txt``
	| Specifies the preferred administrative level (ADM according to RGeoboundaries) used to represent regions in maps. Includes also the corresponding country name and ISO-3 code.

	+-------------+-------------+-----------------+
	| Country     | Country_ISO | Country_ADM     |
	+=============+=============+=================+
	| Italy       | ITA         | ADM2            |
	+-------------+-------------+-----------------+
	| Japan       | JPN         | ADM1            |
	+-------------+-------------+-----------------+
	|  ...        | ...         | ...             |
	+-------------+-------------+-----------------+
	| CountryName | ISO-3       | ADM1/ADM2       |
	+-------------+-------------+-----------------+

#. ``LinVar_ConvTabTracker.txt``
	| Table for SARS-CoV-2 only.
	| Associates lineages with the corresponding variants and classification status (VOC, VOI, VBM), according to the WHO classification. Currently applicable only to SARS-CoV-2. Note that, after March 16th 2023, since the Omicron variant became the only variant of SARS-CoV-2 circulating worldwide, the World Health Organisation (WHO) revised this nomenclature, centering it around Omicron and its lineages (further information can be found in the `WHO official statement <https://www.who.int/news/item/16-03-2023-statement-on-the-update-of-who-s-working-definitions-and-tracking-system-for-sars-cov-2-variants-of-concern-and-variants-of-interest>`_). Consequently, ``LinVar_ConvTabTracker.txt`` contains a column to specifically indicate, only for lineages of the Omicron variant, those that could potentially impact Public Health (as per WHO guidelines) and are thus labelled as Variants Being Monitored (VBM).

	+-------------+--------------+----------------+------------+
	| Lin         | Variant      | Status         | isVBM      |
	+=============+==============+================+============+
	| U.1         | None         | None           | F          |
	+-------------+--------------+----------------+------------+
	| BA.2.86     | Omicron      | VOC            | T          |
	+-------------+--------------+----------------+------------+
	| BA.5.2.48   | Omicron      | VOC            | F          |
	+-------------+--------------+----------------+------------+
	| ...         | ...          | ...            | ...        |
	+-------------+--------------+----------------+------------+
	| LinN        | VarN         |VOC/VOI/VUM/None| T/F        |
	+-------------+--------------+----------------+------------+