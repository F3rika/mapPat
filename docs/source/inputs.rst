Required inputs
---------------

| Tracker Name input consists of a collection of tables in either ``.txt`` or ``.csv`` format.
| Tables are produced starting from metadata in either `HaploCoV <https://rdcu.be/dn7JY>`_ or `NextStrain <https://academic.oup.com/bioinformatics/article/34/23/4121/5001388>`_ format by applying a collection of `Perl <https://www.perl.org/>`_ and `Python3 <https://www.python.org/>`_ custom scripts.
| Input tables can be classified in two main categories:

+ **Counts tables**
   | Summarise the number of sequenced genomes for either variants, lineages or mutations calculated on a weekly basis at either natioinal or regional level.
   | Each analysed country has its specific set of tables.

+ **Configuration tables**
   Summarise some key characteristics associating groups of data and essential to correctly build specific widgets and/or plots.

Counts tables
-------------

| Counts tables are produced starting from availabe metadata applying custom scripts (see above) and summarise the number of sequenced genomes for either variants, lineages or mutations responding to specific characteristics of interest.
| Information regarding:

+ The kind of  variants, lineages and/or mutations identified.
+ The country and region where they were observed.
+ The time point, calculated as number of weeks from a fixed date (more details at INSERIRE LINK INTERNO), in which they were characterised.
+ The number of times they were registered at each time point calculated both nationally and regionally.

| Is collected from the initial input metadata and used to produce the data tables to be visualised through Tracker Name.
| The final collection of counts tables includes:

+ ``Epiweek.Var.COUNTRY.csv``
   Collects the number of sequenced genomes for each identified SARS-CoV-2 variant calculated on a weekly basis at national level.

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
   
+ ``Epiweek.COUNTRY.csv``
   Collects the number of sequenced genomes for each identified SARS-CoV-2 lineage calculated on a weekly basis at national level.
   
   +------------+-------+-------+-------+-------+
   |            | 1     | 2     | ...   | n     |
   +============+=======+=======+=======+=======+
   | AY.20      | 12    | 120   | ...   | 8467  |
   +------------+-------+-------+-------+-------+
   | BA.5.7     | 0     | 1587  | ...   | 17478 |
   +------------+-------+-------+-------+-------+
   | ...        | ...   | ...   | ...   | ...   |
   +------------+-------+-------+-------+-------+
   | LinN       | 345   | 3400  | ...   | 10000 |
   +------------+-------+-------+-------+-------+
   
+ ``HeatmapRegLin_COUNTRY.csv``
   Collects the number of sequenced genomes for each identified SARS-CoV-2 lineage calculated on a weekly basis at regional level.
   
   +------------+------------+-------+-------+-------+-------+
   | lin        | reg        | 1     | 2     | ...   | n     |
   +============+============+=======+=======+=======+=======+
   | B.1.1.7    | Puglia     | 779   | 8576  | ...   | 14085 |
   +------------+------------+-------+-------+-------+-------+
   | B.1.1.7    | Veneto     | 242   | 69590 | ...   | 7189  |
   +------------+------------+-------+-------+-------+-------+
   | ...        | ...        | ...   | ...   | ...   | ...   |
   +------------+------------+-------+-------+-------+-------+
   | B.1.1.7    | RegN       | 1275  | 28900 | ...   | 950   |
   +------------+------------+-------+-------+-------+-------+
   | AY.25      | Puglia     | 6359  | 576   | ...   | 83573 |
   +------------+------------+-------+-------+-------+-------+
   | AY.25      | Veneto     | 494   | 7001  | ...   | 27348 |
   +------------+------------+-------+-------+-------+-------+
   | ...        | ...        | ...   | ...   | ...   | ...   |
   +------------+------------+-------+-------+-------+-------+
   | AY.25      | RegN       | 6323  | 57107 | ...   | 704   |
   +------------+------------+-------+-------+-------+-------+
   | ...        | ...        | ...   | ...   | ...   | ...   |
   +------------+------------+-------+-------+-------+-------+
   | LinN       | Puglia     | 33748 | 8606  | ...   | 539   |
   +------------+------------+-------+-------+-------+-------+
   | LinN       | Veneto     | 6174  | 65    | ...   | 76743 |
   +------------+------------+-------+-------+-------+-------+
   | ...        | ...        | ...   | ...   | ...   | ...   |
   +------------+------------+-------+-------+-------+-------+
   | LinN       | RegN       | 747   | 2720  | ...   | 29309 |
   +------------+------------+-------+-------+-------+-------+
   
+ ``COUNTRY_muts_perLin.csv``
   Collects, for each identified SARS-CoV-2 lineage, the list of associated non-defining mutatations and the number of sequenced genomes that present them, calculated on a weekly basis at both national and regional level.
   
   +------------+------------+------------+-------+-------+-------+-------+
   | lineage    | mutation   | region     | 1     | 2     | ...   | n     |
   +============+============+============+=======+=======+=======+=======+
   | BA.2       | Spike_T732I| ITA        | 9196  | 77381 | ...   | 508111|
   +------------+------------+------------+-------+-------+-------+-------+
   | BA.2       | Spike_T732I| Puglia     | 981   | 2963  | ...   | 24954 |
   +------------+------------+------------+-------+-------+-------+-------+
   | BA.2       | Spike_T732I| Veneto     | 39    | 3605  | ...   | 33421 |
   +------------+------------+------------+-------+-------+-------+-------+
   | ...        | ...        | ...        | ...   | ...   | ...   | ...   |
   +------------+------------+------------+-------+-------+-------+-------+
   | BA.2       | Spike_T732I| RegN       | 882   | 1787  | ...   | 29086 |
   +------------+------------+------------+-------+-------+-------+-------+
   | BA.2       | Spike_T95I | ITA        | 69    | 7206  | ...   | 81526 |
   +------------+------------+------------+-------+-------+-------+-------+
   | BA.2       | Spike_T95I | Puglia     | 905   | 5641  | ...   | 90163 |
   +------------+------------+------------+-------+-------+-------+-------+
   | BA.2       | Spike_T95I | Veneto     | 2662  | 884   | ...   | 38115 |
   +------------+------------+------------+-------+-------+-------+-------+
   | ...        | ...        | ...        | ...   | ...   | ...   | ...   |
   +------------+------------+------------+-------+-------+-------+-------+
   | BA.2       | Spike_T95I | RegN       | 5559  | 5485  | ...   | 7434  |
   +------------+------------+------------+-------+-------+-------+-------+
   | AY.4       | Spike_F175L| ITA        | 48776 | 97983 | ...   | 749833|
   +------------+------------+------------+-------+-------+-------+-------+
   | AY.4       | Spike_F175L| Puglia     | 349   | 8516  | ...   | 35344 |
   +------------+------------+------------+-------+-------+-------+-------+
   | AY.4       | Spike_F175L| Veneto     | 103   | 1673  | ...   | 73486 |
   +------------+------------+------------+-------+-------+-------+-------+
   | ...        | ...        | ...        | ...   | ...   | ...   | ...   |
   +------------+------------+------------+-------+-------+-------+-------+
   | AY.4       | Spike_F175L| RegN       | 147   | 3664  | ...   | 25991 |
   +------------+------------+------------+-------+-------+-------+-------+
   | ...        | ...        | ...        | ...   | ...   | ...   | ...   |
   +------------+------------+------------+-------+-------+-------+-------+
   | LinN       | MutN       | ITA        | 9750  | 13164 | ...   | 931399|
   +------------+------------+------------+-------+-------+-------+-------+
   | LinN       | MutN       | Puglia     | 964   | 6455  | ...   | 81803 |
   +------------+------------+------------+-------+-------+-------+-------+
   | LinN       | MutN       | Veneto     | 3676  | 799   | ...   | 48666 |
   +------------+------------+------------+-------+-------+-------+-------+
   | ...        | ...        | ...        | ...   | ...   | ...   | ...   |
   +------------+------------+------------+-------+-------+-------+-------+
   | LinN       | MutN       | RegN       | 223   | 8926  | ...   | 11090 |
   +------------+------------+------------+-------+-------+-------+-------+
   
+ ``Total_COUNTRY_regions.csv``
   Collects the total number of sequenced genomes calculated on a weekly basis at regional level.
   
   +------------+-------+-------+-------+-------+
   |            | 1     | 2     | ...   | n     |
   +============+=======+=======+=======+=======+
   | Puglia     | 833   | 6317  | ...   | 28821 |
   +------------+-------+-------+-------+-------+
   | Veneto     | 40404 | 2824  | ...   | 158   |
   +------------+-------+-------+-------+-------+
   | ...        | ...   | ...   | ...   | ...   |
   +------------+-------+-------+-------+-------+
   | RegN       | 4572  | 17902 | ...   | 62    |
   +------------+-------+-------+-------+-------+
   
A complete set of the tables listed above is generated for each country that respects the minimum requirements to be included in Tracker Name.

.. note::
   The expression ``COUNTRY`` in the list above is replace by the ISO code of the country in the actual input files.

Configuration tables
--------------------

| Configuration tables were produced with both manual and automated means at the beginning of Tracker Name development and are periodically updated.
| These tables collect key characteristics of the data and how they associate different groups of data.
| Information regarding:

+ The complete list of countries that can be analysed.
+ The complete list of ISO-3 codes and administrative level (ADM, required to design choropleth maps) associated to each analysed country.
+ The association between variants, lineages and their WHO classification.

| Is collected in the configuration tables and used to correctly build specific widgets and/or plots.
| The final collection of configuration tables includes:

+ ``countriesListTracker.txt``
   Collects the complete list of the ISO codes for all the countries that can be analysed using Tracker Name.
   
   +------+
   | ITA  |
   +------+
   | JPN  |
   +------+
   | GBR  |
   +------+
   | ...  |
   +------+
   | CouN |
   +------+
   
+ ``CountryISOADM_AssocTab.txt``
   Collects the complete list of ISO-3 codes and preferred administrative level (ADM, required to design choropleth maps) for each country that can be analysed using Tracker Name.
   
   +------------+-------------+-----------------+
   | Country    | Country_ISO | Country_ADM     |
   +============+=============+=================+
   | Italy      | ITA         | ADM2            |
   +------------+-------------+-----------------+
   | Japan      | JPN         | ADM1            |
   +------------+-------------+-----------------+
   | ...        | ...         | ...             |
   +------------+-------------+-----------------+
   | CouN       | CouISO      | ADM1/ADM2       |
   +------------+-------------+-----------------+
   
+ ``variantsConverterTracker.txt``
   Collects the complete list of SARS-CoV-2 lineages identified worldwide and allows to associate them with the corresponding variant and WHO classification.
   
   +------------+-------------+------------+------------+
   | Lin        | Variant     | VOC-VOI-VUM| OmiGroup   |
   +============+=============+============+============+
   | AT.1       | vumAT.1     | VUM        | None       |
   +------------+-------------+------------+------------+
   | BA.2.2     | Omicron     | VOC        | OmicronVOC |
   +------------+-------------+------------+------------+
   | ...        | ...         | ...        | ...        |
   +------------+-------------+------------+------------+
   | LinN       | VarN        | VOC/VOI/VUM| OmiG/None  |
   +------------+-------------+------------+------------+
   
   .. note::
      The increasing number of lineages falling into the omicrom variant group and the absolute prevalence of this variant worldwide prompted the World Health Organisation (WHO) to completely reconsider its nomeclature centering it around omicron and its lineages (further information can be found here, in the `WHO official statement <https://www.who.int/news/item/16-03-2023-statement-on-the-update-of-who-s-working-definitions-and-tracking-system-for-sars-cov-2-variants-of-concern-and-variants-of-interest>`_). Consequently, in order not to lose information about the classification of variants previously to the WHO decision, ``variantsConverterTracker.txt`` was updated including a fourth column including variants classification with respect to the sole omicron variant.

All the tables listed above are periodically updated by integrating them with novel information from metadata.
