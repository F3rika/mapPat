mapPat quick customisation guide
--------------------------------

It is also possible to use mapPat to analyse custom datasets; in order to do so it is essential to follow some simple key rules during input generation:

#. When producing counts tables for custom data make sure to follow the layouts described at :doc:`inReqCounts`.
#. When producing pathogen specific configuration tables for custom data make sure to follow the layouts described at :doc:`inReqConfig`.
#. Make sure to add to your dataset copies of each general configuration table updated so to include information about your data. This is required for mapPat to handle data properly. Follow the layouts described at :doc:`inReqConfig` to avoid any issues.
#. Please follow the same folders structure as in the precomputed mapPat datasets and put your custom data in a new set of folders. Mind that the name of the newly added dataset and folders must match information from the ``mapPat_inTabUpdates_Availability.txt`` and ``PathogenSelection_ConfigTab.txt`` configuration tables.
#. Store your custom dataset in the ``Datasets`` folder.
#. Remember to comment the code at line 42 of the ``mapPat_config.R`` file. To guarantee periodical updates mapPat automathically downloads a new copy of ``mapPat_inTabUpdates_Availability.txt`` from GitHub every time it is launched; commeting the abovementioned line allows to avoid the unwanted overwriting of your custom file.

Moreover, if custom data to be analysed using mapPat are:

+ SARS-CoV-2 metadata from `GISAID <https://weekly.chinacdc.cn/en/article/doi/10.46234/ccdcw2021.255>`_ or `Nexstrain <https://nextstrain.org/>`_.
+ mPox or other pathogens (excluding SARS-CoV-2) metadata from `Nexstrain <https://nextstrain.org/>`_ or produced through the Nextstrain workflows. In this case metadata should also be accompanied by matched (by identifier) genomic sequences. Incomplete or low quality sequences should be excluded.

In Unix-like systems, mapPat input tables can be generated using a dedicated workflow available through the `mapPat GitHub repository <https://github.com/F3rika/mapPat/tree/mapPat_Current/InputGeneration>`_. This workflow, named ``mapPat_inTabGen_WF.py``, collects and organises a series of custom `Perl <https://www.perl.org/>`_ and `Python3 <https://www.python.org/>`_ scripts and generates all the pathogen specific tables required by mapPat.

The ``mapPat_inTabGen_WF.py`` accepts as inputs:

+ A metadata table downloaded from either GISAID or Nexstrain. Flagged as ``-i`` or ``--input_file``.
+ The path to the ``Config`` folder, which collects all configuration files required by the workflow. Flagged as ``-pc`` or ``--path-config``. Defaults to ``./Config``.
+ The path to the ``Scripts`` folder, which collects all scripts required by the workflow. Flagged as ``-ps`` or ``--path-scripts``. Defaults to ``./Scripts``.
+ Sequencing data in ``FASTA`` format. Flagged as ``-s`` or ``--seq``. Required only to analyse data from Nextstrain.
+ The reference sequence for the pathogen of interest in ``FASTA`` format. Flagged as ``-rs`` or ``--refSeq``. Required only to analyse data from Nextstrain.
+ The name of the pathogen of interest. Flagged as ``-p`` or ``--pathogen``.
+ The name of the database from which metadata is obtained. Can be either ``GISAID`` or  ``Nextstrain``. Flagged as ``-db`` or ``--database``.
+ A string indicating the name of the final output. Flagged as ``-o`` or ``--output_file``.
	| ``mapPat_inTabGen_WF.py`` produces as final output two compressed directories named ``BASENAME_mapPatOut`` and ``BASENAME_mapPatInterOut`` that respectively collect the tables used as mapPat input and the intermediate files produced while running the workflow. ``BASENAME`` is chosen by the user.

For correct functioning ``mapPat_inTabGen_WF.py`` requires:

+ `Perl <https://www.perl.org/>`_ (devt. vers. 5.30.0)
+ `Python3 <https://www.python.org/>`_ (devt. vers. 3.8.10)
+ `Nucmer <https://github.com/mummer4/mummer>`_ (devt. vers. 4.0.0beta2)

Here a quick guide on how to run ``mapPat_inTabGen_WF.py`` in order to generate mapPat pathogen specific input tables:

#. Go to the `mapPat GitHub page <https://github.com/F3rika/mapPat.git>`_.
#. Download the repository.
#. Enter the ``InputGeneration`` folder of the repository.
#. Run the ``mapPat_inTabGen_WF.py`` script using Python3.

| Here are some brief examples of commands used to generate input data for mapPat:

+ For SARS-CoV-2 (GISAID metadata)

::

 nohup python3 mapPat_inTabGen_WF.py -i SARS-CoV-2_metadataGISAID.tsv -pc ./Config -ps ./Scripts -p SARS-CoV-2 -db GISAID -o SARS-CoV-2_metadataGISAID &

+ For SARS-CoV-2 (Nextstrain metadata)

::

 nohup python3 mapPat_inTabGen_WF.py -i SARS-CoV-2_metadataNextstrain.tsv -pc ./Config -ps ./Scripts -p SARS-CoV-2 -db Nextstrain -o SARS-CoV-2_metadataNextstrain &

+ For mPox
	
::
	
 nohup python3 mapPat_inTabGen_WF.py -i mPox_metadataNextstrain.tsv -pc ./Config -ps ./Scripts -s mPox_sequences.fasta -rs mPox_reference.fasta -p mPox -db Nextstrain -o mPox_metadataNextstrain &

| If you need help to generate a custom dataset contact us at `GitHub <https://github.com/F3rika/mapPat.git>`_.
| If you want your custom dataset, your publicly available data or a specific pathogen to be added to the `mapPat Zenodo repository <https://doi.org/10.5281/zenodo.14163899>`_ and made available to all fell free to `open an Issue <https://github.com/F3rika/mapPat/issues>`_.
