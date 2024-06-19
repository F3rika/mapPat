mapPat quick customisation guide
--------------------------------

It is also possible to use mapPat to analyse custom datasets; in order to do so it is essential to follow some simple key rules during input generation:

#. When producing counts tables for custom data make sure to follow the layout described at :doc:`inReqCounts`.
#. When producing pathogen specific configuration tables for custom data make sure to follow the layout described at :doc:`inReqConfig`.
#. Make sure to update the general configuration tables in order to include information about your data. This is required for mapPat to handle data properly. Follow the layout described at :doc:`inReqConfig` to avoid any issues.
#. If possible follow the same structure of data folders as in the `mapPat GitHub repository <https://github.com/F3rika/mapPat.git>`_, and put your custom data in a new folder. Doing so avoids the necessity of updating the ``mapPat_config.R`` file. Mind that the name of the newly added folder must match information from the ``PathogenSelection_ConfigTab.txt`` configuration table.
#. Otherwise it is required to update the ``mapPat_config.R`` file in order to match variables at lines 44-50 with the paths to folders where the new mapPat input is found.

Moreover, if custom data to be analysed using mapPat are:

+ SARS-CoV-2 metadata from `GISAID <https://weekly.chinacdc.cn/en/article/doi/10.46234/ccdcw2021.255>`_.
+ mPox or other pathogens’ (excluding SARS-CoV-2) metadata from `Nexstrain <https://nextstrain.org/>`_ or produced through the Nextstrain workflows. This metadata should also be accompanied by matched (by identifier) genomic sequences. Incomplete or low quality sequences should be excluded.

In Unix-like systems, mapPat input tables can be generated using a dedicated workflow available through the `mapPat GitHub repository <https://github.com/F3rika/mapPat.git>`_. This workflow, named ``mapPat_inTabGen_WF.py``, collects and organises a series of custom `Perl <https://www.perl.org/>`_ and `Python3 <https://www.python.org/>`_ scripts and generates all the pathogen specific tables required by mapPat.

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

+ `Perl <https://www.perl.org/>`_ (devt. vers. 5.10.1)
+ `Python3 <https://www.python.org/>`_ (devt. vers. 3.4.5)
+ `Nucmer <https://github.com/mummer4/mummer>`_ (devt. vers. 4.0.0beta2)

Here a quick guide on how to run ``mapPat_inTabGen_WF.py`` in order to generate mapPat pathogen specific input tables:

#. Go to the `mapPat GitHub page <https://github.com/F3rika/mapPat.git>`_.
#. Download the repository.
#. Enter the ``InputGeneration`` folder of the repository.
#. Run the ``mapPat_inTabGen_WF.py`` script using Python3.

| It is possible to test ``mapPat_inTabGen_WF.py`` using a pre-made dataset available through the `mapPat Github repository <https://github.com/F3rika/mapPat>`__. To download the test dataset, run the ``downloadTestData.sh`` script in the ``TestData`` subfolder from the ``InputGeneration`` folder of the repository (make sure to make the script executable before using it).
| Once the test dataset is available run the ``mapPat_inTabGen_WF.py`` in the ``InputGeneration`` folder  using the following command to generate input data for mapPat:

+ For SARS-CoV-2

::

 nohup python3 mapPat_inTabGen_WF.py -i ./TestData/SARS-CoV-2_metadataGISAID.tsv -pc ./Config -ps ./Scripts -p SARS-CoV-2 -db GISAID -o SARS-CoV-2_testData &

+ For mPox
	
::
	
 nohup python3 mapPat_inTabGen_WF.py -i ./TestData/mPox_metadataNextstrain.tsv -pc ./Config -ps ./Scripts -s ./TestData/mPox_sequences.fasta -rs ./TestData/mPox_reference.fasta -p mPox -db Nextstrain -o mPox_testData &