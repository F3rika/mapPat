Required input
---------------

mapPat’s input consists of a collection of tables in simple text format that summarise key information about viral circulation and evolution at both national and regional level.

Tables are produced by processing publicly available data, downloaded from either `NextStrain <https://academic.oup.com/bioinformatics/article/34/23/4121/5001388>`_ or `GISAID <https://weekly.chinacdc.cn/en/article/doi/10.46234/ccdcw2021.255>`_, using `HaploCoV <https://rdcu.be/dn7JY>`_ together with a collection of custom `Perl <https://www.perl.org/>`_ and `Python3 <https://www.python.org/>`_ scripts.

Only countries for which more than 1000 distinct genome sequences are available are considered by mapPat. A total of five tables (Counts tables) are computed for every country that meets this minimum requirement. Five additional tables (Configuration tables) are used to summarise available data and set the configuration of the tool’s widgets and tabs.