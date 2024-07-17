mapPat quick use guide
----------------------

Here is a quick guide on how to download and run a local installation of mapPat:

#. Go to the `mapPat GitHub page <https://github.com/F3rika/mapPat/tree/mapPat_Current>`_.
#. Download the repository and unzip it (if needed).
#. Enter the ``App`` folder of the repository.
#. Run mapPat using R or R Studio.
	| If your operating system is Microsoft Windows or MAC OSX open the ``mapPat_app.R`` script in R Studio and set the working directory to the ``App`` folder using the dedicated menu (Session > Set Working Directory > Choose Directory). Then run the ``mapPat_app.R`` script clicking on the “Run App” button in the top right corner of the “Source” panel.
 
	| If your operating system is MAC OSX or Linux and you prefer to use a shell environment, run the ``mapPat_appShell.sh`` script from the App folder. Make sure to make the script executable before using it. Then copy the generated url on any browser to correctly visualise mapPat.
 
	| Please mind that if mapPat is not run from the ``App`` folder:
 
		+ The ``mapPat_app.R`` file must be updated so that paths pointing to required ``.R`` files in the ``Scripts`` folder are correct.
		+ The ``mapPat_config.R`` file must be updated so that the ``inputs_path`` variable at line 44 of the file matches the path to the ``Input`` folder.

Please, when testing that mapPat is correctly installed and functioning use data from Italy (for SARS-CoV-2) or United States (for mPox).
