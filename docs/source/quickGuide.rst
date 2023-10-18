Tracker Name quick use guide
----------------------------

Here is a quick guide on how to run Tracker Name:

+ Go to the `Tracker Name GitHub page <https://github.com/F3rika/CorGAT-tracker>`_.
+ Download the repository.
+ Enter the ``AppScripts`` folder.
.. warning::
   | If Tracker Name is not run from the ``AppScripts`` folder you must update the ``tracker_config.R`` file accordingly.
   | The ``inputs_path`` variable at line 30 of the file, in fact, should match the path to the ``InputData`` folder.
  
::
	  
   inputs_path <- “path/to/required/folder/InputData/”

+ Run the ``tracker_app.R`` script using R or R Studio.
.. warning::
   When running the ``tracker_app.R`` script using R, to correctly visualise Tracker Name copy the generated url on any browser.

.. note::
   We recommend testing that Tracker Name is correctly installed and functioning using data from Italy or Japan.