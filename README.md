# MS-Analysis

This a set of routines developed to more or less generalize input/output of data.

Why I developed this?

- I often deal with tons of tables in many different files that need to be systematically analyzed and or used in scripts and/or shiny applications. Without some form of metadata this can quite quickly become a mess
- Some data (Mass Spectrometry files) are not in tables but I extract tabular data from them (e.g. spectra & chromatograms). I need some system to be able to ingest, load this data into R w/o having to extract again and again: this often a tedious and long process.
- For the sake of reproducibility I need to be able to trace the origin of the data I use
- all data used in a script, shiny app needs to be together in a single (database) file
- The functionality of the routines needs to be easily changed (for new types of data, files etc)
- etc etc

Things I'm working on to develop this further:
- I would like to not have to have all data in memory all the time: an option where data is loaded when needed
- more/better plot options for mass spectrometry data (spectra & chromatograms)
- debugging

Eventually I will probably turn (part of) this into a package.

A manual for this routines is in the works!
