# mitre (development version)

# mitre 1.0.0
* Provide sample data sets pre-parsed using data-raw scripts
* Included standards: CPE, CVE, CWE, CAPEC, CAR, ATT&CK and SHIELD
* Public function to build a graph

# mitre 0.6.0
* Removed deprecated nodes in mitre network, kept in standards data frames
* Raw data is downloaded from github repo to avoid official servers rate limitations
* CPE source changed to JSON. It include more references to CVE.

# mitre 0.5.2
* New function as_igraph that transform mitre list of nodes and edges to igraph
* Rscript to easy automate building latest data sets
* CWE updated to parse v4.4
* Added some unit tests for parsed data sets

# mitre 0.5.1
* Added network relations schema
* Include CAR in network
* Multiple network cleansing
* Solved minor issues

# mitre 0.5.0
* Update documentation
* Added vignettes

# mitre 0.4.0
* Removed missing columns
* Filled missing values
* Normalized network colors and groups

# mitre 0.3.5
* Network improvements in nodes populating missing data and some enrichment.

# mitre 0.3.4
* Code style
* Update documentation for CRAN

# mitre 0.3.3
* New ATT&CK parser from latest definitions from official CTI repositories
* Normalized network adding all standards
* Improved nodes and edges details
* Network include deprecated observations with shadow and dashes

# mitre 0.3.2
 
* Developing functions to explore MITRE network

# mitre 0.3.1

* Normalized network nodes and edges.
* Added function to download latest tidy data sets.

# mitre 0.3.0
 
* Added CAR

# mitre 0.2.0

* Added ATT&CK, CVE, CPE, CWE and CAPEC

# mitre 0.1.1

* Added a `NEWS.md` file to track changes to the package.
