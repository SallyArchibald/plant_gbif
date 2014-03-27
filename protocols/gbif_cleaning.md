GBIF data cleaning
==================

There has been ongoing discussion among all of the folks planning on using location data on how to best clean that data, deal with human impact, plantations, invasives, etc.
See comments in issue thread: https://github.com/Fireandplants/bigphylo/issues/1 and the email thread involving detailed all phylogeny group people.  The steps below are based on suggestions made by Sally, Caroline, Beth, Amy, Michelle, Dan and others.

Cleaning steps
--------------
1. Filter Location precision (lat/lon decimal places)
2. Filter on human impact  (method via Sally, Beth). Pick cutoff? 30?  http://sedac.ciesin.columbia.edu/data/set/wildareas-v2-human-footprint-geographic
3. Filter on transformed landscape (should be less restrictive than above, but intersect anyway): https://lpdaac.usgs.gov/products/modis_products_table/mcd12q1
4. Possible filter on invasiveness? fuzzy filter based on word matching in GBIF records (which fields?) Or on list of invasives?
   See Michelle Greve's email 3/07/2014 and other emails in thread. I'm less optimistic about this for the bigphylo project, but a simple version may be possible.  This may be quite viable for the detailed phylogeny projects.
5. Filter records near GBIF HQ - apparently some records have been assigned coordinates of GBIF when no other coordinate is available. 
6. Filter records in the ocean (obvious but important)
7. Preferentially use most recent records: For species with > 100 records in last 20 years use those records. For others, use all records.
8. Filter for records with >= 100 locations (relax slightly?)
9. Highlight records in which the coordinate doesn't match the recorded country and continent as this could indicate an error in the coordinate