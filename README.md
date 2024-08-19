# Dragons-not-lions

Data and code necessary for reproducing analyses in "Tyrannosaurs were dragons, not lions: food webs reveal the ecological role of apex predators in the Dinosaur Provincial Park biota



Data S1 - original matrices presents the consumer-resource matrices for each community in the study, from which edge lists were derived to measure trophic chain lengths in the 'cheddar' R packages.


Data S2 - Chain lengths contains the raw data necessary to conduct trophic level analyses in 'cheddar'. Each version of each community has an edge list 'EL' listing all links, and a corresponding 'nodes' file with properties for each node that composes the food web. 
    • Data S2A, S2B, S2C and S2D are derived datasets used to conduct the final analyses on prey-averaged trophic level (PATL) and mean chain lengths. Data S2A was assembled from all .csv files returning a node-chain length matrix (X.chains); Data S2B and Data S2C were assembled from mean chain lengths calculated based on Data S2A.
    • Dinosaur Park Formation (DPF) fully resolved food web versions only had their PATL calculated in Network3D, so the data generated with that software is provided instead of the 'cheddar' data.


Data S3 - Cenograms and histograms contains the raw data used to plot body size distributions for each community.


Data S4 - Biomass density contains all the files necessary to estimate dinosaur biomass in the DPF, including raw data on body mass, density and juvenile tyrannosaur and megaherbivore abundance (see Data S4C).
