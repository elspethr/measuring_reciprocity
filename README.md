# measuring_reciprocity

This repository contains the code to reproduce the analyses and figures/table in the paper:

Ready, E. and Power, E.A. 2021. Measuring Reciprocity: Double Sampling, Concordance, and Network Construction.

The networks from each dataset are generated in "processing" data files. These are aggregated and networks measures presented in the paper are calculated in the file "compiling.R". 

The R file "paper_figtabs.R" sources the compiling file and generates Figure 2–4 and Tables 1, 2, S2, and S3. File "microfinance_ERGMs.R" runs the multilevel ERGMs and produces Figure 5. Note that the ERGMs take considerable time to run.

The data used in the paper originate from the following sources:

##### Karnataka

Related publication: Jackson, M.O., Rodriguez-Barraquer, T. and Tan, X. 2012. [Social Capital and Social Quilts: Network Patterns of Favor Exchange.](https://www.aeaweb.org/articles?id=10.1257/aer.102.5.1857) *American Economic Review* 102 (5): 1857-97.

Metadata: https://web.stanford.edu/~jacksonm/Data.html

Raw network data: https://dataverse.harvard.edu/dataset.xhtml?persistentId=hdl:1902.1/21538

##### Tamil Nadu

Related publication: Power, E.A. and Ready, E. 2019. [Cooperation beyond consanguinity: Post-marital residence, delineations of kin, and social support among South Indian Tamils.](https://royalsocietypublishing.org/doi/10.1098/rstb.2018.0070) *Proceedings of the Royal Society B* 374(1780): 20180 070.

Requests to access these data should be directed to Eleanor A. Power, e.a.power@lse.ac.uk

##### Kangiqsujuaq

Related publication:  Ready, E. and Power, E.A., 2018. [Why Wage Earners Hunt: Food Sharing, Social Structure, and Influence in an Arctic Mixed Economy.](https://www.journals.uchicago.edu/doi/full/10.1086/696018) *Current Anthropology* 59(1): 74-97.

Requests to access these data should be directed to Elspeth Ready, elspeth_ready@eva.mpg.de

##### Mtakula datasets

Related publication: Kasper, C., and Borgerhoff Mulder, M. 2015. [Who helps and why.](https://www.jstor.org/stable/10.1086/683024?origin=JSTOR-pdf&seq=1#metadata_info_tab_contents) *Current Anthropology* 56(5): 701-732.

Requests to access these data should be directed to Monique Borgerhoff Mulder, mborgerhoffmulder@gmail.com
