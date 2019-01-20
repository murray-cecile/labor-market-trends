# labor-market-trends

Ten years after the onset of the Great Recession, the national labor market is humming, with the official unemployment rate at 3.7 percent and wage growth slowly accelerating. However, the rosy national figures mask important variation in labor market trends across industries, occupations, demographic groups, and geographic areas. This variation should matter to policymakers who are concerned about the extent to which different sectors and groups have actually recovered, and the degree to which economic growth is broadly shared.

In this project, I plan to examine how the recession impacted these different groups as well as the extent to which the recovery is proceeding (or not) by looking at the pattern of job losses and job gains during and after the recession. This exploration could include calculating how long it took each industry or occupational category to return to pre-recession employment levels, comparing unemployment and labor force participation trends by demographic group, and mapping the geographic distribution of the job and wage gains during the economic recovery. 

#### Using blscrapeR

I rely on data from the Bureau of Labor Statisics (BLS) and use the keberwein's blscrapeR package to query the BLS API. You'll need to install blscrapeR if you don't have it already, and you'll need to create an API key. You can do that here: https://data.bls.gov/registrationEngine/

blscrapeR does not appear to be on CRAN but I installed the development version by running devtools::install_github("keberwein/blscrapeR"). Here's the package's GitHub repo: https://github.com/keberwein/blscrapeR
