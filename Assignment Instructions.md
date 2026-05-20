Replication Study Instructions
Create a GitHub repository where you will save your RProj and R script for this work, and make sure that it is set to public. (You will provide a link to this as part of your final grade). 
Read through Hanberry's article (ideally before Wednesday), paying close attention to his methodological choices.
Find Table 4, and note down the thresholds he is using for his sub-geographies (what is the population number, what are their classifications, and what are the units for his density measure?).
Choose a metropolitan area of interest (Lecture 5). Think about using a city with a sizable population (Pittsburgh, Charlotte, Austin). Be careful not to use a city too small (Eugene), and you are welcome to use a large city (LA), but the code may run slowly. Also, think about whether you want a monocentric city or not, or a city on a state's border, etc (either are fine but they will influence the story you see).
Using the 2020 census population count (Walker, Chapter 2), download the population figures by census tract.
You might download this as census tracts for the states covering the MSA, download the MSA's boundaries separetly, and then filter the tracts to the MSA (Walker, Chapter 7). 
Calculate the population density of each tract. You will need to mutate to find out the area of the tract, divide that area by however many square miles go into square kilometers, then you will need to divide the population of each tract by its area. You will then assign that density measure a label.
Mutate the population density figure and create a categorical variable that follows the landscape classification in Hanberry Table 4 (Walker Chapter 3.3.2).
You want two densities of urban, two of suburban, and assign everything else as exurban (wildland and inhabited are too small to care about their trends, so we will aggregate them as exurban). So, five landscapes within your MSA. 
Create a map of your city showing the reader where the subgeographies are located throughout the MSA (either with ggplot or tmap). Write a short descriptive paragraph (like an introduction) where you describe what's going on in this map and this city.
Then find the other variables of request in the ACS five-year survey ending in 2020 (Walker, Chapter 3). 
Any subtables within each variable are fine to use, but understand the limitations of your choices.  E.g., while you might want to know the median household income of young people, your city might not have that many young people, so the data may be limited.  
Create a graph of any kind (Walker Chapter 4), a map of any kind (Walker Chapter 6), and two population pyramids of the overall population by age, one for the overall urban population and one for the overall suburban population (Walker Chapter 4).
Export all figures using a ggsave/tmap save function and load them into a document (Walker Chapter 4.2.3).
Write a descriptive paragraph explaining why you think these landscapes look the way they are in the figures above (120 words).
Push your code to your GitHub repository.
 

Your assignment should include the following elements:

Introduction (state your choice of city, describe what is worth knowing, and describe where the urban/suburban areas are located)
A map of the urban and suburban space following the introduction.
One further map, chart, and pyramid of the variables.
A descriptive paragraph explaining what you see.
A link to your GitHub Repository where the code is located.  
