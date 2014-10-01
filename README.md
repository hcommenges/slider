## User guide

### Description
Interactive tool for exploratory analysis of longitudinal data.

------
### License

GNU AFFERO GENERAL PUBLIC LICENSE

------
### Citation

Please cite the following references in publications:

- **For the slide plot and the SLIDER application:** Commenges H., Pistre P., Cura R. (2014) SLIDER: Software for LongItudinal Data Exploration with R, *Cybergeo: European Journal of Geography*. 
- **For the parallel coordinates plot:** Bürgin R., Ritschard G. (2012) Categorical parallel coordinate plot, *LaCOSA Lausanne Conference On Sequence Analysis*, University of Lausanne, June 6th-8th (Poster).
- **For transition rate, index plot, frequency plot and distribution plot:** Gabadinho A., Ritschard G., Mueller N.S., Studer M. (2011) Analyzing and Visualizing State Sequences in R with TraMineR, *Journal of Statistical Software*, 40(4), pp.1-37.


------
### How to

#### Data requirements

- **Format and variables**. The data format is the so-called *STate-Sequence* (STS) format (see references below, Gabadinho *et al.*). Each individual may be described with two kinds of variables: `Factor variables` are categorical variables (nominal or ordinal) such as sex, profession, age groups, etc. `Time variables` are categorical variables characterizing the following states for each individual at several time steps.

- **Time variables**. Can be integer or character. The only requirement is that the format must be consistent with a unique `alphabet` for the whole time steps, i.e. each field must be filled with the same type (integer or character) describing the same states.
  
- **Factor variables**. Can be integer or character. If you want to draw readable labels on the plots you should fill these fields with short character labels.

- **Alphabet**. The alphabet is the list of distinct modalities found in the time variables. The maximum length of the alphabet is set to 12 distinct modalities. Beyond this value, the plots are no longer readable.

------
#### Import-Export options

- **Load example data**. Load example data from the study by McVicar and Anyadike-Danes (2002) on transition from school to work. This dataset is used in the TraMineR package (mvad) and in the reference paper (see `Citation` below).

- **Import CSV file**. The default parameters are comma as separator and no quoting character. You can change this setting, before uploading the CSV file, by checking the `CSV options` checkbox.

- **Choose time steps**. Mandatory : choose two or more fields containing the time steps you want to explore.

- **Select weighting variable**. Optional : choose a weighting numerical variable, it will be taken into account for the computation and the graphical outputs.

- **Choose factors**. Optional : choose a field containing a factor if you want to explore the patterns for different groups (for example, the sex of the individuals).

- **Choose a group**. Optional : choose the value assigned to the group for which you want to draw the plots.

- **Download**. Plots can be downloaded with the `Download` button. It creates a SVG file with a default size of 20cm (w) x 15 cm (h). You can set the size from 1 to 30 cm. Be aware that index plots can be very heavy if there is a lot of time steps and/or sequences.


------
#### Transition rate

- **Frequencies**. Choose whether to show `absolute frequencies` of transitions, `row percentages` or `column percentages`.


------
#### Index, frequency and distribution plots

- **Draw border**. Choose whether or not to draw borders between successive states.
- **Index of sequences**. Choose which sequences to draw: you can select set any set of sequences from 1 to max.

------
#### Parallel coordinates plot for sequence data

- **Embedding**. The embedding options of the TraMineR function are fixed to "non-embeddable" for the type of sequence and "most-frequent" for the embedding method.

- **Squared symbol size**, **Line width** and **Translation zone**. Choose the size of the squared symbols drawn at the origin and destination of each segment, choose the line width factor and the size of the grey squares containing the squared symbols.

------
#### Slide plot options

- **Download slide plot function**. The implementation of the slide plot may be downloaded with the dowload button (side panel). By clicking this button, you download a text file with .R extension which can be executed or sourced.

- **Threshold**. The slide plot often draws a huge number of micro variations which may harm the readability. Values under the set `threshold` will be treated as residuals. The threshold value is considered as the minimal value.

- **Mask**. After setting the `threshold`, you can choose to draw or to mask the micro variations. If you mask them, they won't appear, if you draw them, they will appear in light grey.

- **Show frequencies**. Use this checkbox to show or mask the frequency of transitions. By checking the box, you show all the frequencies above the `threshold`.

- **Minimal thickness**. The thickness of each segment is proportional to the frequency of the aggregated trajectories it represents. The thickness depends on three parameters: the `threshold` parameter, the maximal value of the dataset and the `minimal thickness` parameter. The `minimal thickness` is assigned to the threshold value and the maximal thickness is assigned to the maximal value. Fot that reason, you can modify the overall appearance using both parameters: `threshold` and `minimal thickness`.

------
