Project provides simple dashboard which accomplishes the following:
 - Allows user to select specie (by scientific or vernacular name) as well as its observations time period.
 - Upon selection displays map (limited to Poland) with dots representing specie observation and dot size number of individuals spoteed within set time period.
 - Once map is rendered simple bar plot allows user to see timeline of observations. Plot is limited to set time period and can be additionally limited to area displayed on the map (which is interactive and can be zoomed in)

Now, project itself is structured as a package. Reason : I could not use scaffolding tools so defaulting to standard structure, which also covers documentation and tests is a good idea. Particularly, since there is not much code in it.
Each of the 3 functionalities/boxes of the app are split into separate Shiny module. Such approach is dictated by requirement, but also provides code requsability, modularization and testing via shiny::testServer(). As an afterhought :
writing those modules with shiny::testServer() usage in mind would allow for a better test coverage.
Its is worth noting that while app bases on 3rd party source data, that data was preprocessed (limited only to Poland and relecant columns) into .RDS file. It both eliminates some computation on app initialization and overall decreases its
memory consumption. As such : meddling with that .RDS file withough preserving its structure and consistency is not adviced. App is written with assumptions regarding that files' content and structure.

Notable features of the modules:
 - dataSubsetSelection:
   - User can choose between scientific and vernacular specie naming convention. Choice is done via checkbox, ensuring that specie selection widget stays in place with updated choices list and prompt.
     In reality there are two such widgets : one for each convention. Checkbox changes which is displayed and which is hidden. This is more optimal then re-rendering them all the time and looks smooth.
   - Scientific and vernacular specie names will back, no matter which naming convention is being displayed/used.
   - Time period for observation is not interactable untill specie is selected (selecion prompt is displayed until then).
   - Time period is set to maximum range for each specie selected by default. It can also not be extended bejond that range and will hanle empty date cases.
 - specieOccurrencesMap:
   - Made with leaflet.
   - Bound to latitute/longitude span that cover all observations within .RDS file.
   - Is replaced by selection prompt if no valid subset selection was made by the user.
   - Is rendered once and updated via proxy : this preserves map focus upon subset selection change.
 - specieOccurrencesTimelinePlot:
   - Made with plotly (which has nice reactivity support, even if not utilized here).
   - Is replaced by selection prompt if no valid subset selection was made by the user.
   - Has checkbox which limits its' data to what is visible on the map. User can zoom the map in and out.
  
As for extra features requested:
 - CSS is utilized to stop leaflet zoom options from covering dropdown menu of the above element.
 - JS is utilized in order to delay leaflet map update via proxy untill map itself is fully rendered.
 - Optimisation is done in following ways:
    - Initial preprocessing of the data-set into .RDS file
    - Limiting data supplied to the modules via args to neccessary minimum. It is important due to R taking deep copy of function arguments and Shiny modules having separate environment for each instance.
      Thus the more one limits data subset supplied to the module, the less RAM is consumed on within lifetime of said modules. Modules outputs are reduced in the similar manner.
    - Using data.table for data processing. Framework is fast overall and syntax used should utilize that.
