Plotting
========
> This section focuses on average (line) plots and options common between these and heatmaps. For heatmap options please refer to heatmap documentation.


Previewing plot
---------------

After calculating or loading a plot set, a **plot array** of checkboxes is displayed to select the desired pairs of features and tracks/motifs. Clicking on the column name (tracks/motifs) or row name (features) selects/deselects the whole column or row.   Clicking on top-left most cell of **plot array** toggles the selection of whole array.

> ![Plot preview plus `Line plot`, `Heatmap` and `refresh` buttons](img/01_QuickStart/01_QuickStart_08.png)

If at least one pair on **plot array** is selected pressing the `Line plot` button produces an average plot preview and the `Heatmap` button produces a heatmap preview. Alternatively, pressing the [RETURN] key will also produce the new selection and options. These operations are done automatically in reactive mode (see **Advanced options** chapter). Plots can be downloaded as PDF files using the Line plot and Heatmap buttons next to Download (at the top of the panel).

Below the plotting buttons are options for labeling plots and setting axes. On application start the first panel responsible for bringing file upload, management and plot set calculation modals is active. The further three panels hold common plot settings.
 
 
Titles and axis panel 
--------------------- 

> ![The view on titles and axis panel](img/07_03.png)

This panel groups settings influencing the plot main title, axis labels, various font sizes plus vertical and horizontal plot limits.

* `Title` - The main title of the plot, shown in top-center part of the figure; default empty
* `X-axis label` - Label shown below horizontal axis; default empty
* `Y-axis label` - Label shown below vertical axis; default empty
* `Title font size` - Font size of the title in points (point = ~1/72 an inch for standard A4 output); default 20 points
* `Labels font size` - Font size of axis labels in points; default 16 points
* `Axis font size` - Controls axis ticks font size, that is size of the numbers indicating position in base pairs on X-axis and means signal value on X-axis; default 14 points
* `Set X-axis limits` - Set hard plotting limits for X-axis; default values are whole range chosen during plot set calculation
* `Set Y-axis limits` - Set hard plotting limits for Y-axis; default values are a range between lowest and highest mean signal extended by error estimate

Guide lines and data scaling
----------------------------

> ![The view on guide lines and data scaling](img/07_04.png)

Controls in this panel controls the display of guide lines and error estimates, and allows to log scale the signal prior to plotting.

* `Transform signal` - if set to *`Log2 transform`* performs log2 transformation of the signal prior to plotting; default setting is *`Do not transform`*
* `Show vertical guide line` - show the vertical line at point 0 - beginning of the feature or midpoint and end of the pseudo-length scaled features (only for anchored plots); turn on by default
* `Show horizontal guide line` - show the horizontal line at user determined height; turn off by default
* `Show error estimates` - show error standard error and 95% confidence interval as fields, if turned off only the line representing the mean signal is shown; turn on by default

Keys, labels and colors panel
-----------------------------

> ![The view on keys, labels and colors panel (left). Color picker, label text input and Priority/Order checkboxes reviled on plot set grid (right).](img/07_05_06.png)

This panel groups two types of controls. `Colors`, `Label` and `Priority/Order` are a checkboxes revealing further controls on **plot set grid**, specific for a feature-track pair or sub-heatmap. `Show plot key`, `Show error estimate key` and `Legend font size` re global controls specific for average plots. Inputs on **plot set grid** do not have specific labels, but the tooltip explaining their meaning is shown on mouse cursor hover.

* `Colors` - checkboxes revealing a color picker on **plot set grid**. This input allows to control the colors of specific feature-track pair average plots or sub-heatmaps. In browser supporting the color picker 'e.g Chrome' the system dialog will show up. In other browsers (e.g. Firefox) the javaScript color picker will be initialized.
* `Label` - checkboxes revealing a label text input **plot set grid**. This controls the names shown on the **key** with average plots or the heatmap top labels.
* `Priority/Order` - checkboxes revealing numeric input on **plot set grid**. These number determine the order of average plots and hetamaps. Feature-track pair with the highest priority will be listed on the top of **key** for average plots and left-most for heatmaps.
* `Show plot key` - shows the key giving the color to feature-track pair label mapping. If turned on the additional drop-down allows to choose the position on the plot, top-right by default
* `Show error estimate key` - shows the key gexplaining the meaning of error fields. If turnedon the additional drop-down allows to choose the position on the plot, top-left by default 
* `Legend font size` - set the size of font used to plot the keys; 12 default 
