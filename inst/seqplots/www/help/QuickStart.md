Quick start demo
================

1. Start SeqPlots. Refer to **installation guides** for platform specific information. After successful initiation the web interface should automatically open in your default web browser. If you are using web server version just navigate your browser to the server address.

    ![The SeqPlots interface in web browser](img/01_QuickStart/01_QuickStart_01.png)

2. Upload feature (BED or GFF) and track (BigWig or WIG) files. They can be gzip compressed (e.g. file1.bed.gz). Press green "Add files..." button or just drag and drop files into the window. The ready to upload files will show up in upload window, where you select user name, reference genome and optionally add some comments.

    ![File upload panel](img/01_QuickStart/01_QuickStart_02.png)

3. When all is done press blue "Start upload" button. After upload and processing is done the green "SUCCESS" label should show. It means that file is on the registered and ready to use. Occasionally the file might be mot formatted properly or chromosome names might not agree with reference genome. In such case a verbose error will window appears and file as labeled as "ERROR". For further information please refer to [**errors chapter**](Errors).
    
    ![File upload progress infoermation](img/01_QuickStart/01_QuickStart_03.png) 


4. Dismiss upload window and press blue "New plot set" button on side panel. This will bring up file management window. In file management window select at least one file from "Features" tab and at least one file from "Tracks" or sequence motif(s). The sequence motifs and tracks can be processed and plotted together. Select files by clicking on file name, selected files will be highlighted.
    
    ![File management panel](img/01_QuickStart/01_QuickStart_04.png) 


5. After choosing files/motifs to plot, set up the processing options. You can find these in the button of plotting window.
    
    ![Plot set calculation options](img/01_QuickStart/01_QuickStart_05.png)


6. After options are set up press blue "Run calculation" button. This will dismiss the file management window and show processing message. Here you can observe the progress of the task and optionally cancel it if no longer required or you forgot to add some important file to the plot-set.
    
    ![Plot set calculation progress window](img/01_QuickStart/01_QuickStart_06.png)


7. After some time the calculation will finish (fingers crossed, without the error) and you will be able to see plot set array. In here you can choose which feature-track or feature-motif pairs to plot. Choose one or more checkboxes and press grey "Line plot" button (or hit RETURN from your keyboard).
    
    ![The plot selection grid](img/01_QuickStart/01_QuickStart_07.png)


8. Congratulation! Your First plot is complete, you can see the preview of it on the side panel.
    
    ![The plot preview panel](img/01_QuickStart/01_QuickStart_08.png)


9. You are able to set up labels, titles, font sizes, legends and many more on control panel tabs - please see **Plotting** chapter for details.
    
    ![Plot settings tabs](img/01_QuickStart/01_QuickStart_09.png)

10. By clicking the plot preview you can enlarge it for better view. When everything is ready you can get the plot as PDF by clicking green "Line plot" button just on the top of side panel.

    ![Average plot example](img/01_QuickStart/01_QuickStart_10.png)


11. You can also visualize the signal as a heatmap. Please note that heatmap plotting is possible only for a single feature file. 
It is possible to sort heatmaps based on mean row signals and/or cluster them using k-means algorithm, hierarchical clustering or self organising maps. 
To learn more about heatmaps see **Heatmaps** chapter.
    
    ![Heatmap settings tab](img/01_QuickStart/01_QuickStart_11.png)

12. Heatmaps can be downloaded sa PDFs using 'Heatmap' button just on the bottom of setup panel.
The small button, at the right sied of 'Heatmap' button  downloads cluster definition and sorting order.

    ![Heatmap example](img/01_QuickStart/01_QuickStart_12.png)

