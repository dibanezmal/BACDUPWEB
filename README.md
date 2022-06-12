# BACDUPWEB
Web application for visualizing duplicate genes of bacteria

## INTRODUCTION

BacDupWeb is a web application to study duplicates of genes in a bacetria genome. In this case the genome is composed of the main chromosome and the plasmids because these ones, the plasmids, contains a large number of duplicates. 

## WHERE TO FIND BACDUPWEB

Yo can find BacDupWeb in this public server of shinyapps.io

https://bacdupweb.shinyapps.io/BacDupWeb8/


## FILES TO TEST BACDUPWEB

To test the app you need two files of a bacteria, the dup_annot.csv file and the length.csv file.

You can find 5 examples in this repository, each one with one GCF code. The examples are in folder BACDUPWEB/arxius/

To test BacDupWeb you have to select the pair with the same GCF code.


## USER MANUAL

To use BacDupWeb is very easy. You only have to upload the pair of files of the same strain. As we said before, the files are the dup_annot.csv file and the length.csv file. Once the files are uploaded you have to push the button GO and the different parts of the data will appear at the screen.

Yo can begin again with the app, changing the uploaded strain with a new pair of files (dup_annot and length).

When the files are uploaded and the Go button pusehd you will see an horizontal bar with 4 options: Table, Circos, Plasmids and Data.

TABLE: The main table (file dup_annot) containing all duplicates founded by BacDup pipeline. You have 13 fields and usually one or few thousand records.
CIRCOS: The graphic view of the genome including plasmids and all the duplicates linked between them all along the genome in a circular plot. 
PLAMIDS: The information of length.csv file with the length of main chromosome and plasmids and the taxonomy data of the strain uploaded.
DATA: The detail of the duplicates in the uploaded strain. Basically the frequency of each number of duplicates from number two forward.

You can modify all the parameters on the left side of the app and check how the data in different options change depending on the selection.
In the top of the app we can see thee original data and the filtered data. 

You can see how table or circos change in real time depending if, for example, we include pseudogenes, number of duplicates, or the strand.

## THE TFM DOC OF BACDUPWEB

You can read or download the pdf memory of the TFM in the doc in folder memory.



## ALL THE DATA IN THIS REPOSITORY

In the main folder BACDUPWEB/ you will find the App.R file that contains BacDupWeb code. Al the code is in this file except the ext_functions.R code of external libraries that includes the Biocircos Plot function. The other files requiered are the images header_bdw.jpg.

