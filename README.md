# AST-GP75_VITEK_pdf_converter
Takes 2 PDF outputs from VITEK and converts it to a csv file

This script is for a very highly specialized task and therefore has specific instructions. Mainly, that this is for the antibiotic resistance evaluation of *Listeria monocytogenes* using the AST-GP75 VITEK card. This is because the VITEK has no breakpoints for *Listeria monocytogenes* at this time. 

To use this, first export the data under the identification as *Staphylococcus aureus*. Name the file YYYYMMDD_WRLP_number_staph.pdf where YYYYMMDD is the date that will end up in the final CSV and the number is the isolate number. The script takes the name of the isolate from the file name, so you may need to adjust the code for your data accordingly. Export data for all isolates with this file naming convention. Then export the data again under the identification *Enterococcus faecalis* with the file name convention YYYYMMDD_WRLP_number_ent.pdf. The reason for the ent PDF files is the script will take the MIC values for Gentamicihn High Level, Streptomycin High level, and Ampicillin. Put **all** of the PDF files in one folder. 

You will need to specify the filepath to the folder with all of the raw PDFs (line 21), what you want the output csv to be called (line 231) and the filepath to where you want it to export (line 232). 

This script is pretty well anotated, so if you need to adjust it for your specific data (specifically the isolate names) you should be able to. 

This software is Copyright (c) 2022 Oregon State University. All Rights Reserved. You may use, modify and redistribute this software under the terms of the GNU Affero General Public License version 3 published by the Free Software Foundation. If you need any other terms of use, please contact the author or advantage@oregonstate.edu
