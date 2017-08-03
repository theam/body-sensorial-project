# Body Sensorial Project

1. First, create a way of combining the data gathered for each patient, combining the CSV files into a single one. The FLAC files could be converted into a single 6-channel WAV file.nother approach could be to just compress all these files in the following ZIP file hierarchy (for patient 13 for example):

    a. 20.659698_-103.349609_170629_1754_00013.zip
    
        i. Acc00.csv
        
        ii. Gyr00.csv
        
        iii. Mag00.csv
        
        iv. Prs00.csv
        
        v. Tmp00.csv
        
        vi. Vas01...Vas06.flac
        
This would allow distributing raw data in an easier manner. Also, the ZIP file could contain the single CSV file and the single WAV file.

2. Develop a GUI application that allows having overlapped plots and toggling different layers to allow, for example, leaving the accelerometer and the sound to be visualized at the same time, while other data is hidden. Sound should be able to be seen not only as amplitude on time, but also as a spectrum of frequencies.

3. The third step suggests to visually explore the data. I understand that this is completely independent from the visualizer, and rather just pure exploratory work, not having to integrate this into the app developed on step two.

4. More pure exploratory process as in step 3

5. Same process as described in step 1?

As I see, this current process is just pure exploratory, and making a system out of this (except for step 2) is not a priority. The process requires pure DSP tools that are integrated with Python, and adding an interoperation layer with Haskell would only add overhead that is no use in pure exploration.
