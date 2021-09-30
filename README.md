# Contaminant_indicators

Creating indicator data (levels/trends) for contaminants in biota on the Norwegian coast, based on both NIFES and NIVA data  
  
* Created data (csv) are saved in the 'Data_export' folder and manually re-saved as Excel    
* Note that resuts use   
    - limits from `Input_data/Grenseverdier_fra_Sylvia.xlsx` 
    - trend functions from `00_Trend_functions.R`  
 Copied to `K:\Avdeling\Mar\NOG\JMGdata\Kart\Indikatorer` 

### Norskehavet - data until 2018, analysis 2019   
* Script 01-04    
    - Data kysttorsk NH nov. 2018.xlsx  
    - GaduMor_2018_withNIFES_ver4.xlsx   
* Final result:   
    - 'Forurensning i blåskjell langs kysten av Norskehavet_rev3001_2019.docx' (from mail form Norman 30.01.2019)  

### Barentshavet - data until 2019, analysis 2020   
* Script 11-14 marked "2020"   
    - Cd Pb torsk lever 2006-2019 HI.xlsx
    - Kvikksølv torskefilet Bhav 2006-2019.xlsx
    - POPs torskelever Bhav 2006-2019.xlsx

### Nordsjøen -  data until 2019, analysis 2021  
* scripts 05, 11, 12, 13, 14 marked "2021"     
* For cod, we use     
    - NIFES data (prepared using scripts 11 and 12)   
    - NIVA data (prepared using script 13)   
    - NIFES + NIVA cod data are combined in script 14    
* For blue mussel and snails (only NIVA data) we use script 05   

### Norwegian Sea (Norskehavet) cod - data until 2020       
* script 11, 12, 14 marked '2021_NorwSea'
* script 13 marked '2021'  
* Overview:  
    - 11: Prepares NIFES data by adding lat-long, Træna (data in 2018 only) is combined with Vikna to form Helgeland  
    - 12: NIFES data (Lofoten, Helgeland, Møre) medians and trends (using the same regression function as Milkys)   
    - 13: Gets NIVA data trends (10 year trend, directly from the big excel sheet from Milkys) + medians    
    - 14: Combines NIFES indicators (11 + 12) with NIVA indicators (13), adds Proref, EQS, food limit   
    
### Norwegian Sea (Norskehavet) blue mussel - data until 2020       
* only NIVA data - script 05 modified and halfway generalized





