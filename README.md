# Contaminant_indicators

Creating indicator data (levels/trends) for contaminants in biota on the Norwegian coast, based on both HI (previously NIFES) and NIVA data   

  * This is the source for the following blue mussel indicators (North Sea, Norwegian Sea and Barents Sea, resp.):   
  
      - https://miljostatus.miljodirektoratet.no/tema/hav-og-kyst/havindikatorer/nordsjoen-skagerrak/forurensende-stoffer/forurensning-i-blaskjell-i-nordsjoen/  
      - https://miljostatus.miljodirektoratet.no/tema/hav-og-kyst/havindikatorer/norskehavet/forurensende-stoffer/miljogifter-i-blaskjell-langs-kysten-av-norskehavet/  
      - https://miljostatus.miljodirektoratet.no/tema/hav-og-kyst/havindikatorer/barentshavet/forurensende-stoffer/forurensning-i-blaskjell-langs-kysten-av-nordland-troms-og-finnmark/
      
  * ...and for the following cod indicators (North Sea, Norwegian Sea and Barents Sea, resp.):    
  
      - https://miljostatus.miljodirektoratet.no/tema/hav-og-kyst/havindikatorer/nordsjoen-skagerrak/forurensende-stoffer/forurensning-i-torsk-i-nordsjoen/  
      - https://miljostatus.miljodirektoratet.no/tema/hav-og-kyst/havindikatorer/norskehavet/forurensende-stoffer/forurensning-i-kysttorsk-i-norskehavet/  
      - https://miljostatus.miljodirektoratet.no/tema/hav-og-kyst/havindikatorer/barentshavet/forurensende-stoffer/forurensning-i-torsk-i-barentshavet/  
      
  
* Created data (csv) are saved in the 'Data_export' folder and manually re-saved as Excel    
* Note that results use   
    - limits from `Input_data/Grenseverdier_fra_Sylvia.xlsx` 
    - trend functions from `00_Trend_functions.R`  
 Copied to `K:\Avdeling\Mar\NOG\JMGdata\Kart\Indikatorer` 

## Overview of excel files produced on K (last versions only)  

```
- Cod (North Sea, Norwegian Sea and Barents Sea, resp.):    
    - "K:\Avdeling\Mar\NOG\JMGdata\Kart\Indikatorer\2020\Grunnlagsdata\GaduMor_2020data_ver02.xlsx"
    - "K:\Avdeling\Mar\NOG\JMGdata\Kart\Indikatorer\2021\Norskehavet torsk\GaduMor_2021data_Norw_Sea_ver06.xlsx"
    - "K:\Avdeling\Mar\NOG\JMGdata\Kart\Indikatorer\2019\Grunnlagsdata\GaduMor_2020_withNIFES_ver06.xlsx"  

- Blue mussel (North Sea, Norwegian Sea and Barents Sea, resp.):  
    - "K:\Avdeling\Mar\NOG\JMGdata\Kart\Indikatorer\2020\Grunnlagsdata\MytiEdu_Snegl_2020_ver4.xlsx"
    - "K:\Avdeling\Mar\NOG\JMGdata\Kart\Indikatorer\2021\Norskehavet blåskjell\MytiEdu_Snegl_2020_ver6.xlsx"  
    - Barents Sea: ?

- Codes in excel sheets (see script 14 in various versions)   
    - trend: 0 = no trend calculated, 1 = zero time trend, 2 = up, 3 = down
    - Proref: Klasse 1-4 = Conc/PROREF ratio is <1, 1-2, 2-10, >10  
    - EQS and Mattrygghet: 1 = below, 2 = above  
```
    
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
    - HI data (prepared using scripts 11 and 12)   
    - NIVA data (prepared using script 13)   
    - HI + NIVA cod data are combined in script 14    
* For blue mussel and snails (only NIVA data) we use script 05   

### Norwegian Sea (Norskehavet) cod - data until 2020 (for HI, these data)       
* script 11, 12, 14 marked '2021_NorwSea'
* script 13 marked '2021'  
* Overview:  
    - 11: Prepares HI data by adding lat-long, Træna (data in 2018 only) is combined with Vikna to form Helgeland  
    - 12: HI data (Lofoten, Helgeland, Møre) medians and trends (using the same regression function as Milkys)   
    - 13: Gets NIVA data trends (10 year trend, directly from the big excel sheet from Milkys) + medians    
    - 14: Combines HI indicators (11 + 12) with NIVA indicators (13), adds Proref, EQS, food limit   
    
### Norwegian Sea (Norskehavet) blue mussel - data until 2020       
* only NIVA data - script 05 modified and halfway generalized
    - 05_Bluemussel_snail_NIVA_data_2021.Rmd - contains all parts of the procedure  





