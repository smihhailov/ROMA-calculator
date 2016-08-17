# ROMA-calculator
Multilingual Risk of Ovarian Malignancy Algorithm (ROMA) calculator

This is an example of a calculator written for laboratory medicine needs in Shiny, which includes some features of interest:
- It is bilingual (as described in https://www.r-bloggers.com/another-take-on-building-a-multi-lingual-shiny-app/)
- Language translation has peculiar twist - this app manages to translate radioButtons labels which was impossible with the method abov
- Does all that is required:
  - takes into consideration CA-125 activity and HE4 concentration
  - takes into account status of menstrual cycle - premenopause/postmenopaus
  - adds official interpetation of the results
- Does a bit more:
  - changes background colour for low/high risk value
  - makes index value easily readable
- Currently limited to the results generated by Roche reagents (for a simple reason - I have them in my lab, but you are welcome to contribute for Abbott reagents)
- Last but not least - this app looks good
