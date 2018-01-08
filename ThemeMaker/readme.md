### Code::Blocks Theme Maker for Fortran Language
This folder contains a fortran code to create a Code::Blocks theme for Fortran language. It consist of

1. A Fortran code to create a theme.conf from a CSV file contains the RGB values for each syntax element
2. An csv file for setting the colors for each syntax element

### How to use
Simply open the csv file and set the elements colors as you like then
1. Run the CB_Fortran_ColorTheme.f90 and give the above csv as input
2. Choose a name for output conf
3. Import the created conf file into C::B configuration (default.conf)
4. Open C::B and from editor choose the new theme


### Automatic csv generation
An Excel (2013) file has been provided to visually set the colors of each syntax element.
This excel file is a macro-enabled and can export your color setting into a csv file
to be used above.
Excel file: CB_Fortran_ColorTheme_Maker.xlsm

## Screenshot

**Excel sheet for automatic generation of csv**

![Excel_Theme_Maker_CB_Fortran](colortheme_maker.png)


