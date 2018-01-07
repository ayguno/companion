###############################################################################
# Scraping the list of companion diagnostics along with drugs and indications
###############################################################################
import pandas as pd

url = "https://www.fda.gov/MedicalDevices/ProductsandMedicalProcedures/InVitroDiagnostics/ucm301431.htm"

companion = pd.read_html(url)

len(companion)

data_table = companion[0]

data_table.info()

essential_table = data_table.iloc[:,0:6]

essential_table.info()

herceptin = essential_table[essential_table['Drug Trade Name(Generic Name)'] == 'Herceptin(trastuzumab)']

herceptin.to_csv("herceptin.csv")
essential_table.to_csv("essential_table.csv")




