"""
About this code --

Script description: Function that downloads UN Comtrade data for a given year, 
a given commodity, and a given trade flow.

Usage: ./get_UN_Comtrade_data.py

Last update: Jan 18 2024

Author: Valentin Mathieu

Institutions: 
1 Université de Lorraine, AgroParisTech, INRAE, UMR SILVA
2 Université de Lorraine, Université de Strasbourg, AgroParisTech, 
CNRS, INRAE, BETA, 54000 Nancy, France
"""

try:
    import comtradeapicall
    print("Package 'comtradeapicall' has been loaded.")
except ImportError:
    import sys
    import subprocess
    subprocess.check_call([sys.executable, '-m', 'pip', 'install', 'comtradeapicall'])
    import comtradeapicall
    print("Package 'comtradeapicall' has been installed and loaded")

def get_UN_Comtrade_data(apikey, year, cmd, flow):
    '''
    Function that downloads UN Comtrade data for a given year, a given 
    commodity, and a given trade flow. Need an API key.

    Parameters
    ----------
    apikey : sting
        The API subscription key to download data.
    year : integer
        The year of trade.
    cmd : string
        The commodity code.
    flow : string
        The trade flow to download (import, export, re-import, re-export...).

    Returns
    -------
    None.

    '''
    
    data = comtradeapicall.getFinalData(
                apikey,
                typeCode='C', 
                freqCode='A', 
                clCode='HS', 
                period=year,
                reporterCode=None, 
                cmdCode=cmd,
                flowCode=flow,
                partnerCode=None,
                partner2Code=None,
                customsCode=None,
                motCode=None,
                format_output='JSON',
                breakdownMode='classic',
                includeDesc=True
                )
    
    # time.sleep(0.5) # Pause in download if overrinding
    
    return data