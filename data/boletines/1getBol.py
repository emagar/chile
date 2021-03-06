# -*- coding: utf-8 -*-
# Code prepared by Eric Magar, 30oct2014 (updated 9nov2017)
# email emagar at gmail dot com

# ## Install Selenium driver for Firefox
# ## See https://pypi.python.org/pypi/selenium
# from selenium import webdriver

# browser = webdriver.Firefox()
# browser.get('http://seleniumhq.org/')

## Urgencias and vetos reached with Selenium
## from http://stackoverflow.com/questions/6116023/screenscaping-aspx-with-python-mechanize-javascript-form-submission
import sys # to get arguments from command line
from selenium import webdriver # navigates javascript-heavy pages easily
from selenium.common.exceptions import NoSuchElementException
import codecs # eases character encoding issues
import csv # handles csv files
from collections import defaultdict  # used to prepare csv data
import os.path # used to check file existence
import time # to add date/time of download

# UNICODE HOWTO IN PYTHON: http://stackoverflow.com/questions/9942594/unicodeencodeerror-ascii-codec-cant-encode-character-u-xa0-in-position-20
# Also http://nedbatchelder.com/text/unipain.html

# # how to run this script from within python
# with open('1getBol.py') as f:
# 	this = f.read()
# exec(this)

id = sys.argv[1]          # passes the argument given when invoking this code in console
bl = sys.argv[2]          # passes the argument given when invoking this code in console
#id = '1252'    # debug: case has urgencias
#bl = '1-07'    # debug: case has urgencias
#id = '2318'    # debug: case has no urgencia
#bl = '1141-13' # debug: case has no urgencia
#id = '8289'    # debug: case has autotes
#bl = '7890-03' # debug: case has autores
filename = 'bol' + bl + '.txt'

# ## Summary and hitos may be reached with requests library --- get full code, with all info raw
# import requests
# url = 'http://camara.cl/pley/pley_detalle.aspx'
# parametros = {'prmID': id, 'prmBL': bl}
# r = requests.get(url, params = parametros)
# #hitos = r.text
# #print r.url
# #print(r.headers)
# # stream content to a file
# with open(filename, 'a') as zeFile:
#    for chunk in r.iter_content():
#       zeFile.write(chunk)

# some handy functions
def foundNoRecordMessage():
    what = 'h2.importante'
    try: driver.find_element_by_css_selector(what)
    except NoSuchElementException: return False
    return True

def getNoRecordMessage():
    what = 'h2.importante'
    return driver.find_element_by_css_selector(what).text.encode('utf-8')

def getSummaryRecord():
    what = 'table.tabla'
    return driver.find_element_by_css_selector(what).text.encode('utf-8')

def foundHitosRecord():
    what = 'ctl00_mainPlaceHolder_grvtramitacion'
    try: driver.find_element_by_id(what)
    except NoSuchElementException: return False
    return True

def getHitosRecord():
    what = 'ctl00_mainPlaceHolder_grvtramitacion'
    return driver.find_element_by_id(what).text.encode('utf-8')

def foundInformesRecord():
    what = 'ctl00_mainPlaceHolder_grvinformes'
    try: driver.find_element_by_id(what)
    except NoSuchElementException: return False
    return True

def getInformesRecord():
    what = 'ctl00_mainPlaceHolder_grvinformes'
    return driver.find_element_by_id(what).text.encode('utf-8')

def foundUrgenciasRecord():
    what = 'ctl00_mainPlaceHolder_grvurgencias'
    try: driver.find_element_by_id(what)
    except NoSuchElementException: return False
    return True

def getUrgenciasRecord():
    what = 'ctl00_mainPlaceHolder_grvurgencias'
    return driver.find_element_by_id(what).text.encode('utf-8')

def foundAutoresRecord():
    what = 'ctl00_mainPlaceHolder_grvAutores'
    try: driver.find_element_by_id(what)
    except NoSuchElementException: return False
    return True

def getAutoresRecord():
    what = 'ctl00_mainPlaceHolder_grvAutores'
    return driver.find_element_by_id(what).text.encode('utf-8')

def foundVotacionesRecord():
    what = 'ctl00_mainPlaceHolder_grvvotaciones'
    try: driver.find_element_by_id(what)
    except NoSuchElementException: return False
    return True

def getVotacionesRecord():
    what = 'ctl00_mainPlaceHolder_grvvotaciones'
    return driver.find_element_by_id(what).text.encode('utf-8')

def foundVetoRecord():
    what = 'ctl00_mainPlaceHolder_grvveto'
    try: driver.find_element_by_id(what)
    except NoSuchElementException: return False
    return True

def getVetoRecord():
    what = 'ctl00_mainPlaceHolder_grvveto'
    return driver.find_element_by_id(what).text.encode('utf-8')

# opens cámara site in browser, finds boletín, clicks urgencias tab
driver = webdriver.Firefox()  # opens a web browser (downside: it actually does it!)
driver.implicitly_wait(10)    # waits 10 seconds for slow elements to populate screen before next command
baseUrl = "http://www.camara.cl/"
target = '/pley/pley_detalle.aspx?prmID=' + id + '&prmBL=' + bl
driver.get(baseUrl + target)

###############################################################################
# get Bill info from summary table (assumes that every boletin has this info) #
###############################################################################
zeText = getSummaryRecord()

# give record some format
output = ['emmStart Summary ' + bl]
output.append( zeText )
output.append('Summary emmEnd')

# append record to file
with open(filename, 'w') as zeFile:
    zeFile.write('* * Bill summary * *\n')
    out = csv.writer(zeFile,
                     delimiter=",",
                     quotechar='"',
                     quoting=csv.QUOTE_NONNUMERIC)
#    colnames = ['what', 'data', 'end']
#    out.writerow(colnames)
    out.writerow( output )
    zeFile.write( 'Downloaded ' + time.strftime("%d-%m-%Y %H:%M:%S") ) # adds date/time of download 


############################
# get Hitos de tramitación #
############################
# extract record (this version is better than those used below, but only works for Hitos)
if foundNoRecordMessage() == True and str(getNoRecordMessage()) == 'No se han encontrado datos': zeText = getNoRecordMessage()
elif foundHitosRecord() == True: zeText = getHitosRecord()
else: zeText = 'Unanticipated structure or 10sec Timeout---check source again for content'

# give record some format
output = ['emmStart Hitos ' + bl]
output.append( zeText )
output.append('Hitos emmEnd')

# append record to file
with open(filename, 'a') as zeFile:
    zeFile.write('\n* * Hitos de tramitación * *\n')
    out = csv.writer(zeFile,
                     delimiter=",",
                     quotechar='"',
                     quoting=csv.QUOTE_NONNUMERIC)
#    colnames = ['start', 'data', 'end']
#    out.writerow(colnames)
    out.writerow( output )

################
# get Informes #
################
# navigate to tab and click it
how = 'id'
what = "ctl00_mainPlaceHolder_btnInformes"
driver.find_element(by=how, value=what).click()

# extract record
if foundInformesRecord() == True: zeText = getInformesRecord()
else: zeText = 'No record'

# give record some format
output = ['emmStart Informes ' + bl]
output.append( zeText )  # esto consigue pegar en objeto output
output.append('Informes emmEnd')  # esto consigue pegar en objeto output

# append record to file
with open(filename, 'a') as zeFile:
    zeFile.write('\n* * Informes * *\n')
    out = csv.writer(zeFile,
                     delimiter=",",
                     quotechar='"',
                     quoting=csv.QUOTE_NONNUMERIC)
    out.writerow( output )

#################
# get Urgencias #
#################
# navigate to tab and click it
how = 'id'
what = "ctl00_mainPlaceHolder_btnUrgencias"
driver.find_element(by=how, value=what).click()

# zeText = driver.page_source.encode('utf-8') # debug saves full source
# execute javascript code: url = driver.evecute_script("return window.location;")
##time.sleep(5) # waits 5 seconds

# extract record
if foundUrgenciasRecord() == True: zeText = getUrgenciasRecord()
else: zeText = 'No record'

# give record some format
output = ['emmStart Urgencias ' + bl]
output.append( zeText )  # esto consigue pegar en objeto output
output.append('Urgencias emmEnd')  # esto consigue pegar en objeto output

# append record to file
with open(filename, 'a') as zeFile:
    zeFile.write('\n* * Urgencias * *\n')
    out = csv.writer(zeFile,
                     delimiter=",",
                     quotechar='"',
                     quoting=csv.QUOTE_NONNUMERIC)
    out.writerow( output )

###############
# get Autores #
###############
# navigate to tab and click it
how = 'id'
what = "ctl00_mainPlaceHolder_btnAutores"
driver.find_element(by=how, value=what).click()

# extract record
if foundAutoresRecord() == True: zeText = getAutoresRecord()
else: zeText = 'No record'

# give record some format
output = ['emmStart Autores ' + bl]
output.append( zeText )  # esto consigue pegar en objeto output
output.append('Autores emmEnd')  # esto consigue pegar en objeto output

# append record to file
with open(filename, 'a') as zeFile:
    zeFile.write('\n* * Autores * *\n')
    out = csv.writer(zeFile,
                     delimiter=",",
                     quotechar='"',
                     quoting=csv.QUOTE_NONNUMERIC)
    out.writerow( output )

##################
# get Votaciones #
##################
# navigate to tab and click it
how = 'id'
what = "ctl00_mainPlaceHolder_btnVotaciones"
driver.find_element(by=how, value=what).click()

# extract record
if foundVotacionesRecord() == True: zeText = getVotacionesRecord()
else: zeText = 'No record'

# give record some format
output = ['emmStart Votaciones ' + bl]
output.append( zeText )  # esto consigue pegar en objeto output
output.append('Votaciones emmEnd')  # esto consigue pegar en objeto output

# append record to file
with open(filename, 'a') as zeFile:
    zeFile.write('\n* * Votaciones * *\n')
    out = csv.writer(zeFile,
                     delimiter=",",
                     quotechar='"',
                     quoting=csv.QUOTE_NONNUMERIC)
    out.writerow( output )

############
# get Veto #
############
# navigate to tab and click it
how = 'id'
what = "ctl00_mainPlaceHolder_btnVeto"
driver.find_element(by=how, value=what).click()

# extract record
if foundVetoRecord() == True: zeText = getVetoRecord()
else: zeText = 'No record'

# give record some format
output = ['emmStart Veto ' + bl]
output.append( zeText )  # esto consigue pegar en objeto output
output.append('Veto emmEnd')  # esto consigue pegar en objeto output

# append record to file
with open(filename, 'a') as zeFile:
    zeFile.write('\n* * Veto * *\n')
    out = csv.writer(zeFile,
                     delimiter=",",
                     quotechar='"',
                     quoting=csv.QUOTE_NONNUMERIC)
    out.writerow( output )

############
# clean up #
############
driver.quit()


