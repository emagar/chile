# -*- coding: utf-8 -*-

# # how to run this script from within python
# with open('1getBol.py') as f:
# 	run_this = f.read()
# exec(run_this)

#id = sys.argv[1]          # passes the argument given when invoking this code in console
#bl = sys.argv[2]          # passes the argument given when invoking this code in console
id = '1252'    # debug: case has urgencias
bl = '1-07'    # debug: case has urgencias
#id = '2318'    # debug: case has no urgencia
#bl = '1141-13' # debug: case has no urgencia

url = 'http://camara.cl/pley/pley_detalle.aspx'
parametros = {'prmID': id, 'prmBL': bl}

# ## Hitos reached with requests library
# import requests
# r = requests.get(url, params = parametros)
# #hitos = r.text
# #print r.url
# #print(r.headers)
# filename = 'bol' + bl + '.txt'
# with open(filename, 'w') as zeFile:
#    zeFile.write('emmHitos ' + bl)
# # stream content to a file
# with open(filename, 'a') as zeFile:
#    for chunk in r.iter_content():
#       zeFile.write(chunk)

## Urgencias and vetos reached with Selenium (allows to click on tabs)
## from http://stackoverflow.com/questions/6116023/screenscaping-aspx-with-python-mechanize-javascript-form-submission
from selenium import webdriver
from selenium.common.exceptions import NoSuchElementException
import csv, codecs
#from selenium.webdriver.common.by import By
#from selenium.webdriver.common.keys import Keys
#from selenium.webdriver.support.ui import Select
#from selenium.common.exceptions import NoAlertPresentException
#from selenium.common.exceptions import TimeoutException
#from selenium.webdriver.support.ui import WebDriverWait
#from selenium.webdriver.support import expected_conditions as EC
#import unittest, time, re

#output = ['emmHitos ' + bl + 'start here']
output = ['emmUrgencias start' + bl]

# with open('tmp.txt', 'a') as zeFile:
# #with open(filename, 'a') as zeFile:
#    zeFile.write('\nemmUrgencias ' + bl + '\n')

# opens cámara site in browser, finds boletín, clicks urgencias tab
driver = webdriver.Firefox()  # opens a web browser (downside: it actually does it!)
driver.implicitly_wait(10)    # waits 10 seconds for slow elements to populate screen before next command
baseUrl = "http://www.camara.cl/"
#        self.verificationErrors = []
#        self.accept_next_alert = True
target = '/pley/pley_detalle.aspx?prmID=' + id + '&prmBL=' + bl
driver.get(baseUrl + target)
how = 'id'
what = "ctl00_mainPlaceHolder_btnUrgencias"
driver.find_element(by=how, value=what).click()

def foundNoRecordMessage():
    what = 'h2.importante'
    try: driver.find_element_by_css_selector(what)
    except NoSuchElementException: return False
    return True

def foundUrgenciasRecord():
    what = 'ctl00_mainPlaceHolder_grvurgencias'
    try: driver.find_element_by_id(what)
    except NoSuchElementException: return False
    return True

def getNoRecordMessage():
    what = 'h2.importante'
    return driver.find_element_by_css_selector(what).text.encode('utf-8')

def getUrgenciasRecord():
#    what = "//table[@id='ctl00_mainPlaceHolder_grvurgencias']"
#    return driver.find_element_by_xpath(what).text
    what = 'ctl00_mainPlaceHolder_grvurgencias'
    return driver.find_element_by_id(what).text.encode('utf-8')

if foundNoRecordMessage() == True and str(getNoRecordMessage()) != '': zeText = getNoRecordMessage()
elif foundUrgenciasRecord() == True: zeText = getUrgenciasRecord()
else: zeText = 'Unanticipated structure or 10sec Timeout---check source again for content'

# what1 = 'h2.importante'
# what2 = 'ctl00_mainPlaceHolder_grvurgencias'
# if test1==True: zeText = str(driver.find_element_by_css_selector(what1).text)
# elif test2==True: zeText = str(driver.find_element_by_id(what2).text)
# else: zeText = 'Unanticipated structure, check source again for content'

output.append( zeText )  # esto consigue pegar en objeto output
output.append('urgencias end')  # esto consigue pegar en objeto output

driver.quit()

# #NEED TO LEARN UNICODE HOWTO IN PYTHON: http://stackoverflow.com/questions/9942594/unicodeencodeerror-ascii-codec-cant-encode-character-u-xa0-in-position-20
# #THIS WILL LIKELY LEAD ME TO DROP str() AND USE .encode('utf-8') INSTEAD. Also http://nedbatchelder.com/text/unipain.html

# # # # # # with open(filename, 'a') as zeFile:
# with codecs.open('tmp.txt', mode='a', encoding='utf-8') as zeFile: 
#      zeFile.write( output )  # check python code sent by memo for ways to export this... str() won't work with weird characters, and without it the list is unacceptable...


# # with open(filename, 'a') as zeFile:
with open('tmp.txt', 'a') as zeFile: 
    out = csv.writer(zeFile,
                     delimiter=",",
                     quotechar='"',
                     quoting=csv.QUOTE_NONNUMERIC)
    colnames = ['what', 'data', 'end']
    out.writerow(colnames)
    out.writerow(output)

# class Tmp(unittest.TestCase):
#     def setUp(self):
#         self.driver = webdriver.Firefox()
#         self.driver.implicitly_wait(30)
#         self.base_url = "http://www.camara.cl/"
#         self.verificationErrors = []
#         self.accept_next_alert = True

#     def test_tmp(self):
#         driver = self.driver
#         aim = '/pley/pley_detalle.aspx?prmID=' + id + '&prmBL=' + bl
#         driver.get(self.base_url + aim)
#         driver.find_element_by_id("ctl00_mainPlaceHolder_btnUrgencias").click()
# #        driver.find_element_by_id("ctl00_mainPlaceHolder_btnAutores").click()
# #        driver.find_element_by_id("ctl00_mainPlaceHolder_btnVotaciones").click()
#         zeText = driver.find_element_by_xpath("//table[@id='ctl00_mainPlaceHolder_grvurgencias']").text # selecciona la tabla
# #        zeText = zeText.encode('utf-8') # esto empeora la cosa, mejor cambio caracteres ex-post
#         output.append(zeText)  # esto consigue pegar en objeto output
#         with open(filename, 'a') as zeFile:
#            zeFile.write( str(output) )
           
#     def is_element_present(self, how, what):
#         try: self.driver.find_element(by=how, value=what)
#         except NoSuchElementException, e: return False
#         return True
    
#     def is_alert_present(self):
#         try: self.driver.switch_to_alert()
#         except NoAlertPresentException, e: return False
#         return True
    
#     def close_alert_and_get_its_text(self):
#         try:
#             alert = self.driver.switch_to_alert()
#             alert_text = alert.text
#             if self.accept_next_alert:
#                 alert.accept()
#             else:
#                 alert.dismiss()
#             return alert_text
#         finally: self.accept_next_alert = True
    
#     def tearDown(self):
#         self.driver.quit()
#         self.assertEqual([], self.verificationErrors)

# if __name__ == "__main__":
#     unittest.main()



# # import json
# # import httplib
# # import http.client
# parametros = {'prmID': id, 'prmBL': bl, '__EVENTTARGET': 'ctl00$mainPlaceHolder$btnUrgencias'}
# #parametros = {'prmID': id, 'prmBL': bl, 'ctl00$mainPlaceHolder$ScriptManager1': 'ctl00$mainPlaceHolder$UpdatePanel1|ctl00$mainPlaceHolder$btnUrgencias'}
# headers = {'content-type': 'application/x-www-form-urlencoded; charset=utf-8'}
# # conexion = httplib.HTTPConnection('http://camara.cl')
# # tmp = '/pley/pley_detalle%prmID=' + id + '&prmBL=' + bl
# # conexion.request('POST', tmp, headers = headers)
# # urgencias = conexion.getresponse()
# p = requests.post(url, data = json.dumps(parametros), headers = headers)
# urgencias = p.text
# print urgencias
# #print(response.status, response.reason)

# import requests
# import http.client
# headers = {'content-type': 'application/x-www-form-urlencoded; charset=utf-8'}
# parametros = {'prmBL': bl, 'prmID': id}
# conn = requests.get('http://camara.cl/pley/pley_detalle.aspx', params = parametros)
# hitos = r.text
# #print(r.headers)
# print hitos
# r.close() # necessary?

# import urllib2

# f = urllib2.urlopen('http://camara.cl/pley/pley_detalle.aspx?prmID=2317&prmBL=1140-05')
# data = f.read()
# f.close()


# # do post request urgencias and veto
# r = requests.post('http://camara.cl/pley/pley_detalle.aspx', params = parametros)


# ### intento con mechanize from http://stackoverflow.com/questions/6116023/screenscaping-aspx-with-python-mechanize-javascript-form-submission
# import mechanize
# direccion = 'http://camara.cl/pley/pley_detalle.aspx?prmID=' + id + '&prmBL=' + bl
# br = mechanize.Browser()
# br.set_handle_robots(False)
# br.addheaders = [('User-agent', 'Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:33.0) Gecko/20100101 Firefox/33.0')]
# br.open(direccion)
# respuesta = br.response().read()
# br.select_form(nr=0)
# respuesta = br.submit(name='ctl00$mainPlaceHolder$btnUrgencias').read()



