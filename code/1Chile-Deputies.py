import re
import csv
import codecs
import string
import datetime

#!/usr/local/bin/python
#-*- coding: utf-8 -*-

numberfiles=1169
filenames=[]

for i in range(0, numberfiles):
	filename='/Users/betuldemirkaya/Desktop/Chile-Deputies-RCV/Chile-Deputies-RCV-2012/ChiDepRCV2012n'+`i+1`+'.html'
	filenames.append(filename)

files=[]
for i in range(0, numberfiles):
	file='ChiDepRCV2012n'+`i+1`
	files.append(file)


#I found the dehtml code here: http://stackoverflow.com/questions/328356/extracting-text-from-html-file-using-python

from HTMLParser import HTMLParser
from re import sub
from sys import stderr
from traceback import print_exc

class _DeHTMLParser(HTMLParser):
    def __init__(self):
        HTMLParser.__init__(self)
        self.__text = []
    
    def handle_data(self, data):
        text = data.strip()
        if len(text) > 0:
            text = sub('[ \t\r\n]+', ' ', text)
            self.__text.append(text + ' ')
    
    def handle_starttag(self, tag, attrs):
        if tag == 'p':
            self.__text.append('\n\n')
        elif tag == 'br':
            self.__text.append('\n')
    
    def handle_startendtag(self, tag, attrs):
        if tag == 'br':
            self.__text.append('\n\n')
    
    def text(self):
        return ''.join(self.__text).strip()


def dehtml(text):
    try:
        parser = _DeHTMLParser()
        parser.feed(text)
        parser.close()
        return parser.text()
    except:
        print_exc(file=stderr)
        return text


def main():
    text = r'''
        <html>
        <body>
        <b>Project:</b> DeHTML<br>
        <b>Description</b>:<br>
        This small script is intended to allow conversion from HTML markup to
        plain text.
        </body>
        </html>
        '''
    print(dehtml(text))


if __name__ == '__main__':
    main()

#I found removeunnecessary code here: http://stackoverflow.com/questions/328356/extracting-text-from-html-file-using-python

unnecessary = {'\n':''}
def removeunnecessary(text):
    for i, j in unnecessary.iteritems():
        text = text.replace(i, j)
        return text

#Read and clean up the files


texts=[]
for i in range(0,numberfiles):
    file=filenames[i]
    htmltext=codecs.open(file,"r",encoding="utf-8").read()
    text=dehtml(htmltext)
    finaltext=removeunnecessary(text)
    print i
    texts.append(finaltext)

#Divide the file into parts

divideds=[]
for i in range(0,numberfiles):
    divided=re.split('Detalle de Votaci\xf3n|Fecha:|Materia:|Art\xedculo:|Sesi\xf3n:|Tr\xe1mite:|Tipo de votaci\xf3n:|Quorum:|Resultado:|A Favor En Contra Abstenci\xf3n Dispensados|A favor|En contra|Abstenci\xf3n|Art\xedculo 5\xb0 B|Pareos',texts[i])
    divideds.append(divided)

divideds2=[]
for i in range(0,numberfiles):
    divided=re.split('Detalle de Votaci\xf3n|Fecha:|Observaciones:|Sesi\xf3n:|Tipo de votaci\xf3n:|Quorum:|Resultado:|A Favor En Contra Abstenci\xf3n Dispensados|A favor|En contra|Abstenci\xf3n|Art\xedculo 5\xb0 B|Pareos',texts[i])
    divideds2.append(divided)

divideds3=[]
for i in range(0,numberfiles):
    divided=re.split('Detalle de Votaci\xf3n|Fecha:|Materia:|Observaciones:|Sesi\xf3n:|Tipo de votaci\xf3n:|Resultado:|A Favor En Contra Abstenci\xf3n Dispensados|A favor|En contra|Abstenci\xf3n|Art\xedculo 5\xb0 B|Pareos',texts[i])
    divideds3.append(divided)

#Check the number of parts
numberparts=[]
for i in range(0,numberfiles):
    numberpart=len(divideds[i])
    numberparts.append(numberpart)

#Create three types of files

filetype1=[i for i in range(0,numberfiles) if numberparts[i]==16]
filetype2=[i for i in range(0,numberfiles) if 'Materia:' not in texts[i] and 'Observaciones:' in texts[i]]
filetype3=[i for i in range(0,numberfiles) if 'Materia:' in texts[i] and 'Observaciones:' in texts[i]]

#Clean dates

dates=[]
for i in range(0,numberfiles):
    fecha=divideds[i][2].split(' ')
    day=int(fecha[1])
    if fecha[3]==u'Ene.': month=1
    if fecha[3]==u'Feb.': month=2
    if fecha[3]==u'Mar.': month=3
    if fecha[3]==u'Abr.': month=4
    if fecha[3]==u'May.': month=5
    if fecha[3]==u'Jun.': month=6
    if fecha[3]==u'Jul.': month=7
    if fecha[3]==u'Ago.': month=8
    if fecha[3]==u'Sep.': month=9
    if fecha[3]==u'Oct.': month=10
    if fecha[3]==u'Nov.': month=11
    if fecha[3]==u'Dic.': month=12
    year=int(fecha[5])
    date=`day`+'/'+`month`+'/'+`year`
    finaldate=datetime.datetime.strptime(date,'%d/%m/%Y').strftime('%m/%d/%Y')
    dates.append(finaldate)

#Create.csv file

csvfile= "/Users/BetulDemirkaya/Desktop/ChileDep2012-Votes.csv"

columnnames=['Number','File','Detail','Date','Subject','Article','Session','Tramite','Type','Quorum','Result','In Favor','Against','Abstencion','Dispensados','Observation']

fout = open(csvfile, "w")
outfilehandle = csv.writer(fout,
                           delimiter=",",
                           quotechar='"',
                           quoting=csv.QUOTE_NONNUMERIC)
outfilehandle.writerow(columnnames)
for i  in filetype1:
    documentrow = []
    documentrow.append(i+1)
    documentrow.append(files[i])
    documentrow.append(divideds[i][1].encode('utf8'))
    documentrow.append(dates[i])
    documentrow.append(divideds[i][3].encode('utf8'))
    documentrow.append(divideds[i][4].encode('utf8'))
    documentrow.append(divideds[i][5].encode('utf8'))
    documentrow.append(divideds[i][6].encode('utf8'))
    documentrow.append(divideds[i][7].encode('utf8'))
    documentrow.append(divideds[i][8].encode('utf8'))
    documentrow.append(divideds[i][9].encode('utf8'))
    results=string.split(divideds[i][10])
    documentrow.append(results[0].encode('utf8'))
    documentrow.append(results[1].encode('utf8'))
    documentrow.append(results[2].encode('utf8'))
    documentrow.append(results[3].encode('utf8'))
    outfilehandle.writerow(documentrow)
for i in filetype2:
    documentrow = []
    documentrow.append(i+1)
    documentrow.append(files[i])
    documentrow.append('')
    documentrow.append(dates[i])
    documentrow.append(divideds2[i][1].encode('utf8'))
    documentrow.append('')
    documentrow.append(divideds2[i][4].encode('utf8'))
    documentrow.append('')
    documentrow.append(divideds2[i][5].encode('utf8'))
    documentrow.append(divideds2[i][6].encode('utf8'))
    documentrow.append(divideds2[i][7].encode('utf8'))
    results=string.split(divideds2[i][8])
    documentrow.append(results[0].encode('utf8'))
    documentrow.append(results[1].encode('utf8'))
    documentrow.append(results[2].encode('utf8'))
    documentrow.append(results[3].encode('utf8'))
    documentrow.append(divideds2[i][3].encode('utf8'))
    outfilehandle.writerow(documentrow)
for i in filetype3:
    documentrow = []
    documentrow.append(i+1)
    documentrow.append(files[i])
    documentrow.append(divideds3[i][1].encode('utf8'))
    documentrow.append(dates[i])
    documentrow.append(divideds3[i][3].encode('utf8'))
    documentrow.append('')
    documentrow.append(divideds3[i][5].encode('utf8'))
    documentrow.append('')
    documentrow.append(divideds3[i][6].encode('utf8'))
    documentrow.append('')
    documentrow.append(divideds3[i][7].encode('utf8'))
    results=string.split(divideds3[i][8])
    documentrow.append(results[0].encode('utf8'))
    documentrow.append(results[1].encode('utf8'))
    documentrow.append(results[2].encode('utf8'))
    documentrow.append(results[3].encode('utf8'))
    documentrow.append(divideds3[i][4].encode('utf8'))
    outfilehandle.writerow(documentrow)
fout.close()

#Function that takes the text with the deputies and makes it into a list. The second works for pareo votes.

def makedeputylist(listext):
    deputylist=[]
    firstlist=re.split('Sr.|Sra.',listext)
    for i in range (0,len(firstlist)):
        if firstlist[i]!=u' ' and firstlist[i]!=u'':
            deputy=list(firstlist[i])
            if deputy[0]==u'.':
                deputy[0]=u''
            if deputy[0]==u' ':
                deputy[0]=u''
            if deputy[1]==u' ':
                deputy[1]=u''
            if deputy[-1]==u' ':
                    deputy[-1]=u''
            deputyname=''.join(deputy)
            deputylist.append(deputyname)
    return deputylist

def makedeputylist2(listext):
    if 'Sr.' in listext or 'Sra.' in listext:
        deputylist=[]
        pareos=re.split('C\xe1mara de Diputados de Chile',listext)
        firstlist=re.split('Sr.|Sra.| con ',pareos[0])
        for i in range (0,len(firstlist)):
            if firstlist[i]!=u' ' and firstlist[i]!=u'':
                deputy=list(firstlist[i])
                if deputy[0]==u'.':
                    deputy[0]=u''
                if deputy[0]==u' ':
                    deputy[0]=u''
                if deputy[1]==u' ':
                    deputy[1]=u''
                if deputy[-1]==u' ':
                    deputy[-1]=u''
                deputyname=''.join(deputy)
                deputylist.append(deputyname)
        return deputylist
    else:
        return []

#Create a list of all deputies from files
deputies=[]
for j in filetype1:
    deputies.append(makedeputylist(divideds[j][11]))
    deputies.append(makedeputylist(divideds[j][12]))
    deputies.append(makedeputylist(divideds[j][13]))
    deputies.append(makedeputylist2(divideds[j][15]))
for j in filetype2 + filetype3:
    deputies.append(makedeputylist(divideds[j][8]))
    deputies.append(makedeputylist(divideds[j][9]))
    deputies.append(makedeputylist(divideds[j][10]))
    deputies.append(makedeputylist2(divideds[j][12]))
		
deputies=[item for sublist in deputies for item in sublist]


#Delete duplicates
deputiesfinal=[]
for i in deputies:
    if i not in deputiesfinal:
        deputiesfinal.append(i)

numberdeputies=len(deputiesfinal)

#Deputies file

csvfile= "/Users/BetulDemirkaya/Desktop/ChileDep2012-Deputies.csv"

fout = open(csvfile, "w")
outfilehandle = csv.writer(fout,
                           delimiter=",",
                           quotechar='"',
                           quoting=csv.QUOTE_NONNUMERIC)
zero=["Member"]
filetype1r=[x+1 for x in filetype1]
filetype2r=[x+1 for x in filetype2]
filetype3r=[x+1 for x in filetype3]

columnames=zero+filetype1r+filetype2r+filetype3r

outfilehandle.writerow(columnames)

for i in range(0,numberdeputies):
    firstrow=[]
    documentrow=[]
    documentrow.append(deputiesfinal[i].encode('utf8'))
    for j in filetype1:        
        if deputiesfinal[i] in makedeputylist(divideds[j][11]): vote='YES'
        elif deputiesfinal[i] in makedeputylist(divideds[j][12]): vote='NO'
        elif deputiesfinal[i] in makedeputylist(divideds[j][13]): vote='ABSTAIN'
        else: vote='NA'
        if deputiesfinal[i] in makedeputylist2(divideds[j][15]):
            pareo='PAREO con '
            deputyindex=makedeputylist2(divideds[j][15]).index(deputiesfinal[i])
            if deputyindex%2==0:
                partner=makedeputylist2(divideds[j][15])[deputyindex+1]
            else:
                partner=makedeputylist2(divideds[j][15])[deputyindex-1]
        else: pareo='NA'
        if pareo=='NA' and vote=='NA':
            documentrow.append('NA')
        elif vote!='NA' and pareo=='NA':
            documentrow.append(vote.encode('utf8'))
        elif vote=='NA' and pareo!='NA':
            documentrow.append(pareo.encode('utf8') + partner.encode('utf8'))
        else: documentrow.append(vote.encode('utf8') + ' - ' + pareo.encode('utf8')+ partner.encode('utf8'))
    for j in filetype2 + filetype3:
        if deputiesfinal[i] in makedeputylist(divideds[j][8]): vote='YES'
        elif deputiesfinal[i] in makedeputylist(divideds[j][9]): vote='NO'
        elif deputiesfinal[i] in makedeputylist(divideds[j][10]): vote='ABSTAIN'
        else: vote='NA'
        if deputiesfinal[i] in makedeputylist2(divideds[j][12]):
            pareo='PAREO con '
            deputyindex=makedeputylist2(divideds[j][12]).index(deputiesfinal[i])
            if deputyindex%2==0:
                partner=makedeputylist2(divideds[j][12])[deputyindex+1]
            else:
                partner=makedeputylist2(divideds[j][12])[deputyindex-1]
        else: pareo='NA'
        if pareo=='NA' and vote=='NA':
            documentrow.append('NA')
        elif vote!='NA' and pareo=='NA':
            documentrow.append(vote.encode('utf8'))
        elif vote=='NA' and pareo!='NA':
            documentrow.append(pareo.encode('utf8') + partner.encode('utf8'))
        else: documentrow.append(vote.encode('utf8') + ' - ' + pareo.encode('utf8') + partner.encode('utf8'))
    outfilehandle.writerow(documentrow)

fout.close()




















