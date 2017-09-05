Notas para Ges y Valeria acerca de los datos dataForUrgencyRegressions.RData

Los objetos contenidos son los siguientes
> ls()
 [1] "allRep"      "allUrg"      "allVot"      "approv"      "bills"       "comPres"     "dat"         "datdir"      "deadline"   
[10] "I"           "%my_within%"

- allRep: Contiene datos de los informes de comisión (committee reports). Incluye la fecha del informe, la cámara, el boletín de la proposición de ley, si es del ejecutivo (mensaje) o de legisladores (moción), y la comisión en cuestión. No estoy 100% seguro de que la lista sea exhaustiva, pero es un primer intento por extraer la info de las propuestas que sí fueron tramitadas por comisiones.

- allUrg: Contiene datos de los mensajes de urgencia. Esta subdividida en dos partes: allUrg$messages y allUrg$chains. 
--allUrg$messages: contiene info de los mensajes de urgencia individuales. Incluye el tipo de urgencia, la fecha en que se activó (on), la fecha límite correspondiente (en días naturales), si la urgencia prescribió (dcaduca---no entiendo bien qué es, ver boletín 1035-07), si el ejecutivo retiró la urgencia, los números de mensaje, si está encadenada a otras urgencias (urgency chains), la cámara que recibe el mensaje, el cambio en deadline (en caso de ser una cadena y el mensaje extiende el deadline), el boletín fecha del informe, la cámara, el número de boletín de la proposición de ley, entre otras.
--allUrg$chains: contiene info de las cadenas de urgencia (de modo que no se repite el número de boletín). Hay info similar a la anterior, además del número de 'eslabones' (links) de la cadena. 

- allVot: Contiene inf de las votaciones nominales que me pasó Memo. Requiere limpieza, sospecho.

- bills: contiene 6,987 bill histories. Está subdividido en múltiples objetos, todos con el mismo número de observaciones (correspondencia una-a-una). Algunos de éstos son:
--bills$info: datos de la proposición, como son su autor, la fecha de iniciación, la(s) comisión(es) que prepara(n) informe(s), el número de mensajes de urgencia que recibió, etc.
--bills$urg: las urgencias que recibió la proposición. En caso que la proposición i no recibió urgencia (ie. bills$info$nUrg[i]==0), el objeto está vacío (ie. bills$urg[[i]]=NULL).

Los demás objetos son de importancia secundaria.

