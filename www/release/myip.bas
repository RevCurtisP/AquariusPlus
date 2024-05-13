100 REM Display Public IP Address and related information
110 'Returns JSON {"ip":"IP-ADDR","country":"COUNTRY","cc":"ISO-CODE"}
115 LOAD "http://api.myip.com",^J$
120 'Split data on quote
125 DIM D$(10):SPLIT J$ INTO *D$ DEL 34
130 'Find field "ip"
135 I=INDEX(*D$,"ip")
140 'Error out if field "ip" not found
145 IF I=0 THEN PRINT "Invalid response from server":END
150 'Skip colon to get to IP address
155 I$=D$(I+2)
160 'Print the IP address
165 PRINT "Public IP Address: ";I$
