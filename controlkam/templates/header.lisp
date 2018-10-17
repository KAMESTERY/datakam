
(PROGN
 (ECO-TEMPLATE:DEFTEMPLATE ECO-TEMPLATE:HEADER (ECO-TEMPLATE::TITLE) NIL
                           (WRITE-STRING "

<title>"
                                         ECO-TEMPLATE::ECO-STREAM)
                           (WRITE-STRING (ECO-TEMPLATE:E ECO-TEMPLATE::TITLE) ECO-TEMPLATE::ECO-STREAM)
                           (WRITE-STRING "</title>

"
                                         ECO-TEMPLATE::ECO-STREAM))) 