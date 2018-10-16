
(PROGN
 (ECO-TEMPLATE:DEFTEMPLATE ECO-TEMPLATE::HOME (ECO-TEMPLATE::TITLE) NIL
                           (WRITE-STRING "
  <h1>"
                                         ECO-TEMPLATE::ECO-STREAM)
                           (WRITE-STRING (ECO-TEMPLATE:E ECO-TEMPLATE::TITLE) ECO-TEMPLATE::ECO-STREAM)
                           (WRITE-STRING "</h1>
  <p>Hello World!</p>
"
                                         ECO-TEMPLATE::ECO-STREAM))) 