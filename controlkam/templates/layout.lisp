
(PROGN
 (ECO-TEMPLATE:DEFTEMPLATE ECO-TEMPLATE::LAYOUT (ECO-TEMPLATE:HEADER ECO-TEMPLATE::CONTENT ECO-TEMPLATE:FOOTER) (:ESCAPE-HTML NIL)
                           (WRITE-STRING "
<!DOCTYPE html>
<html>
  <head>
    <span>
      <a class=\"href\" href=\"/\">Home</a>
      <a class=\"href\" href=\"/hello\">Hello</a>
    </span>
    "
                                         ECO-TEMPLATE::ECO-STREAM)
                           (WRITE-STRING (ECO-TEMPLATE:E ECO-TEMPLATE:HEADER) ECO-TEMPLATE::ECO-STREAM)
                           (WRITE-STRING "
    </head>
    <body>
     "
                                         ECO-TEMPLATE::ECO-STREAM)
                           (WRITE-STRING (ECO-TEMPLATE:E ECO-TEMPLATE::CONTENT) ECO-TEMPLATE::ECO-STREAM)
                           (WRITE-STRING "
     "
                                         ECO-TEMPLATE::ECO-STREAM)
                           (WRITE-STRING (ECO-TEMPLATE:E ECO-TEMPLATE:FOOTER) ECO-TEMPLATE::ECO-STREAM)
                           (WRITE-STRING "
    </body>
</html>
"
                                         ECO-TEMPLATE::ECO-STREAM))) 