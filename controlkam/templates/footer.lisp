
(PROGN
 (ECO-TEMPLATE:DEFTEMPLATE ECO-TEMPLATE::FOOTER (ECO-TEMPLATE::SCRIPT) NIL
                           (WRITE-STRING "
<script>
  console.log(\"Hello Eco :-)\")
</script>
"
                                         ECO-TEMPLATE::ECO-STREAM)
                           (WRITE-STRING (ECO-TEMPLATE:E ECO-TEMPLATE::SCRIPT) ECO-TEMPLATE::ECO-STREAM)
                           (WRITE-STRING "
"
                                         ECO-TEMPLATE::ECO-STREAM))) 