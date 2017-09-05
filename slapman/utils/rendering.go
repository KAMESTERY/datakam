package utils

import (
	"bytes"
	"encoding/json"
	"fmt"
	"html/template"
	"net/http"
	"os"
	"time"
)

// https://github.com/golang-samples/template/tree/master/extends
// https://stackoverflow.com/questions/20716726/golang-text-html-template-call-other-templates-with-dynamic-name
var (
	tpl        = template.New("")
	templating = template.Must(
		tpl.Funcs(template.FuncMap{
			"expand_shortcodes": expandShortcodes,
			//			"internal_url": func(params []string) (internal_url string) {
			//				link_text_buffer := new(bytes.Buffer)
			//				for _, chunk := range params[2:] {
			//					link_text_buffer.WriteString(" " + chunk)
			//				}
			//				internal_url = "<a href='/" + params[0] + "/" + params[1] + "'>" + link_text_buffer.String() + "</a>"
			//				return
			//			},
			"env": func(envVarName string) (envVarValue string) {
				if envVarValue = os.Getenv(envVarName); len(envVarValue) == 0 {
					envVarValue = "No Found"
				}
				return
			},
			"isset": func(value interface{}) (isTrue bool) {
				isTrue = value != nil
				return
			},
			"safeHtml": func(value interface{}) template.HTML {
				return template.HTML(fmt.Sprint(value))
			},
			"include": func(name string, data interface{}) (ret template.HTML, err error) {
				buf := bytes.NewBuffer([]byte{})
				err = tpl.ExecuteTemplate(buf, name, data)
				if err != nil {
					fmt.Printf("ERROR:::: %+v", err.Error())
				}
				ret = template.HTML(buf.String())
				return
			},
			// Check this out: https://stackoverflow.com/questions/23466497/how-to-truncate-a-string-in-a-golang-template
			// Now unicode compliant
			"truncate": func(s string) string {
				var numRunes = 0
				for index := range s {
					numRunes++
					if numRunes > 240 {
						//					if numRunes > 25 {
						return s[:index] + "[...]"
						//						return s[:index]
					}
				}
				return s
			},
			"markdown": func(fileName string) template.HTML {
				var mdHTML string
				if cachedHTML, found := GetCache(nil, fileName); found {
					mdHTML = cachedHTML.(string)
				} else {
					mdPage, err := LoadMarkdown(fileName)
					if err != nil {
						panic(err.Error())
					}
					mdHTML = mdPage.(struct {
						Body string
					}).Body
					AddCache(nil, fileName, mdHTML, 15*time.Minute)
				}
				return template.HTML(mdHTML)
			},
			"md_inline": func(mdString string) template.HTML {
				mdHTML := ConvertToMarkdown([]byte(mdString), true)
				return template.HTML(mdHTML)
			},
			"Now": func(format string) template.HTML {
				now := time.Now()
				now_str := now.Format(format)
				return template.HTML(now_str)
			},
		}).ParseGlob(TemplatesDir))
)

// Rendering START

func RenderCsv(lst []string) string {
	var csv bytes.Buffer
	for index, item := range lst {
		csv.WriteString(item)
		if index < (len(lst) - 1) {
			csv.WriteString(",")
		}
	}
	return csv.String()
}

func RenderString(tmpl string, data interface{}) string {
	buf := new(bytes.Buffer)
	templating.ExecuteTemplate(buf, tmpl, data)
	writtenString := buf.String()
	return writtenString
}

func RenderJsonString(data interface{}) (jsonString string) {
	jsonBytes, err := json.Marshal(data)
	if err != nil {
		Errorf(nil, "ERROR:::: %+v", err.Error())
	}
	jsonString = string(jsonBytes)
	return
}

// RenderTemplate Renders Data in Template with Specified HTTP Ok
func RenderTemplate(w http.ResponseWriter, r *http.Request, tmpl string, data interface{}) {
	RenderTemplateWithCode(w, r, tmpl, data, http.StatusOK)
}

// RenderTemplateWithCode Renders Data in Template with Specified HTTP Code
func RenderTemplateWithCode(w http.ResponseWriter, r *http.Request, tmpl string, data interface{}, code int) {
	err := templating.ExecuteTemplate(w, tmpl, data)
	defer w.WriteHeader(code)
	if err != nil {
		Errorf(r, "ERROR:::: %+v", err.Error())
		http.Error(w, err.Error(), http.StatusInternalServerError)
	}
}

// RenderJSON Renders Data as JSON with HTTP Ok
func RenderJSON(w http.ResponseWriter, r *http.Request, data interface{}) {
	RenderJSONWithCode(w, r, data, http.StatusOK)
}

// RenderJSONWithCode Renders Data as JSON with Specified HTTP Code
func RenderJSONWithCode(w http.ResponseWriter, r *http.Request, data interface{}, code int) {
	w.Header().Set("Content-Type", "application/json; charset=UTF-8")
	defer w.WriteHeader(code)
	if err := json.NewEncoder(w).Encode(data); err != nil {
		Errorf(r, "ERROR:::: %+v", err.Error())
		http.Error(w, err.Error(), http.StatusInternalServerError)
	}
}

// Rendering STOP
