package utils

import (
	//	"fmt"
	"regexp"
	"strings"
)

const (
	SHORTCODE_REGEX = "\\[\\[([a-z-]+)(.*?)\\]\\]" // In the form [[command param param param]]
//	SHORTCODE_PARAMETER_REGEX = "\\s(?:([\\w\\d/-]+)(\\d+)\"(.+?)\")"
)

var (
	re_sht = regexp.MustCompile(SHORTCODE_REGEX)
	//	re_sht_prms = regexp.MustCompile(SHORTCODE_PARAMETER_REGEX)
	renderShtCode func(string, interface{}) string
)

func init() {
	renderShtCode = RenderString
}

func expandShortcodes(document string) string {

	//	fmt.Println("Document Before: \n", document)

	matches := re_sht.FindAllString(document, -1)
	//	fmt.Println(matches)

	for _, match := range matches {
		shtcd_params := strings.Replace(match, "[[", "", -1)
		shtcd_params = strings.Replace(shtcd_params, "]]", "", -1)
		params := strings.Split(shtcd_params, " ")
		//		params := re_sht_prms.FindAllString(shtcd_params, -1)
		//		fmt.Println("Match: ", match, " Raw Params: ", shtcd_params, " Params: ", params)
		//		fmt.Println("First: ", params[0], " and REST: ", params[1:])
		data := struct {
			Tmpl string
			Args []string
		}{params[0], params[1:]}
		chunk := renderShtCode("shortcode", data)
		//		fmt.Println("Chunk: \n", chunk)
		document = strings.Replace(document, match, chunk, -1)
	}

	//	fmt.Println("Document After: \n", document)

	return document
}
