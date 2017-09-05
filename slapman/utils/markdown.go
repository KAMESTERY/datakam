package utils

import (
	"bufio"
	"bytes"
	"fmt"

	"github.com/microcosm-cc/bluemonday"
	"github.com/russross/blackfriday"
	// "io/ioutil"
	"os"
	"path/filepath"
	"strings"
)

var (
	// Mardown Vars
	ErrEmptyPost          = fmt.Errorf("empty post file")
	ErrInvalidFrontMatter = fmt.Errorf("invalid front matter")
	ErrMissingFrontMatter = fmt.Errorf("missing front matter")
)

// ConvertToMarkdown Converts Markdown to HTML
func ConvertToMarkdown(mdBytes []byte, trust bool) (body string) {
	unsafeHTML := blackfriday.MarkdownCommon(mdBytes)
	if trust {
		body = string(unsafeHTML)
	} else {
		bodyBytes := bluemonday.UGCPolicy().SanitizeBytes(unsafeHTML)
		body = string(bodyBytes)
	}
	return
}

// Read the front matter from the post. If there is no front matter, this is
// not a valid post.
func readFrontMatter(s *bufio.Scanner) (map[string]string, error) {
	m := make(map[string]string)
	infm := false
	for s.Scan() {
		l := strings.Trim(s.Text(), " ")
		if l == "---" { // The front matter is delimited by 3 dashes
			if infm {
				// This signals the end of the front matter
				return m, nil
			} else {
				// This is the start of the front matter
				infm = true
			}
		} else if infm {
			sections := strings.SplitN(l, ":", 2)
			if len(sections) != 2 {
				// Invalid front matter line
				return nil, ErrInvalidFrontMatter
			}
			m[sections[0]] = strings.Trim(sections[1], " ")
		} else if l != "" {
			// No front matter, quit
			return nil, ErrMissingFrontMatter
		}
	}
	if err := s.Err(); err != nil {
		return nil, err
	}
	return nil, ErrEmptyPost
}

func getHtml(s *bufio.Scanner) (string, error) {
	buf := bytes.NewBuffer(nil)
	for s.Scan() {
		buf.WriteString(s.Text() + "\n")
	}
	body := ConvertToMarkdown(buf.Bytes(), true)

	return body, nil
}

// Rendering START

// LoadMarkdown Load Mardown Text from File
func LoadMarkdown(title string) (interface{}, error) {
	filename := title + ".md"
	// md, err := ioutil.ReadFile("./pages/" + filename)
	// if err != nil {
	// 	return nil, err
	// }

	// Grab File
	f, err := os.Open(filepath.Join(MarkdownsDir, filename))
	if err != nil {
		return nil, err
	}
	defer f.Close()

	// Grab Metadata
	s := bufio.NewScanner(f)
	m, err := readFrontMatter(s)
	if err != nil {
		return nil, err
	}

	// Read rest of file and Obtain HTML
	mdHtml, err := getHtml(s)
	if err = s.Err(); err != nil {
		return nil, err
	}

	fmt.Printf("Markdown Metadata: %+v", m)

	page := struct {
		Title string
		Body  string
		Attrs map[string]string
		Tmpl  map[string]string
	}{
		title,
		mdHtml,
		m,
		map[string]string{
			"NavTmpl":     "navigation",
			"ContentTmpl": "markdown",
		},
	}

	return page, nil
}

// Rendering STOP
