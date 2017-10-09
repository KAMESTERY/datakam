package middleware

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"net/http"
	"os"
	"path/filepath"
	"slapman/utils"
	"strings"
	"text/template"

	"github.com/facebookgo/stack"
)

// Check this out: https://github.com/dre1080/recover

const (
	applicationJSON            = "application/json"
	applicationJSONCharsetUTF8 = applicationJSON + "; charset=utf-8"
	logFmt                     = "\n[%s] %v \n\nStack Trace:\n============\n\n%s\n\n"
)

var (
	recovery_logger = utils.NewLogger("middlewarerecovery")
	contentType     = http.CanonicalHeaderKey("Content-Type")
	gopaths         []string
)

type jsonError struct {
	Message  interface{} `json:",omitempty"`
	Location string      `json:",omitempty"`
}

func init() {
	for _, p := range strings.Split(os.Getenv("GOPATH"), ":") {
		if p != "" {
			gopaths = append(gopaths, filepath.Join(p, "src")+"/")
		}
	}
}

func mustReadLines(filename string) []string {
	bytes, err := ioutil.ReadFile(filename)
	if err != nil {
		panic(err)
	}
	return strings.Split(string(bytes), "\n")
}

func appendGOPATH(file string) string {
	for _, p := range gopaths {
		f := filepath.Join(p, file)
		if _, err := os.Stat(f); err == nil {
			return f
		}
	}
	return file
}

func compileTemplate(r *http.Request, err interface{}, tmplString string, frames []stack.Frame) []byte {
	file := appendGOPATH(frames[0].File)
	src := mustReadLines(file)

	start := (frames[0].Line - 1) - 5
	// end := frames[0].Line + 5

	// lines := src[start:end]
	lines := src

	var buf bytes.Buffer

	t := template.Must(template.New("recover").Parse(tmplString))
	t.Execute(&buf, struct {
		URL         string
		Err         interface{}
		Name        string
		File        string
		StartLine   int
		SourceLines string
		Frames      []stack.Frame
	}{r.URL.Path, err, frames[0].Name, frames[0].File, start + 1, strings.Join(lines, "\n"), frames})

	return buf.Bytes()
}

// RecoveryNew returns a middleware that: recovers from panics, logs the panic and backtrace,
// writes a HTML response and Internal Server Error status.
//
// If a JSON content type is detected it returns a JSON response.
func RecoveryNew(h http.Handler) http.Handler {
	fn := func(w http.ResponseWriter, r *http.Request) {
		defer func() {
			if err := recover(); err != nil {
				frames := stack.Callers(3)
				recovery_logger.Errorf(fmt.Sprintf(logFmt, "PANIC", err, frames.String()))

				w.WriteHeader(http.StatusInternalServerError)

				ct := r.Header.Get(contentType)
				if strings.HasPrefix(ct, applicationJSON) {
					w.Header().Set(contentType, applicationJSONCharsetUTF8)

					e := jsonError{Message: err, Location: frames[0].String()}
					json.NewEncoder(w).Encode(e)
				} else {
					if mode := os.Getenv("MODE"); mode == "dev" {
						w.Write(compileTemplate(r, err, panicHTML, frames))
					} else {
						w.Write(compileTemplate(r, err, err500, frames))
					}
				}
			}
		}()
		h.ServeHTTP(w, r)
	}
	return http.HandlerFunc(fn)
}

const panicHTML = `<!DOCTYPE html>
<html>
	<head>
		<title>PANIC! at {{.URL}}</title>
		<style type="text/css">
			html { background: #f0f0f5; }
			body {
                                color: #333;
                                font-family: helvetica neue, lucida grande, sans-serif;
                                line-height: 1.5;
                                margin: 0;
                                text-shadow: 0 1px 0 rgba(255, 255, 255, 0.6);
                        }
			.clearfix::after{
                                clear: both;
                                content: ".";
                                display: block;
                                height: 0;
                                visibility: hidden;
                        }

			.container{ max-width: 100%; margin: 0 auto; }
			.content{ overflow: hidden; padding: 15px 20px; }


			pre.prettyprint{
                                background: #fff !important;
                                border:1px solid #ccc !important;
                                font-family: Menlo,'Bitstream Vera Sans Mono','DejaVu Sans Mono',Monaco,Consolas,monospace;
                                font-size: 12px;
                                line-height: 1.5;
                                margin: 0;
                                padding: 10px !important;
                        }

			.pln{ color:#333 !important }

			@media screen {
				.str{color:#d14 !important}.kwd{color:#333 !important}.com{color:#998 !important}.lit,.typ{color:#458 !important}
				.clo,.opn,.pun{color:#333 !important}.tag{color:navy !important}.atn{color:teal !important}.atv{color:#d14 !important}
				.dec{color:#333 !important}.var{color:teal !important}.fun{color:#900 !important}
			}

			@media print, projection {
				.kwd,.tag,.typ{font-weight:700}.str{color:#060 !important}.kwd{color:#006 !important}
				.com{color:#600 !important;font-style:italic}.typ{color:#404 !important}.lit{color:#044 !important}
				.clo,.opn,.pun{color:#440 !important}.tag{color:#006 !important}.atn{color:#404 !important}.atv{color:#060 !important}
			}

			ol.linenums { margin-top:0; margin-bottom:0 }
			ol.linenums li { background:#fff; }
			li.L0, li.L1, li.L2, li.L3, li.L5, li.L6, li.L7, li.L8 { list-style-type:decimal !important }

			@-webkit-keyframes highlight {
				0%   { background: rgba(220, 30, 30, 0.3); }
				100% { background: rgba(220, 30, 30, 0.1); }
			}
			@-moz-keyframes highlight {
				0%   { background: rgba(220, 30, 30, 0.3); }
				100% { background: rgba(220, 30, 30, 0.1); }
			}
			@keyframes highlight {
				0%   { background: rgba(220, 30, 30, 0.3); }
				100% { background: rgba(220, 30, 30, 0.1); }
			}

			ol.linenums li:nth-child(6) {
				background: rgba(220, 30, 30, 0.1);
				-webkit-animation: highlight 400ms linear 1;
				-moz-animation: highlight 400ms linear 1;
				animation: highlight 400ms linear 1;
			}

			header.exception {
				border-bottom: solid 3px #a33;
				padding: 18px 20px;
				height: 65px;
				min-height: 65px;
				overflow: hidden;
				background-color: #20202a;
				color: #aaa;
				text-shadow: 0 1px 0 rgba(0, 0, 0, 0.3);
				font-weight: 200;
				box-shadow: inset 0 -5px 3px -3px rgba(0, 0, 0, 0.05), inset 0 -1px 0 rgba(0, 0, 0, 0.05);
				-webkit-text-smoothing: antialiased;
			}
			header.exception h2 {
				font-weight: 200;
				font-size: 16px;
				margin: 0;
			}
			header.exception h2,
			header.exception p {
				line-height: 1.4em;
				overflow: hidden;
				white-space: pre;
				text-overflow: ellipsis;
			}
			header.exception h2 strong { font-weight: 700; color: #d55; }
			header.exception p {
				font-weight: 200;
				font-size: 20px;
				color: white;
				margin-bottom: 0;
				margin-top: 10px;
			}
			header.exception:hover { height: auto; z-index: 2; }
			header.exception:hover h2,
			header.exception:hover p {
				padding-right: 20px;
				overflow-y: auto;
				word-wrap: break-word;
				white-space: pre-wrap;
				height: auto;
				max-height: 105px;
			}

			.backtrace { padding: 20px; }

			ul.frames { margin:0;padding: 0; }
			ul.frames li {
				background-color: #f8f8f8;
				background: -webkit-linear-gradient(top, #f8f8f8 80%, #f0f0f0);
				background: -moz-linear-gradient(top, #f8f8f8 80%, #f0f0f0);
				background: linear-gradient(top, #f8f8f8 80%, #f0f0f0);
				box-shadow: inset 0 -1px 0 #e2e2e2;
				border-radius: 3px;
				margin-bottom: 5px;
				padding: 7px 20px;
				overflow: hidden;
			}
			ul.frames .name,
			ul.frames .location {
				overflow: hidden;
				height: 1.5em;
				white-space: nowrap;
				word-wrap: none;
				text-overflow: ellipsis;
			}
			ul.frames .func { color: #a33; }
			ul.frames .location {
				font-size: 0.85em;
				font-weight: 400;
				color: #999;
			}
			ul.frames .line { font-weight: bold; }

			ul.frames li:first-child {
				background: #38a;
				box-shadow: inset 0 1px 0 rgba(0, 0, 0, 0.1), inset 0 2px 0 rgba(255, 255, 255, 0.01), inset 0 -1px 0 rgba(0, 0, 0, 0.1);
			}
			ul.frames li:first-child .name,
			ul.frames li:first-child .func,
			ul.frames li:first-child .location {
				color: white;
				text-shadow: 0 1px 0 rgba(0, 0, 0, 0.2);
			}
			ul.frames li:first-child .location { opacity: 0.6; }

			.trace-info, .sidebar { margin-bottom: 15px; }
			@media (min-width: 55em) {
				.trace-info {
                                        float: left;
                                        margin-bottom: 15px;
                                        width: 55%;
                                }
				.sidebar {
                                        float: left;
                                        margin-right: 4%;
                                        margin-bottom: 15px;
                                        width: 40%;
                                }
			}

			.trace-info {
				background: #fff;
				padding: 6px;
				border-radius: 3px;
				margin-bottom: 2px;
				box-shadow: 0 0 10px rgba(0, 0, 0, 0.03), 1px 1px 0 rgba(0, 0, 0, 0.05), -1px 1px 0 rgba(0, 0, 0, 0.05), 0 0 0 4px rgba(0, 0, 0, 0.04);
			}
			.trace-info .title {
				background: #f1f1f1;
				box-shadow: inset 0 1px 0 rgba(255, 255, 255, 0.3);
				overflow: hidden;
				padding: 6px 10px;
				border: solid 1px #ccc;
				border-bottom: 0;
				border-top-left-radius: 2px;
				border-top-right-radius: 2px;
			}
			.trace-info .title .name,
			.trace-info .title .location {
				font-size: 9pt;
				line-height: 26px;
				height: 26px;
				overflow: hidden;
			}
			.trace-info .title .location {
				background:1px solid #aaaaaa;
				float: left;
				font-weight: bold;
				font-size: 10pt;
			}
			.trace-info .title .name {
				float: right;
				font-weight: 200;
				 margin: 0;
			}
		</style>
	</head>
	<body>
		<div class="top">
			<header class="exception">
				<h2><strong>PANIC</strong> <span>at {{.URL}}</span></h2>
				<p>{{.Err}}</p>
			</header>
		</div>

		<section class="backtrace clearfix">
			<nav class="sidebar">
				<ul class="frames">
				{{range .Frames}}
					<li>
						<div class="info">
							<div class="name">
								<strong class="func">{{.Name}}</strong>
							</div>
							<div class="location">
								<span class="filename">{{.File}}</span>, line <span class="line">{{.Line}}</span>
							</div>
						</div>
					</li>
				{{end}}
				</ul>
			</nav>

			<div class="trace-info clearfix">
			    <div class="title">
			        <h2 class="name">{{.Name}}</h2>
			        <div class="location">
					<span class="filename">{{.File}}</span>
				</div>
			    </div>
			    <div class="code-block">
			        <pre class="prettyprint lang-go linenums:{{.StartLine}}">{{.SourceLines}}</pre>
			    </div>
			</div>
		</section>

		<script src="https://cdn.rawgit.com/google/code-prettify/master/loader/run_prettify.js"></script>
	</body>
</html>`

const err500 = `
<!DOCTYPE html>
<html>
<head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <meta name="author" content="">
    <link rel="shortcut icon" href="/static/img/favicon.ico">
    <link rel="stylesheet" href="/static/assets/vendor.min.css" type="text/css" rel="stylesheet">
    <title>Something went Wrong | Status Code: 500</title>
    <style type="text/css">
        body {
            background-color: #585858;
            color: #666;
            text-align: center;
            font-family: arial, sans-serif;
        }

        div.dialog {
            width: 600px;
            height: 350px;
            padding: 70px 20px 0px 20px;
            margin: 4em auto 0 auto;
        background-color: #DF5643;
        border: 1px #353535 solid;
        }

        h1,
        h2,
        p {
            color: white;
        }

        h1 {
            font-size: 100px;
            line-height: 0px;
            margin-bottom: 120px;
            opacity: 0.3;
        }

        h2 {
            line-height: 10px;
            opacity: 0.8;
        }

        p {
            opacity: 0.5;
        }

        a {
            color: white;
        }

    </style>
</head>

<body>
<div class="dialog">
    <h1>Oops!</h1>
    <h2>The server returned a 500</h2>
    <h2>Something went Wrong</h2>
    <p>
        <a href="/">
            <i class="glyphicon glyphicon-home" data-unicode="e021"></i>
            Now go home &raquo;
        </a>
    </p>
</div>
</body>
</html>
`
