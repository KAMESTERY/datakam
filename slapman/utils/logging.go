package utils

import (
	"fmt"
	"log"
	"net/http"
	"os"

	gae_log "google.golang.org/appengine/log"
)

// Check this out: http://www.goinggo.net/2013/11/using-log-package-in-go.html
// Check this out: https://gist.github.com/alphazero/2718939

const (
	delim  = " - "
	FATAL  = "[FATAL]"
	ERROR  = "[ERROR]"
	WARN   = "[WARN ]"
	DEBUG  = "[DEBUG]"
	INFO   = "[INFO ]"
	levlen = len(INFO)
	len2   = len(delim) + levlen
	len3   = 2*len(delim) + levlen
)

var (
	logger *log.Logger
)

func init() {
	// logger = log.New(os.Stdout, "[slapman] ", 0)
	logger = log.New(os.Stdout, "[slapman] ", log.Ldate|log.Ltime)
	// logger = log.New(ioutil.Discard, "[slapman] ", log.Ldate|log.Ltime|log.Lshortfile)
}

// NOTE - the semantics here are different from go's logger.Fatal
// It will neither panic nor exit
//func Fatal(meta string, e error) {
//	go logger.Println(join3(FATAL, meta, e.Error()))
//}
//
//// NOTE - the semantics here are different from go's logger.Fatal
//// It will neither panic nor exit
//func Fatalf(format string, v ...interface{}) {
//	go logger.Println(join2(FATAL, fmt.Sprintf(format, v...)))
//}
//
//func Error(meta string, e error) {
//	go logger.Println(join3(ERROR, meta, e.Error()))
//}
//func Errorf(format string, v ...interface{}) {
//	go logger.Println(join2(ERROR, fmt.Sprintf(format, v...)))
//}
//
//func Debug(msg string) {
//	go logger.Println(join2(DEBUG, msg))
//}
//
//func Debugf(format string, v ...interface{}) {
//	go logger.Println(join2(DEBUG, fmt.Sprintf(format, v...)))
//}
//
//func Warn(msg string) {
//	go logger.Println(join2(WARN, msg))
//}
//
//func Warnf(format string, v ...interface{}) {
//	go logger.Println(join2(WARN, fmt.Sprintf(format, v...)))
//}
//
//func Info(msg string) {
//	go logger.Println(join2(INFO, msg))
//}
//
//func Infof(format string, v ...interface{}) {
//	go logger.Println(join2(INFO, fmt.Sprintf(format, v...)))
//}

func join2(level, msg string) string {
	n := len(msg) + len2
	j := make([]byte, n)
	o := copy(j, level)
	o += copy(j[o:], delim)
	copy(j[o:], msg)
	return string(j)
}

func join3(level, meta, msg string) string {
	n := len(meta) + len(msg) + len3
	j := make([]byte, n)
	o := copy(j, level)
	o += copy(j[o:], delim)
	o += copy(j[o:], meta)
	o += copy(j[o:], delim)
	copy(j[o:], msg)
	return string(j)
}

// GAE Logging

// NOTE - the semantics here are different from go's logger.Fatal
// It will neither panic nor exit

// Fatal Logs Fatal
func Fatal(r *http.Request, meta string, e error) {
	if mode := os.Getenv("MODE"); mode == "appengine" && r != nil {
		ctx := GetCtx(r)
		go gae_log.Criticalf(ctx, meta, e.Error())
	} else {
		go logger.Println(join3(FATAL, meta, e.Error()))
	}
}

// NOTE - the semantics here are different from go's logger.Fatal
// It will neither panic nor exit

// Fatalf Logs Fatal and Format
func Fatalf(r *http.Request, format string, v ...interface{}) {
	if mode := os.Getenv("MODE"); mode == "appengine" && r != nil {
		ctx := GetCtx(r)
		go gae_log.Criticalf(ctx, format, v...)
	} else {
		go logger.Println(join2(FATAL, fmt.Sprintf(format, v...)))
	}
}

// Error Logs Error
func Error(r *http.Request, meta string, e error) {
	if mode := os.Getenv("MODE"); mode == "appengine" && r != nil {
		ctx := GetCtx(r)
		go gae_log.Criticalf(ctx, meta, e.Error())
	} else {
		go logger.Println(join3(ERROR, meta, e.Error()))
	}
}

// Errorf Logs Error and Format
func Errorf(r *http.Request, format string, v ...interface{}) {
	if mode := os.Getenv("MODE"); mode == "appengine" && r != nil {
		ctx := GetCtx(r)
		go gae_log.Criticalf(ctx, format, v...)
	} else {
		go logger.Println(join2(ERROR, fmt.Sprintf(format, v...)))
	}
}

// Debug Logs Debug
func Debug(r *http.Request, msg string) {
	if mode := os.Getenv("MODE"); mode == "appengine" && r != nil {
		ctx := GetCtx(r)
		go gae_log.Debugf(ctx, msg)
	} else {
		go logger.Println(join2(DEBUG, msg))
	}
}

// Debugf Logs Debug and Format
func Debugf(r *http.Request, format string, v ...interface{}) {
	if mode := os.Getenv("MODE"); mode == "appengine" && r != nil {
		ctx := GetCtx(r)
		go gae_log.Debugf(ctx, format, v...)
	} else {
		go logger.Println(join2(DEBUG, fmt.Sprintf(format, v...)))
	}
}

// Warn Logs Warn
func Warn(r *http.Request, msg string) {
	if mode := os.Getenv("MODE"); mode == "appengine" && r != nil {
		ctx := GetCtx(r)
		go gae_log.Warningf(ctx, msg)
	} else {
		go logger.Println(join2(WARN, msg))
	}
}

// Warnf Logs Warn and Format
func Warnf(r *http.Request, format string, v ...interface{}) {
	if mode := os.Getenv("MODE"); mode == "appengine" && r != nil {
		ctx := GetCtx(r)
		go gae_log.Warningf(ctx, format, v...)
	} else {
		go logger.Println(join2(WARN, fmt.Sprintf(format, v...)))
	}
}

// Info Logs Info
func Info(r *http.Request, msg string) {
	if mode := os.Getenv("MODE"); mode == "appengine" && r != nil {
		ctx := GetCtx(r)
		go gae_log.Infof(ctx, msg)
	} else {
		go logger.Println(join2(INFO, msg))
	}
}

// Infof Logs Info and Format
func Infof(r *http.Request, format string, v ...interface{}) {
	if mode := os.Getenv("MODE"); mode == "appengine" && r != nil {
		ctx := GetCtx(r)
		go gae_log.Infof(ctx, format, v...)
	} else {
		go logger.Println(join2(INFO, fmt.Sprintf(format, v...)))
	}
}
