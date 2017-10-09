package utils

import (
	"fmt"
	"log"
	"os"
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

const prefix = "[slapman][%s]"

type Logger struct {
	Logger *log.Logger
}

func NewLogger(filectx string) (logger *Logger) {
	logger = &Logger{}
	logger.Logger = log.New(os.Stdout, fmt.Sprintf(prefix, filectx), log.Ldate|log.Ltime)
	return
}

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

// NOTE - the semantics here are different from go's logger.Fatal
// It will neither panic nor exit

// Fatal Logs Fatal
func (logger *Logger) Fatal(meta string, e error) {
	logger.Logger.Println(join3(FATAL, meta, e.Error()))
}

// NOTE - the semantics here are different from go's logger.Logger.Fatal
// It will neither panic nor exit

// Fatalf Logs Fatal and Format
func (logger *Logger) Fatalf(format string, v ...interface{}) {
	logger.Logger.Println(join2(FATAL, fmt.Sprintf(format, v...)))
}

// Error Logs Error
func (logger *Logger) Error(meta string, e error) {
	logger.Logger.Println(join3(ERROR, meta, e.Error()))
}

// Errorf Logs Error and Format
func (logger *Logger) Errorf(format string, v ...interface{}) {
	logger.Logger.Println(join2(ERROR, fmt.Sprintf(format, v...)))
}

// Debug Logs Debug
func (logger *Logger) Debug(msg string) {
	logger.Logger.Println(join2(DEBUG, msg))
}

// Debugf Logs Debug and Format
func (logger *Logger) Debugf(format string, v ...interface{}) {
	logger.Logger.Println(join2(DEBUG, fmt.Sprintf(format, v...)))
}

// Warn Logs Warn
func (logger *Logger) Warn(msg string) {
	logger.Logger.Println(join2(WARN, msg))
}

// Warnf Logs Warn and Format
func (logger *Logger) Warnf(format string, v ...interface{}) {
	logger.Logger.Println(join2(WARN, fmt.Sprintf(format, v...)))
}

// Info Logs Info
func (logger *Logger) Info(msg string) {
	logger.Logger.Println(join2(INFO, msg))
}

// Infof Logs Info and Format
func (logger *Logger) Infof(format string, v ...interface{}) {
	logger.Logger.Println(join2(INFO, fmt.Sprintf(format, v...)))
}
