package utils

import (
	"context"
	"io/ioutil"
	"net"
	"net/http"
	"os"
	"time"

	gcontext "github.com/gorilla/context"
	//"golang.org/x/net/context"
	"google.golang.org/appengine"
	"google.golang.org/appengine/urlfetch"
)

const X_FORWARDED_FOR = "X-FORWARDED-FOR"

func NewHttpClient(r *http.Request) (client *http.Client) {
	if mode := os.Getenv("MODE"); mode == "appengine" {
		ctx := GetCtx(r)
		client = urlfetch.Client(ctx)
	} else {
		client = http.DefaultClient
	}
	return
}

func HttpGet(r *http.Request, url string) (string, error) {

	respChan := make(chan string, 1)
	errChan := make(chan error, 1)
	client := NewHttpClient(r)

	go func() {
		resp, err := client.Get("http://" + url)
		if err != nil {
			Errorf(r, "ERROR:::: %+v", err.Error())
			errChan <- err
		} else {
			defer resp.Body.Close()
			bodyBytes, err := ioutil.ReadAll(resp.Body)
			if err != nil {
				Errorf(r, "ERROR:::: %+v", err.Error())
				errChan <- err
			} else {
				respChan <- string(bodyBytes)
			}
		}
	}()

	select {
	case body := <-respChan:
		return body, nil
	case err := <-errChan:
		return "", err
	case <-time.After(1 * time.Second):
		return "Too Busy", nil
	}
}

func GetCtx(r *http.Request) (ctx context.Context) {

	if r == nil {
		return // Bail when there is no Request
	}

	//noinspection ALL
	if mode := os.Getenv("MODE"); mode == "appengine" {
		ctx = appengine.NewContext(r)
	} else {
		ctx = context.Background()
		gctx := gcontext.GetAll(r)
		for ctxKey, ctxVal := range gctx {
			context.WithValue(ctx, ctxKey, ctxVal)
		}
	}

	return
}

// http://www.gofragments.net/client/blog/netweb/2015/12/01/httpClientIPaddress/index.html
func getClientIP(r *http.Request) (clientIP net.IP) {
	// get client ip address
	ip, port, err := net.SplitHostPort(r.RemoteAddr)
	if err != nil {
		Errorf(r, "ERROR:::: clientIP: [%+v] is not IP:port", r.RemoteAddr)
	}
	clientIP = net.ParseIP(ip)
	if clientIP == nil {
		Errorf(r, "ERROR:::: clientIP: [%+v] is not IP:port", r.RemoteAddr)
	}

	Infof(r, "Client IP: %+v", clientIP)
	Infof(r, "Client Port: %+v", port)

	// sometimes, the user acccess the web server via a proxy or load balancer.
	// The above IP address will be the IP address of the proxy or load balancer
	// and not the user's machine.
	// let's get the request HTTP header "X-Forwarded-For (XFF)"
	// if the value returned is not null, then this is the real IP address
	// of the user.
	proxied := r.Header.Get(X_FORWARDED_FOR)
	if proxied != "" {
		clientIP = net.ParseIP(proxied)
		Infof(r, "Forwarded for %+v", clientIP)
		Infof(r, "X-Forwarded-For: indicates this %+v is the real address", clientIP)
	}

	Infof(r, "Client IP: %+v", clientIP)
	Infof(r, "Client Port: %+v", port)

	return
}

func SetClientIP(r *http.Request) {
	// get client ip address
	clientIP := getClientIP(r)
	gcontext.Set(r, UserIpCtxKey, clientIP)
}

func GetJson(ctx context.Context, url string, target interface{}) error {

	req, err := http.NewRequest(http.MethodGet, url, nil)
	if err != nil {
		return err
	}

	req = req.WithContext(ctx)

	res, err := http.DefaultClient.Do(req)
	if err != nil {
		return err
	}

	defer res.Body.Close()

	return DecodeJson(res.Body, target)
}

func CorsHeaders(w http.ResponseWriter) {
	// CORS Headers
	w.Header().Set("Access-Control-Allow-Headers", "X-Requested-With, Content-Type")
	w.Header().Set("Access-Control-Allow-Methods", "POST,OPTIONS,GET,PUT,PATCH,DELETE")
	w.Header().Set("Access-Control-Allow-Origin", "*")
}
