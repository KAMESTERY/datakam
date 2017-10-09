package utils

import (
	"context"
	"io/ioutil"
	"net"
	"net/http"
)

const X_FORWARDED_FOR = "X-FORWARDED-FOR"

var httputils_logger = NewLogger("utilshttp")

func GetString(ctx context.Context, url string) (string, error) {

	respChan := make(chan string, 1)
	errChan := make(chan error, 1)
	go func() {
		req, err := http.NewRequest(http.MethodGet, url, nil)
		if err != nil {
			errChan <- err
		}

		req = req.WithContext(ctx)

		res, err := http.DefaultClient.Do(req)
		if err != nil {
			errChan <- err
		}

		defer res.Body.Close()

		bodyBytes, err := ioutil.ReadAll(res.Body)
		if err != nil {
			errChan <- err
		} else {
			respChan <- string(bodyBytes)
		}
	}()
	select {
	case resp := <-respChan:
		return resp, nil
	case err := <-errChan:
		httputils_logger.Errorf("ERROR:::: %+v", err.Error())
		return "", err
	case <-ctx.Done():
		httputils_logger.Errorf("ERROR:::: %+v", ctx.Err())
		return "", ctx.Err()
	}
}

// http://www.gofragments.net/client/blog/netweb/2015/12/01/httpClientIPaddress/index.html
func GetClientIP(r *http.Request) (clientIP net.IP, err error) {

	ctx := r.Context()
	respChan := make(chan bool, 1)
	errChan := make(chan error, 1)

	go func() {
		// get client ip address
		ip, port, err := net.SplitHostPort(r.RemoteAddr)
		if err != nil {
			httputils_logger.Errorf("ERROR:::: clientIP: [%+v] is not IP:port", r.RemoteAddr)
			errChan <- err
		}
		clientIP = net.ParseIP(ip)
		if clientIP == nil {
			httputils_logger.Errorf("ERROR:::: clientIP: [%+v] is not IP:port", r.RemoteAddr)
			errChan <- err
		}

		httputils_logger.Infof("Client IP: %+v", clientIP)
		httputils_logger.Infof("Client Port: %+v", port)

		// sometimes, the user acccess the web server via a proxy or load balancer.
		// The above IP address will be the IP address of the proxy or load balancer
		// and not the user's machine.
		// let's get the request HTTP header "X-Forwarded-For (XFF)"
		// if the value returned is not null, then this is the real IP address
		// of the user.
		proxied := r.Header.Get(X_FORWARDED_FOR)
		if proxied != "" {
			clientIP = net.ParseIP(proxied)
			httputils_logger.Infof("Forwarded for %+v", clientIP)
			httputils_logger.Infof("X-Forwarded-For: indicates this %+v is the real address", clientIP)
		}

		httputils_logger.Infof("Client IP: %+v", clientIP)
		httputils_logger.Infof("Client Port: %+v", port)

		respChan <- true
	}()

	select {
	case <-respChan:
		return
	case err = <-errChan:
		httputils_logger.Errorf("ERROR:::: %+v", err.Error())
		return
	case <-ctx.Done():
		err = ctx.Err()
		httputils_logger.Errorf("ERROR:::: %+v", err)
		return
	}
}

func GetJson(ctx context.Context, url string, target interface{}) error {

	respChan := make(chan bool, 1)
	errChan := make(chan error, 1)

	go func() {
		req, err := http.NewRequest(http.MethodGet, url, nil)
		if err != nil {
			errChan <- err
		}

		req = req.WithContext(ctx)

		res, err := http.DefaultClient.Do(req)
		if err != nil {
			errChan <- err
		}

		defer res.Body.Close()

		err = DecodeJson(res.Body, target)
		if err != nil {
			errChan <- err
		}

		respChan <- true
	}()

	select {
	case <-respChan:
		return nil
	case err := <-errChan:
		httputils_logger.Errorf("ERROR:::: %+v", err.Error())
		return err
	case <-ctx.Done():
		httputils_logger.Errorf("ERROR:::: %+v", ctx.Err())
		return ctx.Err()
	}
}

func CorsHeaders(w http.ResponseWriter) {
	// CORS Headers
	w.Header().Set("Access-Control-Allow-Headers", "X-Requested-With, Content-Type")
	w.Header().Set("Access-Control-Allow-Methods", "POST,OPTIONS,GET,PUT,PATCH,DELETE")
	w.Header().Set("Access-Control-Allow-Origin", "*")
}
