package utils

import (
	"google.golang.org/appengine/memcache"
	"slapman/utils/cache"
	"net/http"
	"time"
)

var (
	utilscache *cache.Cache
)

func init() {
	utilscache = cache.New(15*time.Minute, 60*time.Second)
}

func GetCache(r *http.Request, k string) (interface{}, bool) {
	if r == nil {
		x, found := utilscache.Get(k)
		return x, found
	} else {
		ctx := GetCtx(r)
		x, err := memcache.Get(ctx, k)
		if err != nil {
			return nil, false
		} else {
			return x, true
		}
	}
}

func AddCache(r *http.Request, k string, x interface{}, d time.Duration) error {
	if r == nil {
		cc := cache.New(15*time.Minute, 60*time.Second)
		return cc.Add(k, x, d)
	} else {
		ctx := GetCtx(r)
		item := &memcache.Item{
			Key:        k,
			Object:     x,
			Expiration: d,
		}
		return memcache.Add(ctx, item)
	}
}
