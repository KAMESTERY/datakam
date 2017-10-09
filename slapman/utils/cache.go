package utils

import (
	"context"
	"slapman/utils/cache"
	"time"

	"google.golang.org/appengine/memcache"
)

var (
	utilscache *cache.Cache
)

func init() {
	utilscache = cache.New(15*time.Minute, 60*time.Second)
}

func GetCache(ctx context.Context, k string) (interface{}, bool) {
	if ctx == nil {
		x, found := utilscache.Get(k)
		return x, found
	} else {
		x, err := memcache.Get(ctx, k)
		if err != nil {
			return nil, false
		} else {
			return x, true
		}
	}
}

func AddCache(ctx context.Context, k string, x interface{}, d time.Duration) error {
	if ctx == nil {
		cc := cache.New(15*time.Minute, 60*time.Second)
		return cc.Add(k, x, d)
	} else {
		item := &memcache.Item{
			Key:        k,
			Object:     x,
			Expiration: d,
		}
		return memcache.Add(ctx, item)
	}
}
