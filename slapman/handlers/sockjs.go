package handlers

import (
	"fmt"
	"github.com/igm/pubsub"
	"gopkg.in/igm/sockjs-go.v2/sockjs"
)

var (
	chat pubsub.Publisher
)

func SockEchoHandler(session sockjs.Session) {
	fmt.Println("new sockjs session established")
	var closedSession = make(chan struct{})
	chat.Publish("[info] new participant joined chat")
	defer chat.Publish("[info] participant left chat")
	go func() {
		reader, _ := chat.SubChannel(nil)
		for {
			select {
			case <-closedSession:
				return
			case msg := <-reader:
				if err := session.Send(msg.(string)); err != nil {
					return
				}
			}

		}
	}()
	for {
		if msg, err := session.Recv(); err == nil {
			chat.Publish(msg)
			continue
		}
		break
	}
	close(closedSession)
	fmt.Println("sockjs session closed")
}
