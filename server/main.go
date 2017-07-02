package main

import (
	"bytes"
	"encoding/json"
	"github.com/gorilla/websocket"
	"log"
	"math/rand"
	"net/http"
	"time"
)

type Message struct {
	MeterId  int           `json:"meter_id"`
	Count    int           `json:"count"`
	Duration time.Duration `json:"duration"`
}

func main() {
	addr := ":8001"
	upgrader := websocket.Upgrader{
		CheckOrigin: func(r *http.Request) bool {
			return true
		},
	}

	mux := http.NewServeMux()
	mux.HandleFunc("/ping", cors(ping))
	mux.HandleFunc("/random", createRandom(upgrader))
	if err := http.ListenAndServe(addr, mux); err != nil {
		log.Println(err)
	}
}

func createRandom(upgrader websocket.Upgrader) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		c, err := upgrader.Upgrade(w, r, nil)
		if err != nil {
			log.Println("Upgrade: ", err)
			return
		}
		defer c.Close()

		buf := bytes.NewBuffer([]byte{})
		for {
			buf.Reset()

			msg := randomMsg()
			log.Println(msg)
			err := json.NewEncoder(buf).Encode(msg)
			if err != nil {
				log.Println("Encode: ", err)
				return
			}

			err = c.WriteMessage(websocket.TextMessage, buf.Bytes())
			if err != nil {
				log.Println("Write: ", err)
				return
			}
			duration := time.Millisecond * time.Duration(250+rand.Intn(2250))
			log.Println(duration)
			time.Sleep(duration)
		}
	}
}

func randomMsg() Message {
	meterId := rand.Intn(64352)

	return Message{
		MeterId:  meterId,
		Count:    rand.Intn(21),
		Duration: time.Second * time.Duration(rand.Intn(15)),
	}
}

func ping(w http.ResponseWriter, _ *http.Request) {
	w.Write([]byte("Up"))
}

func cors(next http.HandlerFunc) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Access-Control-Allow-Origin", "*")
		if r.Method == http.MethodOptions {
			return
		}

		next(w, r)
	}
}
