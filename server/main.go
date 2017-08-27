package main

import (
	"bytes"
	"encoding/json"
	"github.com/gorilla/websocket"
	"io"
	"log"
	"math/rand"
	"net/http"
	"os"
	"path/filepath"
	"time"
)

var (
	Address         = ":8001"
	FrontendRelPath = filepath.Join("..", "frontend")
)

type Addition struct {
	MeterId  int           `json:"meter_id"`
	Count    int           `json:"count"`
	Duration time.Duration `json:"duration"`
}

func main() {
	dir, err := filepath.Abs(filepath.Dir(os.Args[0]))
	if err != nil {
		log.Fatal(err)
	}

	upgrader := websocket.Upgrader{
		CheckOrigin: func(r *http.Request) bool {
			return true
		},
	}

	mux := http.NewServeMux()
	mux.HandleFunc("/ping", pingHandler)
	mux.HandleFunc("/random", createRandomHandler(upgrader))
	mux.HandleFunc("/", createDefaultHandler(dir))
	if err := http.ListenAndServe(Address, mux); err != nil {
		log.Println(err)
	}
}

func pingHandler(w http.ResponseWriter, _ *http.Request) {
	w.Write([]byte("Up"))
}

func createRandomHandler(upgrader websocket.Upgrader) http.HandlerFunc {
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

func randomMsg() Addition {
	meterId := rand.Intn(64352)

	return Addition{
		MeterId:  meterId,
		Count:    rand.Intn(21),
		Duration: time.Second * time.Duration(rand.Intn(15)),
	}
}

func createDefaultHandler(dir string) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		path := filepath.Join(dir, FrontendRelPath, r.URL.Path)
		file, err := os.Open(path)
		if err != nil {
			log.Println(path, err)
			if os.IsExist(err) {
				w.WriteHeader(http.StatusNotFound)
				return
			}
			w.WriteHeader(http.StatusInternalServerError)
			return
		}
		defer file.Close()

		io.Copy(w, file)
	}
}
