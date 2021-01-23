package main

import (
	"encoding/json"
	"io/ioutil"
	"log"
	"os"

	onix "github.com/kogai/onix-codegen/generated/go/v2"
)

func main() {
	dist := os.Args[1]
	data, err := onix.Read("fixtures/20201200.onix")
	if err != nil {
		log.Fatal(err)
	}
	bytes, err := json.MarshalIndent(data, "", "  ")
	if err != nil {
		log.Fatal(err)
	}
	err = ioutil.WriteFile(dist, bytes, 0644)
	if err != nil {
		log.Fatal(err)
	}
}
