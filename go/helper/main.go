package main

import (
	"encoding/json"
	"io/ioutil"
	"log"

	onix "github.com/kogai/onix-codegen/go"
)

func main() {
	data, err := onix.Read("fixtures/20201200.onix")
	if err != nil {
		log.Fatal(err)
	}
	bytes, err := json.MarshalIndent(data, "", "  ")
	if err != nil {
		log.Fatal(err)
	}
	err = ioutil.WriteFile("fixtures/20201200.json", bytes, 0644)
	if err != nil {
		log.Fatal(err)
	}
}
