package onix

import (
	"bytes"
	"encoding/xml"
	"io"
	"io/ioutil"
)

// Read read ONIX for Books 2.1 format file.
func Read(input string) (*ONIXMessage, error) {
	file, err := ioutil.ReadFile(input)
	if err != nil {
		return nil, err
	}

	var data ONIXMessage
	decoder := xml.NewDecoder(bytes.NewReader(file))
	decoder.CharsetReader = func(label string, input io.Reader) (io.Reader, error) {
		return input, nil
	}

	if err := decoder.Decode(&data); err != nil {
		return nil, err
	}
	return &data, nil
}
