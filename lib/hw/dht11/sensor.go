package dht11

import (
	"errors"
	"log"
	"time"

	"github.com/mrmorphic/hwio"
)

var (
	ErrBadChecksum = errors.New("Checksum failed!")
)

const (
	MaxTimings = 85 // maximum number of timings from hardware
)

type Sensor struct {
	Pin hwio.Pin
}

func New(pin string) (s Sensor, err error) {
	s.Pin, err = hwio.GetPin(pin)
	if err != nil {
		return s, err
	}

	hwio.PinMode(s.Pin, hwio.OUTPUT)

	log.Printf("Made new dht11.Sensor at %s", pin)

	return s, nil
}

func (s Sensor) Read() (humidity string, temp float32, err error) {
	// Low signal for 18 us
	hwio.DigitalWrite(s.Pin, hwio.LOW)
	log.Print("")
	time.Sleep(18 * time.Microsecond)

	// High signal for 40 us
	hwio.DigitalWrite(s.Pin, hwio.HIGH)
	log.Print("writing high")
	time.Sleep(40 * time.Microsecond)

	// Prepare for input
	hwio.PinMode(s.Pin, hwio.INPUT)
	log.Print("Prepare for input")

	var res []bool
	var lastState int

	for i := 0; i < MaxTimings; i++ {
		log.Printf("In MaxTimings loop %d", i)
		counter := 0

		for {
			log.Printf("in inner loop %d", counter)
			now, err := hwio.DigitalRead(s.Pin)
			if err != nil {
				return "", 0.0, err
			}

			if now != lastState {
				break
			}

			lastState = now

			log.Printf("Read %d", now)

			counter++
			time.Sleep(time.Microsecond)

			if counter == 255 {
				break
			}
		}

		lastState, err = hwio.DigitalRead(s.Pin)
		if err != nil {
			return "", 0.0, err
		}

		if counter == 255 {
			break
		}

		res = append(res, lastState == hwio.HIGH) // Append the result to the list
	}

	log.Printf("%#v", res)

	hwio.PinMode(s.Pin, hwio.OUTPUT)

	return
}
