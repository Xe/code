package utils

import (
	"fmt"
	"os"
	"sync/atomic"
	"time"
)

// Watchdog is a sentry for knowing if something has been triggered or not every
// so often.
type Watchdog struct {
	Duration time.Duration
	last     int64
}

// Feed the watchdog to prevent program death.
func (w *Watchdog) Feed() {
	atomic.StoreInt64(&w.last, time.Now().Unix())
}

// Start kicks off the watchdog in the background
func (w *Watchdog) Start() {
	go func() {
		for {
			foodvalue := atomic.LoadInt64(&w.last)
			dietime := time.Unix(foodvalue, 0).Add(w.Duration)

			now := time.Now()

			/*
				log.Printf("alive at:   %d", foodvalue)
				log.Printf("expect die: %d", dietime.Unix())
				log.Printf("now:        %d", now.Unix())
			*/

			if now.After(dietime) {
				fmt.Printf("No keepalives for %s. Dying.\n", w.Duration.String())
				os.Exit(1)
			}

			time.Sleep(time.Second)
		}
	}()
}
