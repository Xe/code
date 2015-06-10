/*
Command krewella is an HTTP POST -> IRC message gateway.
*/
package main

import (
	"fmt"
	"io/ioutil"
	"net/http"
	"os"
	"strings"

	"christine.website/go/krewella/irc"
	"christine.website/go/krewella/security"
	"github.com/codegangsta/negroni"
	"github.com/drone/routes"
)

var (
	router *routes.RouteMux
	n      *negroni.Negroni
	auth   *security.Auth

	bots   map[string]*irc.Bot // network -> bot pair
	apikey string
)

func createBots() error {
	bots = make(map[string]*irc.Bot)

	networklist := os.Getenv("KREWELLA_NETWORKS")
	networks := strings.Split(networklist, ",")

	for _, network := range networks {
		bot, err := irc.New(network)
		if err != nil {
			return err
		}

		bots[strings.ToLower(network)] = bot
	}

	return nil
}

func destroyBots() {
	for _, bot := range bots {
		bot.IrcObj.Quit()
	}
}

func sendMessageToChannel(w http.ResponseWriter, req *http.Request) {
	params := req.URL.Query()

	network := params.Get(":network")
	channel := params.Get(":channel")

	channel = "#" + channel

	var bot *irc.Bot
	bot, ok := bots[strings.ToLower(network)]
	if !ok {
		return
	}

	body, err := ioutil.ReadAll(req.Body)
	if err != nil {
		w.WriteHeader(http.StatusUnsupportedMediaType)
		fmt.Fprintf(w, "Could not decode message")
		return
	}

	if string(body) == "" {
		w.WriteHeader(http.StatusUnsupportedMediaType)
		fmt.Fprintf(w, "Could not decode message")
		return
	}

	bot.Talk(channel, string(body))

	w.WriteHeader(http.StatusOK)
	fmt.Fprintf(w, "Your message \"%s\" was sent to %s on %s!",
		string(body), channel, network)
}

func main() {
	err := createBots()
	if err != nil {
		panic(err)
	}
	defer destroyBots()

	router = routes.New()

	router.Get("/", func(w http.ResponseWriter, req *http.Request) {
		fmt.Fprint(w, "pancakes")
	})

	router.Post("/message/:network/:channel", sendMessageToChannel)

	n = negroni.Classic()
	n.Use(security.NewAuth(os.Getenv("KREWELLA_KEY")))

	n.UseHandler(router)

	port := os.Getenv("PORT")

	if port == "" {
		port = "3000"
	}

	n.Run(":" + port)
}
