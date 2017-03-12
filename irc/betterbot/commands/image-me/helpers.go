package main

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"net/http"
	"net/url"
	"os"
	"strings"

	"github.com/Pallinder/go-randomdata"
	"github.com/Sirupsen/logrus"
	"github.com/paddycarey/gophy"
)

func helperExtractTerms(commandName string, messageText string) string {
	sentence := strings.SplitAfterN(messageText, commandName, 2)

	return sentence[1]
}

func giphySearch(search string) (string, error) {
	giphyCO := &gophy.ClientOptions{}
	giphyClient := gophy.NewClient(giphyCO)

	escapedStr := url.QueryEscape(search)

	gifs, _, gifErr := giphyClient.SearchGifs(escapedStr, "pg-13", 10, 0)
	if gifErr != nil {
		return "", gifErr
	}

	gifCount := len(gifs)
	if gifCount == 0 {
		return fmt.Sprintf("Sorry, Giphy didn't return any results for _%s_", search), nil
	}

	selectedGif := randomdata.Number(0, gifCount-1)
	return gifs[selectedGif].Images.Original.URL, nil
}

func imageMe(search string, user string) (string, error) {
	// Check to see if the user has hit their Quota...
	// t := time.Now()
	// yr, mon, day := t.Date()
	// limitKey := fmt.Sprintf("%s:%d-%d-%d", user, yr, mon, day)

	// if _, ok := cmdTracking[limitKey]; ok {
	// 	if cmdTracking[limitKey] > 9 {
	// 		return "You have run out of image searches for today. Try again tomorrow"
	// 	}
	// }

	httpClient := &http.Client{}

	APIpath := fmt.Sprintf("https://www.googleapis.com/customsearch/v1?q=%s&key=%s&cx=%s&searchtype=image&num=10",
		url.QueryEscape(search),
		os.Getenv("GOOGLE_CLIENT_ID"),
		os.Getenv("GOOGLE_CLIENT_SECRET"))

	// Create the HTTP Request and Headers for Kong to work.
	httpReq, err := http.NewRequest("GET", APIpath, nil)
	if err != nil {
		return "", err
	}

	httpReq.Header.Add("Content-Type", "application/json")

	httpResponse, err := httpClient.Do(httpReq)
	if err != nil {
		return "", err
	}

	defer httpResponse.Body.Close()
	bodyContent, err := ioutil.ReadAll(httpResponse.Body)
	if err != nil {
		return "", err
	}

	if httpResponse.StatusCode == 403 {
		logrus.Warn("403 Forbidden Status received.")
		return fmt.Sprintf("We appear to have run out of Google Search credits today. These will reset within 24 hours.\n Try not to be so colletively greedy tomorrow, maybe? You can always use _animate me_ for your memes."), nil
	}

	if httpResponse.StatusCode != 200 {
		logrus.Error("Other Non-200 status received.")
		return "Sorry, something went wrong!", nil
	}

	var images GoogleSearch
	json.Unmarshal(bodyContent, &images)

	imgCount := len(images.Items)
	if imgCount == 0 {
		return fmt.Sprintf("Sorry, Google didn't return any results for _%s_", search), nil
	}

	selectedImg := randomdata.Number(0, imgCount-1)

	return images.Items[selectedImg].Pagemap.Imageobject[0].URL, nil
}
