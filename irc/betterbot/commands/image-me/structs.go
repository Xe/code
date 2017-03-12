package main

type GoogleSearch struct {
	Items []struct {
		DisplayLink      string `json:"displayLink"`
		FormattedURL     string `json:"formattedUrl"`
		HTMLFormattedURL string `json:"htmlFormattedUrl"`
		HTMLSnippet      string `json:"htmlSnippet"`
		HTMLTitle        string `json:"htmlTitle"`
		Kind             string `json:"kind"`
		Link             string `json:"link"`
		Pagemap          struct {
			Imageobject []struct {
				Caption         string `json:"caption"`
				Copyrightholder string `json:"copyrightholder"`
				Description     string `json:"description"`
				Height          string `json:"height"`
				URL             string `json:"url"`
				Width           string `json:"width"`
			} `json:"imageobject"`
		} `json:"pagemap"`
		Snippet string `json:"snippet"`
		Title   string `json:"title"`
	} `json:"items"`
}
