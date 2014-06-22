using Soup;

void main () {
    // add your twitter username
    string username = "theprincessxena";
    
    // format the URL to use the username as the filename
    string url = "http://twitter.com/users/%s.xml".printf (username);

    stdout.printf ("Getting status for %s\n", username);

    // create an HTTP session to twitter
    var session = new Soup.Session ();
    var message = new Soup.Message ("GET", url);

    // send the HTTP request and wait for response
    session.send_message (message);

    // output the XML result to stdout 
    stdout.write (message.response_body.data);
}
