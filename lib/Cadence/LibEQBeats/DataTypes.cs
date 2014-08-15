using System;
using System.Collections.Generic;

namespace LibEQBeats {
	public class Download
    {
        public string mp3 { get; set; }
        public string vorbis { get; set; }
    }

    public class Track
    {
        public int id { get; set; }
        public string title { get; set; }
        public Artist artist { get; set; }
        public string link { get; set; }
        public Download download { get; set; }
    }

    public class Artist 
    {
        public int id { get; set; }
        public string name { get; set; }
        public string description { get; set; }
        public string html_description { get; set; }
        public List<Track> tracks { get; set; }
        public List<Playlist> playlists { get; set; }
        public string link { get; set; }
    }

    public class Author
    {
        public int id { get; set; }
        public string name { get; set; }
        public string link { get; set; }
    }

    public class Playlist
    {
        public int id { get; set; }
        public string name { get; set; }
        public Author author { get; set; }
        public string description { get; set; }
        public string html_description { get; set; }
        public List<Track> tracks { get; set; }
        public string link { get; set; }
    }
}