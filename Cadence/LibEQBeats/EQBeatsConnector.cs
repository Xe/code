using System;
using System.Net;
using System.IO;
using System.Text;
using System.Drawing;
using System.Collections.Generic;
using System.Windows.Media.Imaging;
using Newtonsoft.Json;

namespace LibEQBeats {
	public class EQBeatsConnector {

        /// <summary>
        /// Get a track based on an ID
        /// </summary>
        /// <param name="id">The ID number of the track</param>
        /// <returns>A track object with the corresponding ID</returns>
        public Track getTrack(int id)
        {
            string url = "http://eqbeats.org/track/" + id.ToString() + "/json";
            //Console.WriteLine(url);
            return JsonConvert.DeserializeObject<Track>(getUrl(url));
        }

        /// <summary>
        /// Gets complete info about an artist based on the ID
        /// </summary>
        /// <param name="id"></param>
        /// <returns></returns>
        public Artist getCompleteArtist(int id)
        {
            string url = "http://eqbeats.org/user/" + id.ToString() + "/json";
            //Console.WriteLine(url);
            return JsonConvert.DeserializeObject<Artist>(getUrl(url));
        }

        /// <summary>
        /// Gets info about a playlist
        /// </summary>
        /// <param name="id"></param>
        /// <returns></returns>
        public Playlist getPlaylist(int id)
        {
            string url = "http://eqbeats.org/playlist/" + id.ToString() + "/json";
            //Console.WriteLine(url);
            return JsonConvert.DeserializeObject<Playlist>(getUrl(url));
        }

        /// <summary>
        /// Gets the album art for a track. Don't use this one.
        /// </summary>
        /// <param name="trackId"></param>
        /// <returns></returns>
        public String getArt(int trackId)
        {
            string path = "http://eqbeats.org/track/" + trackId.ToString() + "/art";
            
            return path;
        }

        /// <summary>
        /// Returns a list of 50 random tracks
        /// </summary>
        /// <returns></returns>
        public List<Track> getRandomTracks()
        {
            string url = "http://eqbeats.org/tracks/random/json";
            //Console.WriteLine(url);
            return JsonConvert.DeserializeObject<List<Track>>(getUrl(url));
        }

        /// <summary>
        /// Returns the 50 latest tracks uploaded
        /// </summary>
        /// <returns></returns>
        public List<Track> getLatestTracks()
        {
            string url = "http://eqbeats.org/tracks/latest/json";
            //Console.WriteLine(url);
            return JsonConvert.DeserializeObject<List<Track>>(getUrl(url));
        }

        /// <summary>
        /// Returns the 50 most recently featured tracks
        /// </summary>
        /// <returns></returns>
        public List<Track> getFeaturedTracks()
        {
            string url = "http://eqbeats.org/tracks/featured/json";
            //Console.WriteLine(url);
            return JsonConvert.DeserializeObject<List<Track>>(getUrl(url));
        }

        /// <summary>
        /// Searches for a track based on the terms you give
        /// </summary>
        /// <param name="terms"></param>
        /// <returns></returns>
        public List<Track> searchTrack(string terms)
        {
            string url = "http://eqbeats.org/tracks/search/json?q=" + terms;
            //Console.WriteLine(url);
            return JsonConvert.DeserializeObject<List<Track>>(getUrl(url));
        }

        /// <summary>
        /// Searches for an artist based on the terms you give
        /// </summary>
        /// <param name="terms"></param>
        /// <returns></returns>
        public List<Artist> searchArtist(string terms)
        {
            string url = "http://eqbeats.org/users/search/json?q=" + terms;
            //Console.WriteLine(url);
            return JsonConvert.DeserializeObject<List<Artist>>(getUrl(url));
        }

        private string getUrl(string url)
        {
            WebClient wc = new WebClient();
            byte[] data =
                wc.DownloadData(url);
            string strData = Encoding.ASCII.GetString(data);

            return strData;
        }
	}
}

