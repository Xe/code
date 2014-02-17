using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Drawing;
using LibEQBeats;

namespace LibTester
{
    class Program
    {
        static void Main(string[] args)
        {
            EQBeatsConnector test = new EQBeatsConnector();

            foreach (Track t in test.searchTrack("Amazing Wondermare"))
            {
                Artist artist = test.getCompleteArtist(t.artist.id);

                Console.WriteLine("{0}'s discography: \n", artist.name);

                foreach (Track t_ in artist.tracks)
                {
                    Console.WriteLine(t_.title);
                }

                Console.WriteLine("===\n");
            }

            int r = 4;
        }
    }
}
