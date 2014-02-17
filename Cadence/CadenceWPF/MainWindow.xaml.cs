using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Drawing;
using System.Windows;
using System.IO;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;
using System.Windows.Forms;
using LibEQBeats;

namespace CadenceWPF
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        Track track;
        EQBeatsConnector connecter = new EQBeatsConnector();
        
        public MainWindow()
        {
            InitializeComponent();
            track = connecter.getRandomTracks().First();
        }

        private void Window_Loaded(object sender, RoutedEventArgs e)
        {
            SongName.Content = track.title;
            ArtistName.Content = track.artist.name;
            Uri uri = new Uri(connecter.getArt(track.id), UriKind.RelativeOrAbsolute);
            AlbumArt.Navigate(uri);
        }

        private void button1_Click(object sender, RoutedEventArgs e)
        {
            try
            {
                track = connecter.searchTrack(searchBox.Text).First();
                refreshView();
            }
            catch (Exception)
            {
                SongName.Content = searchBox.Text;
                ArtistName.Content = "Not found";
            }
        }

        private void refreshView()
        {
            SongName.Content = track.title;
            ArtistName.Content = track.artist.name;
            Uri uri = new Uri(connecter.getArt(track.id), UriKind.RelativeOrAbsolute);
            AlbumArt.Navigate(uri);
        }
    }
}
