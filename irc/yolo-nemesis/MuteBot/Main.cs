using System;
using System.Collections.Generic;
using System.IO;
using System.Net;
using System.Net.Sockets;
using System.Text;

namespace MuteBot
{
	struct IrcUser {
		public string nick;
		public string user;
		public string host;
		public string channel;
		public string muter;
		public double unMuteTime;
	}

	class MainClass
	{
		private static List<IrcUser> punishees = new List<IrcUser>();
		private static StreamWriter swrite;
		private static StreamReader sread;

		public static void Main (string[] args)
		{
			// Configurable bot vars
			string nickname = "NiiForcer";
			string server = "127.0.0.1";
			int port = 6668;

			// Socket vars
			NetworkStream sstream;
			TcpClient irc;

			// Generic bot vars
			bool botOn = true;
			string line; // incoming line
			string[] splitLine; // array of line, expoded by \s

			try
			{
				// Connect
				irc = new TcpClient(server, port);
				sstream = irc.GetStream();
				sread = new StreamReader(sstream);
				swrite = new StreamWriter(sstream);
			}
			catch (Exception e)
			{
				Console.WriteLine("Error connecting to {0}: {1}", server, e);
				return;
			}

			// Identify
			swrite.WriteLine("USER {0} {0} {0} :{1}", nickname, nickname);
			swrite.WriteLine("NICK {0}", nickname);
			swrite.WriteLine("PASS YOLOSWAG420"); //auth to znc
			swrite.Flush();

			SendLine("PRIVMSG #b0atnet :Bot restarted.");

			while (botOn)
			{

				if ((line = sread.ReadLine()) != null)
				{
					splitLine = line.Split(' ');

					if(splitLine[0].StartsWith(":GlaD0S!") || splitLine[0].StartsWith(":NiiForcer!")) {
						Console.WriteLine("Line ignored!");

						continue;
					}

					if (splitLine.Length > 0)
					{
						switch (splitLine[1])
						{
							case "KICK":
								//:swashy!~llehs.eih@Rizon-7396194D.om.om.cox.net KICK #tulpa.info Tulpa :Tulpa
								IrcUser kicker = MakeUserFromString(line);

								if (!kicker.nick.Equals("Yggdrasil")) {
									SendLine("PRIVMSG #b0atnet :{0} kicked {1} from {2}", new object[] { kicker.nick, splitLine[3], splitLine[2] });
								} else {
									SendLine("PRIVMSG #b0atnet :{0} was banned.", splitLine[3]);
								}
								break;
							case "366":
								Console.WriteLine("Connected to IRC :)");
								break;
								case "MODE":
									if (splitLine[3] == "-v") {
										MuteUser(splitLine[4], splitLine[2], MakeUserFromString(line).nick);
										SendLine("NOTICE {0} :You should explain the reason for the mute of {1} in #b0atnet along with duration. Thanks :)", new object[] { MakeUserFromString(line).nick, splitLine[4] });
										Console.WriteLine("MUTER: " + MakeUserFromString(line).nick);
									} else if (splitLine[3] == "+v") {
										UnMuteUser(splitLine[4], MakeUserFromString(line).nick);
								}
								  break;*/
							case "JOIN":

								if(AllowVoiceUser(line)) {
									SendLine("MODE {0} +v {1}", new object[] {splitLine[2].Substring(1), MakeUserFromString(line).nick});
									Console.WriteLine("User {0} voiced", MakeUserFromString(line).nick);
								} else {
									SendLine("NOTICE {0} :You were muted. You will be unmuted when a mod thinks you should be.", MakeUserFromString(line).nick);
									SendLine("PRIVMSG #b0atnet :Oh goody {0} has attempted to evade a mute. Shame.", MakeUserFromString(line).nick);
								}
								swrite.Flush();

								break;
							case "PRIVMSG":
								if(GetSpokenLine(line).Equals("!listprint") && splitLine[2].Equals("#b0atnet")) {
									IrcUser s = MakeUserFromString(line);
									foreach(IrcUser user in punishees) {
										SendLine("PRIVMSG {4} :367 {0}!{1}@{2} on {3}\n", new object[] { user.nick, user.user, user.host, user.channel, s.nick });
									}
									SendLine("PRIVMSG {0} :368 End of channel mute list", s.nick);

									SendLine("PRIVMSG #b0atnet :{0} Please see my many pm's", s.nick);
								}

								if(GetSpokenLine(line).Equals("!anhero") && splitLine[2].Equals("#b0atnet")) {
									throw new Exception();
								}

								break;
							}

						//CheckExpiredMutes();

						if (splitLine[0] == "PING")
						{
							SendLine("PONG {0}", splitLine[1]);
							swrite.Flush();
						}

					}

					Console.WriteLine(line);

					//CheckExpiredMutes();

				} else {
					//CheckExpiredMutes();
				}

				//CheckExpiredMutes();
			}
			// Clean up
			swrite.Close();
			sread.Close();
			irc.Close();
		}

		private static string GetSpokenLine(string line)
		{
			if (line.Split(':').Length >= 2)
				return line.Split(':')[2];

			return "";
		}

		private static IrcUser MakeUserFromString (string user)
		{
			IrcUser ret = new IrcUser();

			user = user.Split(' ')[0];

			int bang, at;

			bang = user.IndexOf("!");
			at = user.IndexOf("@");

			ret.nick = user.Substring(1,bang-1);
			ret.user = user.Substring(bang+1, (at-bang)-1);
			ret.host = user.Substring(at+1);

			return ret;
		}

		private static bool AllowVoiceUser (string line)
		{
			IrcUser joinee = MakeUserFromString (line);

			foreach (IrcUser user in punishees) {
				if (user.host.Equals (joinee.host) || user.nick.Equals (joinee.nick)) {
					return false;
				}
			}

			return true;
		}

		private static IrcUser DoWhoisLookupOnUser (string nick)
		{
			IrcUser lookup = new IrcUser ();

			lookup.nick = nick;

			SendLine ("WHO {0}", nick);

			string whoLine = sread.ReadLine ();
			sread.ReadLine ();

			Console.Out.WriteLine ("Who: " + whoLine);

			string[] whoLineSplit = whoLine.Split (' ');

			if (whoLineSplit [2].Equals ("MODE")) {
				return DoWhoisLookupOnUser (nick);
			}

			try {
				lookup.user = whoLineSplit [4];
				lookup.host = whoLineSplit [5];
			} catch (Exception e) {
				e.ToString();

				return DoWhoisLookupOnUser(nick);
			}

			return lookup;
		}

		private static void SendLine(string line) 
		{
			swrite.WriteLine(line);
			swrite.Flush();

			Console.WriteLine(">>> " + line);
		}

		private static void SendLine (string line, object thing)
		{
			SendLine(String.Format(line, new object[] {thing}));
		}

		private static void SendLine(string line, object[] stuff) 
		{
			SendLine(String.Format(line, stuff));
		}

		private bool SameUser(IrcUser a, IrcUser b) {
			return (a.host.Equals(b.host));
		}

		private static void MuteUser (string nick, string channel, string muter)
		{ 
			IrcUser punishee = new IrcUser ();

			punishee.nick = nick;

			punishee.channel = channel;

			punishee = DoWhoisLookupOnUser (punishee.nick);

			foreach(IrcUser user in punishees) {
				if (user.nick.Equals(punishee.nick) &&
						user.user.Equals(punishee.user) &&
						user.host.Equals(punishee.host))
				{
					return;
				}
			}

			punishee.unMuteTime = unix_timestamp() + 300;

			punishee.muter = muter;

			punishees.Add(punishee);

			SendLine("NOTICE {0} :You have been devoiced, you may not speak in {1} until a moderator revoices you", new object[] {punishee.nick, channel});
			SendLine("MODE {1} -v {0}", new object[] {nick, channel});

			DateTime unMuteTime = UnixTimeStampToDateTime(punishee.unMuteTime).ToUniversalTime();

			SendLine("PRIVMSG #b0atnet :{0}!{1}@{2} added to devoice list by {3} on {4}, should be revoiced at {5}, {6} UCT.", new object[] { punishee.nick, punishee.user, punishee.host, muter, channel, unMuteTime.ToLongTimeString(), unMuteTime.ToLongDateString() });

			Console.WriteLine("{0}!{1}@{2} added to devoice list\n", new object[] { punishee.nick, punishee.user, punishee.host });
		}

		private static void UnMuteUser (string nick, string muter) {
			int i = 0;

			IrcUser toDelete = new IrcUser();

			toDelete.nick = nick;

			toDelete = DoWhoisLookupOnUser(toDelete.nick);

			foreach (IrcUser user in punishees) {
				if(user.host.Equals(toDelete.host) || user.nick.Equals(toDelete.nick)) {
					punishees.RemoveAt(i);

					SendLine("NOTICE {0} :You have been revoiced, you may speak again.", user.nick);

					SendLine("PRIVMSG #b0atnet :{0}!{1}@{2} removed from devoice list by {3} on.", new object[] { user.nick, user.user, user.host, muter, user.channel });

					break;
				}

				i++;
			}
		}

		public static double unix_timestamp() 
		{
			TimeSpan unix_time = (System.DateTime.UtcNow - new DateTime(1970, 1, 1, 0, 0, 0));
			return unix_time.TotalSeconds;
		}

		public static DateTime UnixTimeStampToDateTime( double unixTimeStamp )
		{
			// Unix timestamp is seconds past epoch
			System.DateTime dtDateTime = new DateTime(1970,1,1,0,0,0,0);
			dtDateTime = dtDateTime.AddSeconds( unixTimeStamp ).ToLocalTime();
			return dtDateTime;
		}
	}
}
