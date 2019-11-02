using System;
using Pop3;
using System.IO;
using System.Text.RegularExpressions;
using System.Xml;
using System.Web.Mail;
using System.Diagnostics;
using System.Windows.Forms;
using System.Threading;
using System.Configuration;
using System.Text;

namespace MailManager
{
	class Class1
	{
		private static AppSettingsReader cfg = new AppSettingsReader();
		private static TextWriter eventlog;

		[STAThread]
		static void Main(string[] args)
		{
			while (true) 
			{
				eventlog = new StreamWriter(Path.Combine(Path.GetDirectoryName(
					Application.ExecutablePath), "event.log"), true, System.Text.Encoding.GetEncoding(1251));
				Log("Starting session");

				try 
				{
					// Perfrom mail processing
					ReceiveMail();
					if ((bool)cfg.GetValue("run-turn", typeof(bool)))
						RunTurn();
					SendMail();

					Log("Ending session\r\n");
				}
				catch (Exception ex) 
				{
					Log(ex.Message + "\r\n" + ex.StackTrace + "\r\n");
				}

				// Sleep until next cycle
				Thread.Sleep(TimeSpan.Parse((string)cfg.GetValue("sleep", typeof(string))));

				eventlog.Close();
			}
		}

		private static void Log(string message) 
		{
			eventlog.WriteLine(DateTime.Now.ToString("dd.MM HH:mm") + " " + message);
			eventlog.Flush();
		}

		private static void RunTurn() 
		{
			// Check if we need to run today
			DayOfWeek turn1 = (DayOfWeek)cfg.GetValue("turn1", typeof(int));
			DayOfWeek turn2 = (DayOfWeek)cfg.GetValue("turn2", typeof(int));
			if (DateTime.Now.DayOfWeek != turn1 && DateTime.Now.DayOfWeek != turn2)
				return;

			// Check if turn already ran
			string folder = (string)cfg.GetValue("gamedir", typeof(string));
			DirectoryInfo old_turn = new DirectoryInfo(Path.Combine(folder, "turn"));
			if (old_turn.Exists && old_turn.CreationTime.DayOfYear == DateTime.Now.DayOfYear)
				return;
			Log("Running turn");

			// Copy received files from mailbox
			DirectoryInfo mailbox = new DirectoryInfo((string)cfg.GetValue("mailbox", typeof(string)));
			foreach (FileInfo fi in mailbox.GetFiles("order.*"))
				fi.MoveTo(Path.Combine(old_turn.FullName, fi.Name));
			foreach (FileInfo fi in mailbox.GetFiles("request.*"))
				fi.MoveTo(Path.Combine(old_turn.FullName, fi.Name));

			// Run engine
			RunEngine("");

			// Copy files to mailbox
			foreach (FileInfo fi in old_turn.GetFiles("report.*"))
				//if (fi.Name != "report.1") // npc faction
					fi.CopyTo(Path.Combine(mailbox.FullName, fi.Name), true);

			// Backup old turn dir
			XmlDocument doc = new XmlDocument();
			doc.Load(Path.Combine(old_turn.FullName, "gamein.xml"));
			string basename = "turn." + doc.SelectSingleNode("game/@turn").Value;
			string turnname = basename;
			int i = 1;
			while (Directory.Exists(Path.Combine(folder, turnname))) 
			{
				turnname = basename + i.ToString();
				i++;
			}
			old_turn.MoveTo(Path.Combine(folder, turnname));

			// Create turn dir
			DirectoryInfo new_turn = Directory.CreateDirectory(Path.Combine(folder, "turn"));

			// Copy gameout
			if (File.Exists(Path.Combine(old_turn.FullName, "gameout.xml")))
				File.Copy(Path.Combine(old_turn.FullName, "gameout.xml"),
					Path.Combine(new_turn.FullName, "gamein.xml"), false);

			// Copy repeating orders
			foreach (FileInfo fi in old_turn.GetFiles("report.*")) 
			{
				if (fi.Name == "report.1")
					continue;

				string orders = "";
				bool read = false;
				TextReader tr = new StreamReader(fi.FullName);
				while (true) 
				{
					string s = tr.ReadLine();
					if (s == null)
						break;
					if (s.IndexOf("#orders ") == 0)
						read = true;
					if (read)
					{
						orders += s + "\r\n";
						if (s == "#end")
							break;
					}
				}
				tr.Close();

				if (orders != "") 
				{
					TextWriter tw = new StreamWriter(Path.Combine(new_turn.FullName, 
						"orders." + fi.Name.Substring(("report.").Length)), false,
						System.Text.Encoding.GetEncoding(1251));
					tw.WriteLine(orders);
					tw.Close();
				}
			}
		}

		private static void ReceiveMail() 
		{
			XmlDocument doc = new XmlDocument();
			doc.Load((string)cfg.GetValue("game.xml", typeof(string)));

			Pop3Client pop3 = new Pop3Client(
				(string)cfg.GetValue("pop3.username", typeof(string)), 
				(string)cfg.GetValue("pop3.password", typeof(string)), 
				(string)cfg.GetValue("pop3.server", typeof(string)));
			pop3.OpenInbox();

			while (pop3.NextEmail())
			{
				Log("Receiving \"" + pop3.Subject + "\" from " + pop3.From);

				string body;
				if (pop3.Charset == "koi8-r")
					body = KoiToWin(pop3.Body);
				else
					body = pop3.Body;

				string filename = "";
				bool checker = false;
				bool save = true;
				string folder = (string)cfg.GetValue("mailbox", typeof(string));
				if (pop3.Subject.ToLower() == "request") 
				{
					int i = 1;
					while (File.Exists(Path.Combine(folder, "request." + i.ToString())))
						i++;
					filename = "request." + i.ToString();
					Log("..this is a request :)");

					TextWriter tw = new StreamWriter(Path.Combine(folder, filename + ".confirm"), false, 
						Encoding.GetEncoding(1251));
					tw.WriteLine("To: " + pop3.From);
					tw.WriteLine("Subject: [Wasteland] Request confirmation");
					tw.WriteLine("");
					tw.WriteLine("Request accepted. Wait for next turn please.");
					tw.Close();
				}
				else 
				{
					string[] ss = body.Split('\n');
					int i = 0;
					for (i = 0; i < ss.Length; i++) 
					{
						string s = ss[i];
						Regex re;

						// Orders
						re = new Regex(@"#orders (\d+) ""(.*?)""");
						if (re.IsMatch(s)) 
						{
							Match m = re.Match(s);
							string faction = m.Groups[1].Value;
							string password = m.Groups[2].Value;

							XmlElement elFaction = (XmlElement)doc.SelectSingleNode(
								String.Format("/game/faction[@num='{0}']",
								faction.Replace("'", "")));
							if (elFaction == null)
								Log("..faction " + faction + " not found");
							else if (elFaction.GetAttribute("password") != password)
								Log("..wrong password for " + faction);
							else 
							{
								Log("..found orders for " + faction);
								filename = "order." + faction;
								checker = true;
								break;
							}
						}

						// Report resend
						re = new Regex(@"#resend (\d+) ""(.*?)""");
						if (re.IsMatch(s)) 
						{
							Match m = re.Match(s);
							string faction = m.Groups[1].Value;
							string password = m.Groups[2].Value;

							XmlElement elFaction = (XmlElement)doc.SelectSingleNode(
								String.Format("/game/faction[@num='{0}']",
								faction.Replace("'", "")));
							if (elFaction == null)
								Log("..faction " + faction + " not found");
							else if (elFaction.GetAttribute("password") != password)
								Log("..wrong password for " + faction);
							else 
							{
								Log("..found resend request for " + faction);

								string gamedir = (string)cfg.GetValue("gamedir", typeof(string));
								int turnnum = Convert.ToInt32(doc.SelectSingleNode("game/@turn").Value);
								string turnname = "turn." + (turnnum-1).ToString();
								FileInfo fi = new FileInfo(Path.Combine(Path.Combine(gamedir, turnname), 
									"report." + faction));
								if (fi.Exists)
									fi.CopyTo(Path.Combine(folder, fi.Name), false);

								checker = false;
								save = false;
								filename = "resend";
								break;
							}
						}

						// Mail
						re = new Regex(@"#mail (person )?(\d+)( (\d+) ""(.*?)"")?");
						if (re.IsMatch(s)) 
						{
							Match m = re.Match(s);
							string person = m.Groups[1].Value;
							string to = m.Groups[2].Value;
							string faction = m.Groups[4].Value;
							string password = m.Groups[5].Value;

							XmlElement elFaction = (XmlElement)doc.SelectSingleNode(
								String.Format("/game/faction[@num='{0}']",
								faction.Replace("'", "")));

							XmlElement elRecipient = null;
							XmlElement elPerson = null;
							if (person != "person ")
								elRecipient = (XmlElement)doc.SelectSingleNode(
									String.Format("/game/faction[@num='{0}']",
									to.Replace("'", "")));
							else 
							{
								elPerson = (XmlElement)doc.SelectSingleNode(
									String.Format("//person[@num='{0}']", to.Replace("'", "")));
								if (elPerson != null)
									elRecipient = (XmlElement)doc.SelectSingleNode(
										String.Format("/game/faction[@num='{0}']",
										  elPerson.GetAttribute("faction")));
							}
							
							if (elFaction != null && elFaction.GetAttribute("password") != password)
								Log("..wrong password for " + faction);
							else if (elRecipient == null || elRecipient.GetAttribute("email") == "")
								Log("..recipient " + to + " not found");
							else 
							{
								Log("..found mail from " + faction);

								filename = "mail." + faction + "-" + to + "." + DateTime.Now.ToString("HHmmssfff");
								TextWriter tw = new StreamWriter(Path.Combine(folder, filename), false, 
									Encoding.GetEncoding(1251));
								tw.WriteLine("To: " + elRecipient.GetAttribute("email"));
								string subj = "Subject: [Wasteland] Private message";
								if (elFaction != null)
									subj += " from " + elFaction.GetAttribute("name") + " (" + 
										elFaction.GetAttribute("num") + ")";
								tw.WriteLine(subj);
								tw.WriteLine("");
								if (elPerson != null)
									tw.WriteLine("Message received by " + Translit(elPerson.GetAttribute("name")) + 
										" (" + to + ").");
								for (int j = i+1; j < ss.Length; j++) 
								{
									if (ss[j].Trim() == "#end")
										break;
									tw.WriteLine(ss[j]);
								}
								tw.Close();

								filename = "confirm." + faction + "-" + to + "." + DateTime.Now.ToString("HHmmssfff");
								tw = new StreamWriter(Path.Combine(folder, filename), false, 
									Encoding.GetEncoding(1251));
								tw.WriteLine("To: " + pop3.From);
								tw.WriteLine("Subject: [Wasteland] Mail confirmation");
								tw.WriteLine("");
								if (elPerson == null)
									tw.WriteLine("Message to " + elRecipient.GetAttribute("name") + 
										" (" + to + ") sent.");
								else
									tw.WriteLine("Message to " + Translit(elPerson.GetAttribute("name")) + 
										" (" + to + ") sent.");
								tw.Close();

								checker = false;
								save = false;
								break;
							}
						}
					}

					if (filename == "") 
					{
						int j = 1;
						while (File.Exists(Path.Combine(folder, "spam." + j.ToString())))
							j++;
						filename = "spam." + j.ToString();
						Log("..this should be spam :/");
					}
				}

				if (save) 
				{
					TextWriter tw = new StreamWriter(Path.Combine(folder, filename), false, 
						Encoding.GetEncoding(1251));
					tw.WriteLine("From: " + pop3.From);
					tw.WriteLine("Subject: " + pop3.Subject);
					tw.WriteLine("");
					tw.WriteLine(body);
					tw.Close();
				}

				if (checker) 
				{
					Log("Checking " + filename);
					RunEngine("/check " + Path.Combine(folder, filename));
				}

				pop3.DeleteEmail();
			}

			pop3.CloseConnection();
		}
	
		private static void SendMail()
		{
			Directory.SetCurrentDirectory((string)cfg.GetValue("temp", typeof(string)));

			DirectoryInfo di = new DirectoryInfo((string)cfg.GetValue("mailbox", typeof(string)));
			foreach (FileInfo fi in di.GetFiles()) 
			{
				if (fi.Extension == ".log" || fi.Extension == ".exe")
					continue;

				string to = null;
				string subject = "no subject";
				bool attachment = false;
				
				// Read headers
				TextReader tr = new StreamReader(fi.FullName, Encoding.GetEncoding(1251));
				while (true) 
				{
					string s = tr.ReadLine();
					if (s == "" || s == null)
						break;
					if (s.IndexOf("To: ") == 0) 
						to = s.Substring(("To: ").Length);
					if (s.IndexOf("Subject: ") == 0)
						subject = s.Substring(("Subject: ").Length);
					if (s.IndexOf("Content-Disposition: ") == 0)
						attachment = (s.IndexOf("attachment") >= 0);
				}

				if (to == null || to.Trim() == "") 
				{
					tr.Close();
					continue;
				}

				Log("Sending \"" + fi.Name + "\" to " + to);

				// Write message body to temp folder
				string body = tr.ReadToEnd();
				tr.Close();

				if (attachment) 
				{
					TextWriter tw = new StreamWriter(fi.Name, false, Encoding.GetEncoding(1251));
					tw.Write(body);
					tw.Close();
					PackFile(fi.Name);
					SendMessage(to, subject, "", fi.Name + ".zip");
				}
				else 
				{
					SendMessage(to, subject, body, null);
				}

				// Delete temp files
				File.Delete(fi.Name);
				File.Delete(fi.Name + ".zip");

				// Delete message file
				if ((bool)cfg.GetValue("delete-messages", typeof(bool)))
					File.Delete(fi.FullName);
			}
		}
	
		private static void SendMessage(string recipient, string subject, string body, string attachment)
		{
			MailMessage msg = new MailMessage();
			msg.From = (string)cfg.GetValue("from", typeof(string));
			msg.To = recipient;
			msg.Subject = subject;
			if (attachment != null)
				msg.Attachments.Add(new MailAttachment(attachment));
			msg.Body = body;

			SMTP.SmtpDirect.SmtpServer = (string)cfg.GetValue("smtp.server", typeof(string));
			SMTP.SmtpDirect.Send(msg);
		}

		private static void PackFile(string filename)
		{
			string arg = "-tzip a " + filename + ".zip " + filename;
			ProcessStartInfo pi = new ProcessStartInfo((string)cfg.GetValue(
				"archiver", typeof(string)), arg);
			pi.WindowStyle = ProcessWindowStyle.Hidden;
			Process p = Process.Start(pi);
			p.WaitForExit();
			if (p.ExitCode != 0)
				throw new Exception("Archiver returned exit code " + p.ExitCode);
		}

		private static void RunEngine(string mode)
		{
			string arg = (string)cfg.GetValue("engine.args", typeof(string));
			if (mode != "")
				arg = mode + " " + arg;
			ProcessStartInfo pi = new ProcessStartInfo((string)cfg.GetValue(
				"engine", typeof(string)), arg);
			pi.WindowStyle = ProcessWindowStyle.Hidden;
			Process p = Process.Start(pi);
			p.WaitForExit();
			if (p.ExitCode != 0)
				throw new Exception("Engine returned exit code " + p.ExitCode);
		}

		private static string KoiToWin(string s) 
		{
			string win = "àáâãäå¸æçèéêëìíîïðñòóôõö÷øùüúûýþÿÀÁÂÃÄÅ¨ÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÜÛÝÞß";
			string koi = "ÁÂ×ÇÄÅ£ÖÚÉÊËÌÍÎÏÐÒÓÔÕÆÈÃÞÛÝØßÙÜÀÑáâ÷çäå³öúéêëìíîïðòóôõæèãþûýÿøùüàñ";

			string res = "";
			foreach (char c in s)
				if (koi.IndexOf(c) >= 0)
					res += win[koi.IndexOf(c)];
				else
					res += c;
			return res;
		}

		static string[] table = new String[32] { 
			"a", "b", "v", "g", "d", "e", "zh", "z", "i", "i", "k", "l", "m", "n", "o",
			"p", "r", "s", "t", "u", "f", "h", "tz", "ch", "sh", "sch", "", "i", 
			"", "e", "yu", "ya" };

		public static string Translit(string ru) 
		{
			ru = ru + " ";
			ru = ru.Replace("ûé ", "y ");
			ru = ru.Replace("èé ", "y ");
			string en = "";
			for (int i = 0; i < ru.Length-1; i++)
				if (ru[i] >= 'à' && ru[i] <= 'ÿ')
					en += table[ru[i] - 'à'];
				else if (ru[i] >= 'À' && ru[i] <= 'ß') 
				{
					if (table[ru[i] - 'À'].Length >= 1)
						en += table[ru[i] - 'À'].Substring(0, 1).ToUpper();
					if (table[ru[i] - 'À'].Length > 1)
						en += table[ru[i] - 'À'].Substring(1);
				}
				else if (ru[i] == '¸')
					en += "å";
				else if (ru[i] == '¨')
					en += "Å";
				else
					en += ru[i];
			return en;
		}
	}
}
