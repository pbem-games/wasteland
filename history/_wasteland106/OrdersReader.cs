using System;
using System.IO;
using System.Collections;

namespace Wasteland 
{
	public class OrdersReader 
	{
		public static void Load(string folder) 
		{
			if (folder == "")
				folder = Directory.GetCurrentDirectory();
			DirectoryInfo di = new DirectoryInfo(folder);
			foreach (FileInfo fi in di.GetFiles("order.*")) 
				Load(fi.FullName, false);
		}

		public static void Load(string filename, bool checker) 
		{
			TextReader tr = new StreamReader(filename, System.Text.Encoding.GetEncoding(1251));

			bool do_read = false;
			Person person = null;
			Faction faction = null;
			bool errors = false;
			ArrayList CheckerOutput = new ArrayList();
			while (true) 
			{
				try 
				{
					string line = tr.ReadLine();
					string s = line;
					if (s == null)
						break;

					// Store repeating lines 
					if (s.Length > 0 && s[0] == '@') 
					{
						if (person != null)
							person.RepeatingLines.Add(line);
						s = s.Substring(1);
					}

					// Strip comments
					s = MyStrings.Uncomment(s).Trim();

					// Get first word as command
					string cmd = MyStrings.GetToken(ref s).ToLower();

					// Directives
					if (cmd == "#orders")
					{
						string t2 = MyStrings.GetToken(ref s);
						if (!MyStrings.IsNumber(t2))
							throw new Exception("Bad faction");
						int num = Convert.ToInt32(t2);
						string password = MyStrings.GetQuotedToken(ref s);
						faction = (Faction)Faction.Get(num);
						if (faction == null)
							throw new Exception("No such faction");
						if (password != faction.Password || faction.IsNPC)
							throw new Exception("Wrong password");

						Console.WriteLine("..orders for " + faction.Num.ToString());

						CheckerOutput.Add("To: " + faction.Email);
						CheckerOutput.Add("Subject: [Wasteland] Checker Output");
						CheckerOutput.Add("");
						CheckerOutput.Add(line);

						do_read = true;
						continue;
					}
					if (cmd == "#end")
						do_read = false;
					if (!do_read || cmd == "")
						continue;
					if (cmd == "person")
					{
						string t2 = MyStrings.GetToken(ref s);
						if (!MyStrings.IsNumber(t2))
							throw new Exception("Bad person");
						int num = Convert.ToInt32(t2);
						person = faction.Persons.GetByNumber(num); 
						if (person == null)
							throw new Exception("This person is not in your faction");
						CheckerOutput.Add("\r\n" + line);
						continue;
					}
					
					CheckerOutput.Add(line);

					if (person == null) 
						throw new Exception("Order given with no person specified");

					Order order = OrdersReader.ParseOrder(person, faction, cmd, s);

					// Overwrite monthlong order
					if (order.IsMonthlong) 
					{
						int i = 0;
						while (i < person.Orders.Count)
							if (((Order)person.Orders[i]).IsMonthlong) 
							{
								person.Orders.RemoveAt(i);
								CheckerOutput.Add("; **** Overwriting previous monthlong order ****\r\n");
								errors = true;
							}
							else
								i++;
					}

					// Overwrite trade order
					if (order.GetType() == typeof(TradeOrder)) 
					{
						int i = 0;
						while (i < person.Orders.Count)
							if (person.Orders[i].GetType() == typeof(TradeOrder)) 
							{
								person.Orders.RemoveAt(i);
								CheckerOutput.Add("; **** Overwriting previous trade order ****\r\n");
								errors = true;
							}
							else
								i++;
					}

					person.Orders.Add(order);
				}
				catch (Exception ex) 
				{
					CheckerOutput.Add("; **** " + ex.Message + " ****\r\n");
					errors = true;
				}
			}

			tr.Close();

			if (checker) 
			{
				string checkername = Path.Combine(Path.GetDirectoryName(filename), 
					"checker." + Path.GetFileName(filename));
				TextWriter tw = new StreamWriter(checkername, false, System.Text.Encoding.GetEncoding(1251));
				if (errors)
					foreach (string s in CheckerOutput)
						tw.WriteLine(s);
				else 
				{
					// Write only message header
					foreach (string s in CheckerOutput) 
					{
						tw.WriteLine(s);
						if (s == "") break;
					}
					tw.WriteLine("Your order was accepted without errors.");
				}
				tw.Close();
			}
		}

		public static Order ParseOrder (Person person, Faction faction, string command, string arguments)
		{
			Order order;
			switch ( command )
			{
				case "address":
					order = new AddressOrder(arguments);
					break;
				case "attack":
					order = new AttackOrder(arguments);
					break;
				case "avoid":
					order = new AvoidOrder(arguments);
					break;
				case "build":
					order = new BuildOrder(arguments);
					break;
				case "burn":
					order = new BurnOrder(arguments);
					break;
				case "consume":
					order = new ConsumeOrder(person, arguments);
					break;
				case "cure":
					order = new CureOrder(arguments);
					break;
				case "declare":
					order = new DeclareOrder(faction, arguments);
					break;
				case "describe":
					order = new DescribeOrder(arguments);
					break;
				case "drive":
					order = new DriveOrder(arguments);
					break;
				case "enter":
					order = new EnterOrder(arguments);
					break;
				case "equipment":
					order = new EquipmentOrder(arguments);
					break;
				case "evict":
					order = new EvictOrder(arguments);
					break;
				case "give":
					order = new GiveOrder(arguments);
					break;
				case "greedy":
					order = new GreedyOrder(arguments);
					break;
				case "hide":
					order = new HideOrder(arguments);
					break;
				case "install":
					order = new InstallOrder(arguments);
					break;
				case "kick":
					order = new KickOrder();
					break;
				case "leave":
					order = new LeaveOrder();
					break;
				case "move":
					order = new MoveOrder(arguments);
					break;
				case "name":
					order = new NameOrder(arguments);
					break;
				case "option":
					order = new OptionOrder(arguments);
					break;
				case "password":
					order = new PasswordOrder(arguments);
					break;
				case "patrol":
					order = new PatrolOrder();
					break;
				case "produce":
					order = new ProduceOrder(arguments);
					break;
				case "quit":
					order = new QuitOrder(faction, arguments);
					break;
				case "scavenge":
					order = new ScavengeOrder(arguments);
					break;
				case "show":
					order = new ShowOrder(arguments);
					break;
				case "spoils":
					order = new SpoilsOrder(arguments);
					break;
				case "team":
					order = new TeamOrder(arguments);
					break;
				case "trade":
					order = new TradeOrder(arguments);
					break;
				case "uninstall":
					order = new UninstallOrder(arguments);
					break;
				default:
					throw new Exception("Bad order " + command);
			}
			return order;
		}

	}

	public class MyStrings 
	{
		public static string GetToken(ref string s) 
		{
			string res = "";
			while (s.Length > 0 && s[0] != ' ') 
			{
				res += s.Substring(0, 1);
				s = s.Substring(1);
			}
			s = s.TrimStart();
			return res;
		}

		public static string GetQuotedToken(ref string s) 
		{
			if (s.Length > 0 && s[0] == '"') 
			{
				string res = "";
				s = s.Substring(1);
				while (s.Length > 0 && s[0] != '"') 
				{
					res += s.Substring(0, 1);
					s = s.Substring(1);
				}
				if (s.Length > 0) s = s.Substring(1);
				s = s.TrimStart();
				return res;
			}
			else 
				return GetToken(ref s);
		}

		public static string GetValidString(string s) 
		{
			string res = "";
			for (int i = 0; i < s.Length; i++) 
			{
				if ((s[i] >= 'A' && s[i] <= 'Z') || (s[i] >= 'a' && s[i] <= 'z')
					|| (s[i] >= '0' && s[i] <= '9')
					|| (s[i] >= 'А' && s[i] <= 'Я') || (s[i] >= 'а' && s[i] <= 'я')
					|| s[i] == 'Ё' || s[i] == 'ё'
					|| s[i] == ' ' || s[i] == '_' || s[i] == '\'' || s[i] == '-' 
					|| s[i] == '+' || s[i] == '.' || s[i] == ',')
					res += s[i];
			}
			return res;
		}

		static string[] table = new String[32] { 
				"a", "b", "v", "g", "d", "e", "zh", "z", "i", "i", "k", "l", "m", "n", "o",
				"p", "r", "s", "t", "u", "f", "h", "tz", "ch", "sh", "sch", "", "i", 
				"", "e", "yu", "ya" };

		public static string Translit(string ru) 
		{
      ru = ru + " ";
			ru = ru.Replace("ый ", "y ");
			ru = ru.Replace("ий ", "y ");
			string en = "";
			for (int i = 0; i < ru.Length-1; i++)
				if (ru[i] >= 'а' && ru[i] <= 'я')
					en += table[ru[i] - 'а'];
				else if (ru[i] >= 'А' && ru[i] <= 'Я') 
				{
					if (table[ru[i] - 'А'].Length >= 1)
						en += table[ru[i] - 'А'].Substring(0, 1).ToUpper();
					if (table[ru[i] - 'А'].Length > 1)
						en += table[ru[i] - 'А'].Substring(1);
				}
				else if (ru[i] == 'ё')
					en += "e";
				else if (ru[i] == 'Ё')
					en += "E";
				else
					en += ru[i];
			return en;
		}

		public static bool IsNumber(string s) 
		{
			try 
			{
				return (Convert.ToInt32(s) >= 0);
			}
			catch (FormatException) 
			{
				return false;
			}
		}

		public static string Uncomment(string s) 
		{
			if (s.IndexOf(';') >= 0)
				return s.Substring(0, s.IndexOf(';'));
			else
				return s;
		}
	}
}
