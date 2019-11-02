using System;
using System.IO;
using System.Collections;

namespace Wasteland 
{
	class OrdersReader 
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
						CheckerOutput.Add("Subject: Checker Output");
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
					
					Order order;

					if (cmd == "address") 
						order = ReadAddressOrder(s);
					else if (cmd == "attack")
						order = ReadPersonNumOrder(s, new AttackOrder());
					else if (cmd == "avoid")
						order = ReadAvoidOrder(s);
					else if (cmd == "build")
						order = ReadBuildOrder(s);
					else if (cmd == "burn")
						order = ReadItemTypeListOrder(s, new BurnOrder());
					else if (cmd == "consume")
						order = ReadItemTypeListOrder(s, new ConsumeOrder());
					else if (cmd == "cure")
						order = ReadCureOrder(s);
					else if (cmd == "declare") 
						order = ReadDeclareOrder(faction, s);
					else if (cmd == "describe")
						order = ReadDescribeOrder(s);
					else if (cmd == "drive")
						order = ReadDriveOrder(s);
					else if (cmd == "enter")
						order = ReadEnterOrder(s);
					else if (cmd == "equipment")
						order = ReadItemTypeListOrder(s, new EquipmentOrder());
					else if (cmd == "evict")
						order = ReadPersonNumOrder(s, new EvictOrder());
					else if (cmd == "give")
						order = ReadGiveOrder(s);
					else if (cmd == "hide")
						order = ReadHideOrder(s);
					else if (cmd == "install")
						order = ReadInstallOrder(s);
					else if (cmd == "kick")
						order = new KickOrder();
					else if (cmd == "leave")
						order = new LeaveOrder();
					else if (cmd == "maintain") 
						order = new MaintainOrder();
					else if (cmd == "move")
						order = ReadMoveOrder(s);
					else if (cmd == "name")
						order = ReadNameOrder(s);
					else if (cmd == "option")
						order = ReadOptionOrder(s);
					else if (cmd == "password") 
						order = ReadPasswordOrder(s);
					else if (cmd == "patrol")
						order = new PatrolOrder();
					else if (cmd == "produce")
						order = ReadProduceOrder(s);
					else if (cmd == "quit") 
						order = ReadQuitOrder(s, faction);
					else if (cmd == "scavenge") 
						order = ReadItemTypeListOrder(s, new ScavengeOrder());
					else if (cmd == "show") 
						order = ReadShowOrder(s);
					else if (cmd == "team") 
						order = ReadTeamOrder(s);
					else if (cmd == "trade") 
						order = ReadTradeOrder(s);
					else if (cmd == "uninstall")
						order = ReadUninstallOrder(s);
					else 
						throw new Exception("Bad order");

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
				TextWriter tw = new StreamWriter(filename + ".checker", false, System.Text.Encoding.GetEncoding(1251));
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


		private static AddressOrder ReadAddressOrder(string s) 
		{
			string t2 = MyStrings.GetQuotedToken(ref s);
			if (t2.Trim() == "")
				throw new Exception("Address can't be empty");
			AddressOrder ord = new AddressOrder();
			ord.Email = t2;
			return ord;
		}

		private static Order ReadPersonNumOrder(string s, PersonNumOrder ord) 
		{
			string t1 = MyStrings.GetToken(ref s);
			if (!MyStrings.IsNumber(t1))
				throw new Exception("Bad target");
			ord.PersonNum = Convert.ToInt32(t1);
			return ord;
		}

		private static AvoidOrder ReadAvoidOrder(string s) 
		{
			string t1 = MyStrings.GetToken(ref s);
			if (t1 != "0" && t1 != "1")
				throw new Exception("Bad parameter");
			AvoidOrder ord = new AvoidOrder();
			ord.Flag = (t1 == "1");
			return ord;
		}
		private static BuildOrder ReadBuildOrder(string s) 
		{
			string t1 = MyStrings.GetQuotedToken(ref s).ToLower();
			BuildOrder ord = new BuildOrder();
			ord.What = BuildingType.GetByAnyName(t1);
			if (ord.What == null)
				throw new Exception("Bad object");
			return ord;
		}

		private static CureOrder ReadCureOrder(string s) 
		{
			CureOrder ord = new CureOrder();
			while (s != "") 
			{
				string t = MyStrings.GetToken(ref s);
				if (!MyStrings.IsNumber(t))
					throw new Exception("Bad target");
				ord.PatientNums.Add(Convert.ToInt32(t));
			}
			return ord;
		}

		private static DeclareOrder ReadDeclareOrder(Faction faction, string s) 
		{
			// declare 12 neutral
			DeclareOrder ord = new DeclareOrder();
      
			string t1 = MyStrings.GetToken(ref s).ToLower();
			if (t1 == "default")
				ord.FactionNum = faction.Num;
			else if (!MyStrings.IsNumber(t1))
				throw new Exception("Bad faction");
			ord.FactionNum = Convert.ToInt32(t1);

			string t2 = MyStrings.GetQuotedToken(ref s);
			Attitude a = Attitude.Hostile;
			while (a <= Attitude.Ally && a.ToString().ToLower() != t2)
				a++;
			if (a > Attitude.Ally) 
				throw new Exception("Bad attitude");
			ord.Attitude = a;

			return ord;
		}

		private static DescribeOrder ReadDescribeOrder(string s) 
		{
			DescribeOrder ord = new DescribeOrder();
			
			string t1 = MyStrings.GetToken(ref s).ToLower();
			if (t1 == "person")
				ord.What = DescribeOrder.Target.Person;
			else if (t1 == "faction")
				ord.What = DescribeOrder.Target.Faction;
			else if (t1 == "object")
				ord.What = DescribeOrder.Target.Object;
			else 
				throw new Exception("Bad target");

			string t2 = MyStrings.GetValidString(MyStrings.GetQuotedToken(ref s));
			if (t2.Trim() == "")
				throw new Exception("Bad description");

			ord.Description = t2;
			return ord;
		}

		private static DriveOrder ReadDriveOrder(string s) 
		{
			DriveOrder ord = new DriveOrder();
			string token = MyStrings.GetToken(ref s).ToLower();
			if (token == "attack") 
			{
				ord.Attack = true;
				token = MyStrings.GetToken(ref s).ToLower();
			}
			while (token != "") 
			{
				switch(token) 
				{
					case "n": ord.Directions.Add(Direction.North); break;
					case "nw": ord.Directions.Add(Direction.Northwest); break;
					case "ne": ord.Directions.Add(Direction.Northeast); break;
					case "s": ord.Directions.Add(Direction.South); break;
					case "sw": ord.Directions.Add(Direction.Southwest); break;
					case "se": ord.Directions.Add(Direction.Southeast); break;
					default: throw new Exception("Bad direction");
				}
				token = MyStrings.GetToken(ref s).ToLower();
			}
			return ord;
		}

		private static EnterOrder ReadEnterOrder(string s) 
		{
			EnterOrder ord = new EnterOrder();
			string t1 = MyStrings.GetToken(ref s);
			if (!MyStrings.IsNumber(t1))
				throw new Exception("Bad object");
			ord.BuildingNum = Convert.ToInt32(t1);
			return ord;
		}

		private static GiveOrder ReadGiveOrder(string s) 
		{
			GiveOrder ord = new GiveOrder();
			
			string t1 = MyStrings.GetToken(ref s);
      if (!MyStrings.IsNumber(t1))
				throw new Exception("Bad target"); 
			ord.Target = Convert.ToInt32(t1);

			string t2 = MyStrings.GetToken(ref s).ToLower();
			if (t2 == "all")
				ord.Amount = -1;
			else 
			{
				if (!MyStrings.IsNumber(t2))
					throw new Exception("Bad amount");
				ord.Amount = Convert.ToInt32(t2);
			}

			string t3 = MyStrings.GetQuotedToken(ref s);
			ord.What = ItemType.GetByAnyName(t3);
      if (ord.What == null)
				throw new Exception("Bad item");

			return ord;
		}

		private static HideOrder ReadHideOrder(string s) 
		{
			string t1 = MyStrings.GetToken(ref s).ToLower();
			HideOrder ord = new HideOrder();
			if (t1 == "" || t1 == "none") 
				ord.Variant = HideOrder.Variants.Not;
			else if (t1 == "faction")
				ord.Variant = HideOrder.Variants.Faction;
			else if (t1 == "person")
				ord.Variant = HideOrder.Variants.Person;
			else
				throw new Exception("Bad parameter");
			return ord;
		}

		private static InstallOrder ReadInstallOrder(string s) 
		{
			InstallOrder ord = new InstallOrder();
			ord.What = ItemType.GetByAnyName(MyStrings.GetQuotedToken(ref s));
			if (ord.What == null)
				throw new Exception("Bad item");
			return ord;
		}

		private static Order ReadItemTypeListOrder(string s, ItemTypeListOrder ord) 
		{
			while (s.Trim() != "") 
			{
				string t2 = MyStrings.GetQuotedToken(ref s).ToLower();
				ItemType it = ItemType.GetByAnyName(t2);
				if (it == null)
					throw new Exception("Bad item");
				ord.What.Add(it);
			}

			return ord;
		}

		private static MoveOrder ReadMoveOrder(string s) 
		{
			MoveOrder ord = new MoveOrder();
			string token = MyStrings.GetToken(ref s).ToLower();
			if (token == "attack") 
			{
				ord.Attack = true;
				token = MyStrings.GetToken(ref s).ToLower();
			}
			while (token != "") 
			{
				switch(token) 
				{
					case "n": ord.Directions.Add(Direction.North); break;
					case "nw": ord.Directions.Add(Direction.Northwest); break;
					case "ne": ord.Directions.Add(Direction.Northeast); break;
					case "s": ord.Directions.Add(Direction.South); break;
					case "sw": ord.Directions.Add(Direction.Southwest); break;
					case "se": ord.Directions.Add(Direction.Southeast); break;
					case "out": ord.Directions.Add(0); break;
					default: // move in building
					{
						try 
						{
							ord.Directions.Add(Convert.ToInt32(token));
						}
						catch (FormatException) 
						{
							throw new Exception("Bad direction");
						}
						break;
					}
				}
				token = MyStrings.GetToken(ref s).ToLower();
			}
			return ord;
		}

		private static NameOrder ReadNameOrder(string s) 
		{
			NameOrder ord = new NameOrder();
			
			string t1 = MyStrings.GetToken(ref s).ToLower();
			if (t1 == "person")
				ord.What = NameOrder.Target.Person;
			else if (t1 == "faction")
				ord.What = NameOrder.Target.Faction;
			else if (t1 == "object")
				ord.What = NameOrder.Target.Object;
			else 
				throw new Exception("Bad target");

			string t2 = MyStrings.GetValidString(MyStrings.GetQuotedToken(ref s));
			if (t2.Trim() == "")
				throw new Exception("Bad name");

			ord.Name = t2;
			return ord;
		}

		private static OptionOrder ReadOptionOrder(string s) 
		{
			OptionOrder ord = new OptionOrder();

      ord.Option = MyStrings.GetToken(ref s).ToLower();
			if (ord.Option == "text-report" || ord.Option == "xml-report") 
			{
				ord.Val = MyStrings.GetToken(ref s).ToLower();
				if (ord.Val != "0" && ord.Val != "1")
					throw new Exception("Wrong option value");
			}
			else if (ord.Option == "language") 
			{
				ord.Val = MyStrings.GetToken(ref s).ToLower();
				if (ord.Val != "ru" && ord.Val != "en")
					throw new Exception("Wrong option value");
			}
			else
				throw new Exception("No such option");

			return ord;
		}

		private static PasswordOrder ReadPasswordOrder(string s) 
		{
			string t1 = MyStrings.GetQuotedToken(ref s);
			if (t1.Trim() == "")
				throw new Exception("Password can't be empty");
			if (t1 != MyStrings.GetValidString(t1))
				throw new Exception("Bad symbols in password");
			PasswordOrder ord = new PasswordOrder();
			ord.Password = t1;
			return ord;
		}

		private static ProduceOrder ReadProduceOrder(string s) 
		{
			string t1 = MyStrings.GetQuotedToken(ref s);
			ItemType it = ItemType.GetByAnyName(t1);
			if (it == null) 
				throw new Exception("Bad item");
			ProduceOrder ord = new ProduceOrder();
			ord.ItemType = it;
			return ord;
		}

		private static QuitOrder ReadQuitOrder(string s, Faction f) 
		{
			string t1 = MyStrings.GetQuotedToken(ref s);
			if (t1 != f.Password)
				throw new Exception("Wrong password");
			QuitOrder ord = new QuitOrder();
			return ord;
		}

		private static ShowOrder ReadShowOrder(string s) 
		{
			ShowOrder ord = new ShowOrder();

			string t1 = MyStrings.GetToken(ref s).ToLower();
			if (t1 == "faction") 
			{
				string t2 = MyStrings.GetToken(ref s);
				if (!MyStrings.IsNumber(t2))
					throw new Exception("Bad faction");
				ord.FactionNum = Convert.ToInt32(t2);
			}
			else if (t1 == "item") 
			{
				string t2 = MyStrings.GetQuotedToken(ref s).ToLower();
				ord.ItemType = ItemType.GetByAnyName(t2);
				if (ord.ItemType == null)
					throw new Exception("Bad item");
			}
			else if (t1 == "skill") 
			{
				string t2 = MyStrings.GetQuotedToken(ref s).ToLower();
				ord.SkillType = SkillType.GetByAnyName(t2);
				if (ord.SkillType == null)
					throw new Exception("Bad skill");
			}
			else if (t1 == "object") 
			{
				string t2 = MyStrings.GetQuotedToken(ref s).ToLower();
				ord.BuildingType = BuildingType.GetByAnyName(t2);
				if (ord.BuildingType == null)
					throw new Exception("Bad object");
			}
			else
				throw new Exception("Bad parameter");
			return ord;
		}

		private static TeamOrder ReadTeamOrder(string s) 
		{
			TeamOrder ord = new TeamOrder();
			string t2 = MyStrings.GetToken(ref s).ToLower();
			if (t2 == "kick") 
			{
				ord.Kick = true;
				t2 = MyStrings.GetToken(ref s).ToLower();
			}
			if (t2 == "" || t2 == "none") 
			{
				ord.LeaderNum = 0;
			}
			else
			{
				if (!MyStrings.IsNumber(t2))
					throw new Exception("Bad target");
				ord.LeaderNum = Convert.ToInt32(t2);
			}
			return ord;
		}

		private static TradeOrder ReadTradeOrder(string s) 
		{
			TradeOrder ord = new TradeOrder();
			
			string t1 = MyStrings.GetToken(ref s).ToLower();
			if (t1 == "with") 
			{
				string num = MyStrings.GetToken(ref s);
				if (!MyStrings.IsNumber(num)) 
					throw new Exception("Bad target");
				ord.PersonNum = Convert.ToInt32(num);
				t1 = MyStrings.GetToken(ref s).ToLower();
			}
			else
				ord.PersonNum = 0;

			if (t1 == "all")
				ord.SellAmount = -1;
			else 
			{
				if (!MyStrings.IsNumber(t1))
					throw new Exception("Bad sell amount");
				ord.SellAmount = Convert.ToInt32(t1);
			}

			ord.SellWhat = ItemType.GetByAnyName(MyStrings.GetQuotedToken(ref s));
			if (ord.SellWhat == null)
				throw new Exception("Bad sell item");
      
			string t2 = MyStrings.GetToken(ref s).ToLower();
			if (t2 == "all")
				ord.BuyAmount = -1;
			else 
			{
				if (!MyStrings.IsNumber(t2))
					throw new Exception("Bad buy amount");
				ord.BuyAmount = Convert.ToInt32(t2);
			}

			ord.BuyWhat = ItemType.GetByAnyName(MyStrings.GetQuotedToken(ref s));
			if (ord.BuyWhat == null)
				throw new Exception("Bad buy item");

			return ord;
		}

		private static UninstallOrder ReadUninstallOrder(string s) 
		{
			UninstallOrder ord = new UninstallOrder();

			string t1 = MyStrings.GetQuotedToken(ref s);
			if (MyStrings.IsNumber(t1)) 
			{
				ord.Amount = Convert.ToInt32(t1);
				t1 = MyStrings.GetQuotedToken(ref s);
			}
			else
				ord.Amount = -1;

			ord.What = ItemType.GetByAnyName(t1);
			if (ord.What == null)
				throw new Exception("Bad item");
			return ord;
		}

	}

	class MyStrings 
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