using System;
using System.IO;
using System.Xml;
using System.Text.RegularExpressions;

namespace Wasteland 
{
	public class DataFile 
	{
		public static SkillList ChosenSkills = new SkillList();
		public static SkillList ChosenSpecial = new SkillList();
		public static ItemList ChosenItems = new ItemList();

		private static XmlDocument LoadXmlDocument(string filename) 
		{
			TextReader tr = new StreamReader(filename, System.Text.Encoding.GetEncoding(1251));
			XmlDocument doc = new XmlDocument();
			doc.LoadXml(tr.ReadToEnd());
			tr.Close();
			return doc;
		}

		public static void LoadGame(string folder) 
		{
			// Create predefined factions
			XmlDocument doc = LoadXmlDocument(Path.Combine(folder, "gamein.xml"));

			// Factions
			LoadInteger(ref Faction.MaxNum, doc.DocumentElement.GetAttribute("max-faction"));
			foreach (XmlElement elFaction in doc.SelectNodes("/game/faction")) 
			{
				int num = Convert.ToInt32(elFaction.GetAttribute("num"));
				Faction f = new Faction(num);
				f.Name = elFaction.GetAttribute("name");
				f.Password = elFaction.GetAttribute("password");
				f.Email = elFaction.GetAttribute("email");
				if (elFaction.HasAttribute("default-attitude"))
					f.DefaultAttitude = (Attitude)Convert.ToInt32(elFaction.GetAttribute("default-attitude"));
				if (elFaction.GetAttribute("text-report") != "")
					f.Options.TextReport = Convert.ToBoolean(elFaction.GetAttribute("text-report"));
				if (elFaction.GetAttribute("xml-report") != "")
					f.Options.XmlReport = Convert.ToBoolean(elFaction.GetAttribute("xml-report"));
				if (elFaction.GetAttribute("lang") == "ru")
					f.Options.Lang = Lang.Ru;

				foreach (XmlElement el in elFaction.SelectNodes("shown-item"))
					f.ShownItems.Add(ItemType.Get(el.GetAttribute("name")));
				foreach (XmlElement el in elFaction.SelectNodes("shown-skill"))
					f.ShownSkills.Add(SkillType.Get(el.GetAttribute("name")));
				foreach (XmlElement el in elFaction.SelectNodes("shown-building"))
					f.ShownBuildings.Add(BuildingType.Get(el.GetAttribute("name")));

				// Attitudes
				foreach (XmlElement elAttitude in elFaction.SelectNodes("attitude")) 
				{
					Attitude a = (Attitude)Convert.ToInt32(elAttitude.GetAttribute("level"));
					int fnum = Convert.ToInt32(elAttitude.GetAttribute("faction"));
					f.Attitudes.Add(fnum, a);
				}
			}

			// Map
			XmlElement elMap = (XmlElement)doc.SelectSingleNode("/game/map");
			Map.SetDimensions(Convert.ToInt32(elMap.GetAttribute("width")), 
				Convert.ToInt32(elMap.GetAttribute("height")));
			LoadInteger(ref Map.Turn, doc.DocumentElement.GetAttribute("turn"));
			Map.Turn++;

			// Regions
			foreach (XmlElement elRegion in elMap.SelectNodes("region")) 
			{
				int x = Convert.ToInt32(elRegion.GetAttribute("x"));
				int y = Convert.ToInt32(elRegion.GetAttribute("y"));
				Region r = Map.Region(x, y);
				r.Terrain = Terrain.Get(elRegion.GetAttribute("terrain"));
				r.Name = elRegion.GetAttribute("name");
				r._radiation = Convert.ToInt32(elRegion.GetAttribute("radiation"));

				// Items
				LoadItems(r.Resources, elRegion.SelectNodes("resource"));
				LoadItems(r.Junk, elRegion.SelectNodes("junk"));

				// Buildings
				foreach (XmlElement elBuilding in elRegion.SelectNodes("building")) 
				{
					BuildingType t = BuildingType.Get(elBuilding.GetAttribute("type"));
          int num = Convert.ToInt32(elBuilding.GetAttribute("num"));
					Building b = new Building(t, r, num);
					b.Name = elBuilding.GetAttribute("name");
					if (elBuilding.HasAttribute("description"))
						b.Description = elBuilding.GetAttribute("description");
					LoadItems(b.Installed, elBuilding.SelectNodes("installed"));
				}

				// Persons
				foreach (XmlElement elPerson in elRegion.SelectNodes("person")) 
				{
					int num = Convert.ToInt32(elPerson.GetAttribute("num"));
					Faction f;
					if (elPerson.HasAttribute("faction"))
						f = Faction.Get(Convert.ToInt32(elPerson.GetAttribute("faction")));
					else
						f = Faction.Get(Constants.NPCFactionNum);
					Person p = new Person(f, r, num);
					p.Name = elPerson.GetAttribute("name");
					if (elPerson.HasAttribute("description"))
						p.Description = elPerson.GetAttribute("description");
					if (elPerson.HasAttribute("insanity"))
						p.Insanity = Convert.ToInt32(elPerson.GetAttribute("insanity"));
					LoadInteger(ref p.BabyTimer, elPerson.GetAttribute("baby-timer"));
					p.FatherSkill = SkillType.Get(elPerson.GetAttribute("father-skill"));
					p.FatherName = elPerson.GetAttribute("father-name");
					p.Chosen = (elPerson.GetAttribute("chosen") == "True");
					p.Patrolling = (elPerson.GetAttribute("patrolling") == "True");
					p.Avoiding = (elPerson.GetAttribute("avoiding") == "True");
					p.Greedy = (elPerson.GetAttribute("greedy") == "True");
					LoadInteger(ref p.Age, elPerson.GetAttribute("age"));
					if (elPerson.GetAttribute("building") != "") 
						p.Building = r.Buildings.GetByNumber(Convert.ToInt32(elPerson.GetAttribute("building")));
					if (elPerson.GetAttribute("leader") != "")
						p.Leader = r.Persons.GetByNumber(Convert.ToInt32(elPerson.GetAttribute("leader")));

					XmlElement elTrade = (XmlElement)elPerson.SelectSingleNode("trade");
					if (elTrade != null) 
					{
						p.TradeOrder = new TradeOrder();
						LoadInteger(ref p.TradeOrder.PersonNum, elTrade.GetAttribute("with"));
            p.TradeOrder.BuyWhat = ItemType.Get(elTrade.SelectSingleNode("buy/@type").Value);
						LoadInteger(ref p.TradeOrder.BuyAmount, elTrade.SelectSingleNode("buy/@amount").Value);
						p.TradeOrder.SellWhat = ItemType.Get(elTrade.SelectSingleNode("sell/@type").Value);
						LoadInteger(ref p.TradeOrder.SellAmount, elTrade.SelectSingleNode("sell/@amount").Value);
					}

					LoadItems(p.Items, elPerson.SelectNodes("item"));
					LoadSkills(p.Skills, elPerson.SelectNodes("skill"));

					foreach (XmlElement el in elPerson.SelectNodes("consume"))
						p.Consume.Add(ItemType.Get(el.GetAttribute("type")));
					foreach (XmlElement el in elPerson.SelectNodes("burn"))
						p.Burn.Add(ItemType.Get(el.GetAttribute("type")));
					foreach (XmlElement el in elPerson.SelectNodes("equipment"))
						p.Equipment.Add(ItemType.Get(el.GetAttribute("type")));
					foreach (XmlElement el in elPerson.SelectNodes("spoils"))
						p.Spoils.Add(ItemType.Get(el.GetAttribute("type")));
				}
			}
		}

		public static void SaveGame(string folder) 
		{
			XmlDocument doc = new XmlDocument();
			doc.LoadXml("<game/>");

			// Factions
			doc.DocumentElement.SetAttribute("max-faction", Faction.MaxNum.ToString());
			foreach (Faction f in Faction.List) 
			{
				// Do not save factions without persons
				if (!f.IsNPC && f.Persons.Count == 0)
					continue;

				XmlElement elFaction = doc.CreateElement("faction");
				doc.DocumentElement.AppendChild(elFaction);
				elFaction.SetAttribute("num", f.Num.ToString());
				elFaction.SetAttribute("name", f.Name.ToString());
				elFaction.SetAttribute("password", f.Password);
				elFaction.SetAttribute("email", f.Email);
				elFaction.SetAttribute("default-attitude", ((int)f.DefaultAttitude).ToString());
				elFaction.SetAttribute("text-report", f.Options.TextReport.ToString());
				elFaction.SetAttribute("xml-report", f.Options.XmlReport.ToString());
				if (f.Options.Lang == Lang.Ru)
					elFaction.SetAttribute("lang", "ru");
				else
					elFaction.SetAttribute("lang", "en");

				foreach (ItemType it in f.ShownItems)
					SaveItemType(it, elFaction, "shown-item");
				foreach (SkillType st in f.ShownSkills)
					SaveSkillType(st, elFaction, "shown-skill");
				foreach (BuildingType bt in f.ShownBuildings)
					SaveBuildingType(bt, elFaction, "shown-building");


				// Attitudes
				foreach (int num in f.Attitudes.Keys) 
				{
					if (Faction.Get(num) == null) 
						continue;
					XmlElement elAttitude = (XmlElement)doc.CreateElement("attitude");
					elFaction.AppendChild(elAttitude);
					elAttitude.SetAttribute("level", ((int)f.Attitudes[num]).ToString());
					elAttitude.SetAttribute("faction", num.ToString());
				}
			}

			// Map
			XmlElement elMap = doc.CreateElement("map");
			doc.DocumentElement.AppendChild(elMap);
			elMap.SetAttribute("width", Map.Width.ToString());
			elMap.SetAttribute("height", Map.Height.ToString());
			doc.DocumentElement.SetAttribute("turn", Map.Turn.ToString());
			
			// Regions
			foreach (Region r in Map.Regions) 
			{
				XmlElement elRegion = doc.CreateElement("region");
				elMap.AppendChild(elRegion);
				elRegion.SetAttribute("x", r.X.ToString());
				elRegion.SetAttribute("y", r.Y.ToString());
				elRegion.SetAttribute("terrain", r.Terrain.Name);
				elRegion.SetAttribute("name", r.Name);
				elRegion.SetAttribute("radiation", r._radiation.ToString());
				
				SaveItems(r.Resources, elRegion, "resource");
				SaveItems(r.Junk, elRegion, "junk");

				// Buildings
				foreach (Building b in r.Buildings) 
				{
					XmlElement elBuilding = doc.CreateElement("building");
					elRegion.AppendChild(elBuilding);
					elBuilding.SetAttribute("type", b.Type.Name);
					elBuilding.SetAttribute("num", b.Num.ToString());
					elBuilding.SetAttribute("name", b.Name);
					if (b.Description != "")
						elBuilding.SetAttribute("description", b.Description);
					SaveItems(b.Installed, elBuilding, "installed");
				}

        // Persons (leaders first)
				foreach (Person p in r.Persons) 
					if (p.Leader == null)
						SavePerson(p, elRegion);
				foreach (Person p in r.Persons) 
					if (p.Leader != null)
						SavePerson(p, elRegion);
			}

			TextWriter tw = new StreamWriter(Path.Combine(folder, "gameout.xml"), false,
				System.Text.Encoding.GetEncoding(1251));
			tw.WriteLine("<?xml version=\"1.0\" encoding=\"windows-1251\"?>");
			tw.Write(doc.OuterXml);
			tw.Close();
		}

		private static void SavePerson(Person p, XmlElement parent) 
		{
			XmlDocument doc = parent.OwnerDocument;
			XmlElement elPerson = doc.CreateElement("person");
			parent.AppendChild(elPerson);
			elPerson.SetAttribute("num", p.Num.ToString());
			elPerson.SetAttribute("name", p.Name);
			if (p.Description != "")
				elPerson.SetAttribute("description", p.Description);
			if (p.Faction != null)
				elPerson.SetAttribute("faction", p.Faction.Num.ToString());
			elPerson.SetAttribute("insanity", p.Insanity.ToString());
			if (p.Avoiding)
				elPerson.SetAttribute("avoiding", "True");
			if (p.Greedy)
				elPerson.SetAttribute("greedy", "True");
			if (p.BabyTimer > 0)
				elPerson.SetAttribute("baby-timer", p.BabyTimer.ToString());
			if (p.FatherSkill != null)
				elPerson.SetAttribute("father-skill", p.FatherSkill.Name);
			if (p.FatherName != "" && p.FatherName != null)
				elPerson.SetAttribute("father-name", p.FatherName);
			elPerson.SetAttribute("age", (p.Age + 1).ToString());
			if (p.Chosen) 
				elPerson.SetAttribute("chosen", "True");
			if (p.Patrolling)
				elPerson.SetAttribute("patrolling", "True");
			if (p.Building != null)
				elPerson.SetAttribute("building", p.Building.Num.ToString());
			if (p.Leader != null)
				elPerson.SetAttribute("leader", p.Leader.Num.ToString());
			
			if (p.TradeOrder != null) 
			{
				XmlElement elTrade = doc.CreateElement("trade");
				elPerson.AppendChild(elTrade);
				if (p.TradeOrder.PersonNum != 0)
					elTrade.SetAttribute("with", p.TradeOrder.PersonNum.ToString());
				XmlElement elBuy = doc.CreateElement("buy");
				elTrade.AppendChild(elBuy);
				elBuy.SetAttribute("type", p.TradeOrder.BuyWhat.Name);
				elBuy.SetAttribute("amount", p.TradeOrder.BuyAmount.ToString());
				XmlElement elSell = doc.CreateElement("sell");
				elTrade.AppendChild(elSell);
				elSell.SetAttribute("type", p.TradeOrder.SellWhat.Name);
				elSell.SetAttribute("amount", p.TradeOrder.SellAmount.ToString());
			}

			// Items
			SaveItems(p.Items, elPerson, "item");

			// Skills
			foreach (Skill s in p.Skills) 
			{
				XmlElement elSkill = doc.CreateElement("skill");
				elPerson.AppendChild(elSkill);
				elSkill.SetAttribute("type", s.Type.Name);
				elSkill.SetAttribute("level", s.Level.ToString());
			}

			foreach (ItemType it in p.Consume) 
			{
				XmlElement el = doc.CreateElement("consume");
				el.SetAttribute("type", it.Name);
				elPerson.AppendChild(el);
			}
			foreach (ItemType it in p.Burn) 
			{
				XmlElement el = doc.CreateElement("burn");
				el.SetAttribute("type", it.Name);
				elPerson.AppendChild(el);
			}
			foreach (ItemType it in p.Equipment) 
			{
				XmlElement el = doc.CreateElement("equipment");
				el.SetAttribute("type", it.Name);
				elPerson.AppendChild(el);
			}
			foreach (ItemType it in p.Spoils) 
			{
				XmlElement el = doc.CreateElement("spoils");
				el.SetAttribute("type", it.Name);
				elPerson.AppendChild(el);
			}
		}

		public static void LoadData(string filename) 
		{
			XmlDocument doc = LoadXmlDocument(filename);
			NameGenerator.Init((XmlElement)doc.SelectSingleNode("data/names"));

			// Terrain
			foreach (XmlElement el in doc.SelectNodes("/data/terrain/entry")) 
				new Terrain(el.GetAttribute("name"));

			// Weather
			foreach (XmlElement el in doc.SelectNodes("/data/weather/entry"))
				new Weather(el.GetAttribute("name"));

			// Item
			foreach (XmlElement el in doc.SelectNodes("/data/item/entry")) 
				new ItemType(el.GetAttribute("name"));

			// Skill
			foreach (XmlElement el in doc.SelectNodes("/data/skill/entry")) 
				new SkillType(el.GetAttribute("name"));

			// Months
			foreach (XmlElement el in doc.SelectNodes("/data/month/entry")) 
			{
				Month m = new Month();
				m.NameEn = el.GetAttribute("name-en");
				m.NameRu = el.GetAttribute("name-ru");
				m.Weather = Weather.Get(el.GetAttribute("weather"));
				m.Temperature = Convert.ToInt32(el.GetAttribute("temperature"));
			}

			// Building
			foreach (XmlElement el in doc.SelectNodes("/data/building/entry")) 
			{
				BuildingType t = new BuildingType(el.GetAttribute("name"));
				t.FullNameEn = el.GetAttribute("name-en");
				t.FullNameRu = el.GetAttribute("name-ru");
				LoadInteger(ref t.Speed, el.GetAttribute("speed"));
				t.DriveTerrain = Terrain.Get(el.GetAttribute("drive-terrain"));
				LoadInteger(ref t.Capacity, el.GetAttribute("capacity"));
				LoadInteger(ref t.Defence, el.GetAttribute("defence"));
				LoadInteger(ref t.HP, el.GetAttribute("hp"));
				LoadInteger(ref t.Radiation, el.GetAttribute("radiation"));
				LoadInteger(ref t.Temperature, el.GetAttribute("temperature"));
				t.NoBuild = (el.GetAttribute("nobuild") == "True");

				t.MaintainSkill = LoadSkill(el, "maintain-skill");
				t.DriveSkill = LoadSkill(el, "drive-skill");
				foreach (XmlElement elItem in el.SelectNodes("material"))
					t.Materials.Add(LoadItem(elItem));
				foreach (XmlElement elItem in el.SelectNodes("optional"))
					t.OptionalMaterials.Add(LoadItem(elItem));
				t.Fuel = ItemType.Get(el.GetAttribute("fuel"));
				t.Resource = LoadItem(el, "resource");
			}

			// Terrain - full
			foreach (XmlElement el in doc.SelectNodes("/data/terrain/entry")) 
			{
				Terrain t = Terrain.Get(el.GetAttribute("name"));
				t.FullNameEn = el.GetAttribute("name-en");
				t.FullNameRu = el.GetAttribute("name-ru");
				LoadInteger(ref t.MP, el.GetAttribute("mp"));
				if (el.GetAttribute("walking") != "")
					t.Walking = Convert.ToBoolean(el.GetAttribute("walking"));
				if (el.GetAttribute("vehicles") != "")
					t.Vehicles = Convert.ToBoolean(el.GetAttribute("vehicles"));
				if (el.GetAttribute("ships") != "")
					t.Ships = Convert.ToBoolean(el.GetAttribute("ships"));
				foreach (XmlElement elMonster in el.SelectNodes("monster"))
					t.Monsters.Add(ItemType.Get(elMonster.GetAttribute("type")),
						Convert.ToInt32(elMonster.GetAttribute("chance")));
			}

			// Weather - full 
			foreach (XmlElement el in doc.SelectNodes("/data/weather/entry")) 
			{
				Weather t = Weather.Get(el.GetAttribute("name"));
				t.FullNameEn = el.GetAttribute("name-en");
				t.FullNameRu = el.GetAttribute("name-ru");
				LoadInteger(ref t.Radiation, el.GetAttribute("radiation"));
			}

			// Skills - full
			foreach (XmlElement el in doc.SelectNodes("/data/skill/entry")) 
			{
				SkillType t = SkillType.Get(el.GetAttribute("name"));
				t.FullNameEn = el.GetAttribute("name-en");
				t.FullNameRu = el.GetAttribute("name-ru");
				t.DescriptionEn = el.GetAttribute("description-en");
				t.DescriptionRu = el.GetAttribute("description-ru");
				t.BasedOn = SkillType.Get(el.GetAttribute("based-on"));
				LoadInteger(ref t.DefaultLevel, el.GetAttribute("default-level"));
				t.Special = (el.GetAttribute("special") == "True");
			}

			// Items - fill
			foreach (XmlElement el in doc.SelectNodes("/data/item/entry")) 
			{
				ItemType t = ItemType.Get(el.GetAttribute("name"));
				t.FullNameEn1 = el.GetAttribute("name-en1");
				t.FullNameEn2 = el.GetAttribute("name-en2");
				t.FullNameRu0 = el.GetAttribute("name-ru0");
				t.FullNameRu1 = el.GetAttribute("name-ru1");
				t.FullNameRu2 = el.GetAttribute("name-ru2");
				
				t.IsMan = (el.GetAttribute("is-man") == "True");
				t.IsWeapon = (el.GetAttribute("is-weapon") == "True");
				t.IsArmor = (el.GetAttribute("is-armor") == "True");
				t.IsMonster = (el.GetAttribute("is-monster") == "True");

				if (el.GetAttribute("nogive") != "")
					t.NoGive = Convert.ToBoolean(el.GetAttribute("nogive"));
				LoadInteger(ref t.Weight, el.GetAttribute("weight"));
				LoadInteger(ref t.Capacity[(int)Movement.Walk], el.GetAttribute("capacity-walk"));
				LoadInteger(ref t.Capacity[(int)Movement.Ride], el.GetAttribute("capacity-ride"));
				t.InstallSkill = LoadSkill(el, "install-skill");
				LoadInteger(ref t.Rations, el.GetAttribute("rations"));
				t.Dead = ItemType.Get(el.GetAttribute("dead"));
				LoadInteger(ref t.DecomposeChance, el.GetAttribute("decompose-chance"));
				LoadInteger(ref t.OwnerRadiation, el.GetAttribute("owner-radiation"));
				LoadInteger(ref t.RegionRadiation, el.GetAttribute("region-radiation"));
				LoadInteger(ref t.OwnerTemperature, el.GetAttribute("owner-temperature"));
				LoadInteger(ref t.TemperatureFrom, el.GetAttribute("temperature-from"));
				LoadInteger(ref t.TemperatureEffect, el.GetAttribute("temperature-effect"));
				t.Burn = (el.GetAttribute("burn") == "True");
				LoadItems(t.Drops, el.SelectNodes("drop"));

				// Production
				t.ProduceSkill = LoadSkill(el, "produce-skill");
				t.ProduceBuilding = BuildingType.Get(el.GetAttribute("produce-inside"));
				LoadInteger(ref t.ProductionRate, el.GetAttribute("production-rate"));
				t.ProduceFrom1 = LoadItem(el, "produce-from1");
				t.ProduceFrom2 = LoadItem(el, "produce-from2");
				t.ProduceAs = ItemType.Get(el.GetAttribute("produce-as"));
				
				// Mutation
				LoadInteger(ref t.RadiationEffect, el.GetAttribute("radiation-effect"));
				LoadInteger(ref t.RadiationFrom, el.GetAttribute("radiation-from"));
				LoadInteger(ref t.RadiationTo, el.GetAttribute("radiation-to"));
				LoadInteger(ref t.MutatePercent, el.GetAttribute("mutate-percent"));
				t.MutateTo = ItemType.Get(el.GetAttribute("mutate-to"));

				// Man
				foreach (XmlElement elFood in el.SelectNodes("food")) 
				{
					ItemType it = ItemType.Get(elFood.GetAttribute("name"));
          t.Food.Add(it);
				}
				t.Medicine = ItemType.Get(el.GetAttribute("medicine"));
				t.Baby = ItemType.Get(el.GetAttribute("baby"));
				t.BabyFrom = ItemType.Get(el.GetAttribute("baby-from"));
				LoadInteger(ref t.BabyTimer, el.GetAttribute("baby-timer"));
				LoadInteger(ref t.MaxHireAmount, el.GetAttribute("max-hire-amount"));
				t.NoLearn = (el.GetAttribute("nolearn") == "True");

				// Monster
				LoadSkills(t.Skills, el.SelectNodes("skill"));
				LoadInteger(ref t.Aggression, el.GetAttribute("aggression"));
				LoadInteger(ref t.PackSize, el.GetAttribute("pack-size"));

				// Baby
				foreach (XmlElement elGrowTo in el.SelectNodes("grow-to")) 
					t.GrowTo.Add(ItemType.Get(elGrowTo.GetAttribute("name")));

				// Weapon
				t.WeaponSkill = SkillType.Get(el.GetAttribute("weapon-skill"));
				if (el.GetAttribute("ranged") != "")
					t.Ranged = Convert.ToBoolean(el.GetAttribute("ranged"));
				if (el.GetAttribute("heavy") != "")
					t.Heavy = Convert.ToBoolean(el.GetAttribute("ranged"));
				if (el.GetAttribute("anti-tank") != "")
					t.AntiTank = Convert.ToBoolean(el.GetAttribute("anti-tank"));
				if (el.GetAttribute("explosive") != "")
					t.Explosive = Convert.ToBoolean(el.GetAttribute("explosive"));
				if (el.GetAttribute("ammo") != "")
					t.Ammo = ItemType.Get(el.GetAttribute("ammo"));
				if (el.GetAttribute("case") != "")
					t.Case = ItemType.Get(el.GetAttribute("case"));
				LoadInteger(ref	t.ToWound, el.GetAttribute("wound-chance"));
				LoadInteger(ref t.HitModifier, el.GetAttribute("hit-modifier"));
				LoadInteger(ref t.ArmorModifier, el.GetAttribute("armor-modifier"));
				LoadInteger(ref t.Attacks, el.GetAttribute("attacks"));
				LoadInteger(ref t.Targets, el.GetAttribute("targets"));
				LoadInteger(ref t.Lethality, el.GetAttribute("lethality"));
				LoadInteger(ref t.HP, el.GetAttribute("hp"));

				// Armor
				LoadInteger(ref t.ArmorSave, el.GetAttribute("armor-save"));
			}

			// Chosen
			LoadSkills(ChosenSkills, doc.SelectNodes("data/chosen/skill"));
			LoadSkills(ChosenSpecial, doc.SelectNodes("data/chosen/special"));
			LoadItems(ChosenItems, doc.SelectNodes("data/chosen/item"));
		}

		public static Skill LoadSkill(XmlElement parent, string name)
		{
			XmlElement elSkill = (XmlElement)parent.SelectSingleNode(name);
			if (elSkill == null)
				return null;
			Skill sk = new Skill(SkillType.Get(elSkill.GetAttribute("name")));
			sk.Level = Convert.ToInt32(elSkill.GetAttribute("level"));
			return sk;
		}

		public static Item LoadItem(XmlElement parent, string name) 
		{
			XmlElement elItem = (XmlElement)parent.SelectSingleNode(name);
			if (elItem == null)
				return null;
			else
				return LoadItem(elItem);
		}

		public static Item LoadItem(XmlElement elItem) 
		{
			Item itm = new Item(ItemType.Get(elItem.GetAttribute("name")));
			itm.Amount = Convert.ToInt32(elItem.GetAttribute("amount"));
			return itm;
		}

		public static void LoadInteger(ref int target, string source) 
		{
			if (source != "")
				target = Convert.ToInt32(source);
		}

		private static void LoadItems(ItemList items, XmlNodeList nodeList)
		{
			foreach (XmlElement elItem in nodeList) 
			{ 
				ItemType t = ItemType.Get(elItem.GetAttribute("type"));
				Item itm = new Item(t);
				items.Add(itm);
				itm.Amount = Convert.ToInt32(elItem.GetAttribute("amount"));
			}
		}

		private static void LoadSkills(SkillList skills, XmlNodeList nodeList)
		{
			foreach (XmlElement elSkill in nodeList) 
			{
				SkillType t = SkillType.Get(elSkill.GetAttribute("type"));
				Skill s = new Skill(t);
				s.Level = Convert.ToInt32(elSkill.GetAttribute("level"));
				skills.Add(s);
			}
		}

		private static void SaveItems(ItemList items, XmlElement parent, string nodeName) 
		{
			foreach (Item itm in items) 
			{
				XmlElement elItem = parent.OwnerDocument.CreateElement(nodeName);
				parent.AppendChild(elItem);
				elItem.SetAttribute("type", itm.Type.Name);
				elItem.SetAttribute("amount", itm.Amount.ToString());
			}
		}

		private static void SaveItemType(ItemType it, XmlElement parent, string nodeName) 
		{
			XmlElement el = parent.OwnerDocument.CreateElement(nodeName);
			parent.AppendChild(el);
			el.SetAttribute("name", it.Name);
		}

		private static void SaveSkillType(SkillType st, XmlElement parent, string nodeName) 
		{
			XmlElement el = parent.OwnerDocument.CreateElement(nodeName);
			parent.AppendChild(el);
			el.SetAttribute("name", st.Name);
		}

		private static void SaveBuildingType(BuildingType bt, XmlElement parent, string nodeName) 
		{
			XmlElement el = parent.OwnerDocument.CreateElement(nodeName);
			parent.AppendChild(el);
			el.SetAttribute("name", bt.Name);
		}

	}
	
}