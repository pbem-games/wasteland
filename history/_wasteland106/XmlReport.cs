using System;
using System.IO;
using System.Collections;
using System.Xml;

namespace Wasteland 
{
	public class XmlReport 
	{
		private static XmlDocument doc = null;
		private static Faction faction = null;

		public static string Generate(Faction f) 
		{
			faction = f;
			doc = new XmlDocument();
			doc.LoadXml("<report/>");
			XmlElement elReport = doc.DocumentElement;
			elReport.AppendChild(doc.CreateElement("factions"));
			elReport.AppendChild(doc.CreateElement("items"));
			elReport.AppendChild(doc.CreateElement("skills"));
			elReport.AppendChild(doc.CreateElement("objects"));
			elReport.AppendChild(doc.CreateElement("terrains"));
			foreach (ItemType it in faction.ItemsToShow)
				ShowItemType(it);
			foreach (SkillType st in faction.SkillsToShow)
				ShowSkillType(st);
			foreach (BuildingType bt in faction.BuildingsToShow)
				ShowBuildingType(bt);
			foreach (int faction_num in faction.Attitudes.Keys)
				if (Faction.Get(faction_num) != null)
					ShowFaction(Faction.Get(faction_num));

			// Header
			elReport.SetAttribute("for", faction.Num.ToString());
			elReport.SetAttribute("turn", Map.Turn.ToString());
			if (faction.Options.Lang == Lang.En)
				elReport.SetAttribute("season", Month.Current.NameEn);
			else
				elReport.SetAttribute("season", Month.Current.NameRu);
			elReport.SetAttribute("engine", MainClass.EngineVersion);
			elReport.SetAttribute("password", faction.Password);
			if (faction.Options.Lang == Lang.En)
				elReport.SetAttribute("language", "en");
			else
				elReport.SetAttribute("language", "ru");
			ShowFaction(faction);

			// Events
			XmlElement elEvents = doc.CreateElement("events");
			elReport.AppendChild(elEvents);
			foreach (Event evt in faction.Events) 
			{
        XmlElement elEvent = doc.CreateElement("event");
				elEvents.AppendChild(elEvent);
				if (evt.Person != null) 
				{
					elEvent.SetAttribute("n", evt.Person.Num.ToString());
					if (evt.Person.Faction != faction)
						elEvent.SetAttribute("name", evt.Person.Name);
				}
				elEvent.SetAttribute("text", evt.ToString(faction.Options.Lang, false));
			}

			// Regions
			XmlElement elMap = doc.CreateElement("map");
			elReport.AppendChild(elMap);
			elMap.SetAttribute("width", Map.Width.ToString());
			elMap.SetAttribute("height", Map.Height.ToString());

			foreach (Region r in Map.Regions) 
			{
				// Is region visible
				int j = r.Persons.Count-1;
				while (j >= 0 && ((Person)r.Persons[j]).Faction != faction) j--;
				if (j < 0) continue;

				// Print region
				XmlElement elRegion = doc.CreateElement("region");
				elMap.AppendChild(elRegion);
				elRegion.SetAttribute("x", r.X.ToString());
				elRegion.SetAttribute("y", r.Y.ToString());
				AddTerrainAttribute(elRegion, r.Terrain, "terrain");
				if (faction.Options.Lang == Lang.En)
					elRegion.SetAttribute("in", MyStrings.Translit(r.Name));
				else
					elRegion.SetAttribute("in", r.Name);

				Map.Turn++;
				elRegion.SetAttribute("weather", r.Weather.ToString(faction.Options.Lang));
				elRegion.SetAttribute("t", r.Temperature.ToString());
				elRegion.SetAttribute("radiation", r.Radiation.ToString());
				foreach (Item itm in r.TurnResources) 
					AddItem(elRegion, itm, "res");
				Map.Turn--;

				foreach (Item itm in r.Junk)
					AddItem(elRegion, itm, "junk");

				// Exits
				for (Direction i = Direction.North; i <= Direction.Northwest; i++) 
				{
					Region exit = r.RegionInDir((Direction)i);
					if (exit == null) 
						continue;
					XmlElement elExit = doc.CreateElement("exit");
					elRegion.AppendChild(elExit);
					elExit.SetAttribute("to", Map.DirectionNames[(int)i-1]);
					elExit.SetAttribute("x", exit.X.ToString());
					elExit.SetAttribute("y", exit.Y.ToString());
					AddTerrainAttribute(elExit, exit.Terrain, "terrain");
					if (faction.Options.Lang == Lang.En)
						elExit.SetAttribute("in", MyStrings.Translit(exit.Name));
					else
						elExit.SetAttribute("in", exit.Name);
				}

				// Persons
				WritePersons(elRegion, r, null);

				// Buildings and persons inside
				foreach (Building b in r.Buildings) 
				{
					XmlElement elBuilding = doc.CreateElement("obj");
					elRegion.AppendChild(elBuilding);

					if (faction.Options.Lang == Lang.En) 
					{
						elBuilding.SetAttribute("name", MyStrings.Translit(b.Name));
						if (b.Description != "")
							elBuilding.SetAttribute("description", MyStrings.Translit(b.Description));
					}
					else 
					{
						elBuilding.SetAttribute("name", b.Name);
						if (b.Description != "")
							elBuilding.SetAttribute("description", b.Description);
					}

					elBuilding.SetAttribute("n", b.Num.ToString());
					elBuilding.SetAttribute("id", b.Type.Name);
					ShowBuildingType(b.Type);

					foreach (Item itm in b.Installed)
						AddItem(elBuilding, itm, "item");
					foreach (Item itm in b.GetNeeds())
						AddItem(elBuilding, itm, "needs");

					WritePersons(elBuilding, r, b);
				}
			}
      
			return "<?xml version=\"1.0\" encoding=\"windows-1251\"?>" +
				doc.OuterXml;
		}

		private static void WritePersons(XmlElement parent, Region r, Building b) 
		{
			// Write team leaders with teams
			foreach (Person p in r.Persons) 
			{
				if (p.Building == b && p.Leader == null) 
				{
					XmlElement elLeader = WritePerson(parent, p);
					foreach (Person subordinate in p.Team)
						WritePerson(elLeader, subordinate);
				}
			}
		}

		private static XmlElement WritePerson(XmlElement parent, Person p) 
		{
			XmlElement elPerson = doc.CreateElement("person");
			parent.AppendChild(elPerson);
			if (faction.Options.Lang == Lang.En) 
			{
				elPerson.SetAttribute("name", MyStrings.Translit(p.Name));
				if (p.Description != "")
					elPerson.SetAttribute("description", MyStrings.Translit(p.Description));
			}
			else 
			{
				elPerson.SetAttribute("name", p.Name);
				if (p.Description != "")
					elPerson.SetAttribute("description", p.Description);
			}
			elPerson.SetAttribute("n", p.Num.ToString());

			// Faction
			if (p.Faction == faction || !p.HideFaction) 
			{
				ShowFaction(p.Faction);
				elPerson.SetAttribute("faction", p.Faction.Num.ToString());
			}

			// Flags
			if (p.Insanity >= Constants.DangerousInsanity)
				elPerson.SetAttribute("insane", "True");
			if (p.Chosen)
				elPerson.SetAttribute("chosen", "True");
			if (p.Patrolling)
				elPerson.SetAttribute("patrolling", "True");
			if (p.Age <= Constants.ChildTurns) 
				elPerson.SetAttribute("child", "True");

			if (p.Faction == faction) 
			{
				if (p.Avoiding)
					elPerson.SetAttribute("avoiding", "True");
				if (p.Greedy)
					elPerson.SetAttribute("greedy", "True");
				if (p.Hide)
					elPerson.SetAttribute("hiding", "person");
				else if (p.HideFaction)
					elPerson.SetAttribute("hiding", "faction");
			}

			// Items
			foreach (Item itm in p.Items) 
			{
				if (itm.Type.Weight == 0 && p.Faction != faction)
					continue;
				AddItem(elPerson, itm, "item");
			}

			if (p.Faction == faction) 
			{
				foreach (Skill sk in p.Skills)
					AddSkill(elPerson, sk, "skill");

				elPerson.SetAttribute("insanity", p.Insanity.ToString());
				int hire = p.GetHireAmount();
				if (hire >= 1)
					elPerson.SetAttribute("hire-demand", hire.ToString());

				foreach (ItemType it in p.Consume)
					AddItemType(elPerson, it, "consume");
				foreach (ItemType it in p.Burn)
					AddItemType(elPerson, it, "burn");
				foreach (ItemType it in p.Equipment)
					AddItemType(elPerson, it, "equipment");
				foreach (ItemType it in p.Spoils)
					AddItemType(elPerson, it, "spoils");
			}
			else if (!faction.IsNPC)
			{
				// Show talents of other factions' persons if Chosen in region
				Person chosen = faction.GetChosen();
				if (chosen != null && p.Region == chosen.Region) 
				{
					foreach (Skill sk in p.Skills)
						if (sk.Type.BasedOn == null) 
						{ 
							XmlElement el = doc.CreateElement("skill");
							elPerson.AppendChild(el);
							el.SetAttribute("id", sk.Type.Name);
							ShowSkillType(sk.Type);
						}						
				}
			}

			// Trade order
			if (p.TradeOrder != null) 
			{
				Person receiver = null;
				if (p.TradeOrder.PersonNum != 0)
					receiver = p.Region.Persons.GetByNumber(p.TradeOrder.PersonNum);
				if (p.TradeOrder.PersonNum == 0 || (receiver != null && receiver.Faction == faction)) 
				{
					XmlElement elTrade = doc.CreateElement("trade");
					elPerson.AppendChild(elTrade);
					AddItem(elTrade, new Item(p.TradeOrder.SellWhat, p.TradeOrder.SellAmount), "sell");
					AddItem(elTrade, new Item(p.TradeOrder.BuyWhat, p.TradeOrder.BuyAmount), "buy");
					if (receiver != null)
						elTrade.SetAttribute("with", receiver.Num.ToString());
				}
			}

			// Orders
			if (p.Faction == faction) 
			{
				string orders = "";
				foreach (string s in p.RepeatingLines) 
				{
					if (orders != "")
						orders += "\\n";
					orders += s.Replace("\\", "\\\\").Replace("<", "").Replace(">", "");
				}
				if (orders != "") 
				{
					XmlElement elOrders = doc.CreateElement("orders");
					elOrders.InnerText = orders;
					elPerson.AppendChild(elOrders);
				}
			}

			return elPerson;
		}

		private static void AddItem(XmlElement parent, Item itm, string nodeName) 
		{
			XmlElement el = doc.CreateElement(nodeName);
			parent.AppendChild(el);
			el.SetAttribute("id", itm.Type.Name);
			el.SetAttribute("amt", itm.Amount.ToString());
			ShowItemType(itm.Type);
		}

		private static void AddItemType(XmlElement parent, ItemType it, string nodeName) 
		{
			XmlElement el = doc.CreateElement(nodeName);
			parent.AppendChild(el);
			el.SetAttribute("id", it.Name);
			ShowItemType(it);
		}

		private static void AddItemAttribute(XmlElement el, ItemType it, string nodeName) 
		{
			el.SetAttribute(nodeName, it.Name);
			ShowItemType(it);
		}

		private static void AddSkill(XmlElement parent, Skill sk, string nodeName) 
		{
			XmlElement el = doc.CreateElement(nodeName);
			parent.AppendChild(el);
			el.SetAttribute("id", sk.Type.Name);
			el.SetAttribute("lv", sk.Level.ToString());
      ShowSkillType(sk.Type);
		}

		private static void AddSkillAttribute(XmlElement el, SkillType st, string nodeName) 
		{
			el.SetAttribute(nodeName, st.Name);
			ShowSkillType(st);
		}

		private static void AddBuildingType(XmlElement parent, BuildingType bt, string nodeName) 
		{
			XmlElement el = doc.CreateElement(nodeName);
			parent.AppendChild(el);
			el.SetAttribute("id", bt.Name);
			ShowBuildingType(bt);
		}

		private static void AddBuildingAttribute(XmlElement el, BuildingType bt, string nodeName) 
		{
			el.SetAttribute(nodeName, bt.Name);
			ShowBuildingType(bt);
		}

		private static void AddTerrainAttribute(XmlElement elTo, Terrain t, string name) 
		{
			elTo.SetAttribute(name, t.Name);
			XmlElement elTerrains = (XmlElement)doc.DocumentElement.SelectSingleNode("terrains");
			XmlElement el = (XmlElement)elTerrains.SelectSingleNode(
				String.Format("terrain[@id='{0}']", t.Name));
			if (el != null)
				return;
			el = doc.CreateElement("terrain");
			elTerrains.AppendChild(el);
			el.SetAttribute("id", t.Name);

			if (faction.Options.Lang == Lang.En)
				el.SetAttribute("name", t.FullNameEn);
			else
				el.SetAttribute("name", t.FullNameRu);

			el.SetAttribute("mp", t.MP.ToString());
			if (!t.Vehicles)
				el.SetAttribute("vehicles", "False");
		}

		private static void ShowFaction(Faction f) 
		{
			XmlElement elFactions = (XmlElement)doc.DocumentElement.SelectSingleNode("factions");
			XmlElement el = (XmlElement)elFactions.SelectSingleNode("faction[@n=" +
				f.Num.ToString() + "]");
			if (el != null)
				return;
			el = doc.CreateElement("faction");
			elFactions.AppendChild(el);
			if (faction.Options.Lang == Lang.En)
				el.SetAttribute("name", MyStrings.Translit(f.Name));
			else
				el.SetAttribute("name", f.Name);
			el.SetAttribute("n", f.Num.ToString());
			if (f == faction)
				el.SetAttribute("attitude", faction.DefaultAttitude.ToString());
			else if (faction.Attitudes.ContainsKey(f.Num))
				el.SetAttribute("attitude", faction.Attitudes[f.Num].ToString());
		}

		private static void ShowItemType(ItemType it) 
		{
			if (faction.ShownItems.Contains(it) && !faction.ItemsToShow.Contains(it))
				return;

			XmlElement elItems = (XmlElement)doc.DocumentElement.SelectSingleNode("items");
			XmlElement el = (XmlElement)elItems.SelectSingleNode(
				String.Format("item[@id='{0}']", it.Name));
			if (el != null)
				return;
			el = doc.CreateElement("item");
			elItems.AppendChild(el);
			el.SetAttribute("id", it.Name);
			
			if (faction.Options.Lang == Lang.En) 
			{
				el.SetAttribute("name1", it.FullNameEn1);
				el.SetAttribute("name2", it.FullNameEn2);
			}
			else 
			{
				el.SetAttribute("name0", it.FullNameRu0);
				el.SetAttribute("name1", it.FullNameRu1);
				el.SetAttribute("name2", it.FullNameRu2);
			}

			if (!faction.ItemsToShow.Contains(it))
				return;

			el.SetAttribute("full", "True");

			el.SetAttribute("weight", it.Weight.ToString());
			if (it.Capacity[(int)Movement.Walk] > it.Weight)
				el.SetAttribute("capacity-walk", (it.Capacity[(int)Movement.Walk] - it.Weight).ToString());
			if (it.Rations > 0)
				el.SetAttribute("rations", it.Rations.ToString());
			if (it.OwnerRadiation != 0)
				el.SetAttribute("owner-radiation", it.OwnerRadiation.ToString());
			if (it.OwnerTemperature != 0)
				el.SetAttribute("owner-temperature", it.OwnerTemperature.ToString());
		
			if (it.ProduceSkill != null) 
			{
				AddSkill(el, it.ProduceSkill, "produce-skill");
				if (it.ProduceFrom1 != null)
					AddItem(el, it.ProduceFrom1, "produce-from1");
				if (it.ProduceFrom2 != null)
					AddItem(el, it.ProduceFrom2, "produce-from2");
				if (it.ProduceBuilding != null)
					AddBuildingAttribute(el, it.ProduceBuilding, "produce-inside");
				el.SetAttribute("production-rate", it.ProductionRate.ToString());
				if (it.ProduceAs != null)
					AddItemAttribute(el, it.ProduceAs, "produce-as");
			}

			if (it.InstallSkill != null) 
			{
				AddSkill(el, it.InstallSkill, "install-skill");

				foreach (BuildingType bt in BuildingType.List)
					if (!bt.NoBuild && bt.Materials.Count > 0 && bt.Materials[0].Type == it)
						AddBuildingType(el, bt, "build");
			}

			if (it.RegionRadiation != 0)
				el.SetAttribute("region-radiation", it.RegionRadiation.ToString());

			if (it.IsMan) 
			{
				el.SetAttribute("is-man", "True");
				if (it.Food.Count > 0)
					foreach (ItemType fit in it.Food)
						AddItemType(el, fit, "food");
				if (it.NoLearn)
					el.SetAttribute("no-learn", "True");
				if (it.RadiationFrom > 0 || (it.RadiationTo > 0 && it.RadiationTo != 1000)) 
				{
					el.SetAttribute("radiation-from", it.RadiationFrom.ToString());
					el.SetAttribute("radiation-to", it.RadiationTo.ToString());
				}
			}

			if (it.Burn)
				el.SetAttribute("burn", "True");

			if (it.IsWeapon) 
			{
				el.SetAttribute("is-weapon", "True");
				if (it.Ranged) el.SetAttribute("ranged", "True");
				if (it.Heavy) el.SetAttribute("heavy", "True");
				if (it.AntiTank) el.SetAttribute("anti-tank", "True");
				if (it.Explosive) el.SetAttribute("explosive", "True");
				if (it.Attacks > 1) el.SetAttribute("attacks", it.Attacks.ToString());
				if (it.Targets > 1) el.SetAttribute("targets", it.Targets.ToString());
				if (it.HitModifier != 0) el.SetAttribute("hit-modifier", it.HitModifier.ToString());
				if (it.ArmorModifier != 0) el.SetAttribute("armor-modifier", it.ArmorModifier.ToString());
				el.SetAttribute("wound-chance", it.ToWound.ToString());
				if (it.HP > 0) el.SetAttribute("hp-inflicted", it.HP.ToString());
				if (!it.AntiTank) el.SetAttribute("lethality", it.Lethality.ToString());
				if (it.Ammo != null) AddItemAttribute(el, it.Ammo, "ammo");
				if (it.WeaponSkill != null) AddSkillAttribute(el, it.WeaponSkill, "weapon-skill");
			}

			if (it.IsArmor) 
			{
				el.SetAttribute("is-armor", "True");
				el.SetAttribute("armor-save", it.ArmorSave.ToString());
			}
		}

		private static void ShowSkillType(SkillType st) 
		{
      if (faction.ShownSkills.Contains(st) && !faction.SkillsToShow.Contains(st))
				return;

			XmlElement elSkills = (XmlElement)doc.DocumentElement.SelectSingleNode("skills");
			XmlElement el = (XmlElement)elSkills.SelectSingleNode(
				String.Format("skill[@id='{0}']", st.Name));
			if (el != null)
				return;
			el = doc.CreateElement("skill");
			elSkills.AppendChild(el);
			el.SetAttribute("id", st.Name);

			if (faction.Options.Lang == Lang.En)
				el.SetAttribute("name", st.FullNameEn);
			else
				el.SetAttribute("name", st.FullNameRu);

			if (!faction.SkillsToShow.Contains(st))
				return;

			el.SetAttribute("full", "True");
      
			if (faction.Options.Lang == Lang.En)
				el.SetAttribute("description", st.DescriptionEn);
			else
				el.SetAttribute("description", st.DescriptionRu);
			if (st.BasedOn != null) 
				AddSkillAttribute(el, st.BasedOn, "based-on");
			if (st.Special)
				el.SetAttribute("special", "True");

			foreach (ItemType it in ItemType.List)
				if (it.ProduceSkill != null && it.ProduceSkill.Type == st)
					AddItemType(el, it, "produce");

			foreach (ItemType it in ItemType.List)
				if (it.InstallSkill != null && it.InstallSkill.Type == st)
					AddItemType(el, it, "install");

			foreach (ItemType it in ItemType.List) 
			{
				if (it.InstallSkill != null && it.InstallSkill.Type == st) 
				{
					foreach (BuildingType bt in BuildingType.List)
						if (!bt.NoBuild && bt.Materials.Count > 0 && bt.Materials[0].Type == it)
							AddBuildingType(el, bt, "build");
				}
			}

			foreach (BuildingType bt in BuildingType.List)
				if (bt.DriveSkill != null && bt.DriveSkill.Type == st)
					AddBuildingType(el, bt, "drive");
		}

		private static void ShowBuildingType(BuildingType bt) 
		{
			if (faction.ShownBuildings.Contains(bt) && !faction.BuildingsToShow.Contains(bt))
				return;

			XmlElement elBuildings = (XmlElement)doc.DocumentElement.SelectSingleNode("objects");
			XmlElement el = (XmlElement)elBuildings.SelectSingleNode(
				String.Format("obj[@id='{0}']", bt.Name));
			if (el != null)
				return;
			el = doc.CreateElement("obj");
			elBuildings.AppendChild(el);
			el.SetAttribute("id", bt.Name);

			if (faction.Options.Lang == Lang.En)
				el.SetAttribute("name", bt.FullNameEn);
			else
				el.SetAttribute("name", bt.FullNameRu);

			if (!faction.BuildingsToShow.Contains(bt))
				return;
      
			el.SetAttribute("full", "True");

			if (bt.DriveSkill != null) 
			{
				el.SetAttribute("is-vehicle", "True");
				el.SetAttribute("speed", bt.Speed.ToString());
				el.SetAttribute("capacity", bt.Capacity.ToString());
				if (bt.Fuel != null) AddItemAttribute(el, bt.Fuel, "fuel");
				if (bt.DriveSkill != null) AddSkill(el, bt.DriveSkill, "drive-skill");
				if (bt.DriveTerrain != null) AddTerrainAttribute(el, bt.DriveTerrain, "drive-terrain");
			}

			if (bt.Defence > 0)
				el.SetAttribute("defence", bt.Defence.ToString());

			if (bt.Radiation != 0)
				el.SetAttribute("radiation-inside", bt.Radiation.ToString());

			if (bt.Temperature != 0)
				el.SetAttribute("temperature-inside", bt.Temperature.ToString());

			if (bt.Resource != null)
				AddItem(el, bt.Resource, "resource");

			if (bt.HP > 0)
				el.SetAttribute("hp", bt.HP.ToString());

			foreach (Item itm in bt.Materials)
				AddItem(el, itm, "material");
			foreach (Item itm in bt.OptionalMaterials)
				AddItem(el, itm, "optional");

			if (bt.NoBuild)
				el.SetAttribute("no-build", "True");
		}

	}
}