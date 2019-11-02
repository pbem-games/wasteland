using System;
using System.IO;
using System.Text;

namespace Wasteland 
{
	class Request
	{
		public static void Load(string folder) 
		{
			if (folder == "")
				folder = Directory.GetCurrentDirectory();
			DirectoryInfo di = new DirectoryInfo(folder);
			foreach (FileInfo fi in di.GetFiles("request.*")) 
			{
        TextReader tr = new StreamReader(fi.FullName, Encoding.GetEncoding(1251));

				string email = "";
				string faction_name = "Faction";
				string gender = "MAN";
				string chosen_name = NameGenerator.Name(gender);
				Lang lng = Lang.En;
				bool body = false;

				while (true) 
				{
					string s = tr.ReadLine();
					if (s == null)
						break;
					if (s.Trim() == "")
						body = true;

					if (s.IndexOf(":") == -1)
						continue;
					string name = s.Substring(0, s.IndexOf(":")).ToLower();
					string val = s.Substring(s.IndexOf(":") + 2);

					if (name == "from")
						email = val;
					
					if (!body)
						continue;
					
					if (name == "faction")
						faction_name = val;
					if (name == "chosen" && val.ToLower() == "woman")
						gender = "WOMA";
					if (name == "name")
						chosen_name = val;
					if (name == "language" && val.ToLower() == "ru") 
						lng = Lang.Ru;
				}
				tr.Close();

				if (email != "") 
				{
					// Create new faction
					Faction f = new Faction();
					f.Email = email;
					f.Name = faction_name;
					f.Options.Lang = lng;
					for (int i = 0; i < 6; i++)
						f.Password += (Char)('a' + Constants.Random('z' - 'a'));

					// Select region with less faction players and monsters and more wanderers
					Region r = Map.Regions[Constants.Random(Map.Regions.Count)];
					int with_faction = 0;
					int without_faction = 0;
					foreach (Person p in r.Persons)
						if (!p.Faction.IsNPC || p.Man.IsMonster)
							with_faction++;
						else
							without_faction++;

          int j = Map.Regions.IndexOf(r);
					int start = j;
					while (true)
					{
						j++;
						if (j >= Map.Regions.Count)
							j = 0;
						if (j == start)
							break;
						Region r2 = Map.Regions[j];
						if (r2._radiation >= 90 || r2.Radiation >= 90 || !r2.Terrain.Walking)
							continue;
						int with_faction2 = 0;
						int without_faction2 = 0;
						foreach (Person p in r2.Persons)
							if (!p.Faction.IsNPC || p.Man.IsMonster)
								with_faction2++;
							else
								without_faction2++;
						
						if (with_faction2 < with_faction
							|| (with_faction2 == with_faction && without_faction2 > without_faction))
						{
							r = r2;
							with_faction = with_faction2;
							without_faction = without_faction2;
						}
					}

					if (r._radiation >= 90 || !r.Terrain.Walking)
						throw new Exception("What region you picked you?");

					// Create Chosen One
					Person chosen = new Person(f, r);
					chosen.Chosen = true;
					chosen.Name = chosen_name;
					chosen.Avoiding = true;

					chosen.AddItems(ItemType.Get(gender), 1);
					foreach (Item itm in DataFile.ChosenItems)
						chosen.AddItems(itm.Type, itm.Amount);
					foreach (Skill sk in DataFile.ChosenSkills) 
						chosen.AddSkill(sk.Type).Level = sk.Level;

					// Show all buildable objects
					foreach (Skill sk in chosen.Skills)
						foreach (BuildingType bt in BuildingType.List)
							if (!bt.NoBuild && bt.Materials.Count > 0 
								&& bt.Materials[0].Type.InstallSkill.Type == sk.Type)
								f.ShowBuilding(bt);

					Console.WriteLine("..Faction created for " + email);
				}
			}
		}
	}
}