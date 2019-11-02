using System;
using System.IO;
using System.Xml;

namespace MapCreator
{
	class Class1
	{
		[STAThread]
		static void Main(string[] args)
		{
			// Initialize
			TextReader tr = new StreamReader("input.txt", System.Text.Encoding.GetEncoding(1251));
			string map = tr.ReadToEnd();
			tr.Close();

			tr = new StreamReader("input.xml", System.Text.Encoding.GetEncoding(1251));
			XmlDocument input = new XmlDocument();
			input.LoadXml(tr.ReadToEnd());
			tr.Close();
			NameGenerator.Init((XmlElement)input.SelectSingleNode("data/names"));

			XmlDocument doc = new XmlDocument();
			doc.LoadXml("<game turn=\"0\"><faction num=\"1\" name=\"NPC\"/></game>");

			// Create map
      XmlElement elMap = doc.CreateElement("map");
			doc.DocumentElement.AppendChild(elMap);
			string[] ss = map.Replace("\r", "").Split('\n');
			int y = 0;
			int max_y = 0;
			int max_x = 0;
			int state = 1;
			foreach (string s in ss) 
			{
				if (s == "") 
				{
					if (y > max_y) max_y = y;
					y = 0;
					state++;
				}
				else 
				{
					switch(state) 
					{
						case 1:
							for (int x = 0; x < s.Length; x++) 
							{
								if (s[x] == ' ') continue;
								XmlElement elRegion = doc.CreateElement("region");
								elMap.AppendChild(elRegion);
								elRegion.SetAttribute("x", x.ToString());
								elRegion.SetAttribute("y", y.ToString());
								elRegion.SetAttribute("terrain", s[x].ToString());
								if (x > max_x) max_x = x;
							}
							y++;
							break;
						case 2:
							for (int x = 0; x < s.Length; x++) 
							{
								if (s[x] == ' ') continue;
								XmlElement elRegion = (XmlElement)elMap.SelectSingleNode("region[@x=" + x.ToString() +
									" and @y=" + y.ToString() + "]");
								elRegion.SetAttribute("name", s[x].ToString());
							}
							y++;
							break;
						case 3:
							string[] nv = s.Split('=');
							foreach (XmlElement elRegion in elMap.SelectNodes("region[@name='" + nv[0] + "']")) 
								elRegion.SetAttribute("name", nv[1]);
							break;
					}
				}
			}
			elMap.SetAttribute("width", (max_x+1).ToString());
			elMap.SetAttribute("height", (max_y).ToString());

			// Process regions
			foreach (XmlElement elRegion in elMap.SelectNodes("region")) 
			{
				XmlElement elTerrain = (XmlElement)input.SelectSingleNode("data/terrain[@type='" +
					elRegion.GetAttribute("terrain") + "']");
				if (elTerrain == null)
					continue;
				AddElements(elTerrain, elRegion);
			}

			// Persons
			int person_num = 1;
			AddEntity("person", ref person_num, elMap, input);
			int building_num = 1;
			AddEntity("building", ref building_num, elMap, input);

			foreach (XmlElement elPlace in input.SelectNodes("data/place-building")) 
			{
				XmlElement elBuilding = (XmlElement)input.SelectSingleNode(
					"data/building[@type='" + elPlace.GetAttribute("type") + "']");
				XmlElement elRegion = (XmlElement)doc.SelectSingleNode(
					String.Format("//region[@x={0} and @y={1}]", elPlace.GetAttribute("x"),
						elPlace.GetAttribute("y")));
        XmlElement el = doc.CreateElement("building");
				elRegion.AppendChild(el);
				el.SetAttribute("num", building_num.ToString());
				el.SetAttribute("name", elPlace.GetAttribute("name"));
				AddElements(elBuilding, el);
				building_num++;
			}
			Console.WriteLine(building_num.ToString() + " building");

			// Write gamein
			TextWriter tw = new StreamWriter("gamein.xml", false, System.Text.Encoding.GetEncoding(1251));
			tw.Write("<?xml version=\"1.0\" encoding=\"windows-1251\"?>" + doc.OuterXml);
			tw.Close();
		}

		public static void AddEntity(string name, ref int num, XmlElement elMap, XmlDocument input) 
		{
			foreach (XmlElement elPerson in input.SelectNodes("data/" + name)) 
			{
				int amount = Convert.ToInt32(elPerson.GetAttribute("amount"));
				for (int i = 0; i < amount; i++) 
				{
					// Select terrain type
					XmlElement elTerrain = RandomElement.Select(elPerson.SelectNodes("terrain"));

					// Select region
					XmlNodeList regions = elMap.SelectNodes("region[@terrain='" + 
						elTerrain.GetAttribute("type") + "']");
					XmlElement elRegion = (XmlElement)regions[Constants.Random(regions.Count)];

					// Add person to region
					XmlElement elRegionPerson = elMap.OwnerDocument.CreateElement(name);
					elRegion.AppendChild(elRegionPerson);
					elRegionPerson.SetAttribute("num", num.ToString());
					num++;
					
					// Nodes
					AddElements(elPerson, elRegionPerson);

					if (name == "person") 
					{
						// Generate name
						string gender = "man";
						if (elRegionPerson.SelectSingleNode("item[@type='WOMA']") != null)
							gender = "woma";
						elRegionPerson.SetAttribute("name", NameGenerator.Name(gender, "",
							elPerson.GetAttribute("name-group")));
					}

				}
			}
			Console.WriteLine(num.ToString() + " " + name);
		}

		public static void AddElements(XmlElement elSource, XmlElement elDestination) 
		{
			foreach (XmlElement el in elSource.SelectNodes("*")) 
			{
				if (el.Name == "terrain") 
					continue;
				if (el.Name == "attribute") 
				{
					elDestination.SetAttribute(el.GetAttribute("name"), el.GetAttribute("value"));
					continue;
				}
				if (el.Name == "num-attribute") 
				{
					SetNumAttributes(elSource, elDestination);
					continue;
				}
				if (el.Name == "group") 
				{
					XmlElement elGroup = RandomElement.Select(el.SelectNodes("or"));
					if (elGroup != null)
						AddElements(elGroup, elDestination);
				}
				else 
				{
					XmlElement elCopy = (XmlElement)elDestination.OwnerDocument.ImportNode(el.Clone(), true);
					SetNumAttributes(elCopy, elCopy);
					elDestination.AppendChild(elCopy);
				}
			}
		}

		public static int NumValue(XmlElement el) 
		{
			int min = Convert.ToInt32(el.GetAttribute("min"));
			int max = Convert.ToInt32(el.GetAttribute("max"));
			return Constants.Random(min, max);
		}

		public static void SetNumAttributes(XmlElement elSource, XmlElement elDestination) 
		{
			XmlNodeList nodes = elSource.SelectNodes("num-attribute");
			foreach (XmlElement elAttr in nodes) 
				elDestination.SetAttribute(elAttr.GetAttribute("name"), NumValue(elAttr).ToString());
			if (elSource == elDestination)
				foreach (XmlElement el in nodes)
					elDestination.RemoveChild(el);
		}
	}

	class Constants 
	{
		private static Random rnd = new Random();
		
		public static int Random(int maxValue)
		{
			return rnd.Next(maxValue);
		}

		public static int Random(int minValue, int maxValue)
		{
			return rnd.Next(maxValue - minValue + 1) + minValue;
		}
	}
}
