using System;
using System.Collections;

namespace Wasteland
{
	enum Direction
	{
		None = 0,
		North = 1,
		Northeast = 2,
		Southeast = 3,
		South = 4,
		Southwest = 5,
		Northwest = 6
	}

	class Region
	{
		public bool[] Walls = new bool[6] 
			{ false, false, false, false, false, false };
		public int X = 0;
		public int Y = 0;
		public Terrain Terrain = null;
		public string Name = "";
		public int _radiation = 0;

		public BuildingArrayList Buildings = new BuildingArrayList();
		public PersonArrayList Persons = new PersonArrayList();
		public ItemArrayList Resources = new ItemArrayList();
		public ItemArrayList TurnResources = null;
		public ItemArrayList Junk = new ItemArrayList();

		public ArrayList BattleReports = new ArrayList();

		public string ToString(Lang lng) 
		{
			if (lng == Lang.Ru)
				return String.Format("{0} ({1},{2}), {3}", Terrain.FullNameRu, X, Y, Name);
			else
				return String.Format("{0} ({1},{2}), {3}", Terrain.FullNameEn, X, Y, MyStrings.Translit(Name));
		}

		public void CalcTurnResources()
		{
			// Get resources from region list
			ItemArrayList res = new ItemArrayList();
			foreach (Item itm in Resources)
				res.Add(new Item(itm.Type, itm.Amount));

			// Apply temperature factor
			int tempr = this.Temperature;
			for (int i = res.Count-1; i >= 0; i--)
			{
				Item itm = res[i];
				int danger = (itm.Type.TemperatureFrom - tempr) * itm.Type.TemperatureEffect;
				if (danger > 0) 
				{
					int died = itm.Amount * Math.Min(100, danger) / 100;
					res.RemoveItems(itm.Type, died);
				}
			}

			// Add resources added by production buildings
			foreach (Building b in Buildings)
				if (b.Type.Resource != null && b.IsComplete)
					res.AddItems(b.Type.Resource.Type, b.Type.Resource.Amount);

			// Apply radiation factor
			int radiation = this.Radiation;
			for (int i = res.Count-1; i >= 0; i--)
			{
				Item itm = res[i];
				int danger = (radiation - itm.Type.RadiationTo) * itm.Type.RadiationEffect;
				if (danger > 0) 
				{
					// Mutate some resource
					int amt = itm.Amount * Math.Min(100, danger) / 100;
					res.RemoveItems(itm.Type, amt);
					int amt_mutated = amt * itm.Type.MutatePercent / 100;
					if (itm.Type.MutateTo != null && amt_mutated > 0)
						res.AddItems(itm.Type.MutateTo, amt_mutated);
				}
			}

			TurnResources = res;
		}

		public void CalcTurnJunk() 
		{
			// Add new junk
			// Get dying resources from region list
			ItemArrayList res = new ItemArrayList();
			foreach (Item itm in Resources)
				if (itm.Type.Dead != null)
					res.Add(new Item(itm.Type, itm.Amount));

			// Apply temperature factor
			int tempr = this.Temperature;
			for (int i = res.Count-1; i >= 0; i--)
			{
				Item itm = res[i];
				int danger = (itm.Type.TemperatureFrom - tempr) * itm.Type.TemperatureEffect;
				if (danger > 0) 
				{
					int died = itm.Amount * Math.Min(100, danger) / 100;
					res.RemoveItems(itm.Type, died);
					Junk.AddItems(itm.Type.Dead, died);
				}
			}

			// Add resources added by production buildings
			foreach (Building b in Buildings)
				if (b.Type.Resource != null && b.Type.Resource.Type.Dead != null
					&& b.IsComplete)
					res.AddItems(b.Type.Resource.Type, b.Type.Resource.Amount);

			// Apply radiation factor
			int radiation = this.Radiation;
			for (int i = res.Count-1; i >= 0; i--)
			{
				Item itm = res[i];
				int danger = Math.Max(radiation - itm.Type.RadiationTo,
					itm.Type.RadiationFrom - radiation) * itm.Type.RadiationEffect;
				if (danger > 0) 
				{
					int amt = itm.Amount * Math.Min(100, danger) / 100;
					int amt_mutated = amt * itm.Type.MutatePercent / 100;
					int amt_dead = amt - amt_mutated;
					if (amt_dead > 0)
						Junk.AddItems(itm.Type.Dead, amt_dead);
				}
			}

			// Decompose junk
			for (int i = Junk.Count-1; i >= 0; i--)
			{
				Item itm = Junk[i];
				if (itm.Type.DecomposeChance > 0) 
				{
					int decomposed = itm.Amount * itm.Type.DecomposeChance / 100;
					if (decomposed == 0 && Constants.Random(100) < itm.Type.DecomposeChance * itm.Amount)
						decomposed = 1;
					Junk.RemoveItems(itm.Type, decomposed);
				}
			}
		}

		public Region RegionInDir(Direction dir) 
		{
			if (Walls[(int)dir - 1])
				return null;
			switch (dir) 
			{
				case Direction.North: return Map.Region(X, Y-2);
				case Direction.Northeast: return Map.Region(X+1, Y-1);
				case Direction.Southeast: return Map.Region(X+1, Y+1);
				case Direction.South: return Map.Region(X, Y+2);
				case Direction.Southwest: return Map.Region(X-1, Y+1);
				default: return Map.Region(X-1, Y-1);
			}
		}

		public int PointsToEnter() 
		{
			return Terrain.MP;
		}

		public int Radiation 
		{
			get 
			{
				int res = _radiation * Weather.Radiation / 100;
				foreach (Person p in Persons)
					foreach (Item itm in p.Items)
						res += itm.Type.RegionRadiation * itm.Amount;
				foreach (Item itm in Junk)
					res += itm.Type.RegionRadiation * itm.Amount;
				return res; 
			}
			set { _radiation = value; }
		}

		public int Temperature 
		{
			get { return Month.Current.Temperature; }
		}

		public Weather Weather 
		{
			get { return Month.Current.Weather; }
		}
	}

	class Map 
	{
		public static string[] DirectionNames = new string[6] 
			{"N", "NE", "SE", "S", "SW", "NW"};
		public static string[] DirectionFullNamesRu = new string[6] 
			{"Север", "Северо-восток", "Юго-восток", "Юг", "Юго-запад", "Северо-запад"};
		
		public static RegionArrayList Regions = new RegionArrayList();
		public static int Width = 0;
		public static int Height = 0;
		public static int Turn = 0;

		public static void SetDimensions(int width, int height) 
		{
			Regions.Clear();
			for (int yy = 0; yy < height; yy++)
				for (int xx = 0; xx < width; xx++) 
				{
					if (xx % 2 == 0 ^ yy % 2 == 0)
						continue;
					Region r = new Region();
					r.X = xx;
					r.Y = yy;
					Regions.Add(r);
				}
			Width = width;
			Height = height;
		}

		public static Region Region(int x, int y) 
		{
			if (x < 0 || y < 0 || x >= Width || y >= Height)
				return null;
			int idx = y * (Width / 2) + x / 2;
			return (Region)Regions[idx];
		}

		public static void CalcTurnResources() 
		{
			foreach (Region r in Regions)
				r.CalcTurnResources();
		}

		public static void CalcTurnJunk() 
		{
			foreach (Region r in Regions)
				r.CalcTurnJunk();
		}
	}

	#region Lists

	class RegionArrayList : ArrayList 
	{
		public new Region this[int index] 
		{
			get { return (Region)base[index]; }
			set { base[index] = value; }
		}
	}

	#endregion

}
