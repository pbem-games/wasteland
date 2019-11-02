using System;
using System.Collections;

namespace Wasteland
{
	class Building 
	{
		private static int _maxNum = 0;
		public PersonArrayList Persons = new PersonArrayList();
		private Region _region;
		public BuildingType Type = null;
		public int Num;
		public string Name = "";
		public string Description = "";
		public ItemArrayList Installed = new ItemArrayList();

		public Building(BuildingType t, Region r) 
		{
			if (t == null)
				throw new Exception("Type cannot be null");
			Type = t;
			Num = _maxNum + 1;
			_maxNum++;
			r.Buildings.Add(this);
			_region = r;
		}

		public Building(BuildingType t, Region r, int num) 
		{
			if (t == null)
				throw new Exception("Type cannot be null");
			Type = t;
			Num = num;
			if (num > _maxNum)
				_maxNum = num;
			r.Buildings.Add(this);
			_region = r;
		}

		public string ToString(Lang lng) 
		{
			if (lng == Lang.Ru)
				return String.Format("{0} ({1})", Name, Num);
			else
				return String.Format("{0} ({1})", MyStrings.Translit(Name), Num);
		}

		public void Damage() 
		{
			// Destroy random component
			int total = 0;
			foreach (Item itm in Installed)
				total += itm.Amount;
			if (total == 0)
				return;
			int idx = Constants.Random(total);
			for (int i = 0; i < Installed.Count; i++) 
			{
				idx -= Installed[i].Amount;
				if (idx > 0) continue;
        Installed.RemoveItems(Installed[i].Type, 1);
				break;
			}

			// If first components spent, collapse
			if (Installed.Count == 0 || 
				(Type.Materials.Count > 0 && Installed.GetByType(Type.Materials[0].Type) == null))
				Collapse();
		}

		public void Collapse() 
		{
			// Evict persons inside
			for (int i = Persons.Count-1; i >= 0; i--)
				Persons[i].Building = null;

			// Drop all components to Junk
			foreach (Item itm in Installed)
				Region.Junk.AddItems(itm.Type, itm.Amount);

			// Destroy building
			Region = null;
		}

		public Region Region 
		{
			get { return _region; }
			set 
			{
				if (_region != null)
					_region.Buildings.Remove(this);
				_region = value;
				if (_region != null)
					_region.Buildings.Add(this);

				foreach (Person p in Persons) 
					p.Region = _region;
			}
		}

		public ItemArrayList GetNeeds()
		{
			ItemArrayList needs = new ItemArrayList();
			foreach (Item itm in Type.Materials) 
			{
				Item installed = Installed.GetByType(itm.Type);
				if (installed == null)
					needs.Add(new Item(itm.Type, itm.Amount));
				else if (installed.Amount < itm.Amount)
					needs.Add(new Item(itm.Type, itm.Amount - installed.Amount));
			}
			return needs;
		}
	
		public bool IsPersonInside(Person p) 
		{
			int defence = Type.Defence;
			if (defence == 0)
				return false;

			foreach (Person p2 in Persons) 
				if (p2.Leader == null) 
				{
					if (p2 == p) 
						return true;
					defence--;
					if (defence == 0)
						return false;
					foreach (Person sub in p2.Team) 
					{
						if (sub == p) 
							return true;
						defence--;
						if (defence == 0)
							return false;
					}
				}

			return false;
		}
	}

	class BuildingType 
	{
    public static ArrayList List = new ArrayList();
		private static Hashtable Hash = new Hashtable();
		public string Name;
		public string FullNameEn;
		public string FullNameRu;
		public Skill MaintainSkill = null;
		public Skill DriveSkill = null;
		public bool NoBuild = false;
		public int Speed = 0;
		public Terrain DriveTerrain = null;
		public int Capacity = 0;
		public int Defence = 0;
		public int HP = 0;
		public int Radiation = 0;
		public int Temperature = 0;
		public ItemArrayList Materials = new ItemArrayList();
		public ItemArrayList OptionalMaterials = new ItemArrayList();
		public ItemType Fuel = null;
		public Item Resource = null;

		public BuildingType(string name) 
		{
			Name = name;
			Hash[name] = this;
			List.Add(this);
		}

		public string ToString(Lang lng) 
		{
			if (lng == Lang.Ru)
				return String.Format("{0} [{1}]", FullNameRu, Name);
			else
				return String.Format("{0} [{1}]", FullNameEn, Name);
		}

		public static BuildingType Get(string name) 
		{
			return (BuildingType)Hash[name];
		}

		public static BuildingType GetByAnyName(string name) 
		{
			foreach (BuildingType bt in List) 
			{
				if (bt.Name.ToLower() == name.ToLower())
					return bt;
			}
			return null;
		}

	}

	#region Lists

	class BuildingArrayList : ArrayList 
	{
		public new Building this[int index] 
		{
			get { return (Building)base[index]; }
			set { base[index] = value; }
		}

		public Building GetByNumber(int num) 
		{
			int i = Count-1;
			while (i >= 0 && this[i].Num != num) i--;
			if (i >= 0)
				return this[i];
			else
				return null;
		}
	}

	class BuildingTypeArrayList : ArrayList 
	{
		public new BuildingType this[int index] 
		{
			get { return (BuildingType)base[index]; }
			set { base[index] = value; }
		}

		public string ToString(Lang lng) 
		{
			string s = "";
			foreach (BuildingType it in this) 
			{
				if (lng == Lang.En) 
				{
					if (s != "")
						s += ", ";
					if (Count > 1 && IndexOf(it) == Count - 1)
						s += "and ";
				}
				else 
				{
					if (Count > 1 && IndexOf(it) == Count - 1)
						s += " и ";
					else if (s != "")
						s += ", ";
				}
				s = s + it.ToString(lng);
			}
			if (s == "") 
			{
				if (lng == Lang.En)	s = "none";
				else s = "нет";
			}
			return s + ".";
		}
	}


	#endregion

}

