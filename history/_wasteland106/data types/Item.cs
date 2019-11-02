using System;
using System.Collections;

namespace Wasteland
{
	public class Item : ILocalized
	{
		public ItemType Type = null;
		public int Amount = 0;

		public Item(ItemType t) 
		{
			if (t == null)
				throw new Exception("Type cannot be null");
			Type = t;
		}

		public Item(ItemType t, int amount) 
		{
			if (t == null)
				throw new Exception("Type cannot be null");
			Type = t;
			Amount = amount;
		}

		public string ToString(Lang lng) 
		{
			return Type.ToString(Amount, lng);
		}
	}

	public class ItemType : ILocalized
	{
		private static Hashtable Hash = new Hashtable();
		public static ItemTypeList List = new ItemTypeList();

		public string Name;
		public string FullNameEn1 = "";
		public string FullNameEn2 = "";
		public string FullNameRu0 = "";
		public string FullNameRu1 = "";
		public string FullNameRu2 = "";
		public bool IsMan = false;
		public bool IsMonster = false;
		public bool IsWeapon = false;
		public bool IsArmor = false;
		public bool NoGive = false;
		public int Weight = 0;
		public int[] Capacity = new int[2];
		public Skill InstallSkill = null;
		public int Rations = 0;
		public bool Burn = false;

		// Temperature
		public int OwnerTemperature = 0;
		public int TemperatureFrom = 0;
		public int TemperatureEffect = 0;

		// Radiation
		public int OwnerRadiation = 0;
		public int RegionRadiation = 0;
		public int RadiationFrom = 0;
		public int RadiationTo = 1000;
		public int RadiationEffect = 1;

		// Production
		public Skill ProduceSkill = null;
		public Item ProduceFrom1 = null;
		public Item ProduceFrom2 = null;
		public int ProductionRate = 1;
		public BuildingType ProduceBuilding = null;
		public ItemType ProduceAs = null;

		// Resource
		public int MutatePercent = 0;
		public ItemType MutateTo = null;
		public ItemType Dead = null;
		public int DecomposeChance = 0;

		// Man
    public ItemTypeList Food = new ItemTypeList();
		public ItemType Medicine = null;
		public ItemType Baby = null;
		public ItemType BabyFrom = null;
		public int BabyTimer = 0;
		public int MaxHireAmount = 0;
		public bool NoLearn = false;

		// Monster
		public SkillList Skills = new SkillList();
		public int Aggression = 0;
		public int PackSize = 1;
		public ItemList Drops = new ItemList();

		// Baby
		public ItemTypeList GrowTo = new ItemTypeList();

		// Weapon
		public SkillType WeaponSkill = null;
		public bool Ranged = false;
		public bool Heavy = false;
		public bool AntiTank = false;
		public bool Explosive = false;
		public int HP = 0;
		public ItemType Ammo = null;
		public int ToWound = 0;
		public int HitModifier = 0;
		public int ArmorModifier = 0;
		public int Attacks = 1;
		public int Targets = 1;
		public int Lethality = 0;
		public ItemType Case = null;

		// Armor
		public int ArmorSave = 0;

		public ItemType(string name) 
		{
			Name = name;
			Hash[name] = this;
			List.Add(this);
		}

		public static ItemType Get(string name) 
		{
			return (ItemType)Hash[name];
		}

		public static ItemType GetByAnyName(string name) 
		{
			name = name.ToLower();
			foreach (ItemType it in List) 
			{
				if (it.Name.ToLower() == name 
					|| it.FullNameEn1.ToLower() == name 
					|| it.FullNameEn2.ToLower() == name
					|| it.FullNameRu0.ToLower() == name
					|| it.FullNameRu1.ToLower() == name
					|| it.FullNameRu2.ToLower() == name)
					return it;
			}
			return null;
		}

		public string ToString(Lang lng) 
		{
			return ToString(1, lng);
		}

		public string ToString(int amount, Lang lng) 
		{
			if (lng == Lang.Ru) 
			{
				if (amount == 1)
					return String.Format("{0} [{1}]", this.FullNameRu1, this.Name);
				else if (amount % 10 >= 5 || amount % 10 == 0 || (amount >= 10 && amount < 20))
					return String.Format("{0} {1} [{2}]", amount, this.FullNameRu0,	this.Name);
				else if (amount % 10 == 1)
					return String.Format("{0} {1} [{2}]", amount, this.FullNameRu1,	this.Name);
				else
					return String.Format("{0} {1} [{2}]", amount, this.FullNameRu2,	this.Name);
			}
			else 
			{
				if (amount == 1)
					return String.Format("{0} [{1}]", this.FullNameEn1, this.Name);
				else
					return String.Format("{0} {1} [{2}]", amount, this.FullNameEn2,	this.Name);
			}
		}

	}

	#region Lists

	public class ItemList : ArrayList, ILocalized
	{
		public new Item this[int index] 
		{
			get { return (Item)base[index]; }
			set { base[index] = value; }
		}

		public override int Add(object value) 
		{
			throw new NotSupportedException();
		}

		public int Add(Item item) 
		{
			return base.Add(item);
		}

		public string ToString(Lang lng) 
		{
			string s = "";
			foreach (Item itm in this) 
			{
				if (s != "")
					s += ", ";
				s = s + itm.ToString(lng);
			}
			if (s == "") 
			{
				if (lng == Lang.En)	s = "none";
				else s = "нет";
			}
			return s + ".";
		}

		public Item GetByType(ItemType it) 
		{
			int i = Count-1;
			while (i >= 0 && this[i].Type != it) i--;
			if (i >= 0)
				return this[i];
			else
				return null;
		}

		public void RemoveItems(ItemType it, int amount) 
		{
			Item itm = GetByType(it);
			if (itm == null)
				return;
			if (amount < itm.Amount)
				itm.Amount -= amount;
			else
				Remove(itm);
		}

		public void AddItems(ItemType it, int amount) 
		{
			Item itm = GetByType(it);
			if (itm == null) 
			{
        itm = new Item(it);
				itm.Amount = 0;
				Add(itm);
			}
			itm.Amount += amount;
		}
	}


	public class ItemTypeList : ArrayList, ILocalized
	{
		public new ItemType this[int index] 
		{
			get { return (ItemType)base[index]; }
			set { base[index] = value; }
		}

		public override int Add(object value) 
		{
			throw new NotSupportedException();
		}

		public int Add(ItemType item) 
		{
			return base.Add(item);
		}

		public string ToString(Lang lng) 
		{
			string s = "";
			foreach (ItemType it in this) 
			{
				if (lng == Lang.En) 
				{
					if (s != "")
						s += ", ";
					if (Count > 1 && IndexOf(it) == Count - 1)
						s += "and ";
					s = s + it.FullNameEn2 + " [" + it.Name + "]";
				}
				else 
				{
					if (Count > 1 && IndexOf(it) == Count - 1)
						s += " и ";
					else if (s != "")
						s += ", ";
					s = s + it.ToString(1, lng);
				}
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
