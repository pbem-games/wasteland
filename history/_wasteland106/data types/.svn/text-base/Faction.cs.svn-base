using System;
using System.Collections;

namespace Wasteland
{
	public enum Attitude 
	{
		Hostile = 0,
		Unfriendly = 1,
		Neutral = 2,
		Friendly = 3,
		Ally = 4
	}

	public class Faction : ILocalized
	{
		public static FactionList List = new FactionList();
		public static int MaxNum = 0;
		public string Name = "";
		public int Num = 0;
		public string Password = "";
		public string Email = "";
		public Attitude DefaultAttitude = Attitude.Neutral;
		public AttitudeHashtable Attitudes = new AttitudeHashtable();
		public PersonList Persons = new PersonList();
		public EventList Events = new EventList();
		public Options Options = new Options();
		
		public ArrayList ItemsToShow = new ArrayList();
		public ArrayList ShownItems = new ArrayList();
		public ArrayList SkillsToShow = new ArrayList();
		public ArrayList ShownSkills = new ArrayList();
		public ArrayList BuildingsToShow = new ArrayList();
		public ArrayList ShownBuildings = new ArrayList();
		
		public ArrayList BattleRegions = new ArrayList();

		public Faction() 
		{
			this.Num = MaxNum + 1;
			MaxNum++;
			List.Add(this);
		}

		public Faction(int num) 
		{
			this.Num = num;
			if (num > MaxNum)
				MaxNum = num;
			List.Add(this);
		}

		public string ToString(Lang lng) 
		{
			if (lng == Lang.En)
				return String.Format("{0} ({1})", MyStrings.Translit(Name), Num);
			else
				return String.Format("{0} ({1})", Name, Num);
		}

		public static Faction Get(int num) 
		{
			int i = List.Count-1;
			while (i >= 0 && ((Faction)List[i]).Num != num) i--;
			if (i >= 0)
				return (Faction)List[i];
			else
				return null;
		}

		public bool IsNPC 
		{
			get { return (Num == Constants.NPCFactionNum); }
		}

		public Person GetChosen() 
		{
			foreach (Person p in Persons)
				if (p.Chosen) return p;
			return null;
		}

		public void ShowItem(ItemType it, bool force) 
		{
			if (!ItemsToShow.Contains(it) && (force || !ShownItems.Contains(it)))
				ItemsToShow.Add(it);
		}

		public void ShowItem(ItemType it) 
		{
			ShowItem(it, false);
		}

		public void ShowSkill(SkillType st, bool force) 
		{
			if (!SkillsToShow.Contains(st) && (force || !ShownSkills.Contains(st))) 
			{
				SkillsToShow.Add(st);
				/*
					// Production
					foreach (ItemType it in ItemType.List)
						if (it.ProduceSkill != null && it.ProduceSkill.Type == st)
							ShowItem(it);

					// Installation
					foreach (ItemType it in ItemType.List)
						if (it.InstallSkill != null && it.InstallSkill.Type == st) 
						{
							ShowItem(it);
							foreach (BuildingType bt in BuildingType.List.Values)
								if (bt.Materials.Count > 0 && bt.Materials[0].Type == it)
									ShowBuilding(bt);
						}
				*/
			}
		}

		public void ShowSkill(SkillType st) 
		{
			ShowSkill(st, false);
		}

		public void ShowBuilding(BuildingType bt, bool force) 
		{
			if (!BuildingsToShow.Contains(bt) && (force || !ShownBuildings.Contains(bt)))
				BuildingsToShow.Add(bt);
		}

		public void ShowBuilding(BuildingType bt) 
		{
			ShowBuilding(bt, false);
		}

		public void AllShown() 
		{
			foreach (ItemType it in ItemsToShow)
				if (!ShownItems.Contains(it))
					ShownItems.Add(it);
			foreach (SkillType st in SkillsToShow)
				if (!ShownSkills.Contains(st))
					ShownSkills.Add(st);
			foreach (BuildingType bt in BuildingsToShow)
				if (!ShownBuildings.Contains(bt))
					ShownBuildings.Add(bt);
		}
	}

	public class Options 
	{
		public bool TextReport = true;
		public bool XmlReport = true;
		public Lang Lang = Lang.En;
		public TemplateType Template = TemplateType.Short;
	}

	public enum TemplateType 
	{
		Long,
		Short
	}

	#region Lists

	public class FactionList : ArrayList 
	{
		public new Faction this[int index] 
		{
			get { return (Faction)base[index]; }
			set { base[index] = value; }
		}

		public override int Add (object value) 
		{
			throw new NotSupportedException();
		}

		public int Add(Faction item) 
		{
			return base.Add(item);
		}

		public Faction GetByNumber(int num) 
		{
			int i = Count-1;
			while (i >= 0 && this[i].Num != num) i--;
			if (i >= 0)
				return this[i];
			else
				return null;
		}
	}

	public class AttitudeHashtable : Hashtable 
	{
		public Attitude this[int key] 
		{
			get { return (Attitude)base[key]; }
			set { base[key] = value; }
		}
	}

	#endregion
}
