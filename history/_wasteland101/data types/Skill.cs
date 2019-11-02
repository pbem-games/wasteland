using System;
using System.Collections;

namespace Wasteland
{
	class Skill 
	{
		public SkillType Type = null;
		public int Level = 0;

		public Skill(SkillType t) 
		{
			if (t == null)
				throw new Exception("Type cannot be null");
			Type = t;
			Level = t.DefaultLevel;
		}

		public string ToString(Lang lng) 
		{
			return Type.ToString(Level, lng);
		}
	}

	class SkillType 
	{
		public static ArrayList List = new ArrayList();
		private static Hashtable Hash = new Hashtable();
		public string Name;
		public string FullNameEn;
		public string FullNameRu;
		public string DescriptionEn;
		public string DescriptionRu;
		public SkillType BasedOn = null;
		public int DefaultLevel = 0;

		public SkillType(string name) 
		{
			Name = name;
			Hash[name] = this;
			List.Add(this);
		}

		public static SkillType Get(string name) 
		{
			return (SkillType)Hash[name];
		}

		public static SkillType GetByAnyName(string name) 
		{
			foreach (SkillType st in Hash.Values) 
			{
				name = name.ToLower();
				if (st.Name.ToLower() == name.ToLower() 
					|| st.FullNameEn.ToLower() == name
					|| st.FullNameRu.ToLower() == name)
					return st;
			}
			return null;
		}

		public string ToString(Lang lng) 
		{
			if (lng == Lang.Ru)
				return String.Format("{0} [{1}]", FullNameRu, Name);
			else
				return String.Format("{0} [{1}]", FullNameEn, Name);
		}

		public string ToString(int level, Lang lng) 
		{
			if (lng == Lang.Ru)
				return String.Format("{0} [{1}] {2}", FullNameRu, Name, level);
			else
				return String.Format("{0} [{1}] {2}", FullNameEn, Name, level);
		}
	}

	#region Lists

	class SkillArrayList : ArrayList 
	{
		public new Skill this[int index] 
		{
			get { return (Skill)base[index]; }
			set { base[index] = value; }
		}

		public string ToString(Lang lng) 
		{
			string s = "";
			foreach (Skill sk in this) 
			{
				if (s != "")
					s += ", ";
				s = s + sk.ToString(lng);
			}
			if (s == "") 
			{
				if (lng == Lang.En)	s = "none";
				else s = "нет";
			}
			return s + ".";
		}

		public Skill GetByType(SkillType st) 
		{
			int i = Count-1;
			while (i >= 0 && this[i].Type != st) i--;
			if (i >= 0)
				return this[i];
			else
				return null;
		}
	}

	#endregion

}
