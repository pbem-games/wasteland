using System;

namespace Wasteland
{
	public class Constants
	{
		public static int RationsPerMonth = 30;
		public static int CompleteInsanity = 10;
		public static int DangerousInsanity = 8;
		public static int LevelToCure = 1;
		public static int SexPercent = 50;
		public static int SexInsanityDecrease = 1;
		public static int LeaderExpirience = 5;
		public static int StudentExpirience = 15;
		public static int WandererMoveChance = 20;
		public static int NPCMateChance = 20;
		public static int UnarmedToWound = 10;
		public static int MaxFriendlyTeam = 5;
		public static int NPCFactionNum = 1;
		public static float HireDropPerFactionPerson = 0.001F;
		public static int ChildTurns = 10;

		private static Random rnd = new Random();

		public static int Random(int maxValue)
		{
      return rnd.Next(maxValue);
		}

		public static SkillType CureSkill 
		{
			get { return SkillType.Get("DOC"); }
		}

		public static SkillType ScavengeSkill 
		{
			get { return SkillType.Get("SCAV"); }
		}

		public static SkillType ReflexesSkill 
		{
			get { return SkillType.Get("REFL"); }
		}

		public static SkillType MeleeSkill 
		{
			get { return SkillType.Get("MELE"); }
		}

		public static SkillType RangedSkill 
		{
			get { return SkillType.Get("RANG"); }
		}

		public static SkillType HeavySkill 
		{
			get { return SkillType.Get("HWPN"); }
		}

	}

	public enum Movement 
	{
		Walk = 0,
		Ride = 1
	}

	public enum Lang
	{
		En,
		Ru
	}
}
