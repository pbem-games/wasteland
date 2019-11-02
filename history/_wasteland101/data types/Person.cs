using System;
using System.Collections;

namespace Wasteland
{
	class Person 
	{
		private static int _maxNum = 0;
		private Faction _faction = null;
		private Region _region = null;
		private Building _building = null;
		private Person _leader = null;
		private int _insanity = 0;

		public static PersonArrayList List = new PersonArrayList();
		public string Name = "";
		public string Description = "";
		public int Num = 0;
		public bool Chosen = false;
		public bool Patrolling = false;
		public bool Avoiding = false;
		public bool Maintaining = false;
		public bool HideFaction = false;
		public bool Hide = false;
		public int BabyTimer = 0;
		public SkillType FatherSkill = null;
		public string FatherName = null;
		public TradeOrder TradeOrder = null;
		public bool ReceivedCombatXP = false;
		public int Age = 100;

		public ItemArrayList Items = new ItemArrayList();
		public SkillArrayList Skills = new SkillArrayList();
		public ArrayList Orders = new ArrayList();
		public PersonArrayList Team = new PersonArrayList();
		public ArrayList RepeatingLines = new ArrayList();
		public ItemTypeArrayList Consume = new ItemTypeArrayList();
		public ItemTypeArrayList Burn = new ItemTypeArrayList();
		public ItemTypeArrayList Equipment = new ItemTypeArrayList();

		public int Insanity 
		{
			get { return _insanity; }
			set { _insanity = Math.Max(0, Math.Min(Constants.CompleteInsanity, value)); }
		}

		public Person(Faction f, Region r)
		{
			if (f == null)
				throw new Exception("Faction should not be null");
			if (r == null)
				throw new Exception("Region should not be null");
			this.Num = _maxNum + 1;
			_maxNum++;
			this.Faction = f;
			this.Region = r;
			List.Add(this);
		}

		public Person(Faction f, Region r, int num) 
		{
			if (f == null)
				throw new Exception("Faction should not be null");
			if (r == null)
				throw new Exception("Region should not be null");
			this.Num = num;
			if (num > _maxNum)
				_maxNum = num;
			this.Faction = f;
			this.Region = r;
			List.Add(this);
		}

		public string ToString(Lang lng) 
		{
			if (lng == Lang.Ru)
				return String.Format("{0} ({1})", Name, Num);
			else
				return String.Format("{0} ({1})", MyStrings.Translit(Name), Num);
		}

		public static Person Get(int num) 
		{
			int i = List.Count-1;
			while (i >= 0 && List[i].Num != num) i--;
			if (i >= 0)
				return List[i];
			else
				return null;
		}
		
		public Building Building 
		{
			get { return _building; }
			set 
			{
				if (_building != null)
					_building.Persons.Remove(this);
				_building = value;
				if (_building != null)
					_building.Persons.Add(this);

				// Relocate team
				foreach (Person sub in Team)
					sub.Building = value;
			}
		}

		public Person Leader 
		{
			get { return _leader; }
			set 
			{
				if (_leader == this)
					throw new Exception("Can't be leader to himself");
				if (_leader != null) 
				{
					if (_leader.Region != this.Region)
						throw new Exception("Leader in other region");
					_leader.Team.Remove(this);
				}
				_leader = value;
				if (_leader != null)
					_leader.Team.Add(this);
				while (Team.Count > 0)
					Team[0].Leader = _leader;
				if (_leader != null)
					Building = _leader.Building;
			}
		}

		public Region Region 
		{
			get { return _region; }
			set 
			{
				if (value == null)
					throw new Exception("Region should not be null");
				if (_region != null)
					_region.Persons.Remove(this);
				_region = value;
				_region.Persons.Add(this);

				// Relocate team
				foreach (Person sub in Team)
					sub.Region = value;
			}
		}

		public Faction Faction
		{
			get { return _faction; }
			set 
			{
				if (value == null)
					throw new Exception("Faction should not be null");
				if (_faction != null)
					_faction.Persons.Remove(this);
				_faction = value;
				_faction.Persons.Add(this);

				if (_faction.IsNPC) 
				{
					Avoiding = false;
					Hide = false;
					HideFaction = false;
				}
			}
		}

		public int GetWeight() 
		{
			int weight = 0;
			foreach (Item itm in Items)
				weight += itm.Type.Weight * itm.Amount;
			return weight;
		}

		public int GetCapacity(Movement ctype) 
		{
			int capacity = 0;
			foreach (Item itm in Items)
				capacity += itm.Type.Capacity[(int)ctype] * itm.Amount;
			return capacity;
		}

		public PersonArrayList GetTeamAndLeader() 
		{
			PersonArrayList list = new PersonArrayList();
			list.Add(this);
			foreach (Person p in Team)
				list.Add(p);
			return list;
		}

		public int GetTeamLevel(SkillType st) 
		{
			if (Leader != null)
				throw new Exception("Should not be called for subordinate");

			// Get leader skill 
			Skill sk1 = Skills.GetByType(st);
			if (sk1 == null)
				return 0;
			int points = sk1.Level;

			// Get team skill
			foreach (Person p in Team) 
			{
				Skill sk = p.Skills.GetByType(st);
				if (sk != null)
					points += sk.Level;
			}
			return points;
		}

		public int GetTeamAmount(ItemType it) 
		{
			if (Leader != null)
				throw new Exception("Should not be called for subordinate");

			int amt = 0;
			foreach (Person p in GetTeamAndLeader()) 
			{
				Item itm = p.Items.GetByType(it);
				if (itm != null)
					amt += itm.Amount;
			}
			return amt;
		}

		public void SpendFromTeam(ItemType it, int amount) 
		{
			if (Leader != null)
				throw new Exception("Should not be called for subordinate");

			foreach (Person p in GetTeamAndLeader()) 
			{
				Item itm = p.Items.GetByType(it);
				if (itm != null) 
				{
					int remove_amt = Math.Min(amount, itm.Amount);
					p.Items.RemoveItems(it, remove_amt);
					amount -= remove_amt;
				}
			}

			if (amount > 0) 
				throw new Exception("Too many items to remove from team");
		}

		public int GetHireAmount() 
		{
			if (this.Chosen)
				return 0;
			if (this.Faction.Num == Constants.NPCFactionNum)
				return 1;

			int effective_insanity = Insanity;

			Person chosen = this.Faction.GetChosen();
			if (chosen != null) 
			{
				// Lower by 2 if in team with Chosen
				if (this.Leader == chosen || chosen.Leader == this
					|| (this.Leader != null && this.Leader == chosen.Leader))
					effective_insanity -= 2;

				// Lower by 1 if in region with Chosen
				if (chosen.Region == this.Region)
					effective_insanity -= 1;
			}

			if (effective_insanity < 0)
				return 0;
			if (effective_insanity > 10)
				effective_insanity = 10;
			
			ItemType man = this.Man;
			if (man == null)
				return 0;

			int amount = Math.Max(1, man.MaxHireAmount / (effective_insanity+1));
			amount = Math.Max(1, (int)Math.Round(amount * (1 - Faction.Persons.Count * Constants.HireDropPerFactionPerson)));

			return amount;
		}

		public ItemType Man 
		{
			get 
			{
				if (Items.Count == 0 || !Items[0].Type.IsMan)
					throw new Exception("No man in person");
				return Items[0].Type;
			}
		}

		public void Kill() 
		{
			Kill(true);
		}

		public void Kill(bool drop) 
		{
			if (drop) 
			{
				// Drop items to Junk
				foreach (Item itm in Items)
					if (!itm.Type.NoGive)
						Region.Junk.AddItems(itm.Type, itm.Amount);
				// Drop drops to Junk
				foreach (Item itm in Man.Drops)
					Region.Junk.AddItems(itm.Type, itm.Amount);
			}

			Leader = null;
			foreach (Person p in Team)
				p.Leader = null;
			Building = null;
			_faction.Persons.Remove(this);
			_faction = null;
			_region.Persons.Remove(this);
			_region = null;
		}

		public bool Killed 
		{
			get { return (Region == null); }
		}

		public Attitude AttitudeTo(Person p) 
		{
			if (p == this)
				return Attitude.Ally;
			if (Man.IsMonster && (p.Man.IsMonster || !p.Faction.IsNPC))
				return Attitude.Hostile;
			if (Faction.IsNPC) 
			{
				if (Insanity >= Constants.DangerousInsanity && !p.Faction.IsNPC)
					return Attitude.Hostile;
				else
					return Attitude.Friendly;
			}

			if (p.Faction == this.Faction)
				return Attitude.Ally;
			if (p.HideFaction || !Faction.Attitudes.ContainsKey(p.Faction.Num)) 
				return Faction.DefaultAttitude;
			else 
				return Faction.Attitudes[p.Faction.Num];
		}

		public void AddEvent(string msg) 
		{
			msg = this.ToString(Lang.En) + ": " + msg.Replace("|", "|" + 
				this.ToString(Lang.Ru) + ": ");
			Faction.Events.Add(msg);
		}

		public void AddItems(ItemType it, int amount) 
		{
			Items.AddItems(it, amount);
			if (Faction != null)
				Faction.ShowItem(it);
		}
		
		public Skill AddSkill(SkillType st) 
		{
			// Get existing skill of this type
			Skill sk = Skills.GetByType(st);

			// If none, create new
			if (sk == null) 
			{
				sk = new Skill(st);
				Skills.Add(sk);
			}

			// Show in report
			Faction.ShowSkill(st, false);

			// If maximum level was set as default, add derived skills
			if (sk.Level >= 100) 
			{
				foreach (SkillType der in SkillType.List) 
				{
					if (der.BasedOn != sk.Type || Skills.GetByType(der) != null)
						continue;
					this.AddSkill(der);
				}
			}

			return sk;
		}

		public int RadiationModifier() 
		{
      int mod = 0;
			int lower = 0;
			foreach (Item itm in Items)
				if (itm.Type.OwnerRadiation > 0)
					// Use all items raising radiation
					mod += itm.Type.OwnerRadiation * itm.Amount;
				else if (itm.Type.OwnerRadiation < lower)
					// And only one lowering radiation
					lower = itm.Type.OwnerRadiation;
			mod += lower;
			
			if (Building != null && Building.IsPersonInside(this))
				mod += Building.Type.Radiation;
			return mod;
		}

		public int RadiationDanger(bool for_next_turn) 
		{
			if (for_next_turn) Map.Turn++;
			int radiation = Region.Radiation;
			if (for_next_turn) Map.Turn--;

			radiation += RadiationModifier();
			if (radiation <= 0)
				return 0;
			
			ItemType man = Man;
			if (man.RadiationTo < radiation)
				return radiation - man.RadiationTo;
			if (man.RadiationFrom > radiation)
				return radiation - man.RadiationFrom;
			else
				return 0;
		}

		public int TemperatureDanger(bool for_next_turn) 
		{
			if (for_next_turn) Map.Turn++;
			int tempr = Region.Temperature;
			if (for_next_turn) Map.Turn--;

			int mod = 0;
			foreach (Item itm in Items)
				if (itm.Type.OwnerTemperature < 0)
					// Use all items lowering temperature
					tempr += itm.Type.OwnerTemperature * itm.Amount;
				else if (itm.Type.OwnerTemperature > mod)
					// And only one raising
					mod = itm.Type.OwnerTemperature;
			tempr += mod;

			if (Building != null && Building.IsPersonInside(this))
				tempr += Building.Type.Temperature;
      return Math.Max(0, -tempr);
		}

		public void PromoteSkill(SkillType st) 
		{
			PromoteSkill(st, false);
		}

		public void PromoteSkill(SkillType st, bool combat) 
		{
			if (Leader != null)
				throw new Exception("Should be given to leader");
			Skill sk = Skills.GetByType(st);
			if (sk == null)
				throw new Exception("Leader has no such skill");
			if (this.Man.NoLearn)
				return;

			if (Faction != null)
				Faction.ShowSkill(st);

			int xp = Constants.LeaderExpirience;
			if (!combat || !this.ReceivedCombatXP)
				PromoteSkill(this, st, ref xp, 100);
			if (combat)
				this.ReceivedCombatXP = true;
			foreach (Person sub in Team) 
			{
				if (!CanPromoteSkill(sub, st) || (combat && sub.ReceivedCombatXP))
					continue;
				if (combat)
					sub.ReceivedCombatXP = true;
				xp = Constants.StudentExpirience;
				PromoteSkill(sub, st, ref xp, sk.Level);
			}
		}

		private static bool CanPromoteSkill(Person p, SkillType st) 
		{
			if (p.Man.NoLearn)
				return false;

      // Add prerequisite skills if none
			Skill sk = p.Skills.GetByType(st);
			if (sk != null)
				return true;
			else if (st.BasedOn != null && CanPromoteSkill(p, st.BasedOn)) 
			{
				p.Skills.Add(new Skill(st));
				return true;
			}
			else
				return false;
		}

		private static void PromoteSkill(Person p, SkillType st, ref int xp, int max) 
		{
			Skill sk = p.Skills.GetByType(st);
			if (sk.Level < max) 
			{
				Skill base_sk = p.Skills.GetByType(st.BasedOn);
			
				int promote = 0;
				while (true) 
				{
					promote = Math.Min(xp, max - sk.Level);
					if (base_sk != null && base_sk.Level < sk.Level + promote)
						PromoteSkill(p, base_sk.Type, ref xp, base_sk.Level + 1);
					else
						break;
				}

				sk.Level += promote;
				xp -= promote;
			}

			// Add skills based on this, if maximum level reached
			if (sk.Level >= 100) 
			{
				foreach (SkillType st2 in SkillType.List) 
				{
					if (st2.BasedOn != st || p.Skills.GetByType(st2) != null)
						continue;
					p.AddSkill(st2);
				}
			}
		}


	}

	#region Lists

	class PersonArrayList : ArrayList 
	{
		public new Person this[int index] 
		{
			get { return (Person)base[index]; }
			set { base[index] = value; }
		}

		public Person GetByNumber(int num) 
		{
			int i = Count-1;
			while (i >= 0 && this[i].Num != num) i--;
			if (i >= 0)
				return this[i];
			else
				return null;
		}

		public new PersonArrayList Clone() 
		{
			PersonArrayList res = new PersonArrayList();
			foreach (Person p in this)
				res.Add(p);
			return res;
		}
	}

	#endregion
}
