using System;
using System.Collections;

namespace Wasteland
{
	public class Person : ILocalized
	{
		private static int _maxNum = 0;
		private Faction _faction = null;
		private Region _region = null;
		private Building _building = null;
		private Person _leader = null;
		private int _insanity = 0;

		public static PersonList List = new PersonList();
		public string Name = "";
		public string Description = "";
		public int Num = 0;
		public bool Chosen = false;
		public bool Patrolling = false;
		public bool Avoiding = false;
		public bool Greedy = false;
		public bool HideFaction = false;
		public bool Hide = false;
		public int BabyTimer = 0;
		public SkillType FatherSkill = null;
		public string FatherName = null;
		public TradeOrder TradeOrder = null;
		public bool HadBattle = false;
		public int Age = 100;
		public Region DiedIn = null;

		public ItemList Items = new ItemList();
		public SkillList Skills = new SkillList();
		public ArrayList Orders = new ArrayList();
		public PersonList Team = new PersonList();
		public ArrayList RepeatingLines = new ArrayList();
		public ItemTypeList Consume = new ItemTypeList();
		public ItemTypeList Burn = new ItemTypeList();
		public ItemTypeList Equipment = new ItemTypeList();
		public ItemTypeList Spoils = new ItemTypeList();

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
					Greedy = false;
					Hide = false;
					HideFaction = false;
					Orders.Clear();
					RepeatingLines.Clear();
					Consume.Clear();
					Burn.Clear();
					Equipment.Clear();
					Spoils.Clear();
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

		public Person GetTeamLeader() 
		{
			if (this.Leader != null)
				return this.Leader;
			else
				return this;
		}

		public PersonList GetTeamAndLeader() 
		{
			PersonList list = new PersonList();
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

		public SkillTypeList GetTalents()
		{
			SkillTypeList talents = new SkillTypeList();
			foreach ( Skill skill in this.Skills )
			{
				if ( skill.Type.BasedOn == null )
				{
					talents.Add(skill.Type);
				}
			}
			return talents;
		}

		public SkillType GetRandomTalent()
		{
			SkillTypeList talents = this.GetTalents();
			if ( talents.Count > 0 )
			{
				return talents[Constants.Random(talents.Count)];
			}
			return null;
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

		public Order GetTurnOrder() 
		{
			int i = Orders.Count-1;
			while (i >= 0 && !((Order)Orders[i]).IsMonthlong)
				i--;
			if (i >= 0)
				return (Order)Orders[i];
			else
				return null;
		}

		public void InheritTurnOrder(Person leader) 
		{
			// Take full-turn order of dead leader if has no full-turn order of his own
			Order leader_order = leader.GetTurnOrder();
			if (leader_order == null || this.GetTurnOrder() != null)
				return;
      this.Orders.Add(leader_order);
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

			foreach (Person p in Team) 
				p.InheritTurnOrder(this);
			Leader = null;
			foreach (Person p in Team) 
				p.Leader = null;
			Building = null;
			_faction.Persons.Remove(this);
			_faction = null;
			DiedIn = _region;
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

		public void AddEvent(Event.Code code) 
		{
			Faction.Events.Add(new Event(this, "", code, new object[0]));
		}

		public void AddEvent(Event.Code code, object param1) 
		{
			Faction.Events.Add(new Event(this, "", code, new object[1] { param1 }));
		}

		public void AddEvent(Event.Code code, object[] parameters) 
		{
			Faction.Events.Add(new Event(this, "", code, parameters));
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
					if (der.BasedOn != sk.Type || Skills.GetByType(der) != null || der.Special)
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
			// Check if this person is leader and have given skill
			if (Leader != null)
				throw new Exception("Should be given to leader");
			Skill sk = Skills.GetByType(st);
			if (sk == null)
				throw new Exception("Leader has no such skill");

			// Promote skill for this person and his team
			foreach (Person p in this.GetTeamAndLeader()) 
			{
				// Check if person can promote this skill
				if (p.Man.NoLearn
					|| (p.Insanity >= Constants.DangerousInsanity)
					|| (combat && p.HadBattle)
					|| (combat && p.Avoiding)
					|| !CanPromoteSkill(p, st))	
					continue;

				if (p.Leader == null) 
				{
					int xp = Constants.LeaderExpirience;
					PromoteSkill(p, st, ref xp, 100);
				}
				else 
				{
					int xp = Constants.StudentExpirience;
					SkillType promoted_st = st;
					while (xp > 0 && promoted_st != null) 
					{
						PromoteSkill(p, promoted_st, ref xp, p.Leader.Skills.GetByType(promoted_st).Level);
						promoted_st = promoted_st.BasedOn;
					}
				}
			}

			// Show skill
			Faction.ShowSkill(st);
		}

		private static bool CanPromoteSkill(Person p, SkillType st) 
		{
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
				Skill base_sk = null;
				if (st.BasedOn != null) 
				{
					base_sk = p.Skills.GetByType(st.BasedOn);
					if (base_sk == null) 
						base_sk = p.AddSkill(st.BasedOn);
				}
			
				int promote = 0;
				while (true) 
				{
					promote = Math.Min(xp, max - sk.Level);
					if (promote > 0 && base_sk != null && base_sk.Level < sk.Level + promote)
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
					if (st2.BasedOn != st || p.Skills.GetByType(st2) != null || st2.Special)
						continue;
					p.AddSkill(st2);
				}
			}
		}

		public ItemTypeList GetConsumeList() 
		{
			ItemTypeList food = new ItemTypeList();

			// First, add CONSUME list
			foreach (ItemType it in Consume)
				if (it.Rations > 0)
					food.Add(it);

			// Then, add all other items person can eat
			foreach (ItemType it in Man.Food)
				if (it.Rations > 0 && !food.Contains(it))
					food.Add(it);

			return food;
		}

		public ItemTypeList GetBurnList() 
		{
			ItemTypeList burn = new ItemTypeList();

			// First, add BURN list
			foreach (ItemType it in Burn)
				if (it.Burn)
					burn.Add(it);

			// Then, add all other items
			foreach (ItemType it in ItemType.List)
				if (it.Burn && !burn.Contains(it))
					burn.Add(it);

			return burn;
		}

		/*public static IEnumerable AlivePersons 
		{
			get { return new AlivePersonsHolder(); }
		}

		private class AlivePersonsHolder : IEnumerable
		{
			public IEnumerator GetEnumerator() 
			{
				return new AlivePersonsEnumerator();
			}
		}

		private class AlivePersonsEnumerator : IEnumerator	
		{
			private int _index = 0;

			public object Current 
			{
				get 
				{
					if (_index >= Person.List.Count)
						return null;
					if (Person.List[_index].Killed && !MoveNext()) 
						return null;
					return Person.List[_index]; 
				}
			}

			public void Reset() 
			{
				_index = 0;
			}

			public bool MoveNext() 
			{
				do 
				{
					_index++;
				} 
				while (_index < Person.List.Count && Person.List[_index].Killed);

				return (_index >= Person.List.Count);
			}
		}*/
	}

	#region Lists

	public class PersonList : ArrayList 
	{
		public new Person this[int index] 
		{
			get { return (Person)base[index]; }
			set { base[index] = value; }
		}

		public override int Add (object value) 
		{
			throw new NotSupportedException();
		}

		public int Add(Person item) 
		{
			return base.Add(item);
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

		public new PersonList Clone() 
		{
			PersonList res = new PersonList();
			foreach (Person p in this)
				res.Add(p);
			return res;
		}
	}

	#endregion
}
