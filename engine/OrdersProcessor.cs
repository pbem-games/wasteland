using System;
using System.Collections;

namespace Wasteland 
{
  public class OrdersProcessor 
  {
    public delegate void PersonOrderDelegate(Person person, Order order);
    
    private class ProduceRequestItem 
    {
      public Person Person;
      public ItemType ItemType;
      public int Amount;
			public int Priority;
    }

    private static ArrayList ProduceRequests = null;
    private static ArrayList ScavengeRequests = null;

    public static void Process() 
    {
			Map.CalcTurnResources();

      DoForAllPersons(new PersonOrderDelegate( DoAddress  ), "ADDRESS", typeof(AddressOrder));
			DoForAllPersons(new PersonOrderDelegate( DoAvoid    ), "AVOID", typeof(AvoidOrder));
			DoForAllPersons(new PersonOrderDelegate( DoEquipment), "EQUIPMENT", typeof(EquipmentOrder));
      DoForAllPersons(new PersonOrderDelegate( DoBurn     ), "BURN", typeof(BurnOrder));
      DoForAllPersons(new PersonOrderDelegate( DoConsume  ), "CONSUME", typeof(ConsumeOrder));
			DoForAllPersons(new PersonOrderDelegate( DoDescribe ), "DESCRIBE", typeof(DescribeOrder));
			DoForAllPersons(new PersonOrderDelegate( DoGreedy   ), "GREEDY", typeof(GreedyOrder));
			DoForAllPersons(new PersonOrderDelegate( DoName     ), "NAME", typeof(NameOrder));
      DoForAllPersons(new PersonOrderDelegate( DoOption   ), "OPTION", typeof(OptionOrder));
      DoForAllPersons(new PersonOrderDelegate( DoPassword ), "PASSWORD", typeof(PasswordOrder));
      DoForAllPersons(new PersonOrderDelegate( DoQuit     ), "QUIT", typeof(QuitOrder));
      DoForAllPersons(new PersonOrderDelegate( DoShow     ), "SHOW", typeof(ShowOrder));
			DoForAllPersons(new PersonOrderDelegate( DoSpoils		), "SPOILS", typeof(SpoilsOrder));
      
      DoForAllPersons(new PersonOrderDelegate( DoAttack   ), "ATTACK", typeof(AttackOrder));
      ResolveHostileAttacks();

      DoForAllPersons(new PersonOrderDelegate( DoDeclare  ), "DECLARE", typeof(DeclareOrder));
			DoForAllPersons(new PersonOrderDelegate( DoTeam     ), "TEAM", typeof(TeamOrder));
			DoForAllPersons(new PersonOrderDelegate( DoLeave    ), "LEAVE", typeof(LeaveOrder));
			DoForAllPersons(new PersonOrderDelegate( DoEnter    ), "ENTER", typeof(EnterOrder));
			DoForAllPersons(new PersonOrderDelegate( DoEvict    ), "EVICT", typeof(EvictOrder));
			DoForAllPersons(new PersonOrderDelegate( DoHide     ), "HIDE", typeof(HideOrder));
			DoForAllPersons(new PersonOrderDelegate( DoKick     ), "KICK", typeof(KickOrder));
			DoForAllPersons(new PersonOrderDelegate( DoPromote  ), "PROMOTE", typeof(PromoteOrder));
      
      DoForAllPersons(new PersonOrderDelegate( DoTrade    ), "TRADE", typeof(TradeOrder));

      DoForAllPersons(new PersonOrderDelegate( DoGive     ), "GIVE", typeof(GiveOrder));
      
      DoForAllPersons(new PersonOrderDelegate( DoBuild    ), "BUILD", typeof(BuildOrder));
			DoForAllPersons(new PersonOrderDelegate( DoCure     ), "CURE", typeof(CureOrder));
			DoForAllPersons(new PersonOrderDelegate( DoInstall  ), "INSTALL", typeof(InstallOrder));
      RemovePatrolFlags();
			DoForAllPersons(new PersonOrderDelegate( DoPatrol   ), "PATROL", typeof(PatrolOrder));
      DoForAllPersons(new PersonOrderDelegate( DoMove     ), "MOVE", typeof(MoveOrder));
      MoveWanderers();
      CreateNewMonsters();
      DoForAllPersons(new PersonOrderDelegate( DoDrive    ), "DRIVE", typeof(DriveOrder));
			
			ProduceRequests = new ArrayList();
      DoForAllPersons(new PersonOrderDelegate( DoProduce  ), "PRODUCE", typeof(ProduceOrder));
			ResolveProduceRequests();
			
			ScavengeRequests = new ArrayList();
			DoForAllPersons(new PersonOrderDelegate( DoScavenge ), "SCAVENGE", typeof(ScavengeOrder));
			ResolveScavengeRequests();

			DoForAllPersons(new PersonOrderDelegate( DoUninstall), "UNINSTALL", typeof(UninstallOrder));

			Map.CalcTurnJunk();
			ResolveMaintenance();
      ResolveRadiation();
      ResolveTemperature();

      NPCMakeFriends();
			ResolveBabies();
			FindTheGreatest();
			RaiseTeamsInsanity();
      LowerWanderersInsanity();
      ResolveInsaneRenegades();
			ClearMonstersAvoid();
      DismissFactionsWithoutChosen();
    }

    private static void DoForAllPersons(PersonOrderDelegate dlg, string order_name, Type type) 
    {
			Console.WriteLine(".." + order_name);
      foreach (Person p in Person.List) 
      {
        if (p.Killed)
          continue;
				int i = 0;
        while (i < p.Orders.Count)
        {
					Order ord = (Order)p.Orders[i];
          if (ord.GetType() == type)
            try 
            {
              dlg(p, ord);
            }
            catch (ProcessException ex) 
            {
							if (p.Faction != null)
								p.Faction.Events.Add(new Event(p, order_name, ex.Code, ex.Params));
            }
					i++;
        }
      }
    }

    private static void DoAddress(Person p, Order order) 
    {
      p.Faction.Email = ((AddressOrder)order).Email;
      p.Faction.Events.Add(new Event(null, "", Event.Code.Address, 
				new object[1] { p.Faction.Email } ));
    }

    private static void DoAttack(Person p, Order order) 
    {
      if (p.Leader != null)
        throw new ProcessException(Event.Code.OrderForLeader);
      AttackOrder ord = (AttackOrder)order;
			if (ord.PersonNum == -1) // order was removed in previous battle
				return;
      Person target = p.Region.Persons.GetByNumber(ord.PersonNum);
      if (target == null)
        throw new ProcessException(Event.Code.NotInRegion, ord.PersonNum);
      if (target.Faction == p.Faction)
        throw new ProcessException(Event.Code.CantAttackSameFaction);
      if ((target.Leader != null && target.Leader == p.Leader) 
        || target.Leader == p || p.Leader == target)
        throw new ProcessException(Event.Code.CantAttackSameTeam);

			PersonList attackers = new PersonList();
			attackers.Add(p);

			// Look for other leaders in region with same target
			foreach (Person p2 in p.Region.Persons) 
			{
				if (p2 == p || p2.AttitudeTo(p) < Attitude.Ally 
					|| p2.Leader != null)
					continue;
				AttackOrder attack_order = null;
				foreach (Order ord2 in p2.Orders)
					if (ord2.GetType() == typeof(AttackOrder) 
						&& ((AttackOrder)ord2).PersonNum == ord.PersonNum) 
					{
						attack_order = (AttackOrder)ord2;
						break;
					}
				if (attack_order == null)
					continue;
				if (p2.Faction == target.Faction || 
					(target.Leader != null && target.Leader == p2.Leader)
					|| target.Leader == p || p.Leader == target)
					continue;

				attackers.Add(p2);
			}
      
      Battle battle = new Battle(attackers, target, target.Region);
    }

    private static void DoAvoid(Person p, Order order) 
    {
      p.Avoiding = ((AvoidOrder)order).Flag;
    }

    private static void DoBuild(Person p, Order order) 
    {
      if (p.Leader != null) 
        throw new ProcessException(Event.Code.OrderForLeader);

			BuildingType bt = ((BuildOrder)order).What;
			if (bt.Materials.Count == 0 || bt.NoBuild) 
				throw new ProcessException(Event.Code.CantBuildObject);
			
			// Attempt to create the building and install all items
			p.Building = new Building(bt, p.Region);
			try 
			{
				p.Building.Name = bt.FullNameRu.Substring(0, 1).ToUpper() +
					bt.FullNameRu.Substring(1);

				InstallOrder ord = new InstallOrder();
				ItemList needs = p.Building.GetNeeds();
				int i = 0;
				while (i < needs.Count 
					&& needs[i].Type.InstallSkill.Type == needs[0].Type.InstallSkill.Type) 
				{
					ord.What.Add(needs[i].Type);
					i++;
				}
				DoInstall(p, ord);
      
				p.Faction.ShowBuilding(bt);
			}
			catch (ProcessException) 
			{
				// If no materials was installed, destroy building
				if (p.Building.Installed.Count == 0) 
					p.Building.Collapse();
				throw;
			}
    }

    private static void DoConsume(Person p, Order order) 
    {
      p.Consume = ((ItemTypeListOrder)order).What;
    }

    private static void DoBurn(Person p, Order order) 
    {
      p.Burn = ((ItemTypeListOrder)order).What;
    }

    private static void DoCure(Person p, Order order) 
    {
      if (p.Leader != null) 
        throw new ProcessException(Event.Code.OrderForLeader);
      
      SkillType stCure = Constants.CureSkill;
      if (p.Skills.GetByType(stCure) == null)
        throw new ProcessException(Event.Code.NoSkill);
      
      int points = p.GetTeamLevel(stCure) / Constants.LevelToCure;

      bool first = true;

      foreach (int num in ((CureOrder)order).PatientNums) 
      {
        if (points <= 0) 
          throw new ProcessException(Event.Code.NotSkilledEnough);

        Person patient = p.Region.Persons.GetByNumber(num);
				if (patient == null) 
				{
					p.AddEvent(Event.Code.NotInRegion, num);
					continue;
				}

				if (patient.AttitudeTo(p) < Attitude.Friendly) 
				{
					p.AddEvent(Event.Code.TargetNotFriendly, patient);
					continue;
				}

        ItemType man = patient.Man;
        if (man == null)
          continue;

				// This should be after medicine check, but Moscow game was badly generated
				// so there was no medicine available to promote Doctor
				if (first)
					p.PromoteSkill(stCure);
				
				ItemType medicine = man.Medicine;
        if (medicine != null) 
        {
          // Spend one pill
          int having = p.GetTeamAmount(medicine);
          if (having == 0) 
            throw new ProcessException(Event.Code.OutOfDrugs);
          p.SpendFromTeam(medicine, 1);
        }

        // Do cure
        points--;
        patient.Insanity--;
        if (patient.Faction != p.Faction && patient.Faction != null)
          patient.AddEvent(Event.Code.CuredBy, p);
        if (medicine == null)
          p.AddEvent(Event.Code.Cures, patient);
        else
					p.AddEvent(Event.Code.CuresUsing, new object[2] { patient, medicine });

        first = false;
      }
    }

    private static void DoDeclare(Person p, Order order) 
    {
      DeclareOrder ord = (DeclareOrder)order;
      Faction f = Faction.Get(ord.FactionNum);
      if (f == null) 
        throw new ProcessException(Event.Code.NoSuchFaction);

      if (f == p.Faction)
        p.Faction.DefaultAttitude = ord.Attitude;
      else
        p.Faction.Attitudes[f.Num] = ord.Attitude;
    }

		private static void DoDescribe(Person p, Order order) 
		{
			DescribeOrder ord = (DescribeOrder)order;
			if (ord.What == DescribeOrder.Target.Person)
				p.Description = ord.Description;
			else if (ord.What == DescribeOrder.Target.Object) 
			{
				if (p.Building == null) 
					throw new ProcessException(Event.Code.NotInside);

				if (p.Building.Persons[0] != p) 
					throw new ProcessException(Event.Code.MustOwnObject);

				p.Building.Description = ord.Description;
			}
		}
		
		private static void DoDrive(Person p, Order order) 
    {
      if (p.Leader != null)
        throw new ProcessException(Event.Code.OrderForLeader);

      DriveOrder ord = (DriveOrder)order;

      if (p.Building == null) 
        throw new ProcessException(Event.Code.NotInside);

      if (p.Building.Type.Speed == 0) 
        throw new ProcessException(Event.Code.ObjectCantMove);

			if (p.Building.Persons[0] != p)
				throw new ProcessException(Event.Code.MustOwnObject);

			if (p.Building.GetNeeds().Count > 0) 
        throw new ProcessException(Event.Code.ObjectIncomplete);

      // Calculate weight inside
      int weight = 0;
      foreach (Person p2 in p.Building.Persons) 
        weight += p2.GetWeight();
      if (weight > p.Building.Type.Capacity) 
        throw new ProcessException(Event.Code.VehicleOverloaded);

      // Calculate team driving points
      if (p.Building.Type.DriveSkill != null) 
      {
        if (p.Skills.GetByType(p.Building.Type.DriveSkill.Type) == null)
          throw new ProcessException(Event.Code.NoSkill);

        int drive_level = 0;
        foreach (Person sub in p.GetTeamAndLeader()) 
        {
          Skill sk = sub.Skills.GetByType(p.Building.Type.DriveSkill.Type);
          if (sk != null)
            drive_level += sk.Level;
        }
        if (drive_level < p.Building.Type.DriveSkill.Level) 
          throw new ProcessException(Event.Code.NotSkilledEnough);
      }

      int points = p.Building.Type.Speed;
      bool first = true;

      foreach (object d in ord.Directions) 
      {
        Region dest = p.Region.RegionInDir((Direction)d);
        if (dest == null || 
					(!dest.Terrain.Vehicles && p.Building.Type.DriveTerrain != dest.Terrain)) 
          throw new ProcessException(Event.Code.CantDriveThere);

        if (points < dest.PointsToEnter()) 
          throw new ProcessException(Event.Code.NotEnoughMP);

				// Check if radiation is absolutely lethal
				if (dest.Radiation + p.RadiationModifier() >= p.Man.RadiationTo + 100)
					throw new ProcessException(Event.Code.RadiationTooHighToEnter, dest);
				
				CheckPatrolBlock(p, dest, ord.Attack);

        ItemType fuel = p.Building.Type.Fuel;
        if (fuel != null) 
        {
          int amt = p.GetTeamAmount(fuel);
          if (amt == 0)
            throw new ProcessException(Event.Code.NoFuel);
          p.SpendFromTeam(fuel, 1);
        }

        points -= dest.PointsToEnter();

				if (fuel != null)
					p.AddEvent(Event.Code.DrivesSpending, new object[3] { p.Region, dest, fuel });
				else
					p.AddEvent(Event.Code.Drives, new object[2] { p.Region, dest });

        if (first && p.Building.Type.DriveSkill != null)
          p.PromoteSkill(p.Building.Type.DriveSkill.Type);
        first = false;
        
        // Distribute message to all team leaders in building
        foreach (Person p2 in p.Building.Persons) 
        {
          if (p2.Leader == null && p2 != p)
						p2.AddEvent(Event.Code.PersonDrives, new object[3] { p, p.Region, dest });
        }
        
        p.Building.Region = dest;
      }
    }

    private static void DoEnter(Person p, Order order) 
    {
      if (p.Leader != null)
        throw new ProcessException(Event.Code.OrderForLeader);

      int num = ((EnterOrder)order).BuildingNum;
      Building b = p.Region.Buildings.GetByNumber(num);
      if (b == null) 
        throw new ProcessException(Event.Code.NotInRegion, num);

			if (b.Persons.Count > 0) 
			{
				Person owner = b.Persons[0];
				if (owner.AttitudeTo(p) < Attitude.Friendly)
					throw new ProcessException(Event.Code.ObjectOwnerNotFriendly);
			}

			if (p.Building == b)
        return;

      p.AddEvent(Event.Code.Enters, b);
      p.Building = b;
    }

		private static void DoEvict(Person p, Order order) 
		{
			if (p.Building == null)
				throw new ProcessException(Event.Code.NotInside);
			if (p.Building.Persons[0] != p)
				throw new ProcessException(Event.Code.MustOwnObject);
			
			int num = ((EvictOrder)order).PersonNum;
			Person target = p.Building.Persons.GetByNumber(num);
			if (target == null)
				throw new ProcessException(Event.Code.NotInObject, num);
			if (target.Leader != null)
				throw new ProcessException(Event.Code.EvictLeader);
			
			target.Building = null;
			
			p.AddEvent(Event.Code.Evicted, target);
			if (target.Faction != p.Faction)
				target.AddEvent(Event.Code.EvictedBy);
		}

    private static void DoEquipment(Person p, Order order) 
    {
      p.Equipment = ((ItemTypeListOrder)order).What;
    }

    private static void DoGive(Person p, Order order) 
    {
      GiveOrder ord = (GiveOrder)order;

			Item what = p.Items.GetByType(ord.What);
			if (what == null) 
				throw new ProcessException(Event.Code.NoSuchItem);
			if (ord.Amount == -1)
				ord.Amount = what.Amount;
			
			Person target = null;
      bool hired = false;
			bool hire_attempt = false;
      if (ord.Target != 0) 
      {
        target = p.Region.Persons.GetByNumber(ord.Target);
        if (target == null) 
          throw new ProcessException(Event.Code.NotInRegion, ord.Target);

        // Check hire for chosen one
        if (target != null && target.Faction != p.Faction && p.Chosen
					&& !target.Man.IsMonster)
        {
          if (target.Man.Food.Contains(ord.What)) 
          {
						int tohire = target.GetHireAmount();
						if (tohire > 0 && tohire <= ord.Amount) 
						{
							// Person is joined faction!
							if (target.Faction.Num != Constants.NPCFactionNum)
								target.Faction.Events.Add(new Event(null, "", Event.Code.Rehired,
									new object[2] { target, p } ));
							target.Faction = p.Faction;
							target.Leader = null;
							hired = true;

							// Add to recipient only up to month maintenance, discard all rest
							ord.Amount = Math.Min(ord.Amount, Constants.RationsPerMonth / ord.What.Rations);

							// Show items and skills
							foreach (Item itm in target.Items)
								p.Faction.ShowItem(itm.Type);
							foreach (Skill sk in target.Skills)
								p.Faction.ShowSkill(sk.Type);
						}
						else 
						{
							hire_attempt = true;
						}
          }
        }
        
        // Check attitude
        if (!hired) 
        {
					if (target.AttitudeTo(p) < Attitude.Friendly) 
					{
						target.AddEvent(Event.Code.RefusesReceive, 
							new object[2] { new Item(ord.What, ord.Amount), p });

						if (!hire_attempt)
							throw new ProcessException(Event.Code.TargetNotFriendly, target);
						else
							throw new ProcessException(Event.Code.NotFriendlyHireFail, target);
					}
        }
      }

      int amt = ord.Amount;
      if (amt > what.Amount) 
        amt = what.Amount;

      // Remove items from giver
      p.Items.RemoveItems(ord.What, amt);
			if (ord.Target != 0) 
			{
				if (!hire_attempt)
					p.AddEvent(Event.Code.Gives, new object[2] { new Item(ord.What, ord.Amount), target } );
				else
					p.AddEvent(Event.Code.GivesNoHire, new object[2] { new Item(ord.What, ord.Amount), target } );
			}
			else
				p.AddEvent(Event.Code.Discards, new Item(ord.What, amt));

      // Add items to recipient
      if (ord.Target != 0) 
      {
        target.Items.AddItems(ord.What, amt);
        if (target.Faction != p.Faction)
					target.AddEvent(Event.Code.Receives, 
						new object[2] { new Item(ord.What, amt), p } );
				target.Faction.ShowItem(ord.What);
      }
			else
				p.Region.Junk.AddItems(ord.What, amt);

      if (hired)
        p.AddEvent(Event.Code.JoinedFaction, target);
    }

		private static void DoGreedy(Person p, Order order) 
		{
			p.Greedy = ((GreedyOrder)order).Flag;
		}

		private static void DoHide(Person p, Order order) 
    {
      HideOrder ord = ((HideOrder)order);
      if (ord.Variant == HideOrder.Variants.Person) 
      {
        if (p.Leader != null || p.Team.Count > 0)
          throw new ProcessException(Event.Code.CantHideInTeam);

        if (p.Building != null) 
          throw new ProcessException(Event.Code.CantHideInObject);
      }
      p.Hide = (ord.Variant == HideOrder.Variants.Person);
      p.HideFaction = (ord.Variant == HideOrder.Variants.Faction);
    }

    private static void DoInstall(Person p, Order order) 
    {
			InstallOrder ord = (InstallOrder)order;
      if (p.Leader != null) 
        throw new ProcessException(Event.Code.OrderForLeader);
      if (p.Building == null) 
        throw new ProcessException(Event.Code.NotInside);
			if (ord.What.Count == 0)
				throw new ProcessException(Event.Code.NoParameters);

			// Get install skill of first item in list
			Skill sk = ord.What[0].InstallSkill;
			if (sk == null)
				throw new ProcessException(Event.Code.CantInstall);

			// Get available points of the skill
			if (p.Skills.GetByType(sk.Type) == null) 
				throw new ProcessException(Event.Code.NoSkill);
			int points = p.GetTeamLevel(sk.Type);

			bool first = true;
			foreach (ItemType what in ((InstallOrder)order).What) 
			{
				Item material = p.Building.GetNeeds().GetByType(what);
				if (material == null) 
				{
					// Look in optional
					material = p.Building.Type.OptionalMaterials.GetByType(what);
					if (material == null)
						throw new ProcessException(Event.Code.CantInstallInObject);

					// Check if other optional material installed
					foreach (Item itm in p.Building.Type.OptionalMaterials)
						if (itm != material && p.Building.Installed.GetByType(itm.Type) != null)
							throw new ProcessException(Event.Code.AnotherOptionInstalled);

					// Check if material of this type already installed
					Item installed = p.Building.Installed.GetByType(what);
					if (installed != null) 
					{
						if (installed.Amount >= material.Amount)
							throw new ProcessException(Event.Code.MaxAmountInstalled);
						material = new Item(material.Type, material.Amount - installed.Amount);
					}
				}

				// Check if material has same install skill with first
				if (material.Type.InstallSkill.Type != sk.Type)
					throw new ProcessException(Event.Code.DifferentInstallSkill);

				// Check material amount
				int having = p.GetTeamAmount(material.Type);
				if (having == 0) 
					throw new ProcessException(Event.Code.NoItemToInstall, material);

				// Promote skill
				if (first) 
				{
					p.PromoteSkill(sk.Type);
					first = false;
				}
    
				// Check if points available
				if (material.Type.InstallSkill.Level > points)
					throw new ProcessException(Event.Code.NoSkillToInstall, material);

				int amt = Math.Min(material.Amount, Math.Min(having, 
					points / material.Type.InstallSkill.Level));

				// Relocate materials
				p.SpendFromTeam(material.Type, amt);
				p.Building.Installed.AddItems(material.Type, amt);
				
				// Spend skill points
				points -= amt * sk.Level;

				p.AddEvent(Event.Code.Installs, new object[2] {p.Building, new Item(what, amt)});
			}
    }

    private static void DoKick(Person p, Order ord) 
    {
      if (p.Chosen)
        throw new ProcessException(Event.Code.CantKickChosen);
      p.AddEvent(Event.Code.Kicked, p);
      p.Leader = null;
      p.Faction = Faction.Get(Constants.NPCFactionNum);
    }

    private static void DoLeave(Person p, Order ord) 
    {
      if (p.Leader != null) 
				throw new ProcessException(Event.Code.OrderForLeader);
      if (p.Building == null) 
				throw new ProcessException(Event.Code.NotInside);

			p.AddEvent(Event.Code.Leaves, p.Building);
      p.Building = null;
    }

    private static void DoMove(Person p, Order order) 
    {
			if (p.Leader != null) 
				throw new ProcessException(Event.Code.OrderForLeader);

      MoveOrder ord = (MoveOrder)order;
      
      Movement mv = Movement.Walk;
      int points = 0;
      bool points_calculated = false;

      foreach (object d in ord.Directions) 
      {
        if (d.GetType() == typeof(Direction)) 
        {
          Region dest = p.Region.RegionInDir((Direction)d);
          if (dest == null || !dest.Terrain.Walking) 
            throw new ProcessException(Event.Code.CantMoveThere);

          #region Calculate remaining movement for first move
          if (!points_calculated) 
          {
            // Determine team speed
            mv = Movement.Ride;
            foreach (Person sub in p.GetTeamAndLeader()) 
            {
              while (mv >= Movement.Walk && sub.GetWeight() > sub.GetCapacity(mv)) mv--;
              if (mv < Movement.Walk) 
								throw new ProcessException(Event.Code.PersonOverloaded);
            }

            // Determine movement points
            points = ((int)mv + 1) * 2;
            points_calculated = true;
          }
          #endregion

          if (points < dest.PointsToEnter()) 
            throw new ProcessException(Event.Code.NotEnoughMP);

					if (p.Building != null) 
					{
						p.AddEvent(Event.Code.Leaves, p.Building);
						p.Building = null;
					}

					// Check if radiation is absolutely lethal
					if (dest.Radiation + p.RadiationModifier() >= p.Man.RadiationTo + 100)
            throw new ProcessException(Event.Code.RadiationTooHighToEnter, dest);

          CheckPatrolBlock(p, dest, ord.Attack);

          points -= dest.PointsToEnter();          
					p.AddEvent(Event.Code.Moves, new object[2] {p.Region, dest});
          p.Region = dest;
        }
        else 
        {
          if ((int)d == 0) 
          {
            if (p.Building == null) 
              throw new ProcessException(Event.Code.NotInside);

            p.AddEvent(Event.Code.Leaves, p.Building);
            p.Building = null;
          }
          else 
          {
            Building b = p.Region.Buildings.GetByNumber((int)d);
            if (b == null) 
              throw new ProcessException(Event.Code.NotInRegion, d);

						if (b.Persons.Count > 0) 
						{
							Person owner = b.Persons[0];
							if (owner.AttitudeTo(p) < Attitude.Friendly)
								throw new ProcessException(Event.Code.ObjectOwnerNotFriendly);
						}

            p.AddEvent(Event.Code.Enters, b);
            p.Building = b;
          }
        }
      }
    }

    private static void CheckPatrolBlock(Person p, Region dest, bool attack) 
    {
      PersonList persons = dest.Persons.Clone();
      foreach (Person p2 in persons) 
      {
        if (!p2.Killed && p2.Patrolling && p2.Leader == null
          && p2.AttitudeTo(p) < Attitude.Neutral)
        {
          if (p2.AttitudeTo(p) == Attitude.Hostile) 
          {
            Battle b = new Battle(p2, p, dest);
            if (b.DefendersWin)
              return;
          }
          else if (p2.AttitudeTo(p) == Attitude.Unfriendly && attack) 
          {
            Battle b = new Battle(p, p2, dest);
            if (b.AttackersWin)
              return;
          }
          if (p2.HideFaction)
            throw new ProcessException(Event.Code.EntryForbidden,
							new object[2] { dest, p2 } );
          else
						throw new ProcessException(Event.Code.EntryForbiddenF,
							new object[3] { dest, p2, p2.Faction } );
				}
      }
    }

    private static void DoName(Person p, Order order) 
    {
      NameOrder ord = (NameOrder)order;
      if (ord.What == NameOrder.Target.Person)
        p.Name = ord.Name;
      else if (ord.What == NameOrder.Target.Faction)
        p.Faction.Name = ord.Name;
      else if (ord.What == NameOrder.Target.Object) 
      {
        if (p.Building == null) 
          throw new ProcessException(Event.Code.NotInside);

        if (p.Building.Persons[0] != p) 
          throw new ProcessException(Event.Code.MustOwnObject);

        p.Building.Name = ord.Name;
      }
    }

    private static void DoOption(Person p, Order order) 
    {
      OptionOrder ord = (OptionOrder)order;
      if (ord.Option == "text-report")
        p.Faction.Options.TextReport = (ord.Val == "1");
      if (ord.Option == "xml-report")
        p.Faction.Options.XmlReport = (ord.Val == "1");
      if (ord.Option == "language") 
      {
        if (ord.Val == "ru")
          p.Faction.Options.Lang = Lang.Ru;
        else
          p.Faction.Options.Lang = Lang.En;
      }
			if (ord.Option == "template") 
			{
				if (ord.Val == "long")
					p.Faction.Options.Template = TemplateType.Long;
				else
					p.Faction.Options.Template = TemplateType.Short;
			}
    }

    private static void DoPassword(Person p, Order order) 
    {
      string password = ((PasswordOrder)order).Password;
      p.Faction.Password = password;
      p.Faction.Events.Add(new Event(Event.Code.PasswordChanged));
    }

    private static void DoPatrol(Person p, Order order) 
    {
      if (p.Leader != null) 
        throw new ProcessException(Event.Code.OrderForLeader);

      foreach (Person sub in p.GetTeamAndLeader()) 
      {
        sub.Avoiding = false;
        sub.Patrolling = true;
      }
      p.AddEvent(Event.Code.Patrols, p.Region);

      // Promote skill
      Soldier s = new Soldier(null, p, Side.Attacker);
      s.PromoteSkill();

			// Set a flag to prevent receiving further combat xp
			foreach (Person sub in p.GetTeamAndLeader())
				sub.HadBattle = true;
    }
    
    private static void DoProduce(Person p, Order order) 
    {
			if (p.Leader != null) 
				throw new ProcessException(Event.Code.OrderForLeader);

      ItemType it = ((ProduceOrder)order).ItemType;
      if (it.ProduceSkill == null) 
        throw new ProcessException(Event.Code.CantProduce);

      if (p.Skills.GetByType(it.ProduceSkill.Type) == null) 
        throw new ProcessException(Event.Code.NoSkill);

      if (it.ProduceBuilding != null) 
      {
        if (p.Building == null || p.Building.Type != it.ProduceBuilding)
          throw new ProcessException(Event.Code.MustBeInside, it.ProduceBuilding);
        if (p.Building.GetNeeds().Count > 0)
          throw new ProcessException(Event.Code.ObjectIncompleteNamed, p.Building);
      }

      if (it.ProduceFrom1 == null) 
      {
        Item itm = p.Region.TurnResources.GetByType(it);
        if (itm == null || itm.Amount == 0)
          throw new ProcessException(Event.Code.NoResource);
      }

      int points = p.GetTeamLevel(it.ProduceSkill.Type) / it.ProduceSkill.Level;

      int amt;
      if (it.ProductionRate >= 0)
        amt = points * it.ProductionRate;
      else
        amt = points / (-it.ProductionRate);

      if (it.ProduceFrom1 == null) 
      {
				// Produce resource from region
				p.PromoteSkill(it.ProduceSkill.Type);
				if (points == 0) 
					throw new ProcessException(Event.Code.NotSkilledEnough);
				
        ProduceRequestItem pri = new ProduceRequestItem();
        pri.Person = p;
        pri.ItemType = it;
        pri.Amount = amt;
        ProduceRequests.Add(pri);
      }
      else 
      {
        // Look for raw materials
        int raw1 = p.GetTeamAmount(it.ProduceFrom1.Type);
        if (raw1 < it.ProduceFrom1.Amount) 
          throw new ProcessException(Event.Code.NotEnoughItem, it.ProduceFrom1);
        amt = Math.Min(amt, raw1 / it.ProduceFrom1.Amount);

        if (it.ProduceFrom2 != null) 
        {
          int raw2 = p.GetTeamAmount(it.ProduceFrom2.Type);
          if (raw2 < it.ProduceFrom2.Amount) 
						throw new ProcessException(Event.Code.NotEnoughItem, it.ProduceFrom2);
					amt = Math.Min(amt, raw2 / it.ProduceFrom2.Amount);
        }

				p.PromoteSkill(it.ProduceSkill.Type);
				if (points == 0) 
					throw new ProcessException(Event.Code.NotSkilledEnough);
				
				// Spend raw materials
        p.SpendFromTeam(it.ProduceFrom1.Type, amt * it.ProduceFrom1.Amount);
        if (it.ProduceFrom2 != null)
          p.SpendFromTeam(it.ProduceFrom2.Type, amt * it.ProduceFrom2.Amount);

        // Get produced items
        ItemType produced = it;
        if (it.ProduceAs != null)
          produced = it.ProduceAs;
        p.AddEvent(Event.Code.Produces, new Item(it, amt));
        p.Items.AddItems(produced, amt);
				p.Faction.ShowItem(it);
      }
    }

    private static void DoQuit(Person p, Order order) 
    {
      Faction f = p.Faction;
      Person chosen = f.GetChosen();
      if (chosen == null)
				return;

      chosen.Faction.Events.Add(new Event(null, "", Event.Code.Quitted, 
				new object[1] {chosen}));
      chosen.Kill();
    }

		private static void DoPromote(Person p, Order order) 
		{
			if (p.Building == null)
				throw new ProcessException(Event.Code.NotInside);
			if (p.Building.Persons[0] != p)
				throw new ProcessException(Event.Code.MustOwnObject);
			
			int num = ((PromoteOrder)order).PersonNum;
			Person target = p.Building.Persons.GetByNumber(num);
			if (target == null)
				throw new ProcessException(Event.Code.NotInObject, num);
			if (target.Leader != null)
				throw new ProcessException(Event.Code.PromoteLeader);
			
			p.Building.Persons.Remove(target);
			p.Building.Persons.Insert(0, target);
			
			p.AddEvent(Event.Code.Promoted, target);
			if (target.Faction != p.Faction)
				target.AddEvent(Event.Code.PromotedBy);
		}

    private static void DoScavenge(Person p, Order order) 
    {
      if (p.Leader != null)
        throw new ProcessException(Event.Code.OrderForLeader);
      ScavengeOrder ord = (ScavengeOrder)order;

      SkillType st = Constants.ScavengeSkill;
      if (p.Skills.GetByType(st) == null)
        throw new ProcessException(Event.Code.NoSkill);

      if (ord.What.Count == 0)
        foreach (Item itm in p.Region.Junk)
          ord.What.Add(itm.Type);

      int points = p.GetTeamLevel(st);
      bool promote = false;
			int priority = 1;
      foreach (ItemType it in ord.What) 
      {
        Item junk = p.Region.Junk.GetByType(it);
        if (junk == null)
          p.AddEvent(Event.Code.NoItemInJunk);

        promote = true;

        ProduceRequestItem pri = new ProduceRequestItem();
        pri.Person = p;
        pri.ItemType = it;
        pri.Amount = points;
				pri.Priority = priority;
				priority++;
        ScavengeRequests.Add(pri);
      }

      if (promote)
        p.PromoteSkill(st);
    }

    private static void DoShow(Person p, Order order) 
    {
      ShowOrder ord = (ShowOrder)order;
      if (ord.ItemType != null) 
      {
        p.Faction.ShowItem(ord.ItemType, true);
      }
      else if (ord.SkillType != null) 
      {
        p.Faction.ShowSkill(ord.SkillType, true);
      }
      else if (ord.BuildingType != null) 
      {
        p.Faction.ShowBuilding(ord.BuildingType, true);
      }
    }

		private static void DoSpoils(Person p, Order order) 
		{
			p.Spoils = ((ItemTypeListOrder)order).What;
		}

		private static void DoTeam(Person p, Order order) 
    {
      TeamOrder ord = (TeamOrder)order;
      if (ord.Kick) 
      {
        // Kick person from team
        Person sub = p.Team.GetByNumber(ord.LeaderNum);
        if (sub == null)
          throw new ProcessException(Event.Code.NotInTeam, ord.LeaderNum);
        sub.Leader = null;
        sub.AddEvent(Event.Code.KickedTeam);
      }
      else 
      {
        if (ord.LeaderNum != 0) 
        {
          // Join team
          Person leader = p.Region.Persons.GetByNumber(ord.LeaderNum);
					if (leader == null) 
					{
						// Check if requested leader is dead
						leader = Person.Get(ord.LeaderNum);
						if (leader == null || leader.DiedIn != p.Region)
							throw new ProcessException(Event.Code.NotInRegion, ord.LeaderNum);
            p.InheritTurnOrder(leader);
						return;
					}

					if (leader == p) 
						throw new ProcessException(Event.Code.CantJoinSelf);

					if (leader.AttitudeTo(p) < Attitude.Friendly) 
						throw new ProcessException(Event.Code.TargetNotFriendly, leader);

					if (leader.Leader != null) 
					{
						if (leader.Faction != p.Faction)
							throw new ProcessException(Event.Code.IsInTeam, leader);
						leader.Leader.AddEvent(Event.Code.LeavesTeam, leader);
						leader.Leader = null;
					}

          p.Leader = leader;
          leader.AddEvent(Event.Code.JoinedTeam, p);
        }
        else 
        {
          // Leave team
          if (p.Leader == null) 
            return;

          p.Leader.AddEvent(Event.Code.LeavesTeam, p);
          p.Leader = null;
        }
      }
    }

    private static void DoTrade(Person p, Order order) 
    {
      TradeOrder ord = (TradeOrder)order;

      Item sell_item = p.Items.GetByType(ord.SellWhat);
      if (sell_item == null || sell_item.Amount < ord.SellAmount)
        throw new ProcessException(Event.Code.NotEnoughOffer, ord.SellWhat);

      if (ord.PersonNum == 0) 
      {
        foreach (Person seller in p.Region.Persons) 
          TradeWith(p, seller, ord, sell_item);
      }
      else 
      {
        Person seller = p.Region.Persons.GetByNumber(ord.PersonNum);
        if (seller == null)
          throw new ProcessException(Event.Code.NotInRegion, ord.PersonNum);
        TradeWith(p, seller, ord, sell_item);
      }

      if (ord.BuyAmount > 0 && ord.SellAmount > 0)
        p.TradeOrder = ord;
    }

    private static void DoUninstall(Person p, Order order) 
    {
      if (p.Leader != null) 
        throw new ProcessException(Event.Code.OrderForLeader);
      if (p.Building == null) 
        throw new ProcessException(Event.Code.NotInside);
      UninstallOrder ord = (UninstallOrder)order;
			if (ord.What.Count == 0)
				throw new ProcessException(Event.Code.NoParameters);
      
			if (p.Building.Persons[0].Faction != p.Faction) 
				throw new ProcessException(Event.Code.FactionMustOwnObject);

			// Check skill level
			SkillType st = ord.What[0].InstallSkill.Type;
			if (p.Skills.GetByType(st) == null) 
				throw new ProcessException(Event.Code.NoSkill);
			int points = p.GetTeamLevel(st);

			bool first = true;
			foreach (ItemType what in ord.What) 
			{
				// Check if material has same install skill with first
				if (what.InstallSkill.Type != st)
					throw new ProcessException(Event.Code.DifferentInstallSkill);

				// Find material in building
				Item material = p.Building.Installed.GetByType(what);
				if (material == null)
					throw new ProcessException(Event.Code.NoItemInstalled, what);

				// Promote skill
				if (first) 
				{
					p.PromoteSkill(st);
					first = false;
				}

				// Check skill level
				if (points == 0)
					throw new ProcessException(Event.Code.NoSkillToUninstall, material);

				int amt = Math.Min(points, material.Amount);
				if (ord.Amount >= 0)
					amt = Math.Min(amt, ord.Amount);

				// Actually remove materials
				p.Building.Installed.RemoveItems(material.Type, amt);
				p.Items.AddItems(material.Type, amt);

				// Spend points
				points -= amt;

				// If first material or all materials removed, collapse
				if ((p.Building.Type.Materials.Count > 0
					&& p.Building.Installed.GetByType(p.Building.Type.Materials[0].Type) == null)
					|| p.Building.Installed.Count == 0) 
				{
					p.AddEvent(Event.Code.UninstallsCollapsed, new Item(what, amt));
					p.Building.Collapse();
					return;
				}
				else
					p.AddEvent(Event.Code.Uninstalls, new Item(what, amt));
				p.Faction.ShowItem(what);
			}
    }

    private static void TradeWith(Person p, Person seller, TradeOrder ord, Item sell_item) 
    {
      if (seller == p || seller.TradeOrder == null)
        return;
      if (seller.TradeOrder.SellWhat != ord.BuyWhat || 
        seller.TradeOrder.BuyWhat != ord.SellWhat)
        return;

      float price = (float)ord.BuyAmount / (float)ord.SellAmount;
      float seller_price = (float)seller.TradeOrder.SellAmount / (float)seller.TradeOrder.BuyAmount;
      if (price > seller_price)
        return;
        
      Item buy_item = seller.Items.GetByType(ord.BuyWhat);
        
      int amt_sell = (int)Math.Min(seller.TradeOrder.BuyAmount, ord.SellAmount);
      int amt_buy = (int)(amt_sell * seller_price);

      seller.Items.RemoveItems(buy_item.Type, amt_buy);
      seller.TradeOrder.SellAmount -= amt_buy;
      p.Items.AddItems(buy_item.Type, amt_buy);
      ord.BuyAmount -= amt_buy;

      p.Items.RemoveItems(sell_item.Type, amt_sell);
      ord.SellAmount -= amt_sell;
      seller.Items.AddItems(sell_item.Type, amt_sell);
      seller.TradeOrder.BuyAmount -= amt_sell;
        
      if (seller.TradeOrder.BuyAmount == 0 || seller.TradeOrder.SellAmount == 0)
        seller.TradeOrder = null;

			p.AddEvent(Event.Code.Trades, new object[3] 
				{ seller, new Item(ord.SellWhat, amt_sell), new Item(ord.BuyWhat, amt_buy) });
			seller.AddEvent(Event.Code.Trades, new object[3] 
				{ p, new Item(ord.BuyWhat, amt_buy), new Item(ord.SellWhat, amt_sell) });
    }

		private static void NPCMakeFriends() 
		{
			foreach (Person p in Person.List) 
			{
				if (p.Killed || !p.Faction.IsNPC || p.Leader != null || p.Team.Count > 0)
					continue;
				if (Constants.Random(100) < Constants.NPCMateChance) 
				{
					// NPC wants to make a friend, looking for 
					foreach (Person p2 in p.Region.Persons) 
					{
						if (p.Killed || !p2.Faction.IsNPC || p2.Leader != null || p2.Team.Count > 0)
							continue;
						if (p.Man.BabyFrom == p2.Man || p2.Man.BabyFrom == p.Man) 
						{
							// Found one, make a team
              p2.Leader = p;
						}
					}
				}
			}
		}

		private static void ResolveBabies() 
		{
			int i = Person.List.Count;
			while (i > 0) 
			{
				i--;
				Person mother = Person.List[i];
				if (mother.Killed) continue;
				if (mother.Man == null || mother.Man.Baby == null)
					continue;

				// If already has a baby, decrease timer. 
				if (mother.BabyTimer > 0) 
				{
					mother.BabyTimer--;
					if (mother.BabyTimer == 0) 
					{
						// Born the baby
						mother.Items.RemoveItems(mother.Man.Baby, 1);

						ItemType grown_to = mother.Man.Baby.GrowTo[Constants.Random(mother.Man.Baby.GrowTo.Count)];

						Person child = new Person(mother.Faction, mother.Region);
						child.Building = mother.Building;
						child.Leader = mother.Leader;
						child.Age = 0;
						if (grown_to == mother.Man)
							child.Name = NameGenerator.Name(grown_to.Name, mother.Name);
						else
							child.Name = NameGenerator.Name(grown_to.Name, mother.FatherName);
						mother.FatherName = "";
						child.Items.AddItems(grown_to, 1);

						// Add talents
						SkillTypeList talents = mother.GetTalents();
						if (mother.FatherSkill != null) 
						{
							child.AddSkill(mother.FatherSkill);
							talents.Remove(mother.FatherSkill);
						}
						while (talents.Count > 0 && child.GetTalents().Count < 2) 
						{
							SkillType t = talents[Constants.Random(talents.Count)];
							child.AddSkill(t);
							talents.Remove(t);
						}

						mother.AddEvent(Event.Code.BornBaby, new object[2]  { grown_to, child });
					}
					continue;
				}
        
				// If not, find a partner in team
				if (mother.Age > Constants.ChildTurns 
					&& Constants.Random(100) < Constants.SexPercent)
				{
					Person leader = mother.GetTeamLeader();
					foreach (Person father in leader.GetTeamAndLeader()) 
					{
						ItemType man = father.Man;
						if (man == null || mother.Man.BabyFrom != man || father.Age <= Constants.ChildTurns) 
							continue;
            
						// Found a father, create baby
						mother.Items.AddItems(mother.Man.Baby, 1);
						mother.BabyTimer = mother.Man.BabyTimer;
						mother.FatherSkill = father.GetRandomTalent();
						mother.FatherName = father.Name;

						mother.AddEvent(Event.Code.HasBabyFrom, father);
						if (father.Faction != mother.Faction)
							father.AddEvent(Event.Code.HadSexWith, mother);

						// Lower Insanity of both parents
						mother.Insanity -= Constants.SexInsanityDecrease;
						father.Insanity -= Constants.SexInsanityDecrease;
						break;
					}
				}
			}
		}

		private static void RaiseTeamsInsanity() 
		{
			foreach (Person p in Person.List) 
			{
				if (p.Killed || (p.Team.Count + 1) <= Constants.MaxFriendlyTeam) 
					continue;
        foreach (Person sub in p.GetTeamAndLeader())
					sub.Insanity++;
				p.AddEvent(Event.Code.TeamQuarrel);
			}
		}

    private static void ResolveProduceRequests() 
    {
      // Distribute resource evenly between producing teams
      foreach (Region r in Map.Regions) 
      {
        foreach (Item resource in r.TurnResources) 
        {
          int startingAmount = resource.Amount;
          ArrayList resource_requests = new ArrayList();
          foreach (ProduceRequestItem pri in ProduceRequests) 
          {
            if (pri.Person.Region == r && pri.ItemType == resource.Type)
              resource_requests.Add(pri);
          }

          int total = 0;
          foreach (ProduceRequestItem pri in resource_requests) 
            total += pri.Amount;

          foreach (ProduceRequestItem pri in resource_requests) 
          {
            int share;
            if (total <= resource.Amount) 
              share = pri.Amount;
            else
              share = (int)Math.Ceiling(((float)pri.Amount / (float)total) * startingAmount);
            if (share > resource.Amount)
              share = resource.Amount;

            ItemType produced = pri.ItemType;
            if (pri.ItemType.ProduceAs != null)
              produced = pri.ItemType.ProduceAs;
            pri.Person.Items.AddItems(produced, share);
            resource.Amount -= share;
            pri.Person.AddEvent(Event.Code.Produces, new Item(pri.ItemType, share));
						pri.Person.Faction.ShowItem(pri.ItemType);
          }
        }
      }
    }

    private static void ResolveScavengeRequests() 
    {
			// Find max priority (minimal priority is the items person will take first)
			int max_priority = 1;
			foreach (ProduceRequestItem pri in ScavengeRequests) 
				max_priority = Math.Max(max_priority, pri.Priority);

      // Distribute resource evenly between producing teams
			Hashtable completed = new Hashtable();
			for (int priority = 1; priority <= max_priority; priority++) 
			{
				foreach (Region r in Map.Regions) 
				{
					foreach (Item itm in r.Junk)
					{
						int startingAmount = itm.Amount;

						// Form list of requests
						ArrayList scavenge_requests = new ArrayList();
						foreach (ProduceRequestItem pri in ScavengeRequests) 
						{
							if (pri.Person.Region == r && pri.ItemType == itm.Type
								&& pri.Priority == priority) 
								scavenge_requests.Add(pri);
						}

						// Sort the list by skill
            for (int i = 0; i < scavenge_requests.Count; i++)
							for (int j = i + 1; j < scavenge_requests.Count; j++)
								if (((ProduceRequestItem)scavenge_requests[i]).Amount <
									((ProduceRequestItem)scavenge_requests[j]).Amount) 
								{
									object t = scavenge_requests[i];
									scavenge_requests[i] = scavenge_requests[j];
									scavenge_requests[j] = t;
								}

						// Count total amount of points
						int total = 0;
						foreach (ProduceRequestItem pri in scavenge_requests) 
						{
							if (completed.ContainsKey(pri.Person.Num))
								total += pri.Amount - (int)completed[pri.Person.Num];
							else
								total += pri.Amount;
						}

						// Distribute
						foreach (ProduceRequestItem pri in scavenge_requests) 
						{
							int request;
							if (completed.ContainsKey(pri.Person.Num))
								request = pri.Amount - (int)completed[pri.Person.Num];
							else
								request = pri.Amount;

							int share;
							if (total <= itm.Amount) 
								share = request;
							else
								share = (int)Math.Ceiling(((float)request / (float)total) * startingAmount);
							if (share > itm.Amount)
								share = itm.Amount;

							if (completed.ContainsKey(pri.Person.Num))
								completed[pri.Person.Num] = (int)completed[pri.Person.Num] + share;
							else
								completed[pri.Person.Num] = share;

							if (share > 0) 
							{
								pri.Person.Items.AddItems(pri.ItemType, share);
								if (share > itm.Amount)
									throw new Exception("Scavenging more than there is");
								itm.Amount -= share; // will remove later

								pri.Person.AddEvent(Event.Code.Scavenges, new Item(pri.ItemType, share));
								pri.Person.Faction.ShowItem(pri.ItemType);
							}
						}
					}

					// Remove spent items from Junk
					for (int i = r.Junk.Count-1; i >= 0; i--) 
					{
						if (r.Junk[i].Amount <= 0)
							r.Junk.RemoveAt(i);
					}
				}
			}
    }

    private static void ResolveMaintenance() 
    {
      foreach (Region r in Map.Regions) 
      {
        Hashtable hungry = new Hashtable();

        // First, people eating their own food
        foreach (Person p in r.Persons) 
        {
          // Wanderers will not need maintenance (do something about it later)
          if (p.Faction.IsNPC)
            continue;

					ItemTypeList food = p.GetConsumeList();
					ItemList consumed = new ItemList();
					int hunger = Constants.RationsPerMonth;

					foreach (ItemType it in food) 
					{
						int items = GetFood(p, it, hunger);
						if (items == 0) 
							continue;
						consumed.Add(new Item(it, items));
						hunger = Math.Max(0, hunger - items * it.Rations);
					}

					if (consumed.Count > 0)
						p.AddEvent(Event.Code.Consumed, consumed);
          if (hunger > 0)
            hungry.Add(p, hunger);
        }
        
        // Then, starting to borrow food
        Person[] keys = new Person[hungry.Keys.Count];
        hungry.Keys.CopyTo(keys, 0);
        foreach (Person p in keys)
        {
          int hunger = (int)hungry[p];
					ItemTypeList food = p.GetConsumeList();

					foreach (ItemType it in food) 
					{
						foreach (Person pal in r.Persons) 
						{
							if (pal != p && pal.AttitudeTo(p) == Attitude.Ally && !pal.Greedy) 
							{
								int items = GetFood(pal, it, hunger);
								if (items == 0)
									continue;
								p.AddEvent(Event.Code.Borrows, new object[2] { pal, new Item(it, items) } );
								hunger = Math.Max(0, hunger - items * it.Rations);
							}
						}
					}

					if (hunger > 0)
            hungry[p] = hunger;
          else
            hungry.Remove(p);
        }

        // Then, starve some men
        foreach (Person p in hungry.Keys) 
        {
          if (Constants.Random(100) < (int)hungry[p]) 
          {
            if (p.Faction != null)
              p.AddEvent(Event.Code.Starved);
            p.Kill();
          }
          else
          {
            p.AddEvent(Event.Code.Hungry, hungry[p]);
            p.Insanity++;
          }
        }
      }
    }

    private static int GetFood(Person p, ItemType it, int hunger) 
    {
			Item itm = p.Items.GetByType(it);
			if (itm == null)
				return 0;
			int rations = Math.Min(hunger, itm.Type.Rations * itm.Amount);
			int items = (int)Math.Ceiling((float)rations / (float)itm.Type.Rations);
			if (items == 0)
				return 0;
			p.Items.RemoveItems(itm.Type, items);
			return items;
		}

    private static void ResolveRadiation() 
    {
      foreach (Person p in Person.List) 
      {
				if (p.Killed)
					continue;
        int dose = p.RadiationDanger(false);

        if (dose != 0) 
        {
          if (Constants.Random(100) < Math.Abs(dose)) 
          {
            // Check mutation
						if (dose > 0
							&& p.Man.MutateTo != null 
							&& Constants.Random(100) < p.Man.MutatePercent) 
						{
							// Mutate
							p.AddEvent(Event.Code.Mutated, p.Man.MutateTo);
							Item itm = p.Items.GetByType(p.Man);
							itm.Type = p.Man.MutateTo;
							p.Insanity = 10;
							
							if (!p.Man.IsMonster) 
							{
								if (Constants.Random(100) < 3)
									p.Description = "’х’’очч„у ≈сс—сть ћмммозгг√гии...";
								else if (Constants.Random(100) < 3)
									p.Description = "√осподи! Ёто что у мен€ тут такое выросло?!";
							}
						}
						else 
						{
							// If no mutation, kill
							if (p.Faction.Num == Constants.NPCFactionNum)
								continue;
							if (dose < 0)
								p.AddEvent(Event.Code.RadiationUnderdose);
							else
								p.AddEvent(Event.Code.RadiationOverdose);
							p.Kill();
							continue;
						}
          }
          else 
          {
            if (dose < 0)
              p.AddEvent(Event.Code.RadiationNotReceived);
            else
              p.AddEvent(Event.Code.RadiationReceived);
            p.Insanity++;
          }
        }
      }
    }

    private static void ResolveTemperature() 
    {
      foreach (Region r in Map.Regions) 
      {
        Hashtable needing = new Hashtable();

        // First, people burn their own wood
        foreach (Person p in r.Persons) 
        {
          // Wanderers will not need maintenance (do something about it later)
          if (p.Faction.IsNPC)
            continue;
          
          // Get person temperature level (inverted)
          int need = p.TemperatureDanger(false);
          if (need <= 0)
            continue;

					ItemTypeList burn = p.GetBurnList();
					ItemList burned = new ItemList();

					foreach (ItemType it in burn) 
					{
						int items = GetBurn(p, it, need);
						if (items == 0) 
							continue;
						burned.Add(new Item(it, items));
						need = Math.Max(0, need - items);
					}

					if (burned.Count > 0)
						p.AddEvent(Event.Code.Burned, burned);
					if (need > 0)
						needing.Add(p, need);
        }
        
        // Then, starting to borrow food
				Person[] keys = new Person[needing.Keys.Count];
        needing.Keys.CopyTo(keys, 0);
        foreach (Person p in keys)
        {
          int need = (int)needing[p];
					ItemTypeList burn = p.GetBurnList();

					foreach (ItemType it in burn) 
					{
						foreach (Person pal in r.Persons) 
						{
							if (pal != p && pal.AttitudeTo(p) == Attitude.Ally && !pal.Greedy) 
							{
								int items = GetBurn(pal, it, need);
								if (items == 0)
									continue;
								p.AddEvent(Event.Code.Borrows, new object[2] { pal, new Item(it, items) } );
								need = Math.Max(0, need - items);
							}
						}
					}

          if (need > 0)
            needing[p] = need;
          else
            needing.Remove(p);
        }

        // Then, freeze some men
        foreach (Person p in needing.Keys) 
        {
          if (Constants.Random(100) < (int)needing[p]) 
          {
            if (p.Faction != null)
              p.AddEvent(Event.Code.Frozen);
            p.Kill();
          }
          else
          {
            p.AddEvent(Event.Code.Cold, needing[p]);
            p.Insanity++;
          }
        }
      }
    }

    private static int GetBurn(Person p, ItemType it, int need) 
    {
			Item itm = p.Items.GetByType(it);
			if (itm == null)
				return 0;
			int items = Math.Min(need, itm.Amount);
			if (items > 0)
				p.Items.RemoveItems(itm.Type, items);
			return items;
		}

    private static void LowerWanderersInsanity() 
    {
      // Insanity of person without faction is lowered by 1 per month
      foreach (Person p in Person.List) 
        if (!p.Killed && p.Faction.IsNPC)
          p.Insanity--;
    }

    private static void ResolveInsaneRenegades() 
    {
      foreach (Person p in Person.List) 
      {
        if (p.Killed || p.Chosen)
					continue;
        if (p.Insanity >= Constants.CompleteInsanity && !p.Faction.IsNPC) 
        {
					p.AddEvent(Event.Code.GoesInsane);

					// Set insulting description
					if (Constants.Random(100) < 10) 
					{
						Person chosen = p.Faction.GetChosen();
						if (chosen != null) 
						{
							if (p.Faction.Options.Lang == Lang.En) 
								p.Description = "God damn you, " + MyStrings.Translit(chosen.Name) + "!";
							else 
								p.Description = "Ќенавижу теб€, " + chosen.Name + "!";
						}
					}
					
					p.Leader = null;
          p.Faction = Faction.Get(Constants.NPCFactionNum);
        }
      }
    }

    private static void MoveWanderers() 
    {
			Console.WriteLine("Moving wanderers");

      // Persons without faction will move to another region if liked it more
      foreach (Person p in Person.List) 
      {
        if (p.Killed)
          continue;
        if (!p.Faction.IsNPC)
          continue;

				// Set "im feeling bad" description
				if (!p.Man.IsMonster && p.Region.Radiation > p.Man.RadiationTo || p.Region.Radiation < p.Man.RadiationFrom) 
				{
          if (Constants.Random(100) < 4)
						p.Description = "„то-то мне нехорошо...";
          else if (Constants.Random(100) < 1)
						p.Description = "ћен€ тошнит...";
				}
				else
					p.Description = "";
				
				if (Constants.Random(100) >= Constants.WandererMoveChance)
          continue;

        Direction dir = Direction.None;
        
        // Wanderers will like places with less man
        int most_like = -p.Region.Persons.Count;
        // And more comfortable radiation
				if (p.Region.Radiation > p.Man.RadiationTo || p.Region.Radiation < p.Man.RadiationFrom)
					most_like -= 1000;
				else 
				{
					if (p.Man != null && p.Man.RadiationFrom == 0)
						most_like -= p.Region.Radiation;
					else 
						most_like += p.Region.Radiation;
				}

        for (Direction i = Direction.North; i <= Direction.Northwest; i++) 
        {
          Region n = p.Region.RegionInDir(i);
          if (n == null || !n.Terrain.Walking || n.Radiation > p.Man.RadiationTo
						|| n.Radiation < p.Man.RadiationFrom)
            continue;
          int like = -n.Persons.Count;
					if (Constants.Random(100) > 20) 
					{
						if (p.Man != null && p.Man.RadiationFrom == 0)
							like -= n.Radiation;
						else 
							like += n.Radiation;
					}

          // Add some impulsive decisions
          like += (int)Constants.Random(50);

          if (like > most_like)
            dir = i;
        }

				if (dir != Direction.None) 
				{
					// Drop too heavy items
					int i = 1;
					while (i <= 4 && p.GetWeight() > p.GetCapacity(Movement.Walk)) 
					{
						for (int j = p.Items.Count-1; j >= 0; j--)
						{
							Item itm = p.Items[j];
							if (!itm.Type.NoGive && itm.Type.Weight > 0
								&& (i > 1 || !(itm.Type.OwnerTemperature > 0))
								&& (i > 2 || !itm.Type.IsWeapon)
								&& (i > 3 || !p.Man.Food.Contains(itm.Type))) 
							{
								p.Region.Junk.AddItems(itm.Type, itm.Amount);
								p.Items.RemoveAt(j);
							}
						}
						i++;
					}

					// Wanderer walked to other place
					if (p.Building != null)
						p.Building = null;
					if (p.Leader != null)
						p.Leader = null;
					Region dest = p.Region.RegionInDir(dir);
					try 
					{
						CheckPatrolBlock(p, dest, false);
						p.Region = dest;
					}
					catch (ProcessException) {}
					p.Description = "";
				}
				else 
				{
					// Set "i like here" description
					if (!p.Man.IsMonster && Constants.Random(100) < 5) 
					{
						if (p.Man.RadiationFrom > 0)
							p.Description = "«десь тепло и светло.";
						else
							p.Description = "ќтличное местечко.";
					}
				}
      }
    }

    private static void CreateNewMonsters() 
    {
			foreach (Region r in Map.Regions) 
			{
				if (r.Terrain.Monsters.Keys.Count == 0)
					continue;
				foreach (ItemType it in r.Terrain.Monsters.Keys) 
				{
					int chance = (int)r.Terrain.Monsters[it];
					if (chance < Constants.Random(1000))
						continue;

					int amount = 1 + Constants.Random(it.PackSize-1);
					Person leader = null;
					for (int j = 0; j < amount; j++) 
					{
						Person p = new Person(Faction.Get(Constants.NPCFactionNum), r);
						if (leader == null)
							leader = p;
						else
							p.Leader = leader;
						p.Name = it.FullNameRu1.Substring(0, 1).ToUpper() + 
							it.FullNameRu1.Substring(1);
						p.Items.AddItems(it, 1);
						foreach (Skill sk in it.Skills) 
						{
							Skill sk2 = new Skill(sk.Type);
							sk2.Level = sk.Level;
							p.Skills.Add(sk2);
						}
					}
				}
			}
    }

    private static void ResolveHostileAttacks() 
    {
      foreach (Region r in Map.Regions) 
      {
        PersonList persons = new PersonList();
        foreach (Person p in r.Persons)
          if (p.Leader == null)
            persons.Add(p);
        foreach (Person p in persons) 
        {
          if (p.Killed 
						|| p.Avoiding
						// NPC will attack depending on aggression level
						|| (p.Faction.IsNPC && Constants.Random(100) > p.Man.Aggression)
						// will not attack by hostility if already attacked
						|| p.HadBattle 
            // will not attack by hostility if has no battle talent
						|| p.Skills.GetByType(Constants.ReflexesSkill) == null) 
            continue;
					PersonList targets = new PersonList();
					foreach (Person target in r.Persons)
						if (!(target == p || target.Leader == p || p.Leader == target 
							|| (p.Leader != null && target.Leader == p.Leader))
							&& (p.AttitudeTo(target) == Attitude.Hostile))
							targets.Add(target);

					if (targets.Count > 0) 
					{
						Person target = targets[Constants.Random(targets.Count)];
						Battle b = new Battle(p, target, target.Region);
					}
        }
      }
    }

		private static void RemovePatrolFlags() 
		{
			foreach (Person p in Person.List) 
			{
				if (p.Killed) continue;
				p.Patrolling = false;
			}
		}

		private static void FindTheGreatest() 
		{
			int max = 0;
			foreach (Faction f in Faction.List) 
				if ((Faction.TheGreatest == null || f.Persons.Count > Faction.TheGreatest.Persons.Count) && !f.IsNPC) 
					Faction.TheGreatest = f;
		}

    private static void DismissFactionsWithoutChosen() 
    {
      foreach (Faction f in Faction.List) 
      {
        if (f.IsNPC || f.GetChosen() != null)
          continue;
        for (int i = f.Persons.Count-1; i >= 0; i--)
          f.Persons[i].Faction = Faction.Get(Constants.NPCFactionNum);
        f.Events.Add(new Event(Event.Code.FactionDismissed));
      }
    }

		private static void ClearMonstersAvoid() 
		{
			foreach (Person p in Person.List)
				if (p.Man.IsMonster)
					p.Avoiding = false;
		}
  }

	enum AskType 
	{
		Anything,
		DefaultSequence,
		Preferred
	}

  public class ProcessException : Exception 
  {
		public Event.Code Code;
		public object[] Params;

		public ProcessException(Event.Code code) : base() 
		{
			Code = code;
			Params = new object[0];
		}

		public ProcessException(Event.Code code, object param1) : base() 
		{
			Code = code;
			Params = new object[1] { param1 };
		}

		public ProcessException(Event.Code code, object[] parameters) : base() 
		{
			Code = code;
			Params = parameters;
		}
	}
}