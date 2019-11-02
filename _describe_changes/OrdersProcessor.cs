using System;
using System.Collections;

namespace Wasteland 
{
  class OrdersProcessor 
  {
    public delegate void PersonOrderDelegate(Person person, Order order);
    
    private class ProduceRequestItem 
    {
      public Person Person;
      public ItemType ItemType;
      public int Amount;
    }

    private static ArrayList ProduceRequests = null;
    private static ArrayList ScavengeRequests = null;

    public static void Process() 
    {
			Map.CalcTurnResources();

      DoForAllPersons(new PersonOrderDelegate( DoAddress  ), "ADDRESS", typeof(AddressOrder));
      DoForAllPersons(new PersonOrderDelegate( DoEquipment), "EQUIPMENT", typeof(EquipmentOrder));
      DoForAllPersons(new PersonOrderDelegate( DoBurn     ), "BURN", typeof(BurnOrder));
      DoForAllPersons(new PersonOrderDelegate( DoConsume  ), "CONSUME", typeof(ConsumeOrder));
      DoForAllPersons(new PersonOrderDelegate( DoName     ), "NAME", typeof(NameOrder));
	  DoForAllPersons(new PersonOrderDelegate( DoDescribe ), "DESCRIBE", typeof(DescribeOrder));
      DoForAllPersons(new PersonOrderDelegate( DoOption   ), "OPTION", typeof(OptionOrder));
      DoForAllPersons(new PersonOrderDelegate( DoPassword ), "PASSWORD", typeof(PasswordOrder));
      DoForAllPersons(new PersonOrderDelegate( DoQuit     ), "QUIT", typeof(QuitOrder));
      DoForAllPersons(new PersonOrderDelegate( DoShow     ), "SHOW", typeof(ShowOrder));
      DoForAllPersons(new PersonOrderDelegate( DoAvoid    ), "AVOID", typeof(AvoidOrder));
      DoForAllPersons(new PersonOrderDelegate( DoAttack   ), "ATTACK", typeof(AttackOrder));
      ResolveHostileAttacks();
      DoForAllPersons(new PersonOrderDelegate( DoDeclare  ), "DECLARE", typeof(DeclareOrder));
DoForAllPersons(new PersonOrderDelegate( DoTeam     ), "TEAM", typeof(TeamOrder));
DoForAllPersons(new PersonOrderDelegate( DoLeave    ), "LEAVE", typeof(LeaveOrder));
DoForAllPersons(new PersonOrderDelegate( DoEnter    ), "ENTER", typeof(EnterOrder));
DoForAllPersons(new PersonOrderDelegate( DoEvict    ), "EVICT", typeof(EvictOrder));
DoForAllPersons(new PersonOrderDelegate( DoHide     ), "HIDE", typeof(HideOrder));
DoForAllPersons(new PersonOrderDelegate( DoKick     ), "KICK", typeof(KickOrder));
      
      DoForAllPersons(new PersonOrderDelegate( DoTrade    ), "TRADE", typeof(TradeOrder));

      DoForAllPersons(new PersonOrderDelegate( DoGive     ), "GIVE", typeof(GiveOrder));
      
      DoForAllPersons(new PersonOrderDelegate( DoBuild    ), "BUILD", typeof(BuildOrder));
			DoForAllPersons(new PersonOrderDelegate( DoCure     ), "CURE", typeof(CureOrder));
			DoForAllPersons(new PersonOrderDelegate( DoInstall  ), "INSTALL", typeof(InstallOrder));
      RemovePatrolFlags();
			DoForAllPersons(new PersonOrderDelegate( DoPatrol   ), "PATROL", typeof(PatrolOrder));
			DoForAllPersons(new PersonOrderDelegate( DoMaintain ), "MAINTAIN", typeof(MaintainOrder));
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

      ResolveBabies();
			RaiseTeamsInsanity();
      LowerWanderersInsanity();
      ResolveInsaneRenegades();
      DismissFactionsWithoutChosen();
    }

    private const string sOrderForLeader = "This order should be given to leader.|" +
      "Этот приказ предназначен для командира бригады.";
    private const string sNotInRegion = "{0} not in region.|{0} не в регионе.";
    private const string sNoSuchItem = "No such item.|Нет такой вещи.";
    private const string sNoSkill = "No skill to do that.|Нет необходимого навыка.";
    private const string sNotSkilledEnough = "Not skilled enough.|Недостаточный уровень навыка.";
    private const string sNotInside = "Should be inside object.|Нужно находиться внутри объекта.";

    private static void DoForAllPersons(PersonOrderDelegate dlg, string order_name, Type type) 
    {
			Console.WriteLine(".." + order_name);
      foreach (Person p in Person.List) 
      {
        if (p.Killed)
          continue;
        foreach (Order ord in p.Orders) 
        {
          if (ord.GetType() == type)
            try 
            {
              dlg(p, ord);
            }
            catch (ProcessException ex) 
            {
							if (!p.Killed)
								p.AddEvent(order_name + ": " + ex.Message.Replace("|", "|" + order_name + ": "));
            }
        }
      }
    }

    private static void DoAddress(Person p, Order order) 
    {
      p.Faction.Email = ((AddressOrder)order).Email;
      p.Faction.Events.Add(String.Format("Address is now {0}.|Адрес изменён на {0}.",
        p.Faction.Email));
    }

    private static void DoAttack(Person p, Order order) 
    {
      if (p.Leader != null)
        throw new ProcessException(sOrderForLeader);
      AttackOrder ord = (AttackOrder)order;
			if (ord.PersonNum == -1) // order was removed in previous battle
				return;
      Person target = p.Region.Persons.GetByNumber(ord.PersonNum);
      if (target == null)
        throw new ProcessException(String.Format(sNotInRegion, ord.PersonNum));
      if (target.Faction == p.Faction)
        throw new ProcessException("Can't attack a person from same faction.|Нельзя атаковать персонажа из своей фракции.");
      if ((target.Leader != null && target.Leader == p.Leader) 
        || target.Leader == p || p.Leader == target)
        throw new ProcessException("Can't attack a person from same team.|Нельзя атаковать персонажа из своей бригады.");

			PersonArrayList attackers = new PersonArrayList();
			attackers.Add(p);

			// Look for other person in region with same target
			foreach (Person p2 in p.Region.Persons) 
			{
				if (p2 == p || p2.AttitudeTo(p) < Attitude.Ally)
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
        throw new ProcessException(sOrderForLeader);
      BuildingType bt = ((BuildOrder)order).What;

      // BUILD is actually INSTALL first material
      if (bt.Materials.Count == 0 || bt.NoBuild) 
        throw new ProcessException("Can't build this object.|Этот объект нельзя построить.");

      Item material = bt.Materials[0];
      int amt = InstallMaterial(p, bt, material);

      // Create building
      p.Building = new Building(bt, p.Region);
      p.Building.Name = bt.FullNameRu.Substring(0, 1).ToUpper() +
				bt.FullNameRu.Substring(1);
			p.Building.Installed.AddItems(material.Type, amt);
      
			p.Faction.ShowBuilding(bt);

      p.AddEvent(String.Format("Started work on {0}, installed {1}.|" +
        "Начата работа над объектом: {2}, установлено: {3}.",
        bt.ToString(Lang.En), material.Type.ToString(amt, Lang.En),
        bt.ToString(Lang.Ru), material.Type.ToString(amt, Lang.Ru)));
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
        throw new ProcessException(sOrderForLeader);
      
      SkillType stCure = Constants.CureSkill;
      if (p.Skills.GetByType(stCure) == null)
        throw new ProcessException(sNoSkill);
      
      int points = p.GetTeamLevel(stCure) / Constants.LevelToCure;

      bool first = true;

      foreach (int num in ((CureOrder)order).PatientNums) 
      {
        if (points <= 0) 
          throw new ProcessException(sNotSkilledEnough);

        Person patient = p.Region.Persons.GetByNumber(num);
        if (patient == null) 
          p.AddEvent(String.Format(sNotInRegion, num));

        if (patient.AttitudeTo(p) < Attitude.Friendly) 
          p.AddEvent(String.Format("{0} is not Friendly.|{0} не относится к вам Дружелюбно.", 
            patient.ToString(Lang.En), patient.ToString(Lang.Ru)));

        ItemType man = patient.Man;
        if (man == null)
          continue;
        ItemType medicine = man.Medicine;
        if (medicine != null) 
        {
          // Spend one pill
          int having = p.GetTeamAmount(medicine);
          if (having == 0) 
            throw new ProcessException("Out of drugs.|Кончились таблетки.");
          p.SpendFromTeam(medicine, 1);
        }

        // Do cure
        points--;
        patient.Insanity--;
        if (patient.Faction != p.Faction && patient.Faction != null)
          patient.AddEvent(String.Format("Cured by {0}.|Принимает лечение от персонажа {1}.", 
            p.ToString(Lang.En), p.ToString(Lang.Ru)));
        if (medicine == null)
          p.AddEvent(String.Format("Cures {0}.|Лечит персонажа {1}.", 
            patient.ToString(Lang.En), patient.ToString(Lang.Ru)));
        else
          p.AddEvent(String.Format("Cures {0} using {1}.|Лечит персонажа {2}; использовано: {3}.",
            patient.ToString(Lang.En),  medicine.ToString(1, Lang.En),
            patient.ToString(Lang.Ru),  medicine.ToString(1, Lang.Ru)));

        if (first)
          p.PromoteSkill(stCure);
        first = false;
      }
    }

    private static void DoDeclare(Person p, Order order) 
    {
      DeclareOrder ord = (DeclareOrder)order;
      Faction f = Faction.Get(ord.FactionNum);
      if (f == null) 
        throw new ProcessException("No such faction.|Нет такой фракции.");

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
		else if (ord.What == DescribeOrder.Target.Faction)
			p.Faction.Description = ord.Description;
		else if (ord.What == DescribeOrder.Target.Object) 
		{
			if (p.Building == null) 
				throw new ProcessException(sNotInside);

			if (p.Building.Persons[0] != p) 
				throw new ProcessException("Must own object to describe.|Нужно быть владельцем объекта.");

			p.Building.Description = ord.Description;
		}
	}

    private static void DoDrive(Person p, Order order) 
    {
      if (p.Leader != null)
        throw new ProcessException(sOrderForLeader);

      DriveOrder ord = (DriveOrder)order;

      if (p.Building == null) 
        throw new ProcessException(sNotInside);

      if (p.Building.Type.Speed == 0) 
        throw new ProcessException("Object can't move.|Объект не может двигаться.");

			if (p.Building.Persons[0] != p)
				throw new ProcessException("Owner must drive vehicle.|Транспортом должен управлять владелец.");

			if (p.Building.GetNeeds().Count > 0) 
        throw new ProcessException("Vehicle is incomplete.|Транспорт не достроен.");

      // Calculate weight inside
      int weight = 0;
      foreach (Person p2 in p.Building.Persons) 
        weight += p2.GetWeight();
      if (weight > p.Building.Type.Capacity) 
        throw new ProcessException("Vehicle is overloaded.|Транспорт перегружен.");

      // Calculate team driving points
      if (p.Building.Type.DriveSkill != null) 
      {
        if (p.Skills.GetByType(p.Building.Type.DriveSkill.Type) == null)
          throw new ProcessException(sNoSkill);

        int drive_level = 0;
        foreach (Person sub in p.GetTeamAndLeader()) 
        {
          Skill sk = sub.Skills.GetByType(p.Building.Type.DriveSkill.Type);
          if (sk != null)
            drive_level += sk.Level;
        }
        if (drive_level < p.Building.Type.DriveSkill.Level) 
          throw new ProcessException(sNotSkilledEnough);
      }

      int points = p.Building.Type.Speed;
      bool first = true;

      foreach (object d in ord.Directions) 
      {
        Region dest = p.Region.RegionInDir((Direction)d);
        if (dest == null || 
					(!dest.Terrain.Vehicles && p.Building.Type.DriveTerrain != dest.Terrain)) 
          throw new ProcessException("Can't drive there.|Туда машина не проедет.");

        if (points < dest.PointsToEnter()) 
          throw new ProcessException("Insufficient movement points.|Недостаточно очков передвижения.");

				// Check if radiation is absolutely lethal
				if (dest.Radiation + p.RadiationModifier() >= p.Man.RadiationTo + 100)
					throw new ProcessException(String.Format("Tried to enter {0}, but feeled woozy and turned back.|Пытается войти в {1}, но чувствует слабость и тошноту и возвращается.",
						dest.ToString(Lang.En), dest.ToString(Lang.Ru)));
				
				CheckPatrolBlock(p, dest, ord.Attack);

        ItemType fuel = p.Building.Type.Fuel;
        if (fuel != null) 
        {
          int amt = p.GetTeamAmount(fuel);
          if (amt == 0)
            throw new ProcessException("No fuel.|Нет горючего.");
          p.SpendFromTeam(fuel, 1);
        }

        points -= dest.PointsToEnter();          
        string msg = String.Format("Drives from {0} to {1}{2}.|" +
          "Покидает {3}, въезжает в {4}{5}.",
          p.Region.ToString(Lang.En), dest.ToString(Lang.En),
          (fuel != null) ? ", spending 1 " + fuel.ToString(1, Lang.En) : "",
          p.Region.ToString(Lang.Ru), dest.ToString(Lang.Ru),
          (fuel != null) ? ", затрачено: 1 " + fuel.ToString(1, Lang.Ru): "");
        p.AddEvent(msg);

        if (first && p.Building.Type.DriveSkill != null)
          p.PromoteSkill(p.Building.Type.DriveSkill.Type);
        first = false;
        
        // Distribute message to all team leaders in building
        foreach (Person p2 in p.Building.Persons) 
        {
          if (p2.Leader == null && p2 != p)
            p2.AddEvent(String.Format("{0} drives from {1} to {2}.|" +
              "{3} покидает {4}, въезжает в {5}.",
              p.ToString(Lang.En), p.Region.ToString(Lang.En), dest.ToString(Lang.En),
              p.ToString(Lang.Ru), p.Region.ToString(Lang.Ru), dest.ToString(Lang.Ru)));
        }
        
        p.Building.Region = dest;
      }
    }

    private static void DoEnter(Person p, Order order) 
    {
      if (p.Leader != null)
        throw new ProcessException(sOrderForLeader);

      int num = ((EnterOrder)order).BuildingNum;
      Building b = p.Region.Buildings.GetByNumber(num);
      if (b == null) 
        throw new ProcessException(String.Format(sNotInRegion, num));

			if (b.Persons.Count > 0) 
			{
				Person owner = b.Persons[0];
				if (owner.AttitudeTo(p) < Attitude.Friendly)
					throw new ProcessException("Object owner is not Friendly.|Владелец объекта не относится к вам дружественно.");
			}

			if (p.Building == b)
        throw new ProcessException("Already in that object.|Уже в этом объекте.");

      p.AddEvent(String.Format("Enters {0}.|Входит в {1}.", 
        b.ToString(Lang.En), b.ToString(Lang.Ru)));
      p.Building = b;
    }

		private static void DoEvict(Person p, Order order) 
		{
			if (p.Building == null)
				throw new ProcessException(sNotInside);
			if (p.Building.Persons[0] != p)
				throw new ProcessException("Must own object.|Надо быть владельцем объекта.");
			
			int num = ((EvictOrder)order).PersonNum;
			Person target = p.Building.Persons.GetByNumber(num);
			if (target == null)
				throw new ProcessException(String.Format("{0} not in same object.|{0} не в этом объекте.", num));
			if (target.Leader != null)
				throw new ProcessException("Must EVICT a team leader.|Надо выселять командира бригады.");
			
			target.Building = null;
			
			p.AddEvent(String.Format("Evicted {0}.|Выселяет персонажа {1}.",
				target.ToString(Lang.En), target.ToString(Lang.Ru)));
			if (target.Faction != p.Faction)
				target.AddEvent("Evicted by owner.|Владелец выселил из объекта.");
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
				throw new ProcessException(sNoSuchItem);
			if (ord.Amount == -1)
				ord.Amount = what.Amount;
			
			Person target = null;
      bool hired = false;
			string hire_msg_en = "";
			string hire_msg_ru = "";
      if (ord.Target != 0) 
      {
        target = p.Region.Persons.GetByNumber(ord.Target);
        if (target == null) 
          throw new ProcessException(String.Format(sNotInRegion, ord.Target));

        // Check hire for chosen one
        if (target != null && target.Faction != p.Faction && p.Chosen
					&& !target.Man.IsMonster)
        {
          if (target.Man.Food.Contains(ord.What)) 
          {
						if (target.GetHireAmount() <= ord.Amount) 
						{
							// Person is joined faction!
							if (target.Faction.Num != Constants.NPCFactionNum)
								target.Faction.GetChosen().AddEvent(
									String.Format("{0} leaves faction and joins {1}.|" +
									"{2} покидает фракцию, персонажа перекупает {3}.",
									target.ToString(Lang.En), p.ToString(Lang.En),
									target.ToString(Lang.Ru), p.ToString(Lang.Ru)));
							target.Faction = p.Faction;
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
							hire_msg_en = " There was not enough to hire this person.";
							hire_msg_ru = " Этого недостаточно, чтобы перекупить этого персонажа.";
						}
          }
        }
        
        // Check attitude
        if (!hired) 
        {
					if (target.AttitudeTo(p) < Attitude.Friendly) 
					{
						target.AddEvent(String.Format("Refused to receive {0} from {1}.|Отказывается от предмета {2}, предложенного персонажем {3}.",
							ord.What.ToString(ord.Amount, Lang.En), p.ToString(Lang.En),
							ord.What.ToString(ord.Amount, Lang.Ru), p.ToString(Lang.Ru)));

						throw new ProcessException(
							String.Format("{0} is not Friendly and do not want it.{2}|" +
							"{1} не относится к вам дружественно и ничего не хочет брать.{3}",
							target.ToString(Lang.En), target.ToString(Lang.Ru),
							hire_msg_en, hire_msg_ru));
					}
        }
      }

      int amt = ord.Amount;
      if (amt > what.Amount) 
        amt = what.Amount;

      // Remove items from giver
      p.Items.RemoveItems(ord.What, amt);
      if (ord.Target != 0)
        p.AddEvent(String.Format("Gives {0} to {1}.{4}|Отдано персонажу {3}: {2}.{5}",
          ord.What.ToString(amt, Lang.En), target.ToString(Lang.En),
          ord.What.ToString(amt, Lang.Ru), target.ToString(Lang.Ru),
					hire_msg_en, hire_msg_ru));
      else
        p.AddEvent(String.Format("Discards {0}.|Выброшено: {1}.",
          ord.What.ToString(amt, Lang.En), ord.What.ToString(amt, Lang.Ru)));

      // Add items to recipient
      if (ord.Target != 0) 
      {
        target.Items.AddItems(ord.What, amt);
        if (target.Faction != p.Faction)
          target.AddEvent(String.Format("Receives {0} from {1}.|" +
            "Получено от персонажа {3}: {2}.", 
            ord.What.ToString(amt, Lang.En), p.ToString(Lang.En),
            ord.What.ToString(amt, Lang.Ru), p.ToString(Lang.Ru)));
				target.Faction.ShowItem(ord.What);
      }

      if (hired)
        p.AddEvent(String.Format("{0} joined your faction.|{1} присоединяется к фракции.",
          target.ToString(Lang.En), target.ToString(Lang.Ru)));
    }

    private static void DoHide(Person p, Order order) 
    {
      HideOrder ord = ((HideOrder)order);
      if (ord.Variant == HideOrder.Variants.Person) 
      {
        if (p.Leader != null || p.Team.Count > 0)
          throw new ProcessException("Can't hide in team.|Нельзя прятаться в бригаде.");

        if (p.Building != null) 
          throw new ProcessException("Can't hide inside object.|Нельзя прятаться в объекте.");
      }
      p.Hide = (ord.Variant == HideOrder.Variants.Person);
      p.HideFaction = (ord.Variant == HideOrder.Variants.Faction);
    }

    private static void DoInstall(Person p, Order order) 
    {
      if (p.Leader != null) 
        throw new ProcessException(sOrderForLeader);
      if (p.Building == null) 
        throw new ProcessException(sNotInside);
      
      ItemType what = ((InstallOrder)order).What;
      Item material = p.Building.GetNeeds().GetByType(what);
			if (material == null) 
			{
				// Look in optional
				material = p.Building.Type.OptionalMaterials.GetByType(what);
				if (material == null)
					throw new ProcessException("Item can't be installed in this object.|Предмет нельзя установить в этот объект.");

				// Check if other optional material installed
				foreach (Item itm in p.Building.Type.OptionalMaterials)
					if (itm != material && p.Building.Installed.GetByType(itm.Type) != null)
						throw new ProcessException("Another optional component installed.|Уже установлен другой опциональный компонент.");

				// Check if material of this type already installed
				Item installed = p.Building.Installed.GetByType(what);
				if (installed != null) 
				{
					if (installed.Amount >= material.Amount)
						throw new ProcessException("Max amount of this item already installed.|Уже установлено максимальное количество.");
					material = new Item(material.Type, material.Amount - installed.Amount);
				}
			}

      // Check and remove installation material from team
      int amt = InstallMaterial(p, p.Building.Type, material);

      // Actually add material to building
      p.Building.Installed.AddItems(what, amt);

      p.AddEvent(String.Format("Installs {0}.|Установлено: {1}.", 
        what.ToString(amt, Lang.En), what.ToString(amt, Lang.Ru)));      
    }

    private static void DoKick(Person p, Order ord) 
    {
      if (p.Chosen)
        throw new ProcessException("Can't kick Chosen One.|Нельзя выгнать Избранного.");
      p.Faction.Events.Add(String.Format("{0} kicked from faction.|{1} исключается из фракции.", 
        p.ToString(Lang.En), p.ToString(Lang.Ru)));
      p.Leader = null;
      p.Faction = null;
    }

    private static void DoLeave(Person p, Order ord) 
    {
      if (p.Leader != null) 
      {
        p.AddEvent(sOrderForLeader);
        return;
      }
      if (p.Building == null) 
      {
        p.AddEvent(sNotInside);
        return;
      }
      p.AddEvent(String.Format("Leaves {0}.|Покидает {1}.", 
        p.Building.ToString(Lang.En), p.Building.ToString(Lang.Ru)));
      p.Building = null;
    }

    private static void DoMove(Person p, Order order) 
    {
      if (p.Leader != null) 
      {
        p.AddEvent(sOrderForLeader);
        return;
      }

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
            throw new ProcessException("Can't move there.|Туда не пройти.");

          #region Calculate remaining movement for first move
          if (!points_calculated) 
          {
            // Determine team speed
            mv = Movement.Ride;
            foreach (Person sub in p.GetTeamAndLeader()) 
            {
              while (mv >= Movement.Walk && sub.GetWeight() > sub.GetCapacity(mv)) mv--;
              if (mv < Movement.Walk) 
              {
                p.AddEvent("Person is overloaded.|Персонаж перегружен.");
                return;
              }
            }

            // Determine movement points
            points = ((int)mv + 1) * 2;
            points_calculated = true;
          }
          #endregion

          if (points < dest.PointsToEnter()) 
            throw new ProcessException("Insufficient movement points.|Закончились очки передвижения.");

					if (p.Building != null) 
					{
						p.AddEvent(String.Format("Leaves {0}.|Покидает {1}.", 
							p.Building.ToString(Lang.En), p.Building.ToString(Lang.Ru)));
						p.Building = null;
					}

					// Check if radiation is absolutely lethal
					if (dest.Radiation + p.RadiationModifier() >= p.Man.RadiationTo + 100)
            throw new ProcessException(String.Format("Tried to enter {0}, but feeled woozy and turned back.|Пытается войти в {1}, но чувствует слабость и тошноту и возвращается.",
							dest.ToString(Lang.En), dest.ToString(Lang.Ru)));

          CheckPatrolBlock(p, dest, ord.Attack);

          points -= dest.PointsToEnter();          
          p.AddEvent(String.Format("Moves from {0} to {1}.|Покидает {2}, входит в {3}.",
            p.Region.ToString(Lang.En), dest.ToString(Lang.En),
            p.Region.ToString(Lang.Ru), dest.ToString(Lang.Ru)));
          p.Region = dest;
        }
        else 
        {
          if ((int)d == 0) 
          {
            if (p.Building == null) 
              throw new ProcessException(sNotInside);

            p.AddEvent(String.Format("Leaves {0}.|Покидает {1}.", 
              p.Building.ToString(Lang.En), p.Building.ToString(Lang.Ru)));
            p.Building = null;
          }
          else 
          {
            Building b = p.Region.Buildings.GetByNumber((int)d);
            if (b == null) 
              throw new ProcessException(String.Format(sNotInRegion, d));

						if (b.Persons.Count > 0) 
						{
							Person owner = b.Persons[0];
							if (owner.AttitudeTo(p) < Attitude.Friendly)
								throw new ProcessException("Object owner is not Friendly.|Владелец объекта не относится к вам дружественно.");
						}

            p.AddEvent(String.Format("Enters {0}.|Входит в {1}.", 
              b.ToString(Lang.En), b.ToString(Lang.Ru)));
            p.Building = b;
          }
        }
      }
    }

    private static void CheckPatrolBlock(Person p, Region dest, bool attack) 
    {
      PersonArrayList persons = dest.Persons.Clone();
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
            throw new ProcessException(String.Format("Entry to {0} forbidden " +
              "by patrol: {1}.|Вход в {2} запрещён патрульным: {3}.",
              p2.ToString(Lang.En), dest.ToString(Lang.En),
              p2.ToString(Lang.Ru), dest.ToString(Lang.Ru)));
          else
            throw new ProcessException(String.Format("Entry to {2} forbidden " +
              "by patrol: {0}, {1}.|Вход в {5} запрещён патрульным: {3}, {4}.",
              p2.ToString(Lang.En), p2.Faction.ToString(Lang.En), dest.ToString(Lang.En),
              p2.ToString(Lang.Ru), p2.Faction.ToString(Lang.Ru), dest.ToString(Lang.Ru)));
        }
      }
    }

    private static void DoMaintain(Person p, Order ord) 
    {
      if (p.Leader != null) 
        throw new ProcessException(sOrderForLeader);

      if (p.Building == null) 
        throw new ProcessException(sNotInside);

      if (p.Building.Type.MaintainSkill != null) 
      {
        if (p.Skills.GetByType(p.Building.Type.MaintainSkill.Type) == null) 
          throw new ProcessException(sNoSkill);

        if (p.GetTeamLevel(p.Building.Type.MaintainSkill.Type) < 
          p.Building.Type.MaintainSkill.Level) 
          throw new ProcessException(sNotSkilledEnough);
      }

      p.AddEvent(String.Format("Maintains {0}.|Обслуживает {1}.", 
        p.Building.ToString(Lang.En), p.Building.ToString(Lang.Ru)));
      p.Maintaining = true;
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
          throw new ProcessException(sNotInside);

        if (p.Building.Persons[0] != p) 
          throw new ProcessException("Must own object to rename.|Нужно быть владельцем объекта.");

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
    }

    private static void DoPassword(Person p, Order order) 
    {
      string password = ((PasswordOrder)order).Password;
      p.Faction.Password = password;
      p.Faction.Events.Add("Password was changed.|Пароль изменён.");
    }

    private static void DoPatrol(Person p, Order order) 
    {
      if (p.Leader != null) 
        throw new ProcessException(sOrderForLeader);

      foreach (Person sub in p.GetTeamAndLeader()) 
      {
        sub.Avoiding = false;
        sub.Patrolling = true;
      }
      p.AddEvent(String.Format("Patrols in {0}.|Патрулирует {1}.", 
        p.Region.ToString(Lang.En), p.Region.ToString(Lang.Ru)));

      // Promote skill
      Soldier s = new Soldier(null, p, Side.Attacker);
      s.PromoteSkill();
    }
    
    private static void DoProduce(Person p, Order order) 
    {
      if (p.Leader != null) 
        throw new ProcessException(sOrderForLeader);

      ItemType it = ((ProduceOrder)order).ItemType;
      if (it.ProduceSkill == null) 
        throw new ProcessException("Can't produce that.|Предмет нельзя произвести.");

      if (p.Skills.GetByType(it.ProduceSkill.Type) == null) 
        throw new ProcessException(sNoSkill);

      if (it.ProduceBuilding != null) 
      {
        if (p.Building == null || p.Building.Type != it.ProduceBuilding)
          throw new ProcessException(String.Format("Must be inside {0}.|" +
            "Нужно быть внутри объекта: {1}.",
            it.ProduceBuilding.ToString(Lang.En), it.ProduceBuilding.ToString(Lang.Ru)));
        if (p.Building.GetNeeds().Count > 0)
          throw new ProcessException(String.Format("Object {0} is incomplete.|Объект {1} не достроен.",
            it.ProduceBuilding.ToString(Lang.En), it.ProduceBuilding.ToString(Lang.Ru)));
      }

      if (it.ProduceFrom1 == null) 
      {
        Item itm = p.Region.TurnResources.GetByType(it);
        if (itm == null || itm.Amount == 0)
          throw new ProcessException("No resource in region.|Ресурс отсутствует в регионе.");
      }

      int points = p.GetTeamLevel(it.ProduceSkill.Type) / it.ProduceSkill.Level;
      p.PromoteSkill(it.ProduceSkill.Type);
      if (points == 0) 
        throw new ProcessException(sNotSkilledEnough);

      int amt;
      if (it.ProductionRate >= 0)
        amt = points * it.ProductionRate;
      else
        amt = points / (-it.ProductionRate);

      if (it.ProduceFrom1 == null) 
      {
        // Produce resource from region
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
          throw new ProcessException(String.Format("Not enough {0}.|Требуется {1}.",
            it.ProduceFrom1.Type.ToString(1, Lang.En),
            it.ProduceFrom1.Type.ToString(1, Lang.Ru)));
        amt = Math.Min(amt, raw1 / it.ProduceFrom1.Amount);

        if (it.ProduceFrom2 != null) 
        {
          int raw2 = p.GetTeamAmount(it.ProduceFrom2.Type);
          if (raw2 < it.ProduceFrom2.Amount) 
            throw new ProcessException(String.Format("Not enough {0}.|Требуется {1}.",
              it.ProduceFrom2.Type.ToString(1, Lang.En),
              it.ProduceFrom2.Type.ToString(1, Lang.Ru)));
          amt = Math.Min(amt, raw2 / it.ProduceFrom2.Amount);
        }

        // Spend raw materials
        p.SpendFromTeam(it.ProduceFrom1.Type, amt * it.ProduceFrom1.Amount);
        if (it.ProduceFrom2 != null)
          p.SpendFromTeam(it.ProduceFrom2.Type, amt * it.ProduceFrom2.Amount);

        // Get produced items
        ItemType produced = it;
        if (it.ProduceAs != null)
          produced = it.ProduceAs;
        p.AddEvent(String.Format("Produces {0}.|Произведено: {1}.",
          it.ToString(amt, Lang.En), it.ToString(amt, Lang.Ru)));
        p.Items.AddItems(produced, amt);
				p.Faction.ShowItem(it);
      }
    }

    private static void DoQuit(Person p, Order order) 
    {
      // Find Chosen One
      Faction f = p.Faction;
      Person chosen = f.GetChosen();
      f.Events.Add(String.Format("{0} committed suicide. " +
        "Bones was lost in desert, but great deeds will stay in the " +
        "hearts of survived.|" +
        "{1} совершает самоубийство. " +
        "Кости потеряны в песках, но великие дела навсегда останутся в памяти выживших.",
        chosen.ToString(Lang.En), chosen.ToString(Lang.Ru)));

      // Kill Chosen One
      chosen.Kill();

      // Dismiss faction members
      while (f.Persons.Count > 0) 
      {
        f.Persons[0].Leader = null;
        f.Persons[0].Faction = Faction.Get(Constants.NPCFactionNum);
      }
    }

    private static void DoScavenge(Person p, Order order) 
    {
      if (p.Leader != null)
        throw new ProcessException(sOrderForLeader);
      ScavengeOrder ord = (ScavengeOrder)order;

      SkillType st = Constants.ScavengeSkill;
      if (p.Skills.GetByType(st) == null)
        throw new ProcessException(sNoSkill);

      if (ord.What.Count == 0)
        foreach (Item itm in p.Region.Junk)
          ord.What.Add(itm.Type);

      int points = p.GetTeamLevel(st);
      bool promote = false;
      foreach (ItemType it in ord.What) 
      {
        Item junk = p.Region.Junk.GetByType(it);
        if (junk == null)
          p.AddEvent("No such item in junk.|В мусоре этого нет.");

        promote = true;

        ProduceRequestItem pri = new ProduceRequestItem();
        pri.Person = p;
        pri.ItemType = it;
        pri.Amount = points;
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
      else
      {
        /* Use #mail directive instead.
				// SHOW faction
        Faction f = Faction.Get(ord.FactionNum);
        if (f == null) 
          throw new ProcessException("Faction not exists.|Нет такой фракции.");

        p.Faction.Events.Add(String.Format("The address of {0} is {1}.|Адрес {2}: {1}.",
          f.ToString(Lang.En), f.Email, f.ToString(Lang.Ru)));
				*/
      }
    }

    private static void DoTeam(Person p, Order order) 
    {
      TeamOrder ord = (TeamOrder)order;
      if (ord.Kick) 
      {
        // Kick person from team
        Person sub = p.Team.GetByNumber(ord.LeaderNum);
        if (sub == null)
          throw new ProcessException("KICK: No such person in team.|KICK: В команде нет указанного персонажа.");
        sub.Leader = null;
        sub.AddEvent("Kicked from team.|Командир выгнал персонажа из бригады.");
      }
      else 
      {
        if (ord.LeaderNum != 0) 
        {
          // Join team
          Person leader = p.Region.Persons.GetByNumber(ord.LeaderNum);
          if (leader == null) 
            throw new ProcessException(String.Format(sNotInRegion, ord.LeaderNum));

          if (leader.Leader != null) 
            throw new ProcessException(String.Format("{0} is in team.|{1} тоже находится в бригаде.",
              leader.ToString(Lang.En), leader.ToString(Lang.Ru)));

          if (leader == p) 
            throw new ProcessException("Can't join himself.|Не может присоединиться сам к себе.");

          if (leader.AttitudeTo(p) < Attitude.Friendly) 
            throw new ProcessException("Can't join to non-Friendly person.|Этот персонаж не относится к вам дружественно.");

          p.Leader = leader;
          leader.AddEvent(String.Format("{0} joined team.|{1} присоединяется к бригаде.",
            p.ToString(Lang.En), p.ToString(Lang.Ru)));
        }
        else 
        {
          // Leave team
          if (p.Leader == null) 
            throw new ProcessException("Has no leader.|Нет командира.");

          p.Leader.AddEvent(String.Format("{0} leaved team.|{1} покидает бригаду.",
            p.ToString(Lang.En), p.ToString(Lang.Ru)));
          p.Leader = null;
        }
      }
    }

    private static void DoTrade(Person p, Order order) 
    {
      TradeOrder ord = (TradeOrder)order;

      Item sell_item = p.Items.GetByType(ord.SellWhat);
      if (sell_item == null || sell_item.Amount < ord.SellAmount)
        throw new ProcessException(String.Format("Not enough {0} to offer.|Недостаточно предметов: {1}.",
          ord.SellWhat.ToString(1, Lang.En), ord.SellWhat.ToString(1, Lang.Ru)));

      if (ord.PersonNum == 0) 
      {
        foreach (Person seller in p.Region.Persons) 
          TradeWith(p, seller, ord, sell_item);
      }
      else 
      {
        Person seller = p.Region.Persons.GetByNumber(ord.PersonNum);
        if (seller == null)
          throw new ProcessException(String.Format(sNotInRegion, ord.PersonNum));
        TradeWith(p, seller, ord, sell_item);
      }

      if (ord.BuyAmount > 0 && ord.SellAmount > 0)
        p.TradeOrder = ord;
    }

    private static void DoUninstall(Person p, Order order) 
    {
      if (p.Leader != null) 
        throw new ProcessException(sOrderForLeader);
      if (p.Building == null) 
        throw new ProcessException(sNotInside);
      UninstallOrder ord = (UninstallOrder)order;
      
			if (p.Building.Persons[0] != p) 
				throw new ProcessException("Must own object.|Нужно быть владельцем объекта.");
			
			Item material = p.Building.Installed.GetByType(ord.What);
      if (material == null)
        throw new ProcessException(String.Format("No materal installed.|Такого материала в объекте нет."));

      // Check skill level
      if (p.Skills.GetByType(material.Type.InstallSkill.Type) == null) 
        throw new ProcessException(sNoSkill);

      int points = p.GetTeamLevel(material.Type.InstallSkill.Type);
      p.PromoteSkill(material.Type.InstallSkill.Type);

      int amt = Math.Min(points, material.Amount);
      if (ord.Amount >= 0)
        amt = Math.Min(amt, ord.Amount);

      // Actually remove materials
      p.Building.Installed.RemoveItems(ord.What, amt);
      p.Items.AddItems(ord.What, amt);

      // If first material or all materials removed, collapse
      if ((p.Building.Type.Materials.Count > 0
        && p.Building.Installed.GetByType(p.Building.Type.Materials[0].Type) == null)
				|| p.Building.Installed.Count == 0) 
      {
        p.AddEvent(String.Format("Uninstalls {0}. Object collapsed.|" +
          "Демонтировано: {1}. Объект разрушен.",
          ord.What.ToString(amt, Lang.En), ord.What.ToString(amt, Lang.Ru))); 
        p.Building.Collapse();
      }
      else
        p.AddEvent(String.Format("Uninstalls {0}.|Демонтировано: {1}.",
          ord.What.ToString(amt, Lang.En), ord.What.ToString(amt, Lang.Ru))); 
      p.Faction.ShowItem(ord.What);
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

      p.AddEvent(String.Format("Trades {0} for {1} with {2}.|Персонажу {5} отдано: {3} за: {4}.",
        ord.SellWhat.ToString(amt_sell, Lang.En),
        ord.BuyWhat.ToString(amt_buy, Lang.En),
        seller.ToString(Lang.En),
        ord.SellWhat.ToString(amt_sell, Lang.Ru),
        ord.BuyWhat.ToString(amt_buy, Lang.Ru),
        seller.ToString(Lang.Ru)));
      
      seller.AddEvent(String.Format("Trades {0} for {1} with {2}.|Персонажу {5} отдано: {3} за: {4}.",
        ord.BuyWhat.ToString(amt_buy, Lang.En),
        ord.SellWhat.ToString(amt_sell, Lang.En),
        p.ToString(Lang.En),
        ord.BuyWhat.ToString(amt_buy, Lang.Ru),
        ord.SellWhat.ToString(amt_sell, Lang.Ru),
        p.ToString(Lang.Ru)));
    }

    private static void ResolveBabies() 
    {
      int i = Person.List.Count;
      while (i > 0) 
      {
        i--;
        Person p = Person.List[i];
				if (p.Killed) continue;
        ItemType woman = p.Man;
        if (woman == null || woman.Baby == null)
          continue;

        // If already has a baby, decrease timer. 
        if (p.BabyTimer > 0) 
        {
          p.BabyTimer--;
          if (p.BabyTimer == 0) 
          {
            // Born the baby
            p.Items.RemoveItems(woman.Baby, 1);

            ItemType grown_to = woman.Baby.GrowTo[Constants.Random(woman.Baby.GrowTo.Count)];

            Person child = new Person(p.Faction, p.Region);
            child.Building = p.Building;
            child.Leader = p.Leader;
						child.Age = 0;
						if (grown_to == p.Man)
							child.Name = NameGenerator.Name(grown_to.Name, p.Name);
						else
							child.Name = NameGenerator.Name(grown_to.Name, p.FatherName);
						p.FatherName = "";
						child.Items.AddItems(grown_to, 1);
            if (p.FatherSkill != null)
              child.AddSkill(p.FatherSkill);
						if (p.Skills.Count > 0) 
						{
							int sk_idx = Constants.Random(p.Skills.Count);
							for (int j = sk_idx; j < p.Skills.Count; j++) 
							{
								Skill sk = p.Skills[j];
								if (sk.Type.BasedOn == null && sk.Type != p.FatherSkill)
									child.AddSkill(sk.Type);
							}
							if (child.Skills.Count <= 1)
								for (int j = 0; j < sk_idx; j++) 
								{
									Skill sk = p.Skills[j];
									if (sk.Type.BasedOn == null && sk.Type != p.FatherSkill)
										child.AddSkill(sk.Type);
								}
						}

            p.AddEvent(String.Format("Born the baby: {0}, {1}.|Рожает ребёнка: {2}, {3}.",
              grown_to.ToString(1, Lang.En), child.ToString(Lang.En),
              grown_to.ToString(1, Lang.Ru), child.ToString(Lang.Ru)));
          }
          continue;
        }
        
        // If not, find a partner in team
        if (p.Age > Constants.ChildTurns 
					&& Constants.Random(100) < Constants.SexPercent)
        {
          Person leader = null;
          if (p.Leader != null)
            leader = p.Leader;
          else if (p.Team.Count > 0)
            leader = p;
          if (leader == null)
            continue;

          foreach (Person pal in leader.GetTeamAndLeader()) 
          {
            ItemType man = pal.Man;
            if (man == null || woman.BabyFrom != man || pal.Age <= Constants.ChildTurns) 
              continue;
            
            // Found a father, create baby
            p.Items.AddItems(woman.Baby, 1);
            p.BabyTimer = woman.BabyTimer;
						if (pal.Skills.Count > 0) 
						{
							int sk_idx = Constants.Random(pal.Skills.Count);
							for (int j = sk_idx; j < pal.Skills.Count; j++) 
							{
								Skill sk = pal.Skills[j];
								if (sk.Type.BasedOn == null)
									p.FatherSkill = sk.Type;
							}
							if (p.FatherSkill == null)
								for (int j = 0; i < sk_idx; j++) 
								{
									Skill sk = p.Skills[j];
									if (sk.Type.BasedOn == null)
										p.FatherSkill = sk.Type;
								}
						}
						p.FatherName = pal.Name;

            p.AddEvent(String.Format("Has baby from {0}.|Заводит ребёнка от персонажа {1}.",
              pal.ToString(Lang.En), pal.ToString(Lang.Ru)));
            if (pal.Faction != p.Faction)
              pal.AddEvent(String.Format("Had sex with {0}.|Спит с персонажем {1}.",
                p.ToString(Lang.En), p.ToString(Lang.Ru)));

            // Lower Insanity of both parents
            p.Insanity -= Constants.SexInsanityDecrease;
            pal.Insanity -= Constants.SexInsanityDecrease;
            return;
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
				p.AddEvent("Persons in team are quarrelling.|Персонажи в бригаде ссорятся.");
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
            pri.Person.AddEvent(String.Format("Produces {0}.|Произведено: {1}.",
              pri.ItemType.ToString(share, Lang.En), pri.ItemType.ToString(share, Lang.Ru)));
						pri.Person.Faction.ShowItem(pri.ItemType);
          }
        }
      }
    }

    private static void ResolveScavengeRequests() 
    {
      // Distribute resource evenly between producing teams
      foreach (Region r in Map.Regions) 
      {
        Hashtable completed = new Hashtable();

        int i = r.Junk.Count-1;
        while (i >= 0)
        {
          Item itm = r.Junk[i];
          
          int startingAmount = itm.Amount;
          ArrayList scavenge_requests = new ArrayList();
          foreach (ProduceRequestItem pri in ScavengeRequests) 
          {
            if (pri.Person.Region == r && pri.ItemType == itm.Type)
              scavenge_requests.Add(pri);
          }

          int total = 0;
          foreach (ProduceRequestItem pri in scavenge_requests) 
          {
            if (completed.ContainsKey(pri.Person.Num))
              total += pri.Amount - (int)completed[pri.Person.Num];
            else
              total += pri.Amount;
          }

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
							itm.Amount -= share;
							if (itm.Amount == 0)
								r.Junk.Remove(itm);
							pri.Person.AddEvent(String.Format("Scavenges {0}.|Найдено в мусоре: {1}.",
								pri.ItemType.ToString(share, Lang.En),
								pri.ItemType.ToString(share, Lang.Ru)));
							pri.Person.Faction.ShowItem(pri.ItemType);
						}
          }

          i--;
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
          
          int hunger = GetFood(p, p, Constants.RationsPerMonth, AskType.Anything);
          if (hunger > 0)
            hungry.Add(p.Num, hunger);
        }
        
        // Then, starting to borrow food
        int[] keys = new int[hungry.Keys.Count];
        hungry.Keys.CopyTo(keys, 0);
        foreach (int num in keys)
        {
          Person p = r.Persons.GetByNumber(num);
          int hunger = (int)hungry[num];
					
					// Ask preferred
          foreach (Person pal in r.Persons) 
          {
            if (pal != p && pal.AttitudeTo(p) == Attitude.Ally)
	            hunger = GetFood(pal, p, hunger, AskType.Preferred);
            if (hunger == 0)
              break;
          }

					// Ask anything else
					if (hunger > 0) 
					{
						foreach (Person pal in r.Persons) 
						{
							if (pal != p && pal.AttitudeTo(p) == Attitude.Ally)
								hunger = GetFood(pal, p, hunger, AskType.DefaultSequence);
							if (hunger == 0)
								break;
						}
					}

          if (hunger > 0)
            hungry[num] = hunger;
          else
            hungry.Remove(num);
        }

        // Then, starve some men
        foreach (int num in hungry.Keys) 
        {
          Person p = r.Persons.GetByNumber(num);
          if (Constants.Random(100) < (int)hungry[num]) 
          {
            if (p.Faction != null)
              p.AddEvent("Starved to death.|Умер от истощения.");
            p.Kill();
          }
          else
          {
            p.AddEvent(String.Format("Hungry, needs {0} more rations.|Голодает, нужно ещё {0} рационов.",
              hungry[num].ToString()));
            p.Insanity++;
          }
        }
      }
    }

    private static int GetFood(Person p, Person eater, int hunger, AskType ask) 
    {
      if (eater.Man == null)
        return 0;

      // Form list of food wanted by eater
      ItemArrayList food = new ItemArrayList();
			if (ask == AskType.Anything || ask == AskType.Preferred) 
			{
				foreach (ItemType it in eater.Consume) 
				{
					if (it.Rations == 0)
						continue;
					Item itm = p.Items.GetByType(it);
					if (itm == null)
						continue;
					int rations_taken = Math.Min(hunger, itm.Type.Rations * itm.Amount);
					int items_taken = (int)Math.Ceiling((float)rations_taken / (float)itm.Type.Rations);
					if (items_taken == 0)
						continue;
					food.Add(new Item(itm.Type, items_taken));
					hunger -= rations_taken;
				}
			}

      // All other food
      if (hunger > 0 && (ask == AskType.Anything || ask == AskType.DefaultSequence)) 
      {
				foreach (ItemType it in eater.Man.Food) 
				{
					Item itm = p.Items.GetByType(it);
					if (itm == null)
						continue;
					if (itm.Type.Rations == 0) 
						continue;
					int rations_taken = Math.Min(hunger, itm.Type.Rations * itm.Amount);
					int items_taken = (int)Math.Ceiling((float)rations_taken / (float)itm.Type.Rations);
					if (items_taken == 0)
						continue;
					food.Add(new Item(itm.Type, items_taken));
					hunger -= rations_taken;
				}
      }
      if (food.Count == 0)
        return hunger;

      // Consume the food
      foreach (Item itm in food)
        p.Items.RemoveItems(itm.Type, itm.Amount);

      if (eater == p)
        eater.AddEvent(String.Format("Consumes {0}|Съедено: {1}",
          food.ToString(Lang.En), food.ToString(Lang.Ru)));
      else
        eater.AddEvent(String.Format("Borrows from {0} {1}|{2} одалживает: {3}",
          p.ToString(Lang.En), food.ToString(Lang.En),
          p.ToString(Lang.Ru), food.ToString(Lang.Ru)));
      
      return hunger;
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
              p.AddEvent(String.Format("Mutated to {0}!|Мутирует - теперь это {1}!",
                p.Man.MutateTo.ToString(1, Lang.En), p.Man.MutateTo.ToString(1, Lang.Ru)));
              Item itm = p.Items.GetByType(p.Man);
              itm.Type = p.Man.MutateTo;
              p.Insanity = 10;
            }
            else 
            {
              // If no mutation, kill
              if (p.Faction.Num == Constants.NPCFactionNum)
                continue;
              if (dose < 0)
                p.AddEvent("Died from lack of radiation.|Умирает от недостатка радиации.");
              else
                p.AddEvent("Died from lethal dose of radiation.|Умирает от летальной дозы радиации.");
              p.Kill();
              continue;
            }
          }
          else 
          {
            if (dose < 0)
              p.AddEvent("Not receiving a vital dose of radiation.|Не получает необходимой дозы радиации.");
            else
              p.AddEvent("Receiving a dose of radiation.|Получает дозу радиации.");
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
          int tempr = p.TemperatureDanger(false);
          if (tempr <= 0)
            continue;

          int need = GetBurn(p, p, tempr, AskType.Anything);
          if (need > 0)
            needing.Add(p.Num, need);
        }
        
        // Then, starting to borrow food
        int[] keys = new int[needing.Keys.Count];
        needing.Keys.CopyTo(keys, 0);
        foreach (int num in keys)
        {
          Person p = r.Persons.GetByNumber(num);
          int need = (int)needing[num];

					// Get preferred
          foreach (Person pal in r.Persons) 
          {
            if (pal != p && pal.AttitudeTo(p) == Attitude.Ally)
              need = GetBurn(pal, p, need, AskType.Preferred);
            if (need == 0)
              break;
          }

					// Get by default sequence
					if (need > 0) 
					{
						foreach (Person pal in r.Persons) 
						{
							if (pal != p && pal.AttitudeTo(p) == Attitude.Ally)
								need = GetBurn(pal, p, need, AskType.DefaultSequence);
							if (need == 0)
								break;
						}
					}

          if (need > 0)
            needing[num] = need;
          else
            needing.Remove(num);
        }

        // Then, freeze some men
        foreach (int num in needing.Keys) 
        {
          Person p = r.Persons.GetByNumber(num);
          if (Constants.Random(100) < (int)needing[num]) 
          {
            if (p.Faction != null)
              p.AddEvent("Frozen to death.|Умирает от переохлаждения.");
            p.Kill();
          }
          else
          {
            p.AddEvent(String.Format("Suffers from cold.|Страдает от холода.",
              needing[num].ToString()));
            p.Insanity++;
          }
        }
      }
    }

    private static int GetBurn(Person pal, Person p, int need, AskType ask) 
    {
      ItemArrayList burned = new ItemArrayList();

      // Get preferred items
			if (ask == AskType.Anything || ask == AskType.Preferred) 
			{
				foreach (ItemType it in p.Burn) 
				{
					if (!it.Burn)
						continue;
					Item itm = pal.Items.GetByType(it);
					if (itm == null || !it.Burn)
						continue;
					int taken = Math.Min(itm.Amount, need);
					if (taken > 0)
						burned.Add(new Item(itm.Type, taken));
					need -= taken;
				}
			}

      if (need > 0 && (ask == AskType.Anything || ask == AskType.DefaultSequence)) 
      {
        // Get all other
				foreach (ItemType it in ItemType.List) 
				{
					Item itm = pal.Items.GetByType(it);
					if (itm == null)
						continue;
					if (!itm.Type.Burn)
						continue;
					int taken = Math.Min(itm.Amount, need);
					if (taken > 0)
						burned.Add(new Item(itm.Type, taken));
					need -= taken;
				}
      }

      if (burned.Count > 0) 
      {
        foreach (Item itm in burned)
          pal.Items.RemoveItems(itm.Type, itm.Amount);
        if (pal == p)
          p.AddEvent(String.Format("Burned {0}|Сжигает: {1}",
            burned.ToString(Lang.En), burned.ToString(Lang.Ru)));
        else
          p.AddEvent(String.Format("Borrows from {2} {0}|{3} одалживает: {1}",
            burned.ToString(Lang.En), burned.ToString(Lang.Ru),
            pal.ToString(Lang.En), pal.ToString(Lang.Ru)));
      }
      return need;
    }

    private static int InstallMaterial(Person p, BuildingType bt, Item material) 
    {
      // Check skill level
      if (p.Skills.GetByType(material.Type.InstallSkill.Type) == null) 
        throw new ProcessException(sNoSkill);

      int points = p.GetTeamLevel(material.Type.InstallSkill.Type);
      if (points < material.Type.InstallSkill.Level) 
        throw new ProcessException(sNotSkilledEnough);

      // Check material amount
      int having = p.GetTeamAmount(material.Type);
      if (having == 0) 
        throw new ProcessException("No material to build that.|Нет неободимого материала.");

      int amt = Math.Min(material.Amount, Math.Min(having, 
				points / material.Type.InstallSkill.Level));

      // Spend some materials
      p.SpendFromTeam(material.Type, amt);

      p.PromoteSkill(material.Type.InstallSkill.Type);

      return amt;
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
          p.AddEvent("Goes insane and leaves faction.|" +
            "Сходит с ума и убегает из фракции.");
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
        if (Constants.Random(100) >= Constants.WandererMoveChance)
          continue;

        Direction dir = Direction.None;
        
        // Wanderers will like places with less man
        int most_like = -p.Region.Persons.Count;
        // And more comfortable radiation
        if (p.Man != null && p.Man.RadiationFrom == 0)
          most_like -= p.Region.Radiation;
        else 
          most_like += p.Region.Radiation;

        for (Direction i = Direction.North; i <= Direction.Northwest; i++) 
        {
          Region n = p.Region.RegionInDir(i);
          if (n == null || !n.Terrain.Walking || n.Radiation > p.Man.RadiationTo)
            continue;
          int like = -n.Persons.Count;
          if (p.Man != null && p.Man.RadiationFrom == 0)
            like -= n.Radiation;
          else 
            like += n.Radiation;

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
        PersonArrayList persons = new PersonArrayList();
        foreach (Person p in r.Persons)
          if (p.Leader == null && !p.Killed)
            persons.Add(p);
        foreach (Person p in persons) 
        {
          if (p.Killed || p.Faction.IsNPC && Constants.Random(100) > p.Man.Aggression)
            continue;
          foreach (Person target in r.Persons)
            if (p.AttitudeTo(target) == Attitude.Hostile) 
            {
              Battle b = new Battle(p, target, target.Region);
              break;
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

    private static void DismissFactionsWithoutChosen() 
    {
      foreach (Faction f in Faction.List) 
      {
        if (f.IsNPC || f.GetChosen() != null)
          continue;
        for (int i = f.Persons.Count-1; i >= 0; i--)
          f.Persons[i].Faction = Faction.Get(Constants.NPCFactionNum);
        f.Events.Add("Faction lost Chosen One and was dismissed.|Фракция потеряла Избранного и распалась.");
      }
    }
  }

	enum AskType 
	{
		Anything,
		DefaultSequence,
		Preferred
	}

  class ProcessException : Exception 
  {
    public ProcessException(string msg) : base(msg) {}
  }
}