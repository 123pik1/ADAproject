
--Autorzy:
--Paweł Rietz
--Paweł Narwojsz
--Piotr Kaczorowski


with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;
with Ada.Numerics.Float_Random;



procedure Simulation is

   


   ----GLOBAL VARIABLES---
   Number_Of_Items: constant Integer := 5;
   Number_Of_Producers: constant Integer := 5;
   Number_Of_Assemblies: constant Integer := 3;
   Number_Of_Customers: constant Integer := 3;
   
   subtype Item_Type is Integer range 1 .. Number_Of_Items;
   subtype Assembly_Type is Integer range 1 .. Number_Of_Assemblies;
   subtype Customer_Type is Integer range 1 .. Number_Of_Customers;
   subtype Godness_Type is Integer range 1 .. 10;

   --each Producer is assigned an Item to produce
   Item_Name: constant array (Item_Type) of String(1 .. 7)
      := ("Pasta  ", "Buns   ", "Tomato ", "Beef   ", "Fries  ");

   --Assembly is a collection of Items (It's best if the Items overlap, so there's less risk of a deadlock)
   Assembly_Name: constant array (Assembly_Type) of String(1 .. 9)
     := ("Bolognese", "Steak    ", "Hamburger");

   ----TASK DECLARATIONS----

   -- Producer produces determined Item
   task type Producer is
      entry Start(Item: in Item_Type; Item_Production_Time: in Integer);
   end Producer;

   -- Customer gets an arbitrary assembly of several Items from the buffer
   -- but he/she orders it randomly
   task type Customer is
      entry Start(Customer_Number: in Customer_Type;
                  Consumption_Time: in Integer);
   end Customer;



   -- Buffer receives Items from Producers and delivers Assemblies to Customers
   task type Buffer is
      -- Accept a Item to the storage (provided there is room for it)
      entry Take(Item: in Item_Type; Number: in Integer; Accepted: out Boolean);
      -- Deliver an assembly (provided there are enough Items for it)
      entry Deliver(Assembly: in Assembly_Type; Number: out Integer);
      -- Accept a noble gift
      entry Noble_gift;
   end Buffer;

   task type Charity_Event is
      entry Noble_gift;
      entry Start;
   end Charity_Event;

   P: array ( 1 .. Number_Of_Producers ) of Producer;
   K: array ( 1 .. Number_Of_Customers ) of Customer;
   B: Buffer;
   N: Charity_Event;



   ----TASK DEFINITIONS----

   --Charity_Event--
 task body Charity_Event is
   package Random_Godness is new Ada.Numerics.Discrete_Random(Godness_Type);
   G: Random_Godness.Generator;
   godness_of_heart_level: Godness_Type;

   
   begin
      Put_Line(ESC & "[92m" & "Charity_Event: Started" & ESC & "[0m");
         Random_Godness.Reset(G);
      loop
         select
            accept Noble_gift do
               B.Noble_gift;
               delay 1.0;
               requeue Start; --requeue do samego siebie
            end Noble_gift;

         or 
            accept Start do
               delay 2.0;
               godness_of_heart_level := Random_Godness.Random(G);
               Put_Line(ESC & "[92m" & "Charity_Event: Godness of heart level: " & Integer'Image(godness_of_heart_level) & ESC & "[0m");
               if godness_of_heart_level >7 then
                     delay 1.0;
                     requeue Noble_gift; --requeue do samego siebie
                  else 
                     delay 1.0;
                     requeue Start; --requeue do samego siebie
               end if;
            
            end Start;
         end select;
      end loop;
   end Charity_Event;


   --Producer--

   task body Producer is
      subtype Item_Production_Time_Range is Integer range 1 .. 3;
      package Random_Ingrediention is new Ada.Numerics.Discrete_Random(Item_Production_Time_Range);
      --  random number generator
      G: Random_Ingrediention.Generator;
      Item_Type_Number: Integer;
      Item_Number: Integer;
      Ingrediention: Integer; -- kolejna useless zmienna???
      Random_Time: Duration;
      Accepted_by_Buffer: Boolean;
   begin
      accept Start(Item: in Item_Type; Item_Production_Time: in Integer) do
         --  start random number generator
         Random_Ingrediention.Reset(G);
         Item_Number := 1;
         Item_Type_Number := Item;
         Ingrediention := Item_Production_Time;
      end Start;
      Put_Line(ESC & "[93m" & "P: Started Production of: " & Item_Name(Item_Type_Number) & ESC & "[0m");
      loop
         Random_Time := Duration(Random_Ingrediention.Random(G));
         delay Random_Time;
         Put_Line(ESC & "[93m" & "P: Produced Item " & Item_Name(Item_Type_Number)
                  & " number "  & Integer'Image(Item_Number) & ESC & "[0m");
         
         -- Accept for storage
         B.Take(Item_Type_Number, Item_Number, Accepted_by_Buffer);
         
         if Accepted_by_Buffer then
            Item_Number := Item_Number + 1;
         else
            Put_Line(ESC & "[93m" & "P: Item " & Item_Name(Item_Type_Number) &
                  " number " & Integer'Image(Item_Number) & " was rejected by Buffer" & ESC & "[0m");
         end if;
      end loop;
   end Producer;


   --Customer--

   task body Customer is
      subtype Consumption_Time_Range is Integer range 4 .. 8;
      package Random_Consumption is new
        Ada.Numerics.Discrete_Random(Consumption_Time_Range);

      --each Customer takes any (random) Assembly from the Buffer
      package Random_Assembly is new
        Ada.Numerics.Discrete_Random(Assembly_Type);

      G: Random_Consumption.Generator;
      GA: Random_Assembly.Generator;
      Customer_Nb: Customer_Type;
      Assembly_Number: Integer;
      Consumption: Integer; -- useless zmienna xD??
      Assembly_Type: Integer;
      Consumer_Name: constant array (1 .. Number_Of_Customers)
        of String(1 .. 9)
        := ("Customer1", "Customer2", "Customer3");
   begin
      accept Start(Customer_Number: in Customer_Type;
                   Consumption_Time: in Integer) do
         Random_Consumption.Reset(G);
         Random_Assembly.Reset(GA);
         Customer_Nb := Customer_Number;
         Consumption := Consumption_Time;
      end Start;
      Put_Line(ESC & "[96m" & "C: Customer named " & Consumer_Name(Customer_Nb)  & " enters restaurant");
      loop
         delay Duration(Random_Consumption.Random(G)); --  simulate consumption
         Assembly_Type := Random_Assembly.Random(GA);
         
         -- Jesli nie mogl dostac dania to Assembly_number ustawi na 0
         B.Deliver(Assembly_Type, Assembly_Number);
         
         if Assembly_Number /= 0 then 
            Put_Line(ESC & "[96m" & "C: " & Consumer_Name(Customer_Nb) & " takes dish " &
                    Assembly_Name(Assembly_Type) & " number " &
                       Integer'Image(Assembly_Number) & ESC & "[0m");
         else
            Put_Line(ESC & "[96m" & "C: " & Consumer_Name(Customer_Nb) & " could not get dish " &
                       Assembly_Name(Assembly_Type) & " due to lack of ingredients" & ESC & "[0m");
         end if;
           
      end loop;
   end Customer;


   --Buffer--

   task body Buffer is


      




      Storage_Capacity: constant Integer := 30;
      type Storage_type is array (Item_Type) of Integer;
      Storage: Storage_type
        := (0, 0, 0, 0, 0);
      Assembly_Content: array(Assembly_Type, Item_Type) of Integer
        := ((1, 0, 2, 1, 0), --skladniki Bolognese
            (0, 0, 0, 3, 1), --skladniki Steak
            (0, 2, 1, 1, 1)); --skladniki Hamburger
      Noble : Charity_Event; -- deklaracja Charity_Event
      -- 1-Pasta 2-Buns 3-Tomato 4-Beef 5-Fries
      -- RECIPES :    
      -- Bolognese: 1 Pasta, 1 Beef, 2 Tomato
      -- Steak: 3 Beef, 1 Fries
      -- Hamburger: 2 Buns, 1 Tomato, 1 Beef, 1 Fries 
      
      Max_Assembly_Content: array(Item_Type) of Integer; --tablica przechowujaca max ilosc kazdego skladnika ktora moze byc potrzebna zeby cos ugotowac
      Assembly_Number: array(Assembly_Type) of Integer
        := (1, 1, 1);
      In_Storage: Integer := 0;


      procedure Complete_Noble_Gift is
      begin
         for I in 1 .. Number_Of_Producers loop
            Storage(I) := Storage(I) /2;
         end loop;
         Put_Line(ESC & "[92m" & "Charity_Event: Resources distributed" & ESC & "[0m");
      end Complete_Noble_Gift;


      procedure Setup_Variables is
      begin
         
         for W in Item_Type loop
            Max_Assembly_Content(W) := 0;
            for Z in Assembly_Type loop
               if Assembly_Content(Z, W) > Max_Assembly_Content(W) then
                  Max_Assembly_Content(W) := Assembly_Content(Z, W);
               end if;
            end loop;
         end loop;
      end Setup_Variables;

      function Can_Accept(Item: Item_Type) return Boolean is --czy moze przyjac skladnik
         Free_Space: Integer;
         Result: Boolean;
         Lacking: array (Item_Type) of Integer;
         Lacking_Space: Integer; 
         
      begin
         if In_Storage >= Storage_Capacity then
            return False;
         end if;
         Free_Space := Storage_Capacity - In_Storage;
         
         for I in Item_Type loop -- jesli ilosc kazdego skladnika jest wystarczajaca zeby utworzyc dowolne danie, to funkcja zwraca True
            if Storage(I) < Max_Assembly_Content(I) then
               Result := False;
            end if;
         end loop;
         
         if Result then
            return True;
         end if;
         
         if Max_Assembly_Content(Item) > Storage(Item) then
            return True;
         end if;
         
         -- Oblicza ile miejsca jest potrzebne do przechowania brakujacych skladnikow, ktore sa ponizej swojego maksymalnego limitu
         Lacking_Space := 1;
         for I in Item_Type loop 
            Lacking(I) := Integer'Max (0, Max_Assembly_Content(I) - Storage(I)); -- Integer'Max(0,...) zeby nie pojawilo sie Lacking(I) ujemne
            Lacking_Space := Lacking_Space + Lacking(I);
         end loop;
         
         if Free_Space >= Lacking_Space then
            return True;
         else
            return False;
         end if;
           
      end Can_Accept;

      function Can_Deliver(Assembly: Assembly_Type) return Boolean is --czy danie o numerze Assembly moze byc utworzone
      begin
         for W in Item_Type loop
            if Storage(W) < Assembly_Content(Assembly, W) then
               return False;
            end if;
         end loop;
         return True;
      end Can_Deliver;

      procedure Storage_Contents is --wypisz zawartosc magazynu
      begin
         for W in Item_Type loop
            Put_Line("|   Storage contents: " & Integer'Image(Storage(W)) & " "
                     & Item_Name(W));
         end loop;
         Put_Line("|   Number of Items in storage: " & Integer'Image(In_Storage));

      end Storage_Contents;


   begin
      Put_Line(ESC & "[91m" & "B: Buffer started" & ESC & "[0m");
      Setup_Variables;
      loop
        
         select
            --przyjmuje lub odrzuca ugotowany skladnik
            accept Take(Item: in Item_Type; Number: in Integer; Accepted: out Boolean) do
               if Can_Accept(Item) then --przyjeto skladnik
                  Put_Line(ESC & "[91m" & "B: Accepted Item " & Item_Name(Item) & " number " &
                             Integer'Image(Number)& ESC & "[0m");
                  Storage(Item) := Storage(Item) + 1;
                  In_Storage := In_Storage + 1;
                  Accepted := True;
               else --skladnik odrzucony
                  Put_Line(ESC & "[91m" & "B: Rejected Item " & Item_Name(Item) & " number " &
                             Integer'Image(Number)& ESC & "[0m");
                  Accepted := False; 
               end if;
            end Take;
            Storage_Contents;
         or
              --(nie)dostarcza wybrane danie
            accept Deliver(Assembly: in Assembly_Type; Number: out Integer) do
               if Can_Deliver(Assembly) then
                  Put_Line(ESC & "[91m" & "B: Delivered dish " & Assembly_Name(Assembly) & " number " &
                             Integer'Image(Assembly_Number(Assembly))& ESC & "[0m");
                  

                  for W in Item_Type loop
                     Storage(W) := Storage(W) - Assembly_Content(Assembly, W);
                     In_Storage := In_Storage - Assembly_Content(Assembly, W);
                  end loop;
                  
                  -- Zwroc numer dostarczonego dania
                  Number := Assembly_Number(Assembly);
                  
                  -- Zwieksz licznik dla tego typu dania
                  Assembly_Number(Assembly) := Assembly_Number(Assembly) + 1;
               else
                  -- Komunikat o braku skladnikow
                  Put_Line(ESC & "[91m" & "B: Lacking Items for dish " & Assembly_Name(Assembly)& ESC & "[0m");
                  
                  -- Zwroc 0, by oznaczyc, ze nie dostarczono dania
                  Number := 0;
               end if;
            end Deliver;
            Storage_Contents;
         or 
            accept Noble_gift do
               Complete_Noble_Gift;
            end Noble_gift;
         or
            delay 2.0; --wejdzie tu jak w ciagu 1 sek nie dostanie Buffer Take albo Buffer Deliver, potem select jest od nowa
            Put_Line(".........Jestem w select or delay 1.0......."); 
            delay 10.0;
            Put_Line("Odczekalem 10sek delayu w select or delay 1.0.....zaczynam selecta od nowa");
         end select;

      end loop;
   end Buffer;


   
  
   ---"MAIN" FOR SIMULATION---
begin
   
   for I in 1 .. Number_Of_Producers loop
      P(I).Start(I, 10);
   end loop;
   for J in 1 .. Number_Of_Customers loop
      K(J).Start(J,12);
   end loop;
   N.Start;
end Simulation;


