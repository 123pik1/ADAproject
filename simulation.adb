
--Autorzy:
--Paweł Rietz
--Paweł Narwojsz
--Piotr Kaczorowski


with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;


procedure Simulation is
   ----GLOBAL VARIABLES---
   -- Number_Of_Ingredients: constant Integer := 5;
   Number_Of_Producers: constant Integer := 5;
   Number_Of_Dishes: constant Integer := 3;
   Number_Of_Consumers: constant Integer := 3;
   --TODO refactor to use enumeration
   -- type Ingredient_Type is 
   --    (Past, Potato, Tomato, Chicken, Pork);
   -- type Dish_Type is
   --    (Bolognes, Steak, Hamburger);
   -- type Consumer_Type is
   --    (Eater1, Eater2, Eater3);
   subtype Ingredient_Type is Integer range 1 .. Number_Of_Producers;
   subtype Dish_Type is Integer range 1 .. Number_Of_Dishes;
   subtype Consumer_Type is Integer range 1 .. Number_Of_Consumers;


   --each Producer is assigned a Ingredient that it produces
   Ingredient_Name: constant array (Ingredient_Type) of String(1 .. 7)
      := ("Pasta  ", "Potato ", "Tomato ", "Chicken", "Pork   ");

   --Assembly is a collection of Ingredients
   Assembly_Name: constant array (Dish_Type) of String(1 .. 9)
     := ("Bolognese", "Steak    ", "Hamburger");   


   ----TASK DECLARATIONS----

   -- Producer produces determined Ingredient
   task type Producer is
      entry Start(Ingredient: in Ingredient_Type; Cooking_Time: in Integer);
   end Producer;

   -- Consumer gets an arbitrary assembly of several Ingredients from the buffer
   -- but he/she orders it randomly
   task type Consumer is
      entry Start(Consumer_Number: in Consumer_Type;
                  Consumption_Time: in Integer);
   end Consumer;

   -- Buffer receives Ingredients from Producers and delivers Assemblies to Consumers
   task type Buffer is
      -- Accept a Ingredient to the storage (provided there is a room for it)
      entry Take(Ingredient: in Ingredient_Type; Number: in Integer);
      -- Deliver an assembly (provided there are enough Ingredients for it)
      entry Deliver(Assembly: in Dish_Type; Number: out Integer);
   end Buffer;

   P: array ( 1 .. Number_Of_Producers ) of Producer;
   K: array ( 1 .. Number_Of_Consumers ) of Consumer;
   B: Buffer;


   ----TASK DEFINITIONS----

   --Producer--

   task body Producer is
      subtype Cooking_Time_Range is Integer range 1 .. 3;
      package Random_Ingrediention is new Ada.Numerics.Discrete_Random(Cooking_Time_Range);
      --  random number generator
      G: Random_Ingrediention.Generator;
      Ingredient_Type_Number: Integer;
      Ingredient_Number: Integer;
      Ingrediention: Integer;
      Random_Time: Duration;
   begin
      accept Start(Ingredient: in Ingredient_Type; Cooking_Time: in Integer) do
         --  start random number generator
         Random_Ingrediention.Reset(G);
         Ingredient_Number := 1;
         Ingredient_Type_Number := Ingredient;
         Ingrediention := Cooking_Time;
      end Start;
      Put_Line(ESC & "[93m" & "P: Cooker started cooking of " & Ingredient_Name(Ingredient_Type_Number) & ESC & "[0m");
      loop
         Random_Time := Duration(Random_Ingrediention.Random(G));
         delay Random_Time;
         Put_Line(ESC & "[93m" & "P: Cooked Ingredient " & Ingredient_Name(Ingredient_Type_Number)
                  & " number "  & Integer'Image(Ingredient_Number) & ESC & "[0m");
         -- Accept for storage
         B.Take(Ingredient_Type_Number, Ingredient_Number);
         Ingredient_Number := Ingredient_Number + 1;
      end loop;
   end Producer;


   --Consumer--

   task body Consumer is
      subtype Consumption_Time_Range is Integer range 4 .. 8;
      package Random_Consumption is new
        Ada.Numerics.Discrete_Random(Consumption_Time_Range);

      --each Consumer takes any (random) Assembly from the Buffer
      package Random_Assembly is new
        Ada.Numerics.Discrete_Random(Dish_Type);

      G: Random_Consumption.Generator;
      GA: Random_Assembly.Generator;
      Consumer_Nb: Consumer_Type;
      Assembly_Number: Integer;
      Consumption: Integer;
      Dish_Type: Integer;
      Consumer_Name: constant array (1 .. Number_Of_Consumers)
        of String(1 .. 6)
        := ("Eater1", "Eater2", "Eater3");
   begin
      accept Start(Consumer_Number: in Consumer_Type;
                   Consumption_Time: in Integer) do
         Random_Consumption.Reset(G);
         Random_Assembly.Reset(GA);
         Consumer_Nb := Consumer_Number;
         Consumption := Consumption_Time;
      end Start;
      Put_Line(ESC & "[96m" & "C: Eater named " & Consumer_Name(Consumer_Nb)  & " enters restaurant");
      loop
         delay Duration(Random_Consumption.Random(G)); --  simulate consumption
         Dish_Type := Random_Assembly.Random(GA);
         -- take an assembly for consumption
         B.Deliver(Dish_Type, Assembly_Number);
         Put_Line(ESC & "[96m" & "C: " & Consumer_Name(Consumer_Nb) & " takes dish " &
                    Assembly_Name(Dish_Type) & " number " &
                    Integer'Image(Assembly_Number) & ESC & "[0m");
      end loop;
   end Consumer;


   --Buffer--

   task body Buffer is
      Storage_Capacity: constant Integer := 30;
      type Storage_type is array (Ingredient_Type) of Integer;
      Storage: Storage_type
        := (0, 0, 0, 0, 0);
      Assembly_Content: array(Dish_Type, Ingredient_Type) of Integer
        := ((2, 1, 2, 0, 2),
            (1, 2, 0, 1, 0),
            (3, 2, 2, 0, 1));
      Max_Assembly_Content: array(Ingredient_Type) of Integer;
      Assembly_Number: array(Dish_Type) of Integer
        := (1, 1, 1);
      In_Storage: Integer := 0;

      procedure Setup_Variables is
      begin
         for W in Ingredient_Type loop
            Max_Assembly_Content(W) := 0;
            for Z in Dish_Type loop
               if Assembly_Content(Z, W) > Max_Assembly_Content(W) then
                  Max_Assembly_Content(W) := Assembly_Content(Z, W);
               end if;
            end loop;
         end loop;
      end Setup_Variables;

      function Can_Accept(Ingredient: Ingredient_Type) return Boolean is
      begin
         if In_Storage >= Storage_Capacity then
            return False;
         else
            return True;
         end if;
      end Can_Accept;

      function Can_Deliver(Assembly: Dish_Type) return Boolean is
      begin
         for W in Ingredient_Type loop
            if Storage(W) < Assembly_Content(Assembly, W) then
               return False;
            end if;
         end loop;
         return True;
      end Can_Deliver;

      procedure Storage_Contents is
      begin
         for W in Ingredient_Type loop
            Put_Line("|   Storage contents: " & Integer'Image(Storage(W)) & " "
                     & Ingredient_Name(W));
         end loop;
         Put_Line("|   Number of Ingredients in storage: " & Integer'Image(In_Storage));

      end Storage_Contents;

   begin
      Put_Line(ESC & "[91m" & "B: Buffer started" & ESC & "[0m");
      Setup_Variables;
      loop
         accept Take(Ingredient: in Ingredient_Type; Number: in Integer) do
            if Can_Accept(Ingredient) then
               Put_Line(ESC & "[91m" & "B: Accepted Ingredient " & Ingredient_Name(Ingredient) & " number " &
                          Integer'Image(Number)& ESC & "[0m");
               Storage(Ingredient) := Storage(Ingredient) + 1;
               In_Storage := In_Storage + 1;
            else
               Put_Line(ESC & "[91m" & "B: Rejected Ingredient " & Ingredient_Name(Ingredient) & " number " &
                          Integer'Image(Number)& ESC & "[0m");
            end if;
         end Take;
         Storage_Contents;

         accept Deliver(Assembly: in Dish_Type; Number: out Integer) do
            if Can_Deliver(Assembly) then
               Put_Line(ESC & "[91m" & "B: Delivered dish " & Assembly_Name(Assembly) & " number " &
                          Integer'Image(Assembly_Number(Assembly))& ESC & "[0m");
               for W in Ingredient_Type loop
                  Storage(W) := Storage(W) - Assembly_Content(Assembly, W);
                  In_Storage := In_Storage - Assembly_Content(Assembly, W);
               end loop;
               Number := Assembly_Number(Assembly);
               Assembly_Number(Assembly) := Assembly_Number(Assembly) + 1;
            else
               Put_Line(ESC & "[91m" & "B: Lacking Ingredients for dish " & Assembly_Name(Assembly)& ESC & "[0m");
               Number := 0;
            end if;
         end Deliver;
         Storage_Contents;

      end loop;
   end Buffer;



   ---"MAIN" FOR SIMULATION---
begin
   for I in 1 .. Number_Of_Producers loop
      P(I).Start(I, 10);
   end loop;
   for J in 1 .. Number_Of_Consumers loop
      K(J).Start(J,12);
   end loop;
end Simulation;


