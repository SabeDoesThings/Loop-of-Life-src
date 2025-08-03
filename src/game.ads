package Game is
   type Game_Mode_Type is (Playing_Level, In_Mini_Game, Is_Sleeping, Is_Switching_Days, Is_Working, Main_Menu);
   Game_Mode : Game_Mode_Type := Playing_Level;

   procedure Run;
   procedure Reset_Game_State;
end Game;