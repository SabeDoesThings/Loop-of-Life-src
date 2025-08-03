with Camera;
with SDL.Video.Rectangles;
with SDL.Video.Surfaces;
with SDL.Video.Textures;
with SDL.Video.Renderers;
with Ada.Text_IO; use Ada.Text_IO;

with Animation;
with Player;

package Level is
   type Level_Data is record
      BG_Path : String (1 .. 11);
      Goal_X : Integer;
      Goal_Y : Integer;
      Player_X, Player_Y : Float;
      Bounds_Width, Bounds_Height : Integer;
      Bounds_X, Bounds_Y : Integer;
      Day_Text : String (1 .. 9);
      Task_Text : String (1 .. 22);
   end record;

   type Level_Index is range 1 .. 30;

   subtype Message_String is String (1 .. 31);
   type Message_Array is array (1 .. 13) of Message_String;

   Levels : constant array (Level_Index) of Level_Data := [
      (BG_Path => "res/bg1.png", Goal_X => 140,  Goal_Y => 65, Player_X => 29.0,  Player_Y => 94.0, Bounds_Width => 160 - 32, Bounds_Height => 128 - 32, Bounds_X => 16,  Bounds_Y => 16, Day_Text => "Monday   ", Task_Text => "Tasks: Go to work     "),  --  level 1 room Monday
      (BG_Path => "res/bg2.png", Goal_X => 533,  Goal_Y => 60, Player_X => -1.0,  Player_Y => 60.0, Bounds_Width => 608,      Bounds_Height => 128 / 6,  Bounds_X => 0,   Bounds_Y => 59, Day_Text => "Monday   ", Task_Text => "Tasks: Walk to work   "),  --  level 2 path
      (BG_Path => "res/bg3.png", Goal_X => 14,   Goal_Y => 60, Player_X => 238.0, Player_Y => 60.0, Bounds_Width => 256 - 16, Bounds_Height => 160 / 7,  Bounds_X => 16,  Bounds_Y => 58, Day_Text => "Monday   ", Task_Text => "Tasks: Get to computer"),  --  level 3 work in
      (BG_Path => "res/bg3.png", Goal_X => 238,  Goal_Y => 64, Player_X => 14.0,  Player_Y => 60.0, Bounds_Width => 256 - 16, Bounds_Height => 160 / 7,  Bounds_X => 16,  Bounds_Y => 58, Day_Text => "Monday   ", Task_Text => "Tasks: Leave work     "),  --  level 4 work out
      (BG_Path => "res/bg2.png", Goal_X => -1,   Goal_Y => 60, Player_X => 533.0, Player_Y => 60.0, Bounds_Width => 608,      Bounds_Height => 128 / 6,  Bounds_X => -64, Bounds_Y => 59, Day_Text => "Monday   ", Task_Text => "Tasks: Walk home      "),  --  level 5 path back
      (BG_Path => "res/bgA.png", Goal_X => 23,   Goal_Y => 92, Player_X => 128.0, Player_Y => 50.0, Bounds_Width => 160 - 32, Bounds_Height => 128 - 32, Bounds_X => 16,  Bounds_Y => 16, Day_Text => "Monday   ", Task_Text => "Tasks: Go to sleep    "),  --  level 6 room sleep

      (BG_Path => "res/bg1.png", Goal_X => 140,  Goal_Y => 65, Player_X => 29.0,  Player_Y => 94.0, Bounds_Width => 160 - 32, Bounds_Height => 128 - 32, Bounds_X => 16,  Bounds_Y => 16, Day_Text => "Tuesday  ", Task_Text => "Tasks: Go to work     "),  --  level 7 room Tuesday
      (BG_Path => "res/bg2.png", Goal_X => 533,  Goal_Y => 60, Player_X => -1.0,  Player_Y => 60.0, Bounds_Width => 608,      Bounds_Height => 128 / 6,  Bounds_X => 0,   Bounds_Y => 59, Day_Text => "Tuesday  ", Task_Text => "Tasks: Walk to work   "),  --  level 8 path
      (BG_Path => "res/bg3.png", Goal_X => 14,   Goal_Y => 60, Player_X => 238.0, Player_Y => 60.0, Bounds_Width => 256 - 16, Bounds_Height => 160 / 7,  Bounds_X => 16,  Bounds_Y => 58, Day_Text => "Tuesday  ", Task_Text => "Tasks: Get to computer"),  --  level 9 work in
      (BG_Path => "res/bg3.png", Goal_X => 238,  Goal_Y => 64, Player_X => 14.0,  Player_Y => 60.0, Bounds_Width => 256 - 16, Bounds_Height => 160 / 7,  Bounds_X => 16,  Bounds_Y => 58, Day_Text => "Tuesday  ", Task_Text => "Tasks: Leave work     "),  --  level 10 work out
      (BG_Path => "res/bg2.png", Goal_X => -1,   Goal_Y => 60, Player_X => 533.0, Player_Y => 60.0, Bounds_Width => 608,      Bounds_Height => 128 / 6,  Bounds_X => -64, Bounds_Y => 59, Day_Text => "Tuesday  ", Task_Text => "Tasks: Walk home      "),  --  level 11 path back
      (BG_Path => "res/bgA.png", Goal_X => 23,   Goal_Y => 92, Player_X => 128.0, Player_Y => 50.0, Bounds_Width => 160 - 32, Bounds_Height => 128 - 32, Bounds_X => 16,  Bounds_Y => 16, Day_Text => "Tuesday  ", Task_Text => "Tasks: Go to sleep    "),  --  level 12 room sleep

      (BG_Path => "res/bg1.png", Goal_X => 140,  Goal_Y => 65, Player_X => 29.0,  Player_Y => 94.0, Bounds_Width => 160 - 32, Bounds_Height => 128 - 32, Bounds_X => 16,  Bounds_Y => 16, Day_Text => "Wednesday", Task_Text => "Tasks: Go to work     "), --  level 13 room Wednesday
      (BG_Path => "res/bg2.png", Goal_X => 533,  Goal_Y => 60, Player_X => -1.0,  Player_Y => 60.0, Bounds_Width => 608,      Bounds_Height => 128 / 6,  Bounds_X => 0,   Bounds_Y => 59, Day_Text => "Wednesday", Task_Text => "Tasks: Walk to work   "), --  level 14 path
      (BG_Path => "res/bg3.png", Goal_X => 14,   Goal_Y => 60, Player_X => 238.0, Player_Y => 60.0, Bounds_Width => 256 - 16, Bounds_Height => 160 / 7,  Bounds_X => 16,  Bounds_Y => 58, Day_Text => "Wednesday", Task_Text => "Tasks: Get to computer"), --  level 15 work in
      (BG_Path => "res/bg3.png", Goal_X => 238,  Goal_Y => 64, Player_X => 14.0,  Player_Y => 60.0, Bounds_Width => 256 - 16, Bounds_Height => 160 / 7,  Bounds_X => 16,  Bounds_Y => 58, Day_Text => "Wednesday", Task_Text => "Tasks: Leave work     "), --  level 16 work out
      (BG_Path => "res/bg2.png", Goal_X => 319,  Goal_Y => 60, Player_X => 533.0, Player_Y => 60.0, Bounds_Width => 608,      Bounds_Height => 128 / 6,  Bounds_X => -64, Bounds_Y => 59, Day_Text => "Wednesday", Task_Text => "Tasks: Walk home      "), --  level 17 path back teleport
      (BG_Path => "res/bg4.png", Goal_X => 593,  Goal_Y => 60, Player_X => 319.0, Player_Y => 60.0, Bounds_Width => 608,      Bounds_Height => 128 / 6,  Bounds_X => 64,  Bounds_Y => 58, Day_Text => "Wednesday", Task_Text => "Tasks: Walk home      "), --  level 18 path reverse
      (BG_Path => "res/bgA.png", Goal_X => 23,   Goal_Y => 92, Player_X => 128.0, Player_Y => 50.0, Bounds_Width => 160 - 32, Bounds_Height => 128 - 32, Bounds_X => 16,  Bounds_Y => 16, Day_Text => "Wednesday", Task_Text => "Tasks: Go to sleep    "), --  level 19 room sleep

      (BG_Path => "res/bg1.png", Goal_X => 140,  Goal_Y => 65, Player_X => 29.0,  Player_Y => 94.0, Bounds_Width => 160 - 32, Bounds_Height => 128 - 32, Bounds_X => 16,  Bounds_Y => 16, Day_Text => "Thursday ", Task_Text => "Tasks: Go to work     "), --  level 20 room Thursday
      (BG_Path => "res/bg6.png", Goal_X => 533,  Goal_Y => 60, Player_X => -1.0,  Player_Y => 60.0, Bounds_Width => 608,      Bounds_Height => 128 / 6,  Bounds_X => 0,   Bounds_Y => 59, Day_Text => "Thursday ", Task_Text => "Tasks: Walk to work   "), --  level 21 path
      (BG_Path => "res/bg3.png", Goal_X => 14,   Goal_Y => 60, Player_X => 238.0, Player_Y => 60.0, Bounds_Width => 256 - 16, Bounds_Height => 160 / 7,  Bounds_X => 16,  Bounds_Y => 58, Day_Text => "Thursday ", Task_Text => "Tasks: Get to computer"), --  level 22 work in
      (BG_Path => "res/bg3.png", Goal_X => 238,  Goal_Y => 64, Player_X => 14.0,  Player_Y => 60.0, Bounds_Width => 256 - 16, Bounds_Height => 160 / 7,  Bounds_X => 16,  Bounds_Y => 58, Day_Text => "Thursday ", Task_Text => "Tasks: Leave work     "), --  level 23 work out
      (BG_Path => "res/bg2.png", Goal_X => -1,   Goal_Y => 60, Player_X => 533.0, Player_Y => 60.0, Bounds_Width => 608,      Bounds_Height => 128 / 6,  Bounds_X => -64, Bounds_Y => 59, Day_Text => "Thursday ", Task_Text => "Tasks: Walk home      "), --  level 24 path back
      (BG_Path => "res/bg5.png", Goal_X => 23,   Goal_Y => 92, Player_X => 128.0, Player_Y => 50.0, Bounds_Width => 160 - 32, Bounds_Height => 128 - 32, Bounds_X => 16,  Bounds_Y => 16, Day_Text => "Thursday ", Task_Text => "Tasks: Go to sleep    "), --  level 25 room sleep

      (BG_Path => "res/bg1.png", Goal_X => 140,  Goal_Y => 65, Player_X => 29.0,  Player_Y => 94.0, Bounds_Width => 160 - 32, Bounds_Height => 128 - 32, Bounds_X => 16,  Bounds_Y => 16, Day_Text => "Friday   ", Task_Text => "Tasks: Go to work     "), --  level 26 room Friday
      (BG_Path => "res/bg7.png", Goal_X => 533,  Goal_Y => 60, Player_X => -1.0,  Player_Y => 60.0, Bounds_Width => 608,      Bounds_Height => 128 / 6,  Bounds_X => 0,   Bounds_Y => 59, Day_Text => "Friday   ", Task_Text => "Tasks: Walk to work   "), --  level 27 path
      (BG_Path => "res/bg3.png", Goal_X => 14,   Goal_Y => 60, Player_X => 238.0, Player_Y => 60.0, Bounds_Width => 256 - 16, Bounds_Height => 160 / 7,  Bounds_X => 16,  Bounds_Y => 58, Day_Text => "Friday   ", Task_Text => "Tasks: Get to computer"), --  level 28 work in
      (BG_Path => "res/bg8.png", Goal_X => 1381, Goal_Y => 64, Player_X => 14.0,  Player_Y => 60.0, Bounds_Width => 1500,     Bounds_Height => 160 / 7,  Bounds_X => 16,  Bounds_Y => 58, Day_Text => "         ", Task_Text => "                      "), --  level 29 work out
      (BG_Path => "res/bg9.png", Goal_X => 1500, Goal_Y => 60, Player_X => 172.0, Player_Y => 60.0, Bounds_Width => 586,      Bounds_Height => 160 / 7,  Bounds_X => 16,  Bounds_Y => 59, Day_Text => "         ", Task_Text => "                      ")  --  level 30 talk
   ];

   type Object is tagged limited record
      BG : SDL.Video.Textures.Texture;
      BG_Surf : SDL.Video.Surfaces.Surface;
      BG_Width, BG_Height : Integer;
      The_Player : Player.Object;
      Goal : SDL.Video.Rectangles.Rectangle;
      Goal_Reached : Boolean := False;
      Bounds : SDL.Video.Rectangles.Rectangle;
      Act_End : SDL.Video.Rectangles.Rectangle;
      Act_End_Triggered : Boolean := False;
      Eye_Surf : SDL.Video.Surfaces.Surface;
      Eye_Sprite : Animation.Sprite_Sheet;
      Eye_Anim : Animation.Sprite_Animation;
      Show_Eye : Boolean := False;
      Eye_Rect : SDL.Video.Rectangles.Rectangle;
      Text_For_Day : String (1 .. 9);
      Last_Level_Messages : Message_Array;
      Current_Message_Index : Integer := 1;
      Message_Timer : Float := 0.0;
      Message_Interval : Float;
      Last_Level_Text_Surface : SDL.Video.Surfaces.Surface;
      Last_Level_Text_Texture : SDL.Video.Textures.Texture;
      All_Messages_Shown : Boolean := False;
      Return_Timer        : Float := 0.0;
   end record;

   procedure Init (
      L : in out Object;
      Ren : SDL.Video.Renderers.Renderer;
      Data : Level_Data;
      Start_Player_Y : Float := -1.0
   );
   procedure Update (
      L : in out Object;
      Dt : Float;
      Ren : SDL.Video.Renderers.Renderer;
      Should_Reset : out Boolean
   );
   procedure Render (
      L : in out Object;
      Ren : in out SDL.Video.Renderers.Renderer;
      Cam : in out Camera.Object
   );
   procedure Destroy (L : in out Object);
   function Rects_Overlap (
      X1, Y1, W1, H1 : Integer;
      X2, Y2, W2, H2 : Integer
   ) return Boolean;
end Level;