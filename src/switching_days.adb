with Game;
with SDL.Images.IO;
with SDL.TTFs.Makers;
with SDL.Video.Palettes;
with SDL.Video.Rectangles;
with SDL.Video.Surfaces;
with SDL.Video.Textures.Makers;
with SDL.Mixer.Music; use SDL.Mixer.Music;

package body Switching_Days is
   Timer_Started : Boolean := False;
   Timer : Float := 0.0;
   Timer_Duration : constant Float := 2.0;

   Text_Surf : SDL.Video.Surfaces.Surface;
   Text_Text : SDL.Video.Textures.Texture;
   Text_Surf2 : SDL.Video.Surfaces.Surface;
   Text_Text2 : SDL.Video.Textures.Texture;
   Font : SDL.TTFs.Fonts;

   BG_Surf : SDL.Video.Surfaces.Surface;
   BG_Text : SDL.Video.Textures.Texture;

   Music : Music_Type;

   procedure Init (Ren : SDL.Video.Renderers.Renderer; Current_Level : Level.Object) is
   begin
      Timer := 0.0;

      SDL.Images.IO.Create (BG_Surf, "res/office.png");
      SDL.Video.Textures.Makers.Create (BG_Text, Ren, BG_Surf);
      SDL.Video.Surfaces.Finalize (BG_Surf);

      SDL.TTFs.Makers.Create (Font, "res/Monocraft.ttf", 36);
      Text_Surf := Font.Render_Solid (Text => Current_Level.Text_For_Day, Colour => SDL.Video.Palettes.Colour'(Red => 255, Green => 255, Blue => 255, Alpha => 255));
      SDL.Video.Textures.Makers.Create (Text_Text, Ren, Text_Surf);

      Text_Surf2 := Font.Render_Solid (Text => "MAJOR company changes friday.", Colour => SDL.Video.Palettes.Colour'(Red => 255, Green => 0, Blue => 0, Alpha => 255));
      SDL.Video.Textures.Makers.Create (Text_Text2, Ren, Text_Surf2);

      Load_MUS ("res/sound1.mp3", Music);
      Play (Music, 0);
   end Init;

   procedure Update (Dt : Float) is
   begin
      Timer := Timer + Dt;
      if Timer >= Timer_Duration then
         Game.Game_Mode := Game.Playing_Level;
         Pause;
         Free (Music);
         Timer_Started := False;
         Timer := 0.0;
      end if;
   end Update;

   procedure Render (Ren : in out SDL.Video.Renderers.Renderer) is
      Text_Dest : SDL.Video.Rectangles.Rectangle;
      Text_Dest2 : SDL.Video.Rectangles.Rectangle;
   begin
      Ren.Set_Draw_Colour ((0, 0, 0, 255));
      Ren.Clear;

      Ren.Copy (BG_Text);

      Text_Dest := (SDL.Coordinate (330), SDL.Coordinate (304), Text_Text.Get_Size.Width, Text_Text.Get_Size.Height);
      Ren.Copy_To (Text_Text, Text_Dest);

      Text_Dest2 := (SDL.Coordinate (70), SDL.Coordinate (400), Text_Text2.Get_Size.Width, Text_Text2.Get_Size.Height);
      Ren.Copy_To (Text_Text2, Text_Dest2);
   end Render;
end Switching_Days;