with Game; use Game;
with SDL.Events.Events;
with SDL.Events.Keyboards; use SDL.Events.Keyboards;
with SDL.Images.IO;
with SDL.TTFs;
with SDL.TTFs.Makers;
with SDL.Video.Palettes;
with SDL.Video.Rectangles;
with SDL.Video.Surfaces;
with SDL.Video.Textures;
with SDL.Video.Textures.Makers;
with SDL.Mixer.Music; use SDL.Mixer.Music;

package body Menu is
   Text_Surf : SDL.Video.Surfaces.Surface;
   Text_Text : SDL.Video.Textures.Texture;
   Text_Surf2 : SDL.Video.Surfaces.Surface;
   Text_Text2 : SDL.Video.Textures.Texture;
   Font : SDL.TTFs.Fonts;

   BG_Surf : SDL.Video.Surfaces.Surface;
   BG_Text : SDL.Video.Textures.Texture;

   Music : Music_Type;

   procedure Init (Ren : SDL.Video.Renderers.Renderer) is
   begin
      SDL.Images.IO.Create (BG_Surf, "res/office3.png");
      SDL.Video.Textures.Makers.Create (BG_Text, Ren, BG_Surf);
      SDL.Video.Surfaces.Finalize (BG_Surf);

      SDL.TTFs.Makers.Create (Font, "res/Monocraft.ttf", 36);
      Text_Surf := Font.Render_Solid (Text => "Loop Of Life", Colour => SDL.Video.Palettes.Colour'(Red => 255, Green => 255, Blue => 255, Alpha => 255));
      Text_Surf2 := Font.Render_Solid (Text => "Press SPACE to begin.", Colour => SDL.Video.Palettes.Colour'(Red => 255, Green => 255, Blue => 255, Alpha => 255));

      SDL.Video.Textures.Makers.Create (Text_Text, Ren, Text_Surf);
      SDL.Video.Textures.Makers.Create (Text_Text2, Ren, Text_Surf2);

      Load_MUS ("res/song1.wav", Music);

      Play (Music, Loop_Forever);
   end Init;

   procedure Update (Dt : Float; Running : in out Boolean) is
      use SDL.Events;
      use SDL.Events.Events;
      Evt : SDL.Events.Events.Events;
   begin
      while SDL.Events.Events.Poll (Evt) loop
         case Evt.Common.Event_Type is
            when SDL.Events.Keyboards.Key_Down =>
               if Evt.Keyboard.Key_Sym.Key_Code = Code_Space then
                  if Game_Mode = Main_Menu then
                     Game_Mode := Playing_Level;
                     Pause;
                     Free (Music);
                  end if;
               end if;

            when SDL.Events.Quit =>
               Running := False;

            when others =>
               null;
         end case;
      end loop;
   end Update;

   procedure Render (Ren : in out SDL.Video.Renderers.Renderer) is
      Text_Dest : SDL.Video.Rectangles.Rectangle;
      Text_Dest2 : SDL.Video.Rectangles.Rectangle;
   begin
      Ren.Set_Draw_Colour ((0, 0, 0, 255));
      Ren.Clear;

      Ren.Copy (BG_Text);

      Text_Dest := (SDL.Coordinate (256), SDL.Coordinate (304), Text_Text.Get_Size.Width, Text_Text.Get_Size.Height);
      Ren.Copy_To (Text_Text, Text_Dest);

      Text_Dest2 := (SDL.Coordinate (156), SDL.Coordinate (400), Text_Text2.Get_Size.Width, Text_Text2.Get_Size.Height);
      Ren.Copy_To (Text_Text2, Text_Dest2);
   end Render;
end Menu;