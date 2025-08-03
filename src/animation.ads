with SDL.Video.Rectangles;
with SDL.Video.Textures;
with SDL.Video.Renderers;

package Animation is
   type Flip_Type is (None, Horizontal);

   type Sprite_Sheet is limited record
      Texture : SDL.Video.Textures.Texture;
      Frame_Width, Frame_Height, Frame_Count : Integer;
   end record;

   type Sprite_Animation is tagged limited record
      Current_Frame : Natural;
      Timer : Float := 0.0;
      Frame_Delay : Float := 100.0;
      Start_Frame, End_Frame : Integer;
   end record;

   procedure Update (A : in out Sprite_Animation; Dt : Float);
   procedure Render (
      A : Sprite_Animation;
      S : Sprite_Sheet;
      Ren : in out SDL.Video.Renderers.Renderer;
      Dest : in out SDL.Video.Rectangles.Rectangle;
      Flip : Flip_Type := None
   );
end Animation;