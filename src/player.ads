with SDL.Video.Rectangles;
with SDL.Video.Renderers;
with Animation; use Animation;

with Camera;

package Player is
   type Object is tagged limited record
      X, Y : Float;
      Sheet : Sprite_Sheet;
      Idle_Anim : Sprite_Animation;
      Walk_Anim : Sprite_Animation;
      Is_Moving : Boolean := False;
      Speed : Float;
      Facing_Left : Boolean := False;
      Collider : SDL.Video.Rectangles.Rectangle;
      Collider_Offset : Float := 6.0;
   end record;

   procedure Init (P : in out Object; Ren : SDL.Video.Renderers.Renderer);
   procedure Update (P : in out Object; Dt : Float; Bounds : SDL.Video.Rectangles.Rectangle);
   procedure Render (
      P : in out Object;
      Ren : in out SDL.Video.Renderers.Renderer;
      Cam : in out Camera.Object
   );
   procedure Destroy (P : in out Object);
end Player;