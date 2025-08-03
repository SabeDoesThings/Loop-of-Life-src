with Level;
with SDL.Video.Renderers;

package Mini_Game is
   procedure Init (Ren : SDL.Video.Renderers.Renderer; Level_Number : Integer);
   procedure Update (Dt : Float; Running : in out Boolean; Current_Level : in out Level.Object);
   procedure Render (Ren : in out SDL.Video.Renderers.Renderer);
end Mini_Game;