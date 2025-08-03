with SDL;
with SDL.Images;
with Game;
with SDL.Mixer; use SDL.Mixer;
with SDL.TTFs;

procedure Loop_Of_Life is
begin
   if not SDL.Initialise (SDL.Enable_Video) then
      raise Program_Error with "Failed to initialize SDL!";
   end if;

   if not SDL.Images.Initialise (SDL.Images.Enable_PNG) then
      raise Program_Error with "Failed to initialize SDL_image!";
   end if;

   if not SDL.TTFs.Initialise then
      raise Program_Error with "Failed to initialize SDL_ttf!";
   end if;

   Open (Frequency  => Default_Frequency,
         Format     => (Bits => 16, Signed => True, others => False), -- 16#8010#,
         Channels   => 2,
         Chunk_Size => 2048);
   SDL.Mixer.Initialise (Init_Flac + Init_MOD + Init_MP3 + Init_OGG + Init_MID + Init_Opus);

   Game.Run;

   SDL.Finalise;
   SDL.Images.Finalise;
   SDL.TTFs.Finalise;
end Loop_Of_Life;
