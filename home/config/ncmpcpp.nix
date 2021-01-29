{
  user_interface = "alternative";
  lyrics_directory = "~/.local/share/ncmpcpp/lyrics";
  visualizer_data_source = "/tmp/mpd.fifo";
  visualizer_output_name = "mpd_visualizer_fifo";
  visualizer_in_stereo = "yes";
  visualizer_fps = 60;
  visualizer_type = "ellipse";
  visualizer_look = "+*";
  visualizer_autoscale = "yes";
  execute_on_song_change = ''
    notify-desktop "Now Playing" "$(mpc --format '\n%title% - %artist%' current)"'';
}
