''
  audio_output {
    type "pulse"
    name "mpd pulse-audio-output"
  }
  audio_output {
    type "fifo"
    name "mpd visualizer-fifo"
    path "/tmp/mpd.fifo"
    format "44100:16:2"
  }
''
