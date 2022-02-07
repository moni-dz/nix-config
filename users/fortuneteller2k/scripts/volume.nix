''
  MAX_VOLUME=0

  current=$(pactl list sinks | rg 'Volume:' | head -n $((SINK + 1)) | tail -n 1 | sed -e 's,.* \([0-9][0-9]*\)%.*,\1,')
  current_sink=$(pactl list short sinks | awk '{print $1}')
  active_port=$(pactl list sinks | rg 'Active Port: ' | awk '{print $3}')

  set_max_volume() {
    if [ "$active_port" = "analog-output-headphones" ]; then
      MAX_VOLUME=100
    elif [ "$active_port" = "analog-output-speaker" ]; then
      MAX_VOLUME=200
    else
      MAX_VOLUME=100
    fi
  }

  volume() {
    set_max_volume
    case "$1" in
      up)
        pactl set-sink-mute "$current_sink" 0
        if [ "$current" -lt "$MAX_VOLUME" ]; then
          if [ $((current + 10)) -gt "$MAX_VOLUME" ]; then
            pactl set-sink-volume "$current_sink" +$(("$MAX_VOLUME" - current))%
          else
            pactl set-sink-volume "$current_sink" +10%
          fi
        fi
        ;;
      down)
        if [ $((current - 10)) -eq 0 ]; then
          pactl set-sink-mute "$current_sink" 1
        else
          pactl set-sink-volume "$current_sink" -10%
        fi
        ;;
      toggle)
        pactl set-sink-mute "$current_sink" toggle
        ;;
      *)
        echo 'Usage: volume <up/down/toggle>'
        exit 1;
        ;;
    esac;
  }

  volume "$1"
''
