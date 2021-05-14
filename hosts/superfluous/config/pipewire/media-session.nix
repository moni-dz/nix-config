{
  alsa-monitor.rules = [{
    # NOTE: replace with your sink name
    #
    # Obtain the sink name with `pw-dump | grep node.name | grep alsa'
    matches = [{ "node.name" = "alsa_output.pci-0000_00_14.2.analog-stereo"; }];

    actions.update-props = {
      "audio.format" = "S16LE";
      "audio.rate" = 48000;
      "api.alsa.period-size" = 32;
    };
  }];
}
