
alsa_monitor.properties = {
  -- Create a JACK device. This is not enabled by default because
  -- it requires that the PipeWire JACK replacement libraries are
  -- not used by the session manager, in order to be able to
  -- connect to the real JACK server.
  --["alsa.jack-device"] = false,

  -- Reserve devices via org.freedesktop.ReserveDevice1 on D-Bus
  ["alsa.reserve"] = true,
  --["alsa.reserve.priority"] = -20,
  --["alsa.reserve.application-name"] = "WirePlumber",

  -- Enables monitoring of alsa MIDI devices
  ["alsa.midi.monitoring"] = true,
}

alsa_monitor.rules = {
  -- An array of matches/actions to evaluate.
  {
    -- Rules for matching a device or node. It is an array of
    -- properties that all need to match the regexp. If any of the
    -- matches work, the actions are executed for the object.
    matches = {
      {
        -- This matches all cards.
        { "device.name", "matches", "alsa_card.*" },
      },
    },
    -- Apply properties on the matched object.
    apply_properties = {
      -- Use ALSA-Card-Profile devices. They use UCM or the profile
      -- configuration to configure the device and mixer settings.
      ["api.alsa.use-acp"] = true,

      -- Use UCM instead of profile when available. Can be
      -- disabled to skip trying to use the UCM profile.
      --["api.alsa.use-ucm"] = true,

      -- Don't use the hardware mixer for volume control. It
      -- will only use software volume. The mixer is still used
      -- to mute unused paths based on the selected port.
      --["api.alsa.soft-mixer"] = false,

      -- Ignore decibel settings of the driver. Can be used to
      -- work around buggy drivers that report wrong values.
      --["api.alsa.ignore-dB"] = false,

      -- The profile set to use for the device. Usually this is
      -- "default.conf" but can be changed with a udev rule or here.
      --["device.profile-set"] = "profileset-name",

      -- The default active profile. Is by default set to "Off".
      --["device.profile"] = "default profile name",

      -- Automatically select the best profile. This is the
      -- highest priority available profile. This is disabled
      -- here and instead implemented in the session manager
      -- where it can save and load previous preferences.
      ["api.acp.auto-profile"] = false,

      -- Automatically switch to the highest priority available port.
      -- This is disabled here and implemented in the session manager instead.
      ["api.acp.auto-port"] = false,

      -- Other properties can be set here.
      --["device.nick"] = "My Device",
    },
  },
  {
    matches = {
      {
        -- Matches all sources.
        { "node.name", "matches", "alsa_input.*" },
      },
      {
        -- Matches all sinks.
        { "node.name", "matches", "alsa_output.*" },
      },
    },
    apply_properties = {
      --["node.nick"]              = "My Node",
      --["priority.driver"]        = 100,
      --["priority.session"]       = 100,
      --["node.pause-on-idle"]     = false,
      --["resample.quality"]       = 4,
      --["channelmix.normalize"]   = false,
      --["channelmix.mix-lfe"]     = false,
      --["audio.channels"]         = 2,
      --["audio.format"]           = "S16LE",
      --["audio.rate"]             = 44100,
      --["audio.position"]         = "FL,FR",
      --["api.alsa.period-size"]   = 1024,
      --["api.alsa.headroom"]      = 0,
      --["api.alsa.disable-mmap"]  = false,
      --["api.alsa.disable-batch"] = false,
    },
  },
}
