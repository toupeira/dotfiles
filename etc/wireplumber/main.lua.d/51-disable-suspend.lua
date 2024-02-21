table.insert (alsa_monitor.rules, {
  matches = {
    {
      { "node.name", "matches", "alsa_output.*hdmi*" },
    },
  },
  apply_properties = {
    ["session.suspend-timeout-seconds"] = 0,
    ["node.always-process"] = true,
    ["dither.noise"] = 1,
  },
})
