table.insert (alsa_monitor.rules, {
  matches = {
    {
      { "node.name", "matches", "alsa_input.*" },
    },
    {
      { "node.name", "matches", "alsa_output.*" },
    },
  },
  apply_properties = {
    ["session.suspend-timeout-seconds"] = 0,
  },
})
