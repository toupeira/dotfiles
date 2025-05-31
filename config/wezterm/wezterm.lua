local wezterm = require 'wezterm'
local config = wezterm.config_builder()
local action = wezterm.action

config.adjust_window_size_when_changing_font_size = false
config.check_for_updates = false
config.enable_scroll_bar = false
config.hide_tab_bar_if_only_one_tab = true
config.max_fps = 120
config.scrollback_lines = 10000
config.window_background_opacity = 0.85

config.window_padding = {
  left = 0,
  right = 0,
  top = 0,
  bottom = 0,
}

config.colors = {
  foreground = 'white',
  background = 'black',
  cursor_bg = 'white',
  cursor_border = 'white',
  selection_fg = 'black',
  selection_bg = '#fdd',
  visual_bell = '#545652',

  ansi = {
    '#000000',
    '#FF4040',
    '#4D9A05',
    '#C3A000',
    '#417FCF',
    '#8E6199',
    '#05979A',
    '#D3D6CF',
  },

  brights = {
    '#545652',
    '#FF4040',
    '#89E234',
    '#FBE84F',
    '#82B1FF',
    '#AC7EA8',
    '#34E2E2',
    '#EDEDEB'
  },
}

config.visual_bell = {
  fade_in_duration_ms = 50,
  fade_out_duration_ms = 75,
}

-- https://wezfurlong.org/wezterm/config/keys.html
config.disable_default_key_bindings = true
config.keys = {
  { key = '-', mods = 'CTRL', action = action.SendKey { key = '_', mods = 'CTRL' } },

  { key = '+', mods = 'SHIFT|CTRL', action = action.IncreaseFontSize },
  { key = '_', mods = 'SHIFT|CTRL', action = action.DecreaseFontSize },
  { key = ')', mods = 'SHIFT|CTRL', action = action.ResetFontSize },

  { key = 'c', mods = 'SHIFT|CTRL', action = action.CopyTo 'Clipboard' },
  { key = 'v', mods = 'SHIFT|CTRL', action = action.PasteFrom 'Clipboard' },

  { key = 'l', mods = 'SHIFT|CTRL', action = action.ShowDebugOverlay },
  { key = 'p', mods = 'SHIFT|CTRL', action = action.ActivateCommandPalette },
  { key = 'r', mods = 'SHIFT|CTRL', action = action.ReloadConfiguration },
  { key = 'u', mods = 'SHIFT|CTRL', action = action.CharSelect{ copy_on_select = true, copy_to =  'ClipboardAndPrimarySelection' } },
  { key = 'phys:Space', mods = 'SHIFT|CTRL', action = action.QuickSelect },
}

-- Work around https://github.com/wez/wezterm/issues/3803
config.hyperlink_rules = wezterm.default_hyperlink_rules()
config.hyperlink_rules[4] = {
  regex = '[^(]\\b(\\w+://\\S+[)/a-zA-Z0-9-]+)',
  format = '$1',
  highlight = 1,
}

return config
