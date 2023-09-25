local wezterm = require 'wezterm'
local config = {}

if wezterm.config_builder then
  config = wezterm.config_builder()
end

config.color_scheme = 'Novel'
config.use_fancy_tab_bar = true
config.font = wezterm.font { family = 'Hack' }
config.font_size = 11.0
config.window_close_confirmation = 'NeverPrompt'

config.window_frame = {
  font = wezterm.font { family = 'Hack' },
  font_size = 11.0,
}

config.colors = {
  tab_bar = {
    background = '#999999',
    active_tab = {
      bg_color = '#ababab',
      fg_color = '#000000',
    },
    new_tab = {
      bg_color = '#aaaaaa',
      fg_color = '#000000',
    },
  },
}

return config
