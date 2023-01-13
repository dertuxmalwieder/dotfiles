local wezterm = require 'wezterm'
return {
	default_prog = { 'C:\\Program Files\\PowerShell\\7\\pwsh.exe', '--NoLogo' },
	color_scheme = "Ashes (base16)",
	font = wezterm.font_with_fallback {
		'Consolas',
		'JetBrains Mono',
		'Fira Code',
	},
	font_size = 11.0,
}
