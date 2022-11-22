# Improving PowerShell on Windows Terminal.
# ------------------------------------------

# ReadLine -> Emacs:
Import-Module PSReadLine
Set-PSReadlineOption -EditMode Emacs

# Chocolatey:
$ChocolateyProfile = "$env:ChocolateyInstall\helpers\chocolateyProfile.psm1"
if (Test-Path($ChocolateyProfile)) {
	Import-Module "$ChocolateyProfile"
}

# posh-git:
Import-Module posh-git

# Simulate the "su" command:
function su {
	If (-NOT ([Security.Principal.WindowsPrincipal] [Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator))
	{
		If ($env:ALACRITTY_LOG -ne $null) {
			# Alacritty:
			Start-Process alacritty -Verb RunAs
		}
		Else {
			# Probably, Windows Terminal (at least on my computers):
			Start-Process wt -Verb RunAs
		}

		# Optionally close the non-elevated terminal:
		# exit
	}
}

# tail-f:
function tail-f([string]$filename) {
	Get-Content "$filename" -Tail 10 -Wait
}

# which:
New-Alias which Get-Command

# vi:
New-Alias vi nvim

# find:
function find([string]$file) {
	gci -r -fi $file
}

# Celsius <-> Fahrenheit:
function FtoC([double]$fahrenheit) {
	$celsius = ($fahrenheit - 32) * (5/9)
	'{0} °F = {1} °C' -f $fahrenheit,[math]::Round($celsius,3)
}

function CtoF([double]$celsius) {
	$fahrenheit = ($celsius * (9/5)) + 32
	'{0} °C = {1} °F' -f $celsius,[math]::Round($fahrenheit,3)
}
