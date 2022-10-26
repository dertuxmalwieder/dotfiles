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
