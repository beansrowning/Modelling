# Script to send a keypress (.) every 60 seconds to prevent screensaver from turning on.
# The script will run for as many minutes as the $minutes parameter is set to run for
# source: https://stackoverflow.com/questions/15835941/powershell-mouse-move-does-not-prevent-idle-mode
 param($minutes = 300)

$myshell = New-Object -com "Wscript.Shell"

for ($i = 0; $i -lt $minutes; $i++) {
   Start-Sleep -Seconds 60
   $myshell.sendkeys(".")
 }