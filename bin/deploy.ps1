Copy-Item $env:AppData\.emacs $env:AppData\.emacsbak

Robocopy.exe .\ $env:AppData dotEmacs.el

Move-Item $env:AppData\dotEmacs.el $env:AppData\.emacs -Force

$container = "$env:AppData\.emacs.d\MyEmacs"
if (-not (Test-Path $container)) {
	New-Item -ItemType Directory -Force -Path $container
}

Robocopy.exe ..\MyEmacs $container *.el /IS /S
