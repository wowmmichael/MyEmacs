Copy-Item $env:HOME\.emacs $env:HOME\.emacsbak

Robocopy.exe .\ $env:HOME dotEmacs.el

Move-Item $env:HOME\dotEmacs.el $env:HOME\.emacs -Force

$container = "$env:HOME\.emacs.d\MyEmacs"
if (-not (Test-Path $container)) {
        New-Item -ItemType Directory -Force -Path $container
}

Robocopy.exe ..\MyEmacs $container *.el /IS /S
