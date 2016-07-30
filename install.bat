@ECHO OFF
MKLINK %UserProfile%\.zshrc %~dp0.zshrc
MKLINK %UserProfile%\.gitconfig %~dp0.gitconfig.windows
MKLINK /D %UserProfile%\.emacs.d %~dp0.emacs.d
