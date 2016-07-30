@ECHO OFF
SETLOCAL ENABLEDELAYEDEXPANSION
FOR %%f IN (.zshrc,.gitconfig.windows,.emacs.d) DO (
  SET f=%%f
  MKLINK %UserProfile%\!f:.windows=! %~dp0%%f
  ECHO Created %UserProfile%\!f:.windows=!
)
