@ECHO OFF
FOR %%f IN (.zshrc,.gitconfig.windows,.emacs.d) DO (
  MKLINK %HOME%\%%f %~dp0%%f
  ECHO Created %HOME%\%%f
)
