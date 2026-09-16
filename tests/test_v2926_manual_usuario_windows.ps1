param(
  [Parameter(Mandatory=$true)][string]$Helper,
  [Parameter(Mandatory=$true)][string]$Candidate,
  [Parameter(Mandatory=$true)][string]$OutputDir
)
$ErrorActionPreference = "Stop"
$env:RSTUDIO_PANDOC = "C:\Program Files\RStudio\resources\app\bin\quarto\bin\tools"
$rscript = "C:\Program Files\R\R-4.6.0\bin\x64\Rscript.exe"
$test = Join-Path $PSScriptRoot "test_v2926_manual_usuario_atualizado.R"
$saida = & $rscript $test $Helper $Candidate $OutputDir 2>&1
$codigo = $LASTEXITCODE
$saida | Tee-Object -FilePath ($OutputDir + ".test.log") | Write-Output
if ($codigo -ne 0) { exit $codigo }
