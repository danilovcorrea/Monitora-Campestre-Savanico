param([Parameter(Mandatory = $true)][string]$SuiteDir)
$ErrorActionPreference = "Stop"
$rscript = "C:\Program Files\R\R-4.6.0\bin\x64\Rscript.exe"
if (-not (Test-Path -LiteralPath $rscript)) { throw "Rscript Windows indisponível." }
function Invoke-Gate {
  param([string]$Name, [string]$Script, [string[]]$GateArgs)
  $stdout = Join-Path $SuiteDir ($Name + "_stdout.txt")
  $stderr = Join-Path $SuiteDir ($Name + "_stderr.txt")
  $argumentList = @('"' + $Script + '"') + @($GateArgs | ForEach-Object { '"' + $_ + '"' })
  $p = Start-Process -FilePath $rscript -ArgumentList $argumentList `
    -RedirectStandardOutput $stdout -RedirectStandardError $stderr `
    -NoNewWindow -Wait -PassThru
  if (Test-Path -LiteralPath $stdout) { Get-Content -LiteralPath $stdout }
  if (Test-Path -LiteralPath $stderr) { Get-Content -LiteralPath $stderr }
  if ($p.ExitCode -ne 0) { throw "$Name falhou; exit=$($p.ExitCode)" }
}
$publica = Join-Path $SuiteDir "monitora_campsav_alvo_global_v2.9.25.R"
$baseline = Join-Path $SuiteDir "monitora_campsav_alvo_global_v2.9.24.R"
$candidata = Join-Path $SuiteDir "monitora_campsav_alvo_global_v2.9.25-dev_r09.R"
Invoke-Gate -Name "contrato" -Script (Join-Path $SuiteDir "test_v2925_integridade_contrato_release.R") -GateArgs @($publica,$baseline)
Invoke-Gate -Name "promocao" -Script (Join-Path $SuiteDir "test_v2925_promocao_candidata.R") -GateArgs @($candidata,$publica)
Write-Output "TEST_V2925_RELEASE_WINDOWS_OK"
