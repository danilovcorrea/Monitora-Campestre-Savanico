param(
  [Parameter(Mandatory = $true)][string]$RepoRoot,
  [Parameter(Mandatory = $true)][string]$RealCsv
)

$ErrorActionPreference = "Stop"
$rscript = "C:\Program Files\R\R-4.6.0\bin\x64\Rscript.exe"
$publico = Join-Path $RepoRoot "monitora_campsav_alvo_global_v2.9.24.R"
$baseline = Join-Path $RepoRoot "monitora_campsav_alvo_global_v2.9.23.R"

foreach ($arquivo in @($rscript, $publico, $baseline, $RealCsv)) {
  if (-not (Test-Path -LiteralPath $arquivo)) {
    throw "Arquivo obrigatório ausente: $arquivo"
  }
}

function Invoke-RGate {
  param(
    [Parameter(Mandatory = $true)][string]$Nome,
    [Parameter(Mandatory = $true)][string]$Script,
    [Parameter(Mandatory = $true)][string[]]$Argumentos
  )
  $stdout = Join-Path $env:TEMP ("monitora_v2924_" + $Nome + "_stdout.txt")
  $stderr = Join-Path $env:TEMP ("monitora_v2924_" + $Nome + "_stderr.txt")
  $argumentList = @('"' + $Script.Replace('"', '\"') + '"') + @(
    $Argumentos | ForEach-Object { '"' + $_.Replace('"', '\"') + '"' }
  )
  $processo = Start-Process -FilePath $rscript `
    -ArgumentList $argumentList `
    -RedirectStandardOutput $stdout `
    -RedirectStandardError $stderr `
    -NoNewWindow `
    -Wait `
    -PassThru
  if (Test-Path -LiteralPath $stdout) { Get-Content -LiteralPath $stdout }
  if (Test-Path -LiteralPath $stderr) { Get-Content -LiteralPath $stderr }
  if ($processo.ExitCode -ne 0) {
    throw "Gate $Nome falhou no R do Windows; exit=$($processo.ExitCode)."
  }
}

Invoke-RGate `
  -Nome "contrato" `
  -Script (Join-Path $RepoRoot "tests\test_v2924_integridade_contrato_release.R") `
  -Argumentos @($publico, $baseline)
Invoke-RGate `
  -Nome "aspas" `
  -Script (Join-Path $RepoRoot "tests\test_v2924_csv_aspas_extremas.R") `
  -Argumentos @($publico, $RealCsv)
Invoke-RGate `
  -Nome "grafia_uc" `
  -Script (Join-Path $RepoRoot "tests\test_v2924_uc_grafia_windows.R") `
  -Argumentos @($publico, $baseline)

Write-Output "TEST_V2924_RELEASE_WINDOWS_OK"
