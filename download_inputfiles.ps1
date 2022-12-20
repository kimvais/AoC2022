$URI = "https://adventofcode.com"
$webSession = [Microsoft.PowerShell.Commands.WebRequestSession]::new()
$webSession.Cookies.Add($URI,[System.Net.Cookie]::new("session", $Env:AOC_SESSION_COOKIE))

for ($i = 1; $i -lt 26; $i++) {
    $fileUrl = "https://adventofcode.com/2022/day/$i/input"
    Write-Host "Downloading $fileUrl"
    try
    {
        Invoke-Webrequest $fileUrl -WebSession $webSession -OutFile input/$i.txt | Out-Null
    }
    catch
    {
        break
    }
}

