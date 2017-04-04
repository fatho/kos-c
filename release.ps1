$cabal_file = "kos-c.cabal"
$release_dir = "release-packages"

# check that we are in project root
If (-Not (Test-Path $cabal_file)) {
    Write-Error "Not in project root"
    Exit 1
}

# create release directory if necessary
If (-Not (Test-Path $release_dir)) {
    New-Item -ItemType Directory -Path $release_dir
}

# Build project
& stack build 2>&1 | %{ "$_" }

If ($LASTEXITCODE -ne 0) {
    Write-Error "Failed to build project"
    Exit 1
}

# Find version
$version = (Select-String -Path $cabal_file -Pattern "version:\s*(\d\.\d\.\d\.\d)").Matches[0].Groups[1].Value

Write-Output "Packaging release v$version"

# create target directory
$package_name="kos-c-$version-windows-x86_64"
$package_dir="$release_dir\$package_name"
$zip_path="$release_dir\$package_name.zip"

If (Test-Path $package_dir) {
    Write-Output "Package already exists. Rebuild?"
    $title = "Rebuild package"
    $message = "Package already exists. Delete and rebuild?"
    $yes = New-Object System.Management.Automation.Host.ChoiceDescription "&Yes", `
        "Deletes package."
    $no = New-Object System.Management.Automation.Host.ChoiceDescription "&No", `
        "Exits."
    $options = [System.Management.Automation.Host.ChoiceDescription[]]($yes, $no)
    $result = $host.ui.PromptForChoice($title, $message, $options, 0) 
    switch ($result)
        {
            0 { 
                Write-Output "Deleting package."
                Remove-Item -Path $package_dir -Recurse 
                If (Test-Path $zip_path) {
                    Write-Output "Deleting zip."
                    Remove-Item -Path $zip_path
                }
            }
            1 { exit 1 }
        }
}
New-Item -ItemType Directory -Path $package_dir

# copy stuff to target
Copy-Item  -Path ".\.stack-work\install\65995373\bin\koscc.exe" -Destination ".\$package_dir"
Copy-Item -Path ".\kosc-prelude" -Destination ".\$package_dir" -Recurse

# create zip
Compress-Archive -Path ".\$package_dir" -DestinationPath ".\$zip_path"

Write-Output "Done!"