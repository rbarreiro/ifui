docker build --compress --rm -t ifui_dev_env .
if ($?) {
    docker run --rm -v ((Get-Item ..).FullName + ":/ifui") --name ifui_dev_env -it ifui_dev_env
}
