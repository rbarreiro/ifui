docker build --compress --rm -t ifui_dev_env . &&\
docker run --rm  -p "6012:6012" -p "6011:6011"  -v "$(pwd | xargs dirname):/ifui" --name ifui_dev_env -it ifui_dev_env
