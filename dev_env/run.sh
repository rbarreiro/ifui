docker build --compress --rm -t ifui_dev_env . &&\
docker run --rm  -v "$(pwd | xargs dirname):/ifui" --name ifui_dev_env -it ifui_dev_env
