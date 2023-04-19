- Set working directory as top-level `R` project directory (i.e. `HEAT-development`)

  - **NOTE:** Replace `<MY_GITHUB_PAT>` with your own GitHub PAT 

```bash
#
build -f ./data-raw/Docker/app_base/Dockerfile --build-arg N_CPUS=4 --build-arg GITHUB_PAT=<MY_GITHUB_PAT> --platform linux/amd64 -t heat-app-base:0.1 .
```
