- Set working directory as top-level `R` project directory (i.e. `HEAT-development`)

  - **NOTE:** Replace `<MY_GITHUB_PAT>` with your own GitHub PAT 

```bash
#
docker build -f ./HEAT-development/data-raw/Docker/app_heatplus/Dockerfile --build-arg N_CPUS=4 --platform linux/amd64 -t heat-plus-app:0.1 .

# Determine the image ID
docker image ls

# The number at the end is the image ID
docker run --rm -p 8080:8080 
```

Then browse to `http://localhost:8080/`
