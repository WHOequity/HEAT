

```bash
#
build -f ./data-raw/Docker/app_base/Dockerfile --build-arg N_CPUS=4 --build-arg GITHUB_PAT=<MY_GITHUB_PAT> --platform linux/amd64 -t heat-app-base:0.1 .

export AWS_PROFILE=PROJECT
aws ecr describe-repositories
aws ecr get-login-password | docker login --username AWS --password-stdin 460267073744.dkr.ecr.us-east-1.amazonaws.com/heatplus
docker tag heat-plus-app:0.1 460267073744.dkr.ecr.us-east-1.amazonaws.com/heatplus:latest
docker push 460267073744.dkr.ecr.us-east-1.amazonaws.com/heatplus:latest

```
