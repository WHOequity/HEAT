type credentials into /Users/zevross/.aws/credentials
normally the 759 thing is docker.io
docker help login
you could add to .aws/config region = us-east-1

```
export AWS_PROFILE=abcd
aws ecr get-login-password --region us-east-1 | docker login --username AWS --password-stdin 759878630063.dkr.ecr.us-east-1.amazonaws.com
aws ecr describe-repositories --region us-east-1
docker tag heat-app:0.1 759878630063.dkr.ecr.us-east-1.amazonaws.com/heat-app:0.1
docker push 759878630063.dkr.ecr.us-east-1.amazonaws.com/heat-app:0.1
```
