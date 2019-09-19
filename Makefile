BASEDIR=$(PWD)
APPNAME=datakam
PROJECTID=kamestery

container:
	gcloud builds submit --tag gcr.io/$(PROJECTID)/$(APPNAME)

deploy: container
	gcloud beta run deploy --image gcr.io/$(PROJECTID)/$(APPNAME) --platform managed

