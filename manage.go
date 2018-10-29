// +build ignore

package main

import (
	"bytes"
	"flag"
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"os/exec"
	"path/filepath"
	"sync"
	"text/template"
	"time"
)

// Flags
var (
	devopsInit         = flag.Bool("devops:init", false, "Initializes DevOps Tooling.")
	workerBuilderImage = flag.Bool("worker:builder:image", false, "Build Worker Docker Image.")
	deployFunctions    = flag.Bool("deploy:functions", true, "Deploy all Lambda Functions.")
)

// Directories
var (
	baseDir = func() string {
		bd := os.Getenv("BASEDIR")
		if len(bd) == 0 {
			dir, err := filepath.Abs(filepath.Dir(os.Args[1]))
			if err != nil {
				dir = "."
			}
			bd = dir
		}
		return bd
	}()
	templatesDir = baseDir + "/resources/templates"
)

func main() {
	log.SetFlags(0)
	flag.Parse()

	if *devopsInit {
		terraformInit()
	}

	if *workerBuilderImage {
		buildWorkerDockerImage()
	}

	if *deployFunctions {
		preBuild()
		deploymentAssets := baseDir + "/deployment_assets"
		defer cleanupDeploymentAssets(deploymentAssets)
		prepDeploymentAssets(deploymentAssets)
		buildWorkerDockerImage()
		var wg sync.WaitGroup
		wg.Add(1)
		go buildWorkers(&wg, deploymentAssets)
		wg.Add(1)
		go buildWebKam(&wg, deploymentAssets)
		wg.Wait()
		log.Print("Completed Functions Deployment")
	}
}

func preBuild() {
	log.Print("Pre-Building...")
	submodulesUpdateCmd := exec.Command("sh", baseDir+"/cmd.sh", "sub.update")
	execute(submodulesUpdateCmd)
	genRsaCmd := exec.Command("sh", baseDir+"/cmd.sh", "gen.rsa")
	execute(genRsaCmd)
}

func terraformInit() {
	log.Print("Initializing Terraform...")
	devopsInitCmd := exec.Command("sh", baseDir+"/cmd.sh", "sub.update")
	execute(devopsInitCmd)
}

func buildWorkerDockerImage() {
	log.Printf("Building Woker Docker Image")
	cmd := exec.Command("docker", "build", "-t", "og-rust-lambda:latest", baseDir+"/infrastructure")
	execute(cmd)
}

func buildWorkers(wg *sync.WaitGroup, distBase string) {
	log.Print("Building Workers...")
	buildWorkerCmd := exec.Command("sh", baseDir+"/cmd.sh", "build.workers")
	execute(buildWorkerCmd)
	publishBinaryCmd := exec.Command("cp", baseDir+"/target/release/worker-fn", distBase+"/workerfn/server")
	execute(publishBinaryCmd)
	publishBinaryCmd = exec.Command("cp", baseDir+"/target/release/worker-rpc", distBase+"/workerfn/worker-rpc")
	execute(publishBinaryCmd)
	deployCmd := exec.Command("sh", baseDir+"/cmd.sh", "deploy.function", distBase+"/workerfn/", "data_dev")
	execute(deployCmd)
	wg.Done()
}

func buildWebKam(wg *sync.WaitGroup, distBase string) {
	log.Print("Building WebKam...")
	buildWorkerCmd := exec.Command("sh", baseDir+"/cmd.sh", "build.webkam")
	execute(buildWorkerCmd)
	publishBinaryCmd := exec.Command("cp", baseDir+"/webkam/server", distBase+"/webkam/server")
	execute(publishBinaryCmd)
	publishBinaryCmd = exec.Command("cp", "-R", baseDir+"/webkam/templates", distBase+"/webkam/templates")
	execute(publishBinaryCmd)
	publishBinaryCmd = exec.Command("cp", "-R", baseDir+"/webkam/static", distBase+"/webkam/static")
	execute(publishBinaryCmd)
	deployCmd := exec.Command("sh", baseDir+"/cmd.sh", "deploy.function", distBase+"/webkam/", "dev")
	execute(deployCmd)
	wg.Done()
}

func prepDeploymentAssets(deploymentAssets string) {
	log.Print("Prepping the Deployment Assets...")
	cleanupDeploymentAssets(deploymentAssets)
	assetsFoldersCmd := exec.Command("mkdir", "-p", deploymentAssets+"/workerfn")
	execute(assetsFoldersCmd)
	assetsFoldersCmd = exec.Command("mkdir", deploymentAssets+"/webkam")
	execute(assetsFoldersCmd)
	cargoFolderCmd := exec.Command("mkdir", "-p", baseDir+"/cargo")
	execute(cargoFolderCmd)
	// Prep WorkerFn
	upWorkerFn := renderTmpl(
		"workerfn_up_json.tmpl",
		map[string]string{
			"log_level": "actix_web,worker_lib,worker_fn=debug",
		},
	)
	writeFile(deploymentAssets+"/workerfn/up.json", upWorkerFn)
	// Prep WebKam
	upWebkam := renderTmpl(
		"webkam_up_json.tmpl",
		map[string]string{
			"log_level":   "DEBUG",
			"backend_key": "data-dev",
		},
	)
	writeFile(deploymentAssets+"/webkam/up.json", upWebkam)
}

func cleanupDeploymentAssets(deploymentAssets string) {
	cleanupDeploymentAssets := exec.Command("rm", "-rf", deploymentAssets)
	execute(cleanupDeploymentAssets)
}

func execute(cmd *exec.Cmd) {
	log.Print("......................................................................")
	var buf bytes.Buffer
	cmd.Stderr = &buf
	if err := cmd.Run(); err != nil {
		log.Fatal(fmt.Errorf("ERROR:::: %v, %v", err, buf.String()))
	}
}

func writeFile(path, content string) {
	log.Printf("Writing File: %s", path)
	// write the whole body at once
	if err := ioutil.WriteFile(path, []byte(content), 0644); err != nil {
		log.Fatal(fmt.Errorf("ERROR:::: %v", err))
	}
}

func renderTmpl(tmpl string, data interface{}) string {
	buf := new(bytes.Buffer)
	templating.ExecuteTemplate(buf, tmpl, data)
	writtenString := buf.String()
	return writtenString
}

// Templating
var (
	tpl        = template.New("")
	templating = template.Must(
		tpl.Funcs(template.FuncMap{
			"env": func(envVarName string) (envVarValue string) {
				if envVarValue = os.Getenv(envVarName); len(envVarValue) == 0 {
					envVarValue = "No Found"
				}
				return
			},
			"Now": func(format string) string {
				now := time.Now()
				now_str := now.Format(format)
				return now_str
			},
		}).ParseGlob(templatesDir + "/*"))
)
