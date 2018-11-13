// +build ignore

package main

import (
	"bytes"
	"flag"
	"fmt"
	"io"
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
	workerBuilderImage = flag.Bool("worker:builder:images", false, "Build Worker Docker Image.")
	prepDeploy         = flag.Bool("prep:deploy", false, "Prepare Deployment Assets.")
	compileWorkers     = flag.Bool("compile:workers", false, "Cross Compile Workers for Lambda/Ubuntu Target Platforms.")
	deployFunctions    = flag.Bool("deploy:functions", false, "Deploy all Lambda Functions.")
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

	if *prepDeploy {
		deploymentAssets := baseDir + "/deployment_assets"
		cleanupDeploymentAssets(deploymentAssets)
		prepDeploymentAssets(deploymentAssets)
	}

	if *compileWorkers {
		buildWorkers()
	}

	if *deployFunctions {
		preBuild()
		deploymentAssets := baseDir + "/deployment_assets"
		defer cleanupDeploymentAssets(deploymentAssets)
		prepDeploymentAssets(deploymentAssets)
		// buildWorkerDockerImage()
		var wg sync.WaitGroup
		wg.Add(1)
		go buildWorkersForDeploy(&wg, deploymentAssets)
		wg.Add(1)
		go buildWebApp(&wg, deploymentAssets, "webkam", "dev")
		wg.Add(1)
		go buildWebApp(&wg, deploymentAssets, "controlkam", "ck_dev")
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
	log.Printf("Building Worker Docker Images")
	var wgDockerBuilder sync.WaitGroup
	wgDockerBuilder.Add(1)
	go func(wg *sync.WaitGroup) {
		lambdaWorkerBuilderCmd := exec.Command("docker", "build", "-t", "outcastgeek/rust-lambda:0.1", "-f", baseDir+"/infrastructure/Dockerfile.lambda", baseDir+"/infrastructure")
		execute(lambdaWorkerBuilderCmd)
		wg.Done()
	}(&wgDockerBuilder)
	// wgDockerBuilder.Add(1)
	// go func(wg *sync.WaitGroup) {
	// 	ubuntuWorkerBuildermd := exec.Command("docker", "build", "-t", "kamestery/worker_rpc:0.1", "-f", baseDir+"/infrastructure/Dockerfile.ubuntu", baseDir)
	// 	execute(ubuntuWorkerBuildermd)
	// 	wg.Done()
	// }(&wgDockerBuilder)
	wgDockerBuilder.Wait()
}

func buildWorkers() {
	log.Print("Cross Compiling Workers...")
	var wgWorker sync.WaitGroup
	wgWorker.Add(1)
	go func(wg *sync.WaitGroup) {
		log.Print("Cross Compiling Lambda Workers...")
		buildWorkerLambdaCmd := exec.Command("sh", baseDir+"/cmd.sh", "build.workers.lambda")
		execute(buildWorkerLambdaCmd)
		wg.Done()
	}(&wgWorker)
	// wgWorker.Add(1)
	// go func(wg *sync.WaitGroup) {
	// 	log.Print("Cross Compiling Ubuntu Workers...")
	// 	buildWorkerUbuntuCmd := exec.Command("sh", baseDir+"/cmd.sh", "build.workers.ubuntu")
	// 	execute(buildWorkerUbuntuCmd)
	// 	wg.Done()
	// }(&wgWorker)
	wgWorker.Wait()
}

func buildWorkersForDeploy(wg *sync.WaitGroup, distBase string) {
	log.Print("Building Workers for Deployment...")
	buildWorkers()
	publishBinaryCmd := exec.Command("cp", baseDir+"/target/release/worker-fn", distBase+"/workerfn/server")
	execute(publishBinaryCmd)
	// publishBinaryCmd = exec.Command("cp", baseDir+"/target/release/worker-rpc", distBase+"/webkam/worker-rpc")
	// execute(publishBinaryCmd)
	deployCmd := exec.Command("sh", baseDir+"/cmd.sh", "deploy.function", distBase+"/workerfn/", "data_dev")
	execute(deployCmd)
	wg.Done()
}

func buildWebApp(wg *sync.WaitGroup, distBase, webapp, env string) {
	log.Printf("Building %s...", webapp)
	buildWorkerCmd := exec.Command("sh", baseDir+"/cmd.sh", "build.webapp", baseDir+"/"+webapp)
	execute(buildWorkerCmd)
	publishBinaryCmd := exec.Command("cp", baseDir+"/"+webapp+"/server", distBase+"/"+webapp+"/server")
	execute(publishBinaryCmd)
	publishBinaryCmd = exec.Command("cp", "-R", baseDir+"/"+webapp+"/templates", distBase+"/"+webapp+"/templates")
	execute(publishBinaryCmd)
	publishBinaryCmd = exec.Command("cp", "-R", baseDir+"/"+webapp+"/static", distBase+"/"+webapp+"/static")
	execute(publishBinaryCmd)
	deployCmd := exec.Command("sh", baseDir+"/cmd.sh", "deploy.function", distBase+"/"+webapp+"/", env)
	execute(deployCmd)
	wg.Done()
}

func prepDeploymentAssets(deploymentAssets string) {
	log.Print("Prepping the Deployment Assets...")
	cleanupDeploymentAssets(deploymentAssets)
	assetsFoldersCmd := exec.Command("mkdir", "-p", deploymentAssets+"/workerfn")
	execute(assetsFoldersCmd)
	assetsFoldersCmd = exec.Command("mkdir", "-p", deploymentAssets+"/webkam")
	execute(assetsFoldersCmd)
	assetsFoldersCmd = exec.Command("mkdir", "-p", deploymentAssets+"/controlkam")
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
			"log_level":       "DEBUG",
			"backend_key":     "data-dev",
			"rpc_backend_key": "35.175.245.161:80",
		},
	)
	writeFile(deploymentAssets+"/webkam/up.json", upWebkam)
	// Prep ControlKam
	upControlkam := renderTmpl(
		"controlkam_up_json.tmpl",
		map[string]string{
			"log_level":       "DEBUG",
			"backend_key":     "data-dev",
			"rpc_backend_key": "35.175.245.161:80",
		},
	)
	writeFile(deploymentAssets+"/controlkam/up.json", upControlkam)
}

func cleanupDeploymentAssets(deploymentAssets string) {
	cleanupDeploymentAssets := exec.Command("rm", "-rf", deploymentAssets)
	execute(cleanupDeploymentAssets)
}

func execute(cmd *exec.Cmd) {
	log.Print("......................................................................")
	stdout, err := cmd.StdoutPipe()
	checkError(err)
	stderr, err := cmd.StderrPipe()
	checkError(err)

	// Start command
	err = cmd.Start()
	checkError(err)

	defer cmd.Wait() // Doesn't block

	// Non-blockingly echo command output to terminal
	go io.Copy(os.Stdout, stdout)
	go io.Copy(os.Stderr, stderr)
}

func checkError(err error) {
	if err != nil {
		log.Fatalf("ERROR:::: %+v", err)
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
