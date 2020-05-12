pipeline {
    agent any 
    stages {
        stage('Build') { 
            cmakeBuild
            generator: 'Make',
            buildDir: 'build',
            sourceDir: 'src',
            installation: 'InSearchPath',
            steps: [
                [args: 'all install', envVars: 'DESTDIR=${WORKSPACE}/artifacts']
            ]
        }
    }
}