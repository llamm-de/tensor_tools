pipeline {
    agent any 
    stages {
        stage('Build') { 
            cmakeBuild
            generator: 'make',
            buildDir: 'build',
            sourceDir: 'src',
            installation: 'InSearchPath',
            steps: [
                [args: 'all install', envVars: 'DESTDIR=${WORKSPACE}/artifacts']
            ]
        }
    }
}