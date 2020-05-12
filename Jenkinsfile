pipeline {
    agent any 
    stages {
        stage('Build') { 
            steps{
                cmake arguments: '', installation: 'InSearchPath'
                cmakeBuild buildType: 'Debug', cleanBuild: true, installation: 'InSearchPath', steps: [[withCmake: true]]
            }
        }
    }
}