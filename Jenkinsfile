pipeline {
    agent any 
    stages {
        stage('Build') { 
            steps{
                cmake arguments: '-DCMAKE_PREFIX_PATH=/usr/lib/pfUnit'
                installation: 'InSearchPath'
                cmakeBuild buildType: 'Debug'
                cleanBuild: true
                installation: 'InSearchPath'
                steps: [[withCmake: true]]
            }
        }
    }
}