pipeline {
    agent any 
    stages {
        stage('Build') { 
            steps {
                sh 'mkdir build'
                sh 'cd build'
                sh 'cmake ..'
                sh 'make'
            }
        }
    }
}