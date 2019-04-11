pipeline {
    agent any
    stages {
      stage('Testing') {
        steps {
          sh '/var/lib/jenkins/.cabal/bin/doctest `find ./ -name "*.hs"`'
        }
      }
    }
}
