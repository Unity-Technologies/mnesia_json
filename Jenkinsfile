pipeline {
    agent { label 'linux_build' }

    stages {
        stage('Build') {
            steps {
                script {
                    initGitlabStatus(STAGE_NAME)
                    deleteDir()
                    final scmVars = checkout scm
                    def gitCommit = scmVars.GIT_COMMIT
                    def shortGitCommit = gitCommit[0..6]
                    currentBuild.setDescription(shortGitCommit)
                }
                
                sh 'make test'
                archiveArtifacts artifacts: 'artifacts/**', fingerprint: true
            }
            
            post {
                success {
                    handleStatusUpdate(STAGE_NAME, true, true)
                }
                failure {
                    handleStatusUpdate(STAGE_NAME, false, true)
                }
            }
        }
    }    
}

def initGitlabStatus(stageName) {
    updateGitlabCommitStatus name: STAGE_NAME, state: 'running'
}

def handleStatusUpdate(stageName, passed, sendMessage = false) {
    slackChannel = '#appservices-jenkins'

    if (passed) {
        state = 'success'
        messageColor = '#439fe0'
        message = "SUCCESSFUL: Job '${env.JOB_NAME} [${env.BUILD_NUMBER}]' (${env.BUILD_URL})"
    } else {
        state = 'failed'
        messageColor = '#FF0000'
        message = "FAILED: Job '${env.JOB_NAME} [${env.BUILD_NUMBER}]' (${env.BUILD_URL})"
    }

    if (env.JOB_NAME.contains('webhook')) {
        message += " \nMerge Request: ${env.gitlabSourceBranch} => ${env.gitlabTargetBranch} (http://git.vivox.com/dev-public/mnesia-json/merge_requests/${env.gitlabMergeRequestIid})"
    }

    if (sendMessage) {
        slackSend channel: slackChannel, color: messageColor, message: message
    }

    updateGitlabCommitStatus name: stageName, state: state
}

