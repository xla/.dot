function start_agent
  echo "Initializing new SSH agent ..."
  ssh-agent -c | sed 's/^echo/#echo/' > $SSH_ENV
  chmod 600 $SSH_ENV 
  . $SSH_ENV > /dev/null
  ssh-add $HOME/.ssh/id_(nice hostname)
end
