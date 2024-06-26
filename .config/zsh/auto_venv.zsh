# This scrirpt will attempt to automatically activate an venv at '.env'
# if it is avaiable upon cd'ing into it. When cd'ing out of it, it will
# attempt to deactivate the environment again.

VENV_DEFAULT=".venv"
function mkvenv() {
  local VENV_NAME="${1:-$VENV_DEFAULT}"
  echo "Creating venv in $VENV_NAME"
  PY3=`which python3`
  $PY3 -m venv ${VENV_NAME}

  if [[ ! -d ".git" ]]; then
    echo "Initializing a new Git repository..."
    git init
  else
    echo "Git repository already exists."
  fi

  autoactivate_python_venv
}


VENV_CURRENT=""
function autoactivate_python_venv() {
  # Grab the path to the current git project
  local PROJ=`git-root 2> /dev/null` || ""

  # If we are no longer in a project and venv seems active, deactivate now
  if [[ -z $PROJ ]]; then
  	if [[ -d $VENV_CURRENT ]]; then
  	  deactivate > /dev/null 2>&1
  	  VENV_CURRENT=""
  	fi
    if [[ -n $CONDA_PREV_ENV ]]; then
      conda activate $CONDA_PREV_ENV
      CONDA_PREV_ENV=""
    fi

  	return
  fi

  # We are in a git project, let's see if we can find a venv directory
  local ENVDIR=$PROJ/$VENV_DEFAULT

  # If a venv directory is present...
  if [[ -d $ENVDIR ]]; then
    CONDA_PREV_ENV=$CONDA_DEFAULT_ENV
    if [[ -n CONDA_DEFAULT_ENV ]]; then
      conda deactivate
    fi
  	if [[ $ENVDIR != $VENV_CURRENT ]]; then
  	  # Deactivate the venv we're currently in
  	  [[ -d $VENV_CURRENT ]] && deactivate > /dev/null 2>&1

  	  # Activate the new venv
      source $ENVDIR/bin/activate > /dev/null 2>&1
  	fi

   	# Record the currently active venv
   	VENV_CURRENT=$ENVDIR
  fi
}

autoload -U add-zsh-hook
add-zsh-hook chpwd autoactivate_python_venv

alias activate_venv=autoactivate_python_venv
