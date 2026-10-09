function fish_prompt
  # Keep the prompt red on failure; reset it on the next successful command.
  set -l previous_status $status
  set -l color normal
  if test $previous_status -gt 0
      set color red
  end

  set_color $color
  echo -n "|> "

  set_color normal
end
