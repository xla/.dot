function fish_prompt
  # Print a red dot if the exit status of the previous command isn't 0.
  if test $status -gt 0
      set color red
  end
  
  set_color $color
  echo -n "|> "

  set_color normal
end
