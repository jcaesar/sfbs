#!/usr/bin/env nu

def main [--keep-min: int = 5, --keep-max: int = 100, --keep-duration: duration = 2wk] {
  let cands = glob builds/*/meta.json
    | each {|f| open $f | insert path ($f | path dirname) }
    | reject revs
    | update bdate { into datetime }
    | sort-by bdate
  let drop_before = (date now) - $keep_duration
  let old = $cands | drop $keep_min | where bdate < $drop_before
  let many = $cands | drop $keep_max
  let del = [$old $many] | flatten | uniq | sort-by bdate
  if ($del | length | $in > 0) {
    let print = $del
      | update path { path relative-to (pwd) | str substring 0..18 | ($in)… }
      | select path bdate
      | table --index=false --theme light
    print $"Deleting the following paths\n($print)"
    rm -rf ...($del.path)
  }
}
