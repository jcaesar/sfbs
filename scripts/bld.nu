#!/usr/bin/env nu
 
let infod = (pwd)/builds
let repos = [njx tunixgut]
let branches = [HEAD autoflup]
let sets = $repos | each {|repo| $branches | each {|branch| { branch: $branch, repo: $repo } } } | flatten
let buildinfo = glob ($infod)/*/meta.json | par-each { do -i { from json } } 

let revs = $sets | each { git -C repos/($in.repo) rev-parse origin/($in.branch) }
let hash = $revs | sort | str join " " | hash sha256
let bdir = ($infod)/($hash)
if ($bdir | path exists) {
  print "Already built this combination"
  exit 0
}

mkdir $bdir
{
  revs: $revs
  bdate: (date now)
} | save ($bdir)/meta.json

with-env {
  SFBS_OUTPUT: ($bdir)/build.json
  SFBS_LINKS: $bdir
} {
  sfbs-build ...($sets | each { [--sysflake $"repos/($in.repo)?ref=origin/($in.branch)"] } | flatten)
}
