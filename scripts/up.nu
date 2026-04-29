#!/usr/bin/env nu

let dir = (pwd)/repos
mkdir $dir
let tmp = mktemp -t -d
cd $tmp

def prep [repo] {
  let c = ($dir)/($repo)
  if (git -C $c status | complete | $in.exit_code != 0) {
    rm -rf $c
    git clone git@github.com:jcaesar/($repo) $c
  }
  git -C $c fetch
  git -C $c worktree prune
  git -C $c worktree add --detach ($tmp)/($repo) origin/HEAD
}
def commit [repo] {
  let c = ($tmp)/($repo)
  git -C $c config user.email sfbs@liftm.de
  git -C $c config user.name Autoflaker
  git -C $c commit -am "mec flak updat"
  let changed = git -C $c diff --exit-code --no-ext-diff origin/autoflup..HEAD
    | complete |  $in.exit_code != 0
  if $changed {
    git -C $c push -f origin HEAD:autoflup
  } else {
    git -C $c reset --hard origin/autoflup
  }
}

def lockover [base, repo] {
  let newmeta = nix flake metadata ($tmp)/($base) --json | from json
  def mklock [data] { {
    locked: ($data.locked | select lastModified narHash rev)
  } }
  let over = {
    nodes: {
      nixpkgs: (mklock $newmeta.locks.nodes.nixpkgs),
      njx: (mklock $newmeta)
    }
  }
  let lf = ($tmp)/($repo)/flake.lock
  let lock = open $lf | from json
  $lock | merge deep $over | to json | save -f $lf
  commit $repo
}

prep njx
prep tunixgut
nix flake update --flake ($tmp)/njx nixpkgs
commit njx
lockover njx tunixgut

cd /
rm -rf $tmp
