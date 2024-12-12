{
  description = "Bazaar game";

  # Binary server configuration
  nixConfig = {
    extra-experimental-features = "nix-command flakes";
    extra-substituters = "https://nix-cache.cajun.page/public https://pre-commit-hooks.cachix.org";
    extra-trusted-public-keys = "public:Ts+1e+F/BjkLKF/7eqbHa7x/wKWXA5PzU8bVRBy0ysU= pre-commit-hooks.cachix.org-1:Pkk3Panw5AW24TOv6kz3PvLhlH8puAsJTBbOPmBo7Rc=";
    # netrc-file = ./netrc; # Use if cache is private
  };

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    pre-commit-hooks-nix.url = "github:cachix/pre-commit-hooks.nix";

    json-stream = {
      url = "github:ondrap/json-stream";
      flake = false;
    };

    effectful = {
      url = "github:haskell-effectful/effectful";
      flake = false;
    };

    th-abstraction = {
      url = "github:glguy/th-abstraction";
      flake = false;
    };

    strict-mutable-base = {
      url = "github:arybczak/strict-mutable";
      flake = false;
    };

    static-ls = {
      url = "github:josephsumabat/static-ls";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    tree-sitter-simple-repo = {
      # url = "https://github.com/oberblastmeister/tree-sitter-simple";
      url = "git+https://github.com/oberblastmeister/tree-sitter-simple?submodules=1";
      flake = false;
    };

    karp-miller = {
      url = "github:cajunvoodoo/karp-miller";
      flake = false;
    };
    vass = {
      url = "github:cajunvoodoo/vass";
      flake = false;
    };
    duvet = {
      url = "github:cajunvoodoo/duvet";
      flake = false;
    };

    network-effectful = {
      url = "github:cajunvoodoo/network-effectful";
      flake = false;
    };

    network-run = {
      url = "github:kazu-yamamoto/network-run";
      flake = false;
    };

    network = {
      url = "github:haskell/network";
      flake = false;
    };
  };

  outputs = inputs @ {
    flake-parts,
    nixpkgs,
    pre-commit-hooks-nix,
    ...
  }: let
    pname = "xbazaar"; # Your cabal project's name
    buildProject = true; # Include your project (useful for cabal init)
  in
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = ["x86_64-linux" "aarch64-darwin"];
      imports = [
        # Ensure the extra-substituters is correctly configured, otherwise the
        # entire world will be rebuilt :3
        pre-commit-hooks-nix.flakeModule
      ];
      perSystem = {
        config,
        pkgs,
        system,
        self',
        ...
      }: let
        utils = import ./utils.nix {inherit pkgs;};
        hsSrc = {dir ? ./.}:
          with pkgs.lib.fileset;
            toSource {
              root = dir;
              fileset =
                intersection
                (gitTracked dir)
                (unions [
                  (fileFilter (file: file.hasExt "hs") dir)
                  (fileFilter (file: file.hasExt "cabal") dir)
                  (fileFilter (file: file.hasExt "png") dir)
                  (fileFilter (file: file.hasExt "ttf") dir)
                  (fileFilter (file: file.name == "LICENSE") dir)
                ]);
            };
        hp =
          if buildProject
          then
            pkgs.haskellPackages.override {
              overrides = final: prev: {
                ${pname} = utils.mkStatic (final.callCabal2nix pname (hsSrc {dir = ./.;}) {}) {extraFlags = ["-f-dynamic-gui"];};
                "${pname}-dynamic" = utils.fixup (final.callCabal2nix pname (hsSrc {dir = ./.;}) {}) {extraFlags = ["-fdynamic-gui"];};
                # ${pname} = final.callCabal2nix pname (hsSrc {dir = ./.;}) {};
                json-stream = final.callCabal2nix inputs.json-stream "${inputs.json-stream}" {};
                effectful-core = final.callCabal2nix inputs.effectful "${inputs.effectful}/effectful-core" {};
                effectful-plugin = final.callCabal2nix inputs.effectful "${inputs.effectful}/effectful-plugin" {};
                effectful = final.callCabal2nix inputs.effectful "${inputs.effectful}/effectful" {};
                network-effectful = final.callCabal2nix inputs.network-effectful "${inputs.network-effectful}" {};
                # network-run = final.callCabal2nix inputs.network-run "${inputs.network-run}" {};
                # network = final.callCabal2nix inputs.network "${inputs.network}" {};
                strict-mutable-base = final.callCabal2nix inputs.strict-mutable-base "${inputs.strict-mutable-base}/strict-mutable-base" {};
                # karp-miller = inputs.karp-miller.packages.${system}.default;
                vass = final.callCabal2nix inputs.vass "${inputs.vass}" {};
                duvet = final.callCabal2nix inputs.duvet "${inputs.duvet}" {};
                karp-miller = final.callCabal2nix inputs.karp-miller "${inputs.karp-miller}" {};

                sdl2-image = pkgs.lib.pipe prev.sdl2-image [
                  pkgs.haskell.lib.markUnbroken
                  (drv:
                    drv.overrideAttrs (attrs: {
                      strictDeps = true;
                    }))
                ];


                # wxdirect = utils.fixup prev.wxdirect {jailbreak = true;};
                # sdl2-image = utils.fixup prev.sdl2-image {};

                # lsp = utils.fixup prev.lsp {jailbreak = true;};

                # tree-sitter-haskell = prev.callCabal2nix "tree-sitter-haskell" "${(inputs.tree-sitter-simple-repo)}/tree-sitter-haskell" {};

                # tree-sitter-simple =
                #   prev.callCabal2nix "tree-sitter-simple" "${inputs.tree-sitter-simple-repo}/tree-sitter-simple" {};

                # tree-sitter-ast =
                #   prev.callCabal2nix
                #   "tree-sitter-ast" "${(inputs.tree-sitter-simple-repo)}/tree-sitter-ast" {};

                # text-range =
                #   prev.callCabal2nix
                #     "text-range" "${(inputs.tree-sitter-simple-repo)}/text-range" {};
                # haskell-ast =
                #   prev.callCabal2nix
                #   "haskell-ast" "${(inputs.tree-sitter-simple-repo)}/haskell-ast" {};
                # tasty-expect = prev.callCabal2nix "tasty-expect" (pkgs.fetchgit {
                #   url = "https://github.com/oberblastmeister/tasty-expect.git";
                #   sha256 = "sha256-KxunyEutnwLeynUimiIkRe5w/3IdmbD/9hGVi68UVfU=";
                #   rev = "ec14d702660c79a907e9c45812958cd0df0f036f";
                # }) {};

                # hiedb = pkgs.haskell.lib.dontCheck (prev.callHackage "hiedb" "0.6.0.1" {});
                # text-rope = prev.callHackage "text-rope" "0.2" {};

                # lsp = prev.callHackage "lsp" "2.4.0.0" {};
                # lsp-types = prev.callHackage "lsp-types" "2.1.0.0" {};

                # static-ls = utils.fixup (final.callCabal2nix inputs.static-ls "${inputs.static-ls}" {}) {doCheck = false;};
              };
            }
          else pkgs.haskellPackages;
      in {
        ########################################################################
        ##                       PRIMARY CONFIGURATION                        ##
        ########################################################################
        formatter = pkgs.alejandra;

        packages.default =
          if buildProject
          then
            pkgs.symlinkJoin {
              name = "all";
              paths = [
                hp.${pname}
                hp."${pname}-dynamic"
              ];
            }
          else pkgs.hello;

        packages.dynamic =
          if buildProject
          then hp."${pname}-dynamic"
          else pkgs.hello;

        devShells.default = hp.shellFor {
          packages = hpkgs:
            with hpkgs; (
              if buildProject
              then [self'.packages.dynamic]
              else []
            );
          nativeBuildInputs = with hp;
          with pkgs; [
            cabal-fmt
            cabal-install
            fourmolu
            haskell-language-server
            pre-commit
            ghcid
            llvm_15

            ghciwatch
            # static-ls
            # ((import ./static-ls-index.nix {lib = pkgs.lib; hiedb = hp.hiedb; runCommand = pkgs.runCommand;})
            #                                 self'.packages.default)

            graphmod
            xdot
          ];
          inputsFrom = [self'.packages.dynamic.env];
          shellHook = ''
            ${config.pre-commit.installationScript}
            echo 1>&2 "Welcome to the development shell!"
          '';
        };

        ########################################################################
        ##                          PRE-COMMIT HOOKS                          ##
        ########################################################################
        pre-commit = {
          check.enable = true;
          settings = {
            hooks = {
              # See issue #250 Hook order ignored. AAA and ZZZ for forced
              # Lexicographic order
              fourmolu.enable = true;
              cabal-fmt.enable = true;
              module-diagram = {
                enable = true;
                name = "module-diagram";
                description = "Create the module diagram before committing";
                pass_filenames = false;
                # files = "modules.png";
                entry = let
                  script = pkgs.writeShellScript "precommit-module-diagram" ''
                    set -e
                    ${pkgs.haskellPackages.graphmod}/bin/graphmod \
                      --remove-qual=test \
                      --remove-qual=app \
                      --no-cluster \
                      -s 1\
                      -q \
                      | ${pkgs.graphviz}/bin/tred \
                      | ${pkgs.graphviz}/bin/dot -Tpng -Gdpi=300 > Bazaar/Other/modules.png
                  '';
                in
                  builtins.toString script;
              };

              zzz-deploy-nix = {
                enable = true;
                name = "deploy-nix";
                description = "Deploy binaries to project dir";
                pass_filenames = false;
                # files = "^[xbazaar|xtest]$";
                entry = let
                  script = pkgs.writeShellScript "precommit-deploy-nix" ''
                    set -e
                    ${pkgs.nix}/bin/nix build --accept-flake-config --offline
                    flock ./xtest
                    flock ./xbazaar
                    flock ./3/xeq
                    flock ./4/xturn
                    flock ./6/xrules
                    cp -f result/bin/xbazaar .
                    cp -f result/bin/xtest .
                    cp -f result/bin/xeq ./3/xeq
                    cp -f result/bin/xturn ./4/xturn
                    cp -f result/bin/xrules ./6/xrules
                    flock -u ./xtest
                    flock -u ./xbazaar
                    flock -u ./3/xeq
                    flock -u ./4/xturn
                    flock -u ./6/xrules
                  '';
                in
                  builtins.toString script;
              };

              check-todos = {
                enable = true;
                name = "check-todos-nix";
                description = "Prevent unhandled TODOs from being committed";
                files = "^.*.hs$";
                pass_filenames = false;
                entry = let
                  script = pkgs.writeShellScript "check-todos-nix" ''
                    #!/bin/sh

                    die_with_status () {
                    	status=$1
                    	shift
                    	printf >&2 '%s\n' "$*"
                    	exit "$status"
                    }

                    die () {
                    	die_with_status 1 "$@"
                    }

                    if git diff-index -p -M --cached HEAD -- \
                    | ${pkgs.ripgrep}/bin/rg '^+' \
                    | ${pkgs.ripgrep}/bin/rg  -i -- TODO; then
                        die Blocking commit because string '-- TODO' detected in patch
                    fi
                  '';
                in
                  builtins.toString script;
              };

              alejandra.enable = true;
              check-symlinks.enable = true;
              # trim-trailing-whitespace.enable = true;
            };
          };
        };

        ########################################################################
        _module.args.pkgs = import nixpkgs {
          inherit system;
          config.allowUnfree = true;
        };
      };
    };
}
