let 
    overlay = (final: prev: {
        ocaml = final.symlinkJoin rec {
            inherit (prev.ocaml) name;
            version = final.lib.getVersion prev.ocaml;

            paths = [ prev.ocaml ];
            buildInputs = [ final.makeWrapper ];

            postBuild = ''
            wrapProgram $out/bin/ocaml \
                --add-flags "-I ${final.ocamlPackages.findlib}/lib/ocaml/${version}/site-lib"
            '';
        };
    });
in
{ pkgs ? import <nixpkgs> {
    overlays = [ overlay ];
}, ... }:

pkgs.mkShell {
    nativeBuildInputs = [ 
        pkgs.ocaml
        pkgs.ocamlPackages.findlib
        pkgs.ocamlPackages.graphics
        pkgs.ocamlPackages.ocaml_lwt       
        pkgs.ocamlPackages.ocaml_oasis     
        pkgs.ocamlPackages.utop  
    ];
}