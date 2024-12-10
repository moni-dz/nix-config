{ config
, inputs'
, pkgs
, ...
}:

/*
  home-manager configuration
  Useful links:
  - Home Manager Manual: https://nix-community.gitlab.io/home-manager/
  - Appendix A. Configuration Options: https://nix-community.gitlab.io/home-manager/options.html
*/
{
  home = {
    packages = __attrValues {
      inherit (pkgs)
        curl
        nix-output-monitor
        parallel
        fd
        ripgrep
        jq
        sapling
        ;
    };
  };

  xdg.configFile."ghostty/testing.glsl".text = ''
    void mainImage(out vec4 fragColor, in vec2 fragCoord)
    {
            vec2 uv = fragCoord.xy / iResolution.xy;
            vec3 gradientStartColor = vec3(0.188,0.204,0.275);
            vec3 gradientEndColor = vec3(0.067,0.067,0.106);
            float mixValue = distance(uv, vec2(0.25, 0.5));
            vec3 gradientColor = mix(gradientStartColor, gradientEndColor, mixValue);
            vec4 terminalColor = texture(iChannel0, uv);
            float mask = 1.0 - step(0.5, dot(terminalColor.rgb, vec3(1.0)));
            vec3 blendedColor = mix(terminalColor.rgb, gradientColor, mask);
            fragColor = vec4(blendedColor, terminalColor.a);
    }
  '';

  programs = {
    fish = {
      interactiveShellInit = ''
        function export
          if [ $argv ] 
            set var (echo $argv | cut -f1 -d=)
            set val (echo $argv | cut -f2 -d=)
            set -g -x $var $val
          else
            echo 'export var=value'
          end
        end

        . ${config.age.secrets.tokens.path}
        # . /Users/moni/.local/share/miniconda/etc/fish/conf.d/conda.fish
      '';

      shellInit = ''
        # fish_add_path /Users/moni/Library/Python/3.11/bin
        # fish_add_path -m /Users/moni/.local/share/miniconda/bin
        # fish_add_path -amP /Applications/ArmGNUToolchain/13.2.Rel1/arm-none-eabi/bin
        fish_add_path -amP /usr/bin
        fish_add_path -amP /opt/homebrew/bin
        fish_add_path -amP /Users/moni/.modular/bin
        # fish_add_path -amP /usr/local/smlnj/bin
        fish_add_path -amP /opt/local/bin
        fish_add_path -amP /opt/homebrew/opt/llvm/bin
        fish_add_path -amP /Users/moni/.local/share/modular/pkg/packages.modular.com_mojo/bin
        fish_add_path -m /run/current-system/sw/bin
        fish_add_path -m /Users/moni/.nix-profile/bin
      '';
    };
  };
}
