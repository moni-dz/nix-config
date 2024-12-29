{
  config,
  lib,
  pkgs,
  ...
}:

{
  services = {
    crowdsec = {
      enable = true;
      allowLocalJournalAccess = true;
      enrollKeyFile = config.age.secrets.crowdsec.path;

      settings =
        let
          yaml = (pkgs.formats.yaml { }).generate;
          acquisitions_file = yaml "acquisitions.yaml" {
            source = "journalctl";
            journalctl_filter = [ "_SYSTEMD_UNIT=sshd.service" ];
            labels.type = "syslog";
          };
        in
        {
          crowdsec_service.acquisition_path = acquisitions_file;
        };
    };

    crowdsec-firewall-bouncer.enable = true;
  };

  systemd.tmpfiles.rules = [
    "Z '/var/lib/crowdsec' 0764 crowdsec crowdsec - -"
    "Z '/var/lib/crowdsec/data' 0764 crowdsec crowdsec - -"
    "Z '/var/lib/crowdsec/hub' 0764 crowdsec crowdsec - -"
  ];

  systemd.services = {
    crowdsec.serviceConfig =
      let
        cfg = config.services.crowdsec;
        format = pkgs.formats.yaml { };
        configFile = format.generate "crowdsec.yaml" cfg.settings;
        pkg = cfg.package;
      in
      {
        ExecStart = lib.mkForce "${pkgs.coreutils}/bin/stdbuf -oL -- ${pkg}/bin/crowdsec -c ${configFile}";

        ExecStartPre =
          let
            setup = pkgs.writeScriptBin "crowdsec-setup" ''
              #!${pkgs.runtimeShell}
              set -eu
              set -o pipefail

              ${lib.optionalString cfg.settings.api.server.enable ''
                if [ ! -s "${cfg.settings.api.client.credentials_path}" ]; then
                  cscli machine add "${cfg.name}" --auto
                fi
              ''}

              ${lib.optionalString (cfg.enrollKeyFile != null) ''
                if ! grep -q password "${cfg.settings.api.server.online_client.credentials_path}" ]; then
                  cscli capi register
                fi

                if [ ! -e "${cfg.settings.api.server.console_path}" ]; then
                  cscli console enroll "$(cat ${cfg.enrollKeyFile})" --name ${cfg.name}
                fi
              ''}
            '';
          in
          lib.mkForce [
            "${setup}/bin/crowdsec-setup"
          ];
      };

    crowdsec-firewall-bouncer.serviceConfig =
      let
        cfg = config.services.crowdsec-firewall-bouncer;
        pkg = cfg.package;
      in
      {
        ExecStart = lib.mkForce "${pkg}/bin/cs-firewall-bouncer -c ${config.age.secrets.bouncer.path}";
        ExecStartPre = lib.mkForce [
          "${pkg}/bin/cs-firewall-bouncer -t -c ${config.age.secrets.bouncer.path}"
        ];
      };

    crowdsec-update-hub.serviceConfig.ExecStartPost = lib.mkForce "";
  };
}
