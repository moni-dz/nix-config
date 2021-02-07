final: prev: {
  ungoogled-chromium = prev.ungoogled-chromium.override {
    enableVaapi = true;
  };
}
