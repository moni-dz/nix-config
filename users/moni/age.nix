{
  # NOTE: you should either change this or disable it completely by setting agenix = false;
  age.secrets.github-token = {
    file = ../../secrets/github-token.age;
    mode = "600";
  };
}
