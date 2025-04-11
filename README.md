# Shiny Shuffle Application

## Description

Use the Anthopic API to generate Shiny app styling css and see the changes in real-time.

<img width="1415" alt="Screenshot 2025-04-11 at 11 57 29 AM" src="https://github.com/user-attachments/assets/61421696-038b-4400-89fa-4034d9c2c994" />

## Deploying to Connect Cloud

1. Log in or sign-up: https://connect.posit.cloud/
2. Click Publish
3. Select Shiny
4. Point to this GitHub repo with App.R as your primary file
5. In Advanced, select `Configure variables` to set your ANTHROPIC_API_KEY
6. Click Publish


Note: R deployments on Connect Cloud require a manifest.json file to load your dependencies. Generate this in your project via the `rsconnect` library and include in the repo:

```
rsconnect::writeManifest()
```


