# Version: 1.0.0
# Note: This script creates the Azure Resources required for the OpenDataApp service

"""
This script automates the creation of Azure resources necessary for the OpenDataApp service. 
It includes the following steps:
1. Resource Group creation
2. Azure Container Registry (ACR) creation
3. App Service Plan configuration
4. App Service for Container deployment
5. Enable ACR authentication for the App Service

Usage:
Run this script with appropriate Azure CLI authentication to set up the required resources for OpenDataApp.
"""

# Set variables
RESOURCE_GROUP="OfwatShinyR"
ACR_NAME="abshinyracr"
APP_NAME="abshinyr-app"
PLAN_NAME="abshinyr-plan"

# Create resource group
az group create --name $RESOURCE_GROUP --location uksouth

# Create Azure Container Registry (ACR)
az acr create --resource-group $RESOURCE_GROUP --name $ACR_NAME --sku Basic

# Create an App Service Plan
az appservice plan create --name $PLAN_NAME --resource-group $RESOURCE_GROUP --sku B1 --is-linux

# Create App Service for Container
az webapp create --resource-group $RESOURCE_GROUP --plan $PLAN_NAME --name $APP_NAME --deployment-container-image-name $ACR_NAME.azurecr.io/shinyr-app:latest

# Enable ACR authentication for App Service
az webapp config container set --name $APP_NAME --resource-group $RESOURCE_GROUP --docker-custom-image-name $ACR_NAME.azurecr.io/shinyr-app:latest --docker-registry-server-url https://$ACR_NAME.azurecr.io
