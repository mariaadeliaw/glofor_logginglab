library(tidyverse)
library(LoggingLab)


# getting to know the data ------------------------------------------------

# permanent plot data
Paracou6_2016 <- LoggingLab::Paracou6_2016

# log area
PlotMask <- LoggingLab::PlotMask

# species data
SpeciesCriteria <- LoggingLab::SpeciesCriteria

# volume parameters table
ForestZoneVolumeParametersTable <- LoggingLab::ForestZoneVolumeParametersTable

# management scenario parameter
ScenariosTable <- LoggingLab::ScenariosTable


# setting variables -------------------------------------------------------

# detailed logging simulation

inventory <- Paracou6_2016 %>%
  # check format
  inventorycheckformat() %>%
  # only take the trees that are inside the plot
  cleaninventory(plotmask = PlotMask, loggingparameters())

# compute tree dimension
inventory <- inventory %>%
  addtreedim(volumeparameters = ForestZoneVolumeParametersTable)
# lookup table to calculate agb and volume of each trees inside the plot

# main trail in plot (roads etc, for now it's only terrain data)
maintrails <- maintrailextract(DTMParacou) #doesnt functioning, no road data


# determine harvestable area ----------------------------------------------

HarvestableArea <- harvestableareadefinition(
  topography = DTMParacou,
  creekverticaldistance = CreekDistances$distvert,
  creekhorizontaldistance = CreekDistances$disthorz,
  maintrails = MainTrails,
  plotmask = PlotMask,
  scenario = "manual",
  winching = "2",
  advancedloggingparameters = loggingparameters()
)

LoggingMap <- ggplot() +
  # Harvestable zones
  geom_sf(
    data = HarvestableArea$HarvestablePolygons,
    fill = "olivedrab", alpha = 0.1
  ) +
  # harvestable by machine
  geom_sf(
    data = HarvestableArea$MachinePolygons,
    fill = "olivedrab", alpha = 0.5
  )

# select tree to harvest
inventory <- inventory %>%
  # Include commercial criteria to the inventory
  commercialcriteriajoin(SpeciesCriteria)

TreeSelection <- inventory %>%
  treeselection(
    topography = DTMParacou,
    speciescriteria = SpeciesCriteria,
    scenario ="manual",
    objective = 30,
    fuel = "2",
    winching = "0",
    diversification = TRUE,
    specieslax = FALSE,
    objectivelax = TRUE,
    harvestablearea = HarvestableArea$HarvestableArea,
    plotslope = HarvestableArea$PlotSlope,
    maintrails = MainTrails,
    harvestablepolygons = HarvestableArea$HarvestablePolygons,
    advancedloggingparameters = loggingparameters()
  )

ScndTrailOutputs <- secondtrailsopening(
  topography = DTMParacou,
  plotmask = PlotMask,
  maintrails = MainTrails,
  plotslope = HarvestableArea$PlotSlope,
  harvestablepolygons = HarvestableArea$HarvestablePolygons,
  machinepolygons = HarvestableArea$MachinePolygons,
  treeselectionoutputs = TreeSelection,
  scenario = "manual",
  winching = "2",
  fuel = "2",
  advancedloggingparameters = loggingparameters()
)
inventory <- ScndTrailOutputs$inventory


# plotting maps -----------------------------------------------------------

inventory %>%
  filter(LoggingStatus == "non-harvestable") %>%
  st_as_sf(coords = c("Xutm", "Yutm")) %>%
  st_set_crs(st_crs(SecondaryTrails$MainTrailsAccess)) ->
  NonHarvestable

inventory %>%
  filter(LoggingStatus == "harvestable") %>%
  st_as_sf(coords = c("Xutm", "Yutm")) %>%
  st_set_crs(st_crs(SecondaryTrails$MainTrailsAccess)) ->
  Harvestable

inventory %>%
  filter(LoggingStatus == "reserve") %>%
  st_as_sf(coords = c("Xutm", "Yutm")) %>%
  st_set_crs(st_crs(SecondaryTrails$MainTrailsAccess)) ->
  Reserve

inventory %>%
  filter(LoggingStatus == "future") %>%
  st_as_sf(coords = c("Xutm", "Yutm")) %>%
  st_set_crs(st_crs(SecondaryTrails$MainTrailsAccess)) ->
  Future

LoggingMap +
  geom_sf(
    data = Harvestable,
    aes(colour = "Harvestable"),
    size = 4
  ) +
  geom_sf(
    data = Reserve,
    aes(colour = "Reserve"),
    size = 4
  ) +
  geom_sf(
    data = Future,
    aes(colour = "Future"),
    size = 4
  ) +
  labs(colour = "Trees") +
  # 2ndary trails
  geom_sf(
    data = st_as_sf(SecondaryTrails$SmoothedTrails),
    col = "darkgreen"
  ) +
  geom_sf(
    data = st_as_sf(SecondaryTrails$MainTrailsAccess),
    col = "black"
  )


# tree felling ------------------------------------------------------------

inventory %>%
  treefelling(
    scenario = "manual",
    fuel = "2",
    winching = "2",
    directionalfelling = "2",
    maintrailsaccess = ScndTrailOutputs$MainTrailsAccess,
    scndtrail = ScndTrailOutputs$SmoothedTrails,
    advancedloggingparameters = loggingparameters()
  ) ->
  inventory

# map the result

# Selected trees
inventory %>%
  filter(Selected == "1") %>%
  st_as_sf(coords = c("Xutm", "Yutm")) %>%
  st_set_crs(st_crs(SecondaryTrails$MainTrailsAccess)) ->
  Selected

# Treefalls
inventory %>%
  filter(LoggingStatus == "treefall2nd") %>%
  st_as_sf(coords = c("Xutm", "Yutm")) %>%
  st_set_crs(st_crs(SecondaryTrails$MainTrailsAccess)) ->
  Treefall

# Make an sf with inventory
inventory %>%
  sf::st_as_sf(coords = c("Xutm", "Yutm")) %>%
  sf::st_set_crs(sf::st_crs(MainTrails)) ->
  Inventory_crs

# Make an sf with felled trees
inventory %>%
  getgeometry(TreePolygon) %>%
  sf::st_set_crs(sf::st_crs(MainTrails)) ->
  TreePolygon

# Make a map
ggplot() +
  geom_sf(data = Inventory_crs) +
  geom_sf(data = NonHarvestable, aes(colour = "Non-harvestable"), show.legend = "point") +
  geom_sf(data = Future, aes(colour = "Future"), size = 4, show.legend = "point") +
  geom_sf(data = Reserve, aes(colour = "Reserve"), size = 4, show.legend = "point") +
  geom_sf(data = Harvestable, aes(colour = "Harvestable"), size = 4, show.legend = "point") +
  # Felled trees
  geom_sf(data = TreePolygon, alpha = 0.5, fill = "red") +
  geom_sf(data = Selected, aes(colour = "Selected"), show.legend = "point") +
  geom_sf(data = Treefall, aes(colour = "Treefall2nd"), show.legend = "point") +
  geom_sf(data = SecondaryTrails$maintrailsaccess, alpha = 0.5, fill = "black") +
  geom_sf(data = SecondaryTrails$SmoothedTrails, alpha = 0.5, fill = "black") +
  scale_colour_manual(
    values = c(
      "Non-harvestable" = "grey",
      "Visible defect" = "pink",
      "Harvestable" = "skyblue",
      "HarvestableUp" = "blue",
      "Selected" = "red",
      "Future" = "orange",
      "Reserve" = "purple",
      "Probed hollow" = "forestgreen",
      "Treefall2nd" = "chocolate4"
    )
  ) +
  labs(color = "Logging status")


# compute logged trees -------------------------------------------------------

TimberV <- timberharvestedvolume(
  inventory,
  scenario = "manual",
  fuel = "2",
  advancedloggingparameters = loggingparameters()
)
inventory <- TimberV$inventory

FuelV <- harvestablefuelwood(
  inventory,
  scenario = "manual",
  fuel = "2",
  TimberLoggedVolume = TimberV$TimberLoggedVolume,
  NoHollowTimberLoggedVolume = TimberV$NoHollowTimberLoggedVolume,
  advancedloggingparameters = loggingparameters()
)
inventory <- FuelV$inventory
