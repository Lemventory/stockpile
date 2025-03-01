-- Enhanced schema definition for inventory management system
let EntityType =
  < Record 
  | Newtype
  | Variant
  >

let SimpleFieldType = 
  < FTText
  | FTInt
  | FTNatural
  | FTDouble
  | FTScientific
  | FTUuid
  | FTBool
  | FTEnum : Text
  | FTRecord : Text
  | FTVariant : Text
  >

let Field =
  { fieldName : Text
  , fieldType : SimpleFieldType
  , fieldArray : Bool  -- Whether this field is an array of its type
  , fieldRequired : Bool
  , fieldDescription : Text
  , fieldIsInDB : Bool
  , fieldCustomJSON : Optional Text -- Optional custom JSON handling
  }

let defaultField =
  { fieldName = ""
  , fieldType = SimpleFieldType.FTText
  , fieldArray = False
  , fieldRequired = True
  , fieldDescription = ""
  , fieldIsInDB = True
  , fieldCustomJSON = None Text
  }

let VariantConstructor =
  { constructorName : Text
  , constructorFields : List Field
  , constructorDescription : Text
  }

let Entity =
  { entityName : Text
  , entityType : EntityType
  , entityFields : List Field
  , entityVariants : List VariantConstructor
  , entityPrimaryKey : List Text
  , entityDescription : Text
  , entityCustomJSON : Optional Text
  }

let EnumDef =
  { enumName : Text
  , enumValues : List Text
  , enumDescription : Text
  }

let Schema =
  { schemaName : Text
  , schemaEntities : List Entity
  , schemaEnums : List EnumDef
  , schemaDescription : Text
  }

-- Enums
let speciesEnum = 
  { enumName = "Species"
  , enumValues = 
    [ "Indica"
    , "IndicaDominantHybrid"
    , "Hybrid"
    , "SativaDominantHybrid"
    , "Sativa"
    ]
  , enumDescription = "Species of cannabis"
  }

let itemCategoryEnum = 
  { enumName = "ItemCategory"
  , enumValues = 
    [ "Flower"
    , "PreRolls"
    , "Vaporizers"
    , "Edibles"
    , "Drinks"
    , "Concentrates"
    , "Topicals"
    , "Tinctures"
    , "Accessories"
    ]
  , enumDescription = "Category of inventory item"
  }

-- Define the MenuItem entity as a regular record
let menuItemEntity = 
  { entityName = "MenuItem"
  , entityType = EntityType.Record
  , entityFields = 
    [ { fieldName = "sort"
      , fieldType = SimpleFieldType.FTInt
      , fieldArray = False
      , fieldRequired = True
      , fieldDescription = "Display order"
      , fieldIsInDB = True
      , fieldCustomJSON = None Text
      }
    , { fieldName = "sku"
      , fieldType = SimpleFieldType.FTUuid
      , fieldArray = False
      , fieldRequired = True
      , fieldDescription = "Unique identifier"
      , fieldIsInDB = True
      , fieldCustomJSON = None Text
      }
    , { fieldName = "brand"
      , fieldType = SimpleFieldType.FTText
      , fieldArray = False
      , fieldRequired = True
      , fieldDescription = "Item brand"
      , fieldIsInDB = True
      , fieldCustomJSON = None Text
      }
    , { fieldName = "name"
      , fieldType = SimpleFieldType.FTText
      , fieldArray = False
      , fieldRequired = True
      , fieldDescription = "Item name"
      , fieldIsInDB = True
      , fieldCustomJSON = None Text
      }
    , { fieldName = "price"
      , fieldType = SimpleFieldType.FTScientific
      , fieldArray = False
      , fieldRequired = True
      , fieldDescription = "Item price"
      , fieldIsInDB = True
      , fieldCustomJSON = None Text
      }
    , { fieldName = "measure_unit"
      , fieldType = SimpleFieldType.FTText
      , fieldArray = False
      , fieldRequired = True
      , fieldDescription = "Unit of measurement"
      , fieldIsInDB = True
      , fieldCustomJSON = None Text
      }
    , { fieldName = "per_package"
      , fieldType = SimpleFieldType.FTText
      , fieldArray = False
      , fieldRequired = True
      , fieldDescription = "Amount per package"
      , fieldIsInDB = True
      , fieldCustomJSON = None Text
      }
    , { fieldName = "quantity"
      , fieldType = SimpleFieldType.FTInt
      , fieldArray = False
      , fieldRequired = True
      , fieldDescription = "Inventory quantity"
      , fieldIsInDB = True
      , fieldCustomJSON = None Text
      }
    , { fieldName = "category"
      , fieldType = SimpleFieldType.FTEnum "ItemCategory"
      , fieldArray = False
      , fieldRequired = True
      , fieldDescription = "Item category"
      , fieldIsInDB = True
      , fieldCustomJSON = None Text
      }
    , { fieldName = "subcategory"
      , fieldType = SimpleFieldType.FTText
      , fieldArray = False
      , fieldRequired = True
      , fieldDescription = "Item subcategory"
      , fieldIsInDB = True
      , fieldCustomJSON = None Text
      }
    , { fieldName = "description"
      , fieldType = SimpleFieldType.FTText
      , fieldArray = False
      , fieldRequired = True
      , fieldDescription = "Item description"
      , fieldIsInDB = True
      , fieldCustomJSON = None Text
      }
    , { fieldName = "tags"
      , fieldType = SimpleFieldType.FTText
      , fieldArray = True
      , fieldRequired = True
      , fieldDescription = "Item tags"
      , fieldIsInDB = True
      , fieldCustomJSON = None Text
      }
    , { fieldName = "effects"
      , fieldType = SimpleFieldType.FTText
      , fieldArray = True
      , fieldRequired = True
      , fieldDescription = "Item effects"
      , fieldIsInDB = True
      , fieldCustomJSON = None Text
      }
    , { fieldName = "strain_lineage"
      , fieldType = SimpleFieldType.FTRecord "StrainLineage"
      , fieldArray = False
      , fieldRequired = True
      , fieldDescription = "Strain lineage information"
      , fieldIsInDB = False
      , fieldCustomJSON = None Text
      }
    ]
  , entityVariants = [] : List VariantConstructor
  , entityPrimaryKey = ["sku"]
  , entityDescription = "Menu item"
  , entityCustomJSON = None Text
  }

-- Define the StrainLineage entity as a regular record
let strainLineageEntity = 
  { entityName = "StrainLineage"
  , entityType = EntityType.Record
  , entityFields = 
    [ { fieldName = "thc"
      , fieldType = SimpleFieldType.FTText
      , fieldArray = False
      , fieldRequired = True
      , fieldDescription = "THC percentage"
      , fieldIsInDB = True
      , fieldCustomJSON = None Text
      }
    , { fieldName = "cbg"
      , fieldType = SimpleFieldType.FTText
      , fieldArray = False
      , fieldRequired = True
      , fieldDescription = "CBG percentage"
      , fieldIsInDB = True
      , fieldCustomJSON = None Text
      }
    , { fieldName = "strain"
      , fieldType = SimpleFieldType.FTText
      , fieldArray = False
      , fieldRequired = True
      , fieldDescription = "Strain name"
      , fieldIsInDB = True
      , fieldCustomJSON = None Text
      }
    , { fieldName = "creator"
      , fieldType = SimpleFieldType.FTText
      , fieldArray = False
      , fieldRequired = True
      , fieldDescription = "Strain creator"
      , fieldIsInDB = True
      , fieldCustomJSON = None Text
      }
    , { fieldName = "species"
      , fieldType = SimpleFieldType.FTEnum "Species"
      , fieldArray = False
      , fieldRequired = True
      , fieldDescription = "Species type"
      , fieldIsInDB = True
      , fieldCustomJSON = None Text
      }
    , { fieldName = "dominant_terpene"
      , fieldType = SimpleFieldType.FTText
      , fieldArray = False
      , fieldRequired = True
      , fieldDescription = "Dominant terpene"
      , fieldIsInDB = True
      , fieldCustomJSON = None Text
      }
    , { fieldName = "terpenes"
      , fieldType = SimpleFieldType.FTText
      , fieldArray = True
      , fieldRequired = True
      , fieldDescription = "List of terpenes"
      , fieldIsInDB = True
      , fieldCustomJSON = None Text
      }
    , { fieldName = "lineage"
      , fieldType = SimpleFieldType.FTText
      , fieldArray = True
      , fieldRequired = True
      , fieldDescription = "Strain lineage"
      , fieldIsInDB = True
      , fieldCustomJSON = None Text
      }
    , { fieldName = "leafly_url"
      , fieldType = SimpleFieldType.FTText
      , fieldArray = False
      , fieldRequired = True
      , fieldDescription = "Leafly URL"
      , fieldIsInDB = True
      , fieldCustomJSON = None Text
      }
    , { fieldName = "img"
      , fieldType = SimpleFieldType.FTText
      , fieldArray = False
      , fieldRequired = True
      , fieldDescription = "Image URL"
      , fieldIsInDB = True
      , fieldCustomJSON = None Text
      }
    ]
  , entityVariants = [] : List VariantConstructor
  , entityPrimaryKey = [] : List Text
  , entityDescription = "Strain lineage information"
  , entityCustomJSON = None Text
  }

-- Define the Inventory entity as a newtype
let inventoryEntity = 
  { entityName = "Inventory"
  , entityType = EntityType.Newtype
  , entityFields = 
    [ { fieldName = "items"
      , fieldType = SimpleFieldType.FTRecord "MenuItem"
      , fieldArray = True
      , fieldRequired = True
      , fieldDescription = "List of menu items"
      , fieldIsInDB = False
      , fieldCustomJSON = None Text
      }
    ]
  , entityVariants = [] : List VariantConstructor
  , entityPrimaryKey = [] : List Text
  , entityDescription = "Inventory collection"
  , entityCustomJSON = Some ''
      instance ToJSON Inventory where
        toJSON (Inventory {items = items}) = toJSON items

      instance FromJSON Inventory where
        parseJSON v = Inventory <$> parseJSON v
    ''
  }

-- Define the InventoryResponse entity as a variant/sum type
let inventoryResponseEntity = 
  { entityName = "InventoryResponse"
  , entityType = EntityType.Variant
  , entityFields = [] : List Field
  , entityVariants = 
    [ { constructorName = "InventoryData"
      , constructorFields = 
        [ { fieldName = "inventory"
          , fieldType = SimpleFieldType.FTRecord "Inventory"
          , fieldArray = False
          , fieldRequired = True
          , fieldDescription = "Inventory data"
          , fieldIsInDB = False
          , fieldCustomJSON = None Text
          }
        ]
      , constructorDescription = "Successful inventory response"
      }
    , { constructorName = "Message"
      , constructorFields = 
        [ { fieldName = "message"
          , fieldType = SimpleFieldType.FTText
          , fieldArray = False
          , fieldRequired = True
          , fieldDescription = "Error or info message"
          , fieldIsInDB = False
          , fieldCustomJSON = None Text
          }
        ]
      , constructorDescription = "Message response"
      }
    ]
  , entityPrimaryKey = [] : List Text
  , entityDescription = "Inventory API response"
  , entityCustomJSON = Some ''
      instance ToJSON InventoryResponse where
        toJSON (InventoryData inv) =
          object
            [ "type" .= T.pack "data"
            , "value" .= toJSON inv 
            ]
        toJSON (Message msg) =
          object
            [ "type" .= T.pack "message"
            , "value" .= msg
            ]

      instance FromJSON InventoryResponse
    ''
  }

-- Define the inventory schema
let inventorySchema : Schema = 
  { schemaName = "Inventory"
  , schemaDescription = "Cannabis inventory management system"
  , schemaEnums = [speciesEnum, itemCategoryEnum]
  , schemaEntities = 
    [ strainLineageEntity
    , menuItemEntity
    , inventoryEntity
    , inventoryResponseEntity
    ]
  }

in inventorySchema