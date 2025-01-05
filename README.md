An Ultra-Lightweight Purescript Deku/Hyrule Point of Sale System for 🎋 Dispensaries

Current Componenents:
1. runLiveView: Simple, Live, Configurable Real-Time Inventory Visualization that can switch between http post requests and json reads based on config parameters.
2. createItem: a form for input of new items with built-in validation using arbitrary and fully programmable rules.  The idea is to make it impossible to submit an improperly-formatted inventory item by guarding the button activation behind.     
  - todo: massage the json writing part of the program to properly form the expected types and write to a json file.
 
Future Components:,
1. modifyItem: obviously, my next task is to make this app able to modify an item in the menu.  
2. pos: this part depends heavily on the other parts of the app.
