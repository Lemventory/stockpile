module API.Transaction where

import Prelude
import Types.Common
import Types.Transaction

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, attempt)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Fetch (Method(..), fetch)
import Fetch.Yoga.Json (fromJSON)
import NetworkConfig (currentConfig)
import Types.UUID (UUID(..))
import Yoga.JSON (writeJSON)

baseUrl :: String
baseUrl = currentConfig.apiBaseUrl

createTransaction :: Transaction -> Aff (Either String Transaction)
createTransaction transaction = do
  result <- attempt do
    let content = writeJSON transaction
    liftEffect $ Console.log "Creating new transaction..."
    liftEffect $ Console.log $ "Sending content: " <> content

    response <- fetch (baseUrl <> "/transaction")
      { method: POST
      , body: content
      , headers:
          { "Content-Type": "application/json"
          , "Accept": "application/json"
          , "Origin": currentConfig.appOrigin
          }
      }
    fromJSON response.json

  pure case result of
    Left err -> Left $ "Create transaction error: " <> show err
    Right response -> Right response

addTransactionItem :: TransactionItem -> Aff (Either String TransactionItem)
addTransactionItem item = do
  result <- attempt do
    let content = writeJSON item
    liftEffect $ Console.log "Adding item to transaction..."

    response <- fetch (baseUrl <> "/transaction/item")
      { method: POST
      , body: content
      , headers:
          { "Content-Type": "application/json"
          , "Accept": "application/json"
          , "Origin": currentConfig.appOrigin
          }
      }
    fromJSON response.json

  pure case result of
    Left err -> Left $ "Add item error: " <> show err
    Right response -> Right response

removeTransactionItem :: UUID -> Aff (Either String Unit)
removeTransactionItem itemId = do
  result <- attempt do
    liftEffect $ Console.log $ "Removing item from transaction: " <> show itemId

    _ <- fetch (baseUrl <> "/transaction/item/" <> show itemId)
      { method: DELETE
      , headers:
          { "Content-Type": "application/json"
          , "Accept": "application/json"
          , "Origin": currentConfig.appOrigin
          }
      }
    pure unit

  pure case result of
    Left err -> Left $ "Remove item error: " <> show err
    Right _ -> Right unit

addPaymentTransaction
  :: PaymentTransaction -> Aff (Either String PaymentTransaction)
addPaymentTransaction payment = do
  result <- attempt do
    let content = writeJSON payment
    liftEffect $ Console.log "Adding payment to transaction..."

    response <- fetch (baseUrl <> "/transaction/payment")
      { method: POST
      , body: content
      , headers:
          { "Content-Type": "application/json"
          , "Accept": "application/json"
          , "Origin": currentConfig.appOrigin
          }
      }
    fromJSON response.json

  pure case result of
    Left err -> Left $ "Add payment error: " <> show err
    Right response -> Right response

removePaymentTransaction :: UUID -> Aff (Either String Unit)
removePaymentTransaction paymentId = do
  result <- attempt do
    liftEffect $ Console.log $ "Removing payment from transaction: " <> show
      paymentId

    _ <- fetch (baseUrl <> "/transaction/payment/" <> show paymentId)
      { method: DELETE
      , headers:
          { "Content-Type": "application/json"
          , "Accept": "application/json"
          , "Origin": currentConfig.appOrigin
          }
      }
    pure unit

  pure case result of
    Left err -> Left $ "Remove payment error: " <> show err
    Right _ -> Right unit

finalizeTransaction :: UUID -> Aff (Either String Transaction)
finalizeTransaction transactionId = do
  result <- attempt do
    liftEffect $ Console.log $ "Finalizing transaction: " <> show transactionId

    response <- fetch
      (baseUrl <> "/transaction/finalize/" <> show transactionId)
      { method: POST
      , headers:
          { "Content-Type": "application/json"
          , "Accept": "application/json"
          , "Origin": currentConfig.appOrigin
          }
      }
    fromJSON response.json

  pure case result of
    Left err -> Left $ "Finalize transaction error: " <> show err
    Right response -> Right response

getTransaction :: UUID -> Aff (Either String Transaction)
getTransaction transactionId = do
  result <- attempt do
    liftEffect $ Console.log $ "Fetching transaction: " <> show transactionId

    response <- fetch (baseUrl <> "/transaction/" <> show transactionId)
      { method: GET
      , headers:
          { "Content-Type": "application/json"
          , "Accept": "application/json"
          , "Origin": currentConfig.appOrigin
          }
      }
    fromJSON response.json

  pure case result of
    Left err -> Left $ "Get transaction error: " <> show err
    Right response -> Right response

getAllTransactions :: Aff (Either String (Array Transaction))
getAllTransactions = do
  result <- attempt do
    liftEffect $ Console.log "Fetching all transactions..."

    response <- fetch (baseUrl <> "/transaction")
      { method: GET
      , headers:
          { "Content-Type": "application/json"
          , "Accept": "application/json"
          , "Origin": currentConfig.appOrigin
          }
      }
    fromJSON response.json

  pure case result of
    Left err -> Left $ "Get all transactions error: " <> show err
    Right response -> Right response

voidTransaction :: UUID -> String -> Aff (Either String Transaction)
voidTransaction transactionId reason = do
  result <- attempt do
    let content = writeJSON { reason }
    liftEffect $ Console.log $ "Voiding transaction: " <> show transactionId

    response <- fetch (baseUrl <> "/transaction/void/" <> show transactionId)
      { method: POST
      , body: content
      , headers:
          { "Content-Type": "application/json"
          , "Accept": "application/json"
          , "Origin": currentConfig.appOrigin
          }
      }
    fromJSON response.json

  pure case result of
    Left err -> Left $ "Void transaction error: " <> show err
    Right response -> Right response

refundTransaction :: UUID -> String -> Aff (Either String Transaction)
refundTransaction transactionId reason = do
  result <- attempt do
    let content = writeJSON { reason }
    liftEffect $ Console.log $ "Refunding transaction: " <> show transactionId

    response <- fetch (baseUrl <> "/transaction/refund/" <> show transactionId)
      { method: POST
      , body: content
      , headers:
          { "Content-Type": "application/json"
          , "Accept": "application/json"
          , "Origin": currentConfig.appOrigin
          }
      }
    fromJSON response.json

  pure case result of
    Left err -> Left $ "Refund transaction error: " <> show err
    Right response -> Right response