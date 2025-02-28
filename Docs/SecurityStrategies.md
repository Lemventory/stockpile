# Cheeblr Security Documentation

This document outlines the security architecture, best practices, and implementation guidelines for the Cheeblr application when deployed on a public network.

## Table of Contents

1. [Security Principles](#security-principles)
2. [Authentication Methods](#authentication-methods)
3. [Transport Layer Security](#transport-layer-security)
4. [WebSocket Security](#websocket-security)
5. [Libsodium Implementation](#libsodium-implementation)
6. [Rate Limiting and Protection](#rate-limiting-and-protection)
7. [Deployment Checklist](#deployment-checklist)
8. [Security Audit Guidelines](#security-audit-guidelines)

## Security Principles

### Core Security Principles for Public Networks

| Principle | Description | Security Level | Implementation Difficulty |
|-----------|-------------|---------------|---------------------------|
| End-to-End Encryption | All communications are encrypted using TLS (HTTPS and WebSockets over WSS) | Very High | Low |
| Zero Trust Authentication | Never trust the client; require cryptographic proof of identity | Very High | Medium |
| Replay Attack Protection | Use one-time challenges (nonces) to prevent replay attacks | Very High | Medium |
| Tamper-Resistant Storage | Securely store secrets on both client and server | High | Medium |
| Rate Limiting | Detect and block brute-force attempts | High | Low |
| Anomaly Detection | Monitor and alert on unusual authentication patterns | High | Medium |

## Authentication Methods

### Comparison of Authentication Methods

| Authentication Method | Security Level | Ease of Use | Privacy | Implementation Complexity | Recommended Use Case |
|----------------------|----------------|-------------|---------|---------------------------|---------------------|
| Libsodium Public-Key Authentication | ★★★★★ | ★★★☆☆ | ★★★★★ | ★★★★☆ | Highest security needs, self-contained systems |
| OAuth 2.0 / OpenID Connect | ★★★★☆ | ★★★★★ | ★★☆☆☆ | ★★★☆☆ | Applications needing social login integration |
| JWT with Secure Signatures | ★★★★☆ | ★★★★☆ | ★★★★☆ | ★★★☆☆ | Services with distributed microservices |
| HMAC-based Authentication | ★★★★☆ | ★★★☆☆ | ★★★★☆ | ★★★☆☆ | API authentication |
| Password + Argon2id Hashing | ★★★☆☆ | ★★★★☆ | ★★★★☆ | ★★☆☆☆ | Traditional user authentication |
| Machine ID (LAN-only) | ★★☆☆☆ | ★★★★★ | ★★★☆☆ | ★★☆☆☆ | Controlled LAN environments only |

### Recommended Primary Authentication Method: Libsodium Public-Key Authentication

Based on objective security assessment, a Libsodium-based challenge-response with public-key cryptography offers the highest security level while maintaining reasonable usability for a specialized application like Cheeblr.

#### Key advantages:

1. Provides cryptographic security without requiring passwords
2. Prevents credential theft even if the server database is compromised
3. Creates an unforgeable link between the user and their device
4. Resists sophisticated attack vectors including replay attacks
5. Maintains full privacy without relying on third-party providers

## Transport Layer Security

### TLS Configuration Guidelines

All Cheeblr communications should use TLS 1.2+ with the following configuration:

| Setting | Recommendation | Security Level |
|---------|---------------|----------------|
| Minimum TLS Version | TLS 1.2 (TLS 1.3 preferred) | ★★★★★ |
| Cipher Suites | Strong AEAD ciphers only (e.g., AES-GCM, ChaCha20-Poly1305) | ★★★★★ |
| Certificate Type | OV or EV certificate from trusted CA | ★★★★☆ |
| Key Length | RSA 2048+ or ECC P-256+ | ★★★★★ |
| HSTS | Enabled with includeSubDomains and preload | ★★★★☆ |
| Certificate Renewal | Automatic via Let's Encrypt or similar | ★★★★☆ |
| OCSP Stapling | Enabled | ★★★★☆ |

### Implementation in Haskell Backend

```haskell
-- TLS configuration for Warp in Haskell (already used in your Cheeblr backend)
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS

tlsSettings :: TLSSettings
tlsSettings = tlsSettingsChain
    "/path/to/certificate.pem"        -- Your certificate
    ["/path/to/intermediate-cert.pem"] -- Chain certificates if needed
    "/path/to/private-key.pem"        -- Private key
    
secureApp :: Application -> IO ()
secureApp app = do
    let settings = setPort 8443 defaultSettings
    runTLS tlsSettings settings app
```

### Implementation in PureScript Frontend

In the PureScript frontend, ensure all endpoints use HTTPS and WSS protocols:

```purescript
baseUrl :: String
baseUrl = "https://api.cheeblr.com"

wsUrl :: String
wsUrl = "wss://api.cheeblr.com/ws"
```

## WebSocket Security

### Secure WebSocket Implementation

| Security Measure | Description | Security Level |
|-----------------|-------------|----------------|
| WSS Protocol | Use wss:// instead of ws:// | ★★★★★ |
| Authentication Handshake | Require authentication before establishing persistent connection | ★★★★☆ |
| Message Authentication | Sign individual messages | ★★★★☆ |
| Heartbeat Pings | Implement heartbeat mechanism to prevent connection hijacking | ★★★☆☆ |
| Connection Timeouts | Set reasonable timeouts to limit exposure | ★★★☆☆ |
| Message Validation | Strictly validate all incoming messages | ★★★★☆ |

## Libsodium Implementation

### Client-Side Key Generation and Storage in PureScript

```purescript
-- PureScript FFI to libsodium
foreign import generateKeyPair :: Effect (Promise KeyPair)
foreign import signChallenge :: String -> String -> Effect (Promise String)
foreign import storeKeys :: KeyPair -> Effect (Promise Unit)
foreign import retrieveKeys :: Effect (Promise (Maybe KeyPair))

-- PureScript implementation
generateAndStoreClientKeys :: Aff (Either String String)
generateAndStoreClientKeys = do
  keyPairResult <- attempt $ liftEffect $ toAffE generateKeyPair
  case keyPairResult of
    Left err -> pure $ Left $ "Failed to generate key pair: " <> show err
    Right keyPair -> do
      storeResult <- attempt $ liftEffect $ toAffE $ storeKeys keyPair
      case storeResult of
        Left err -> pure $ Left $ "Failed to store keys: " <> show err
        Right _ -> pure $ Right keyPair.publicKey

-- Type definitions
type KeyPair = 
  { publicKey :: String
  , privateKey :: String
  , created :: String
  }
```

### WebSocket Authentication Flow

1. **Client Authentication in PureScript**

```purescript
-- WebSocket authentication in PureScript
authenticateWebSocket :: String -> Aff (Either String WebSocket)
authenticateWebSocket url = do
  wsResult <- attempt $ liftEffect $ newWebSocket url {}
  
  case wsResult of
    Left err -> 
      pure $ Left $ "Failed to connect: " <> show err
    
    Right ws -> do
      -- Set up message handler
      _ <- liftEffect $ ws.onOpen $ \_ -> do
        retrieveKeys >>= case _ of
          Just keys -> do
            let authRequest = 
                  writeJSON { type: "auth_request"
                            , publicKey: keys.publicKey 
                            }
            ws.send authRequest
          Nothing ->
            Console.error "No keys found for authentication"
      
      -- Handle authentication messages
      _ <- liftEffect $ ws.onMessage $ \event -> do
        case readJSON event.data of
          Right msg -> case msg.type of
            "challenge" -> do
              retrieveKeys >>= case _ of
                Just keys -> do
                  signatureResult <- signChallenge keys.privateKey msg.challenge
                  let authResponse = 
                        writeJSON { type: "auth_response"
                                  , signature: signatureResult 
                                  }
                  ws.send authResponse
                Nothing ->
                  Console.error "No keys found to sign challenge"
                  
            "auth_success" ->
              Console.log "Authentication successful"
              
            "auth_failure" ->
              Console.error "Authentication failed"
              
            _ -> pure unit
              
          Left _ ->
            Console.error "Invalid message format"
              
      pure $ Right ws
```

2. **Server Handles Authentication (Haskell Implementation)**

```haskell
-- Server-side WebSocket authentication in Haskell for Cheeblr
import qualified Data.Aeson as A
import qualified Crypto.Sign.Ed25519 as Ed25519
import qualified Data.ByteString.Base64 as B64
import Data.Time.Clock (getCurrentTime, addUTCTime)
import Database.PostgreSQL.Simple (Connection, execute, query)

-- Authentication message types
data AuthMessage 
  = AuthRequest { clientPubKey :: Text }
  | AuthChallenge { challenge :: Text }
  | AuthResponse { signature :: Text }
  | AuthSuccess { sessionToken :: Text }
  | AuthFailure { reason :: Text }
  deriving (Show, Generic)

instance A.ToJSON AuthMessage
instance A.FromJSON AuthMessage

-- Authentication handler for WebSockets
handleWSAuth :: Pool Connection -> WS.Connection -> IO ()
handleWSAuth pool wsConn = do
    -- Receive auth request
    msg <- WS.receiveData wsConn
    case A.decode msg of
        Just (AuthRequest clientPubKey) -> do
            -- Generate random challenge (nonce)
            challenge <- generateSecureRandomBS 32 >>= return . B64.encode
            
            -- Store challenge with timestamp for expiration (5 minute validity)
            now <- getCurrentTime
            let expiry = addUTCTime 300 now
            withResource pool $ \conn ->
                execute conn 
                    "INSERT INTO auth_challenges (public_key, challenge, expires_at) VALUES (?, ?, ?)"
                    (clientPubKey, challenge, expiry)
            
            -- Send challenge to client
            WS.sendTextData wsConn $ A.encode $ AuthChallenge 
                { challenge = decodeUtf8 challenge }
            
            -- Receive client's signed challenge
            signedMsg <- WS.receiveData wsConn
            case A.decode signedMsg of
                Just (AuthResponse signature) -> do
                    -- Verify challenge is valid and not expired
                    validChallenge <- withResource pool $ \conn -> do
                        results <- query conn 
                            "SELECT 1 FROM auth_challenges WHERE public_key = ? AND challenge = ? AND expires_at > NOW()"
                            (clientPubKey, challenge) :: IO [Only Int]
                        return $ not $ null results
                    
                    -- Verify signature
                    let pubKeyBS = B64.decodeLenient $ encodeUtf8 clientPubKey
                        signatureBS = B64.decodeLenient $ encodeUtf8 signature
                        challengeBS = B64.decodeLenient challenge
                        
                    let signatureValid = case Ed25519.signature signatureBS of
                            Nothing -> False
                            Just sig -> case Ed25519.publicKey pubKeyBS of
                                Nothing -> False
                                Just pk -> Ed25519.verify pk challengeBS sig
                    
                    if validChallenge && signatureValid
                        then do
                            -- Generate session token
                            sessionToken <- generateSecureRandomBS 32 >>= return . decodeUtf8 . B64.encode
                            
                            -- Store session
                            withResource pool $ \conn ->
                                execute conn 
                                    "INSERT INTO sessions (token, public_key, created_at) VALUES (?, ?, NOW())"
                                    (sessionToken, clientPubKey)
                                    
                            -- Send success response
                            WS.sendTextData wsConn $ A.encode $ AuthSuccess 
                                { sessionToken = sessionToken }
                        else do
                            -- Send failure response
                            WS.sendTextData wsConn $ A.encode $ AuthFailure 
                                { reason = "Authentication failed" }
                
                _ -> WS.sendTextData wsConn $ A.encode $ AuthFailure 
                        { reason = "Invalid response format" }
        
        _ -> WS.sendTextData wsConn $ A.encode $ AuthFailure 
                { reason = "Invalid request format" }
```

## Rate Limiting and Protection

### Recommended Protection Measures

| Measure | Implementation | Security Level |
|---------|---------------|----------------|
| IP-based Rate Limiting | Limit auth attempts per IP address (e.g., 5 attempts per minute) | ★★★★☆ |
| Exponential Backoff | Increase delay after failed attempts | ★★★★☆ |
| Request Throttling | Limit overall API requests per client | ★★★☆☆ |
| CAPTCHA for Suspicious Activity | Require CAPTCHA after multiple failures | ★★★☆☆ |
| IP Reputation | Block known malicious IP addresses | ★★★☆☆ |
| Anomaly Detection | Monitor for unusual patterns of authentication | ★★★★☆ |

### Rate Limiting Implementation for Cheeblr

```haskell
-- Rate limiting middleware for Haskell Servant backend
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Data.Time
import qualified Data.Map.Strict as Map
import Network.Wai
import Network.HTTP.Types

-- Rate limit configuration
data RateLimiter = RateLimiter
  { rlMaxAttempts :: Int             -- Max attempts per window
  , rlWindowSeconds :: Int           -- Time window in seconds
  , rlBackoffFactor :: Double        -- Exponential backoff factor
  , rlCache :: TVar (Map.Map String [UTCTime]) -- IP -> timestamps map
  }

-- Create a new rate limiter
newRateLimiter :: Int -> Int -> Double -> IO RateLimiter
newRateLimiter maxAttempts windowSeconds backoffFactor = do
  cache <- newTVarIO Map.empty
  return RateLimiter
    { rlMaxAttempts = maxAttempts
    , rlWindowSeconds = windowSeconds
    , rlBackoffFactor = backoffFactor
    , rlCache = cache
    }

-- Check if a request should be rate limited
checkRateLimit :: RateLimiter -> String -> IO Bool
checkRateLimit limiter clientIP = do
  now <- getCurrentTime
  let windowStart = addUTCTime (fromIntegral $ negate $ rlWindowSeconds limiter) now
  
  atomically $ do
    cache <- readTVar (rlCache limiter)
    let attempts = Map.findWithDefault [] clientIP cache
    
    -- Filter out attempts outside the current window
    let recentAttempts = filter (>= windowStart) attempts
    
    -- Add the current attempt 
    let newAttempts = now : recentAttempts
    
    -- Update the cache
    writeTVar (rlCache limiter) $ Map.insert clientIP newAttempts cache
    
    -- Check if we're over the limit
    if length recentAttempts >= rlMaxAttempts limiter
      then do
        -- Calculate backoff time based on excess attempts
        let excessAttempts = length recentAttempts - rlMaxAttempts limiter + 1
        let backoffSeconds = (rlBackoffFactor limiter) ^ excessAttempts
        let lastAttempt = maximum recentAttempts
        let earliestAllowed = addUTCTime (realToFrac backoffSeconds) lastAttempt
        
        -- Compare with current time
        return (now < earliestAllowed)
      else return False

-- Rate limiting middleware for WAI applications
rateLimitMiddleware :: RateLimiter -> Middleware
rateLimitMiddleware limiter app req respond = do
  -- Get client IP from request
  let clientIP = show $ remoteHost req
  
  -- Check rate limit
  limited <- checkRateLimit limiter clientIP
  
  if limited
    then do
      -- Respond with 429 Too Many Requests
      respond $ responseLBS
        status429
        [("Content-Type", "application/json"), ("Retry-After", "60")]
        "{\"error\":\"Too many requests. Please try again later.\"}"
    else
      -- Continue with the application
      app req respond
```

## Deployment Checklist

### Security Configuration Checklist

✅ TLS 1.2+ configured with strong ciphers  
✅ All endpoints use HTTPS/WSS  
✅ Public key authentication implemented with Libsodium  
✅ Challenge-response mechanism with nonces  
✅ Rate limiting and protection against brute force  
✅ Secure key storage on client and server  
✅ CORS policy configured properly  
✅ HTTP security headers implemented  
✅ WebSocket connections authenticated before use  
✅ Session management and timeouts implemented  
✅ Logging and monitoring for security events  

## Security Audit Guidelines

Before deploying Cheeblr to a public network, conduct a security audit covering:

1. **Authentication Implementation**
   - Verify the cryptographic implementation is correct
   - Ensure nonces are truly random and used only once
   - Check key storage mechanisms for vulnerabilities

2. **Transport Security**
   - Verify TLS configuration using tools like SSL Labs
   - Ensure no mixed content issues exist
   - Test certificate validation

3. **API Security**
   - Verify all endpoints require authentication
   - Check for proper input validation
   - Test rate limiting effectiveness

4. **WebSocket Security**
   - Verify authentication before session establishment
   - Test timeouts and disconnection handling
   - Ensure message validation is thorough

5. **Threat Modeling**
   - Identify sensitive data and protection mechanisms
   - Model potential attack vectors
   - Develop mitigation strategies for identified threats