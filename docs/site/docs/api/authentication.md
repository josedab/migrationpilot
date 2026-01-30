# API Authentication

MigrationPilot supports multiple authentication methods to secure API access.

## Authentication Methods

| Method | Use Case | Header |
|--------|----------|--------|
| JWT Token | Web dashboard, user sessions | `Authorization: Bearer <token>` |
| API Key | CLI, programmatic access | `Authorization: Bearer <api-key>` |
| OAuth2/OIDC | Enterprise SSO integration | Standard OAuth2 flow |

## JWT Authentication

### Obtaining a Token

```bash
POST /api/auth/login
Content-Type: application/json

{
  "email": "user@example.com",
  "password": "your-password"
}
```

Response:

```json
{
  "token": "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...",
  "expiresIn": 86400,
  "user": {
    "id": "user-uuid",
    "email": "user@example.com",
    "name": "John Doe",
    "role": "developer"
  }
}
```

### Using the Token

Include the token in all API requests:

```bash
curl -X GET http://localhost:4000/api/projects \
  -H "Authorization: Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9..."
```

### Token Refresh

```bash
POST /api/auth/refresh
Authorization: Bearer <current-token>
```

Response:

```json
{
  "token": "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...",
  "expiresIn": 86400
}
```

## API Keys

API keys are ideal for CLI and automated access.

### Creating an API Key

Via the web dashboard:

1. Navigate to **Settings** → **API Keys**
2. Click **Create New Key**
3. Enter a name and select permissions
4. Copy the key (shown only once)

Via the API:

```bash
POST /api/auth/api-keys
Authorization: Bearer <jwt-token>
Content-Type: application/json

{
  "name": "CI/CD Pipeline",
  "permissions": ["projects:read", "projects:write", "migrations:run"],
  "expiresAt": "2025-12-31T23:59:59Z"
}
```

Response:

```json
{
  "id": "key-uuid",
  "name": "CI/CD Pipeline",
  "keyPrefix": "mp_sk_",
  "key": "mp_sk_live_abc123def456...",  // Full key shown only once
  "permissions": ["projects:read", "projects:write", "migrations:run"],
  "expiresAt": "2025-12-31T23:59:59Z",
  "createdAt": "2024-01-28T10:00:00Z"
}
```

### Using an API Key

```bash
curl -X GET http://localhost:4000/api/projects \
  -H "Authorization: Bearer mp_sk_live_abc123def456..."
```

### Key Permissions

| Permission | Description |
|------------|-------------|
| `projects:read` | View projects |
| `projects:write` | Create/update projects |
| `projects:delete` | Delete projects |
| `files:read` | View source files |
| `files:write` | Upload/delete files |
| `migrations:run` | Execute migrations |
| `rules:read` | View business rules |
| `rules:write` | Approve/reject rules |
| `admin` | Full access |

### Revoking API Keys

```bash
DELETE /api/auth/api-keys/{keyId}
Authorization: Bearer <jwt-token>
```

## OAuth2/OIDC Integration

For enterprise SSO integration.

### Configuration

Set environment variables:

```env
# Generic OIDC
OIDC_ISSUER=https://auth.example.com
OIDC_CLIENT_ID=migrationpilot
OIDC_CLIENT_SECRET=your-client-secret
OIDC_REDIRECT_URI=https://app.migrationpilot.com/auth/callback

# Azure AD
AZURE_AD_TENANT_ID=your-tenant-id
AZURE_AD_CLIENT_ID=your-client-id
AZURE_AD_CLIENT_SECRET=your-client-secret

# Okta
OKTA_DOMAIN=your-org.okta.com
OKTA_CLIENT_ID=your-client-id
OKTA_CLIENT_SECRET=your-client-secret
```

### OAuth2 Flow

1. **Initiate login**:
   ```
   GET /api/auth/oauth/authorize?provider=azure
   ```

2. **Redirect to provider** (automatic)

3. **Callback with code**:
   ```
   GET /api/auth/oauth/callback?code=AUTH_CODE&state=STATE
   ```

4. **Receive JWT token**:
   ```json
   {
     "token": "eyJhbGciOiJIUzI1NiIs...",
     "user": { ... }
   }
   ```

## Role-Based Access Control

### Roles

| Role | Description |
|------|-------------|
| `admin` | Full access to organization |
| `developer` | Create/manage projects |
| `reviewer` | Review and approve rules |
| `viewer` | Read-only access |

### Checking Permissions

The API returns `403 Forbidden` for unauthorized actions:

```json
{
  "error": "Forbidden",
  "message": "Insufficient permissions",
  "required": ["projects:delete"],
  "current": ["projects:read", "projects:write"]
}
```

## Security Best Practices

### 1. Use HTTPS

Always use HTTPS in production:

```bash
# Bad
curl http://api.example.com/...

# Good
curl https://api.example.com/...
```

### 2. Rotate API Keys

Regularly rotate API keys:

```bash
# Create new key
POST /api/auth/api-keys

# Update applications to use new key

# Revoke old key
DELETE /api/auth/api-keys/{oldKeyId}
```

### 3. Use Minimal Permissions

Request only needed permissions:

```json
{
  "name": "Read-Only Monitoring",
  "permissions": ["projects:read", "rules:read"]
}
```

### 4. Set Expiration

Always set expiration for API keys:

```json
{
  "name": "Temporary Access",
  "expiresAt": "2024-02-28T23:59:59Z"
}
```

### 5. Monitor Usage

Check API key usage:

```bash
GET /api/auth/api-keys/{keyId}/usage
```

Response:

```json
{
  "keyId": "key-uuid",
  "lastUsed": "2024-01-28T15:30:00Z",
  "usageCount": 1523,
  "usageByEndpoint": {
    "/api/projects": 500,
    "/api/projects/{id}/files": 1023
  }
}
```

## Error Responses

### 401 Unauthorized

No token or invalid token:

```json
{
  "error": "Unauthorized",
  "message": "Invalid or missing authentication token"
}
```

### 403 Forbidden

Valid token but insufficient permissions:

```json
{
  "error": "Forbidden",
  "message": "You do not have permission to perform this action"
}
```

### 419 Token Expired

JWT token has expired:

```json
{
  "error": "TokenExpired",
  "message": "Your session has expired. Please log in again."
}
```

## CLI Authentication

Configure the CLI with your API key:

```bash
# Interactive setup
migrationpilot config init

# Or set directly
migrationpilot config set apiKey mp_sk_live_abc123...
migrationpilot config set apiUrl https://api.migrationpilot.com
```

## Related Topics

- [Security](../deployment/security.md) - Security best practices
- [CLI Config](../cli/config.md) - CLI authentication setup
- [API Overview](./overview.md) - API introduction
