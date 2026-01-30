# config

Manage MigrationPilot CLI configuration.

## Usage

```bash
migrationpilot config <command> [options]
```

## Configuration File

Configuration is stored in `~/.migrationpilot/config.json`.

## Subcommands

### init

Initialize configuration interactively.

```bash
migrationpilot config init
```

This command prompts for:
- **API URL** - MigrationPilot server endpoint
- **API Key** - Authentication key (optional)
- **Default source language** - COBOL, Fortran, VB6, or Legacy Java
- **Default target language** - Java, Python, TypeScript, Go, or C#

**Example:**

```bash
$ migrationpilot config init

? MigrationPilot API URL: http://localhost:3001
? API Key (optional): ****
? Default source language: cobol
? Default target language: java

✓ Configuration saved to /home/user/.migrationpilot/config.json
```

---

### get

Get a specific configuration value.

```bash
migrationpilot config get <key>
```

**Example:**

```bash
migrationpilot config get apiUrl
# Output: http://localhost:3001
```

---

### set

Set a configuration value.

```bash
migrationpilot config set <key> <value>
```

**Example:**

```bash
migrationpilot config set apiUrl https://api.migrationpilot.example.com
# Output: ✓ Set apiUrl = https://api.migrationpilot.example.com
```

---

### list

List all configuration values.

```bash
migrationpilot config list
```

**Example:**

```bash
$ migrationpilot config list

MigrationPilot Configuration
────────────────────────────────────────
  apiUrl: http://localhost:3001
  apiKey: ****1234
  defaultLanguage: cobol
  defaultTarget: java
```

---

### path

Show the configuration file path.

```bash
migrationpilot config path
```

**Example:**

```bash
$ migrationpilot config path
/home/user/.migrationpilot/config.json
```

## Configuration Keys

| Key | Description | Example |
|-----|-------------|---------|
| `apiUrl` | MigrationPilot API server URL | `http://localhost:3001` |
| `apiKey` | API authentication key | `mp_abc123...` |
| `defaultLanguage` | Default source language | `cobol` |
| `defaultTarget` | Default target language | `java` |
| `outputFormat` | Default output format | `json` |

## Environment Variables

Configuration can also be set via environment variables:

```bash
export MIGRATIONPILOT_API_URL=https://api.example.com
export MIGRATIONPILOT_API_KEY=your-api-key
```

Environment variables take precedence over config file values.

## Example Configuration File

```json
{
  "apiUrl": "http://localhost:3001",
  "apiKey": "mp_secret_key_here",
  "defaultLanguage": "cobol",
  "defaultTarget": "java",
  "outputFormat": "json"
}
```
