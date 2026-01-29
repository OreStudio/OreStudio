# ORE Studio Database User Management

## Multi-Environment Architecture

ORE Studio supports multiple isolated development environments:

```
ores_admin          # Admin utilities (shared)
ores_template       # Template for tests (shared)
ores_dev_local1     # Environment-specific database
ores_dev_local2     # Environment-specific database
ores_dev_local3     # etc.
```

### Database Naming Convention

- **Environment databases**: `ores_dev_<env>` (e.g., `ores_dev_local2`)
- **Template database**: `ores_template` (used by tests only)
- **Admin database**: `ores_admin` (cluster utilities)

### Scripts

| Script | Purpose |
|--------|---------|
| `recreate_database.sh` | Full teardown + recreate everything |
| `recreate_template.sh` | Recreate just ores_template (for tests) |
| `recreate_env.sh -e local2` | Recreate just ores_dev_local2 |

### Environment Isolation

- Environment databases are created **from scratch** (not from template)
- Recreating one environment does NOT affect others
- Tests use the template to create temporary databases
- Each checkout (local1, local2, etc.) auto-detects its database name

## Role-Based Architecture

ORE Studio now uses a role-based database user system with the following users:

### Application Users
- `ores_ddl_user` - For DDL operations (schema changes, migrations)
- `ores_cli_user` - For command-line interface operations
- `ores_wt_user` - For web toolkit operations
- `ores_comms_user` - For communications service operations
- `ores_http_user` - For HTTP service operations

### Test Users
- `ores_test_ddl_user` - For test database creation/dropping operations
- `ores_test_dml_user` - For running actual tests

### Support Users
- `ores_readonly_user` - For read-only access (analytics, BI)

## Role Hierarchy
- `ores_owner` (nologin) - Base role for DDL operations
- `ores_rw` (nologin) - Base role for read-write operations
- `ores_ro` (nologin) - Base role for read-only operations

Each service user inherits from the appropriate base role.

## Managing Credentials Idiomatically

Due to the number of users, it's recommended to use PostgreSQL's service file mechanism:

### Using pg_service.conf

1. Create a service file at `~/.pg_service.conf` or `./pg_service.conf`:
```
[ores_ddl]
host=localhost
port=5432
dbname=ores_default
user=ores_ddl_user
password=your_password

[ores_cli]
host=localhost
port=5432
dbname=ores_default
user=ores_cli_user
password=your_password

# ... etc for other services
```

2. Set appropriate permissions:
```bash
chmod 600 ~/.pg_service.conf
```

3. Use the service name instead of individual parameters:
```bash
psql service=ores_cli
```

### Environment Variables Alternative

For development and CI/CD environments, you can also use environment variables:

```bash
export PGUSER=ores_cli_user
export PGPASSWORD=your_password
export PGHOST=localhost
export PGPORT=5432
export PGDATABASE=ores_default
```

### Application Configuration

Applications can be configured to use different users based on their function:
- CLI tools use `ores_cli_user`
- Web services use `ores_wt_user`
- Communication services use `ores_comms_user`
- HTTP services use `ores_http_user`
- Migration scripts use `ores_ddl_user`
- Tests use `ores_test_ddl_user` and `ores_test_dml_user`