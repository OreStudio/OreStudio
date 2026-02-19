#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#
# Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
#
# This program is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation; either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along with
# this program; if not, write to the Free Software Foundation, Inc., 51
# Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
#
"""
PlantUML ER Diagram - SQL Parser

Parses SQL CREATE/DROP statements to generate a JSON model for ER diagram
generation. Validates SQL against conventions defined in SKILL.org.
"""

import argparse
import json
import re
import sys
from dataclasses import dataclass, field, asdict
from datetime import datetime, timezone
from pathlib import Path
from typing import Optional


# Known component prefixes and their descriptions
# Order determines package display order in diagram
# All tables/functions are in the public schema with ores_ product prefix
COMPONENT_PREFIXES = {
    'ores_iam_': {'name': 'iam', 'description': 'Identity & Access Management', 'schema': 'public', 'color': '#E8F4FD', 'order': 1},
    'ores_assets_': {'name': 'assets', 'description': 'Digital Assets', 'schema': 'public', 'color': '#F3E5F5', 'order': 2},
    'ores_refdata_': {'name': 'refdata', 'description': 'Reference Data', 'schema': 'public', 'color': '#FFF3E0', 'order': 3},
    'ores_dq_': {'name': 'dq', 'description': 'Data Quality', 'schema': 'public', 'color': '#E1F5FE', 'order': 4},
    'ores_variability_': {'name': 'variability', 'description': 'Feature Flags', 'schema': 'public', 'color': '#E8F5E9', 'order': 5},
    'ores_telemetry_': {'name': 'telemetry', 'description': 'Telemetry & Logging', 'schema': 'public', 'color': '#FCE4EC', 'order': 6},
    'ores_geo_': {'name': 'geo', 'description': 'Geolocation', 'schema': 'public', 'color': '#FFF9C4', 'order': 7},
    'ores_fsm_': {'name': 'fsm', 'description': 'Finite State Machine', 'schema': 'public', 'color': '#E8EAF6', 'order': 8},
    'ores_trade_': {'name': 'trade', 'description': 'Trade Envelope & Lifecycle', 'schema': 'public', 'color': '#E8F5E9', 'order': 9},
    'ores_utility_': {'name': 'utility', 'description': 'Utility Functions', 'schema': 'public', 'color': '#ECEFF1', 'order': 9},
    'ores_seed_': {'name': 'seed', 'description': 'Seed Functions', 'schema': 'public', 'color': '#ECEFF1', 'order': 10},
}

# Valid schema names for pattern matching
SCHEMA_PATTERN = r'(?:public)'

# Columns that are known FK references by name
FK_COLUMNS = {
    'change_reason_code',  # References change reason codes
    'coding_scheme_code',  # References coding schemes
    'category_code',       # References change reason categories
    'account_id',          # References iam_accounts_tbl
    'role_id',             # References iam_roles_tbl
    'permission_id',       # References iam_permissions_tbl
    'image_id',            # References assets_images_tbl
    'tag_id',              # References assets_tags_tbl
    'dataset_id',          # References dq_datasets_tbl
    'methodology_id',      # References dq_methodologies_tbl
    'authority_type',      # References dq_coding_scheme_authority_types_tbl
    'nature_code',         # References dq_nature_dimensions_tbl
    'origin_code',         # References dq_origin_dimensions_tbl
    'treatment_code',      # References dq_treatment_dimensions_tbl
    'country_code',        # References refdata_countries_tbl
    'currency_code',       # References refdata_currencies_tbl
}

# FK column to target table mapping for relationship inference
FK_TARGET_TABLES = {
    # IAM
    'account_id': 'iam_accounts_tbl',
    'role_id': 'iam_roles_tbl',
    'permission_id': 'iam_permissions_tbl',
    'session_id': 'iam_sessions_tbl',
    # Assets
    'image_id': 'assets_images_tbl',
    'tag_id': 'assets_tags_tbl',
    # Data Quality - Core
    'dataset_id': 'dq_datasets_tbl',
    'coding_scheme_code': 'dq_coding_schemes_tbl',
    'change_reason_code': 'dq_change_reasons_tbl',
    'category_code': 'dq_change_reason_categories_tbl',
    'catalog_name': 'dq_catalogs_tbl',
    'domain_name': 'dq_data_domains_tbl',
    'subject_area_name': 'dq_subject_areas_tbl',
    'methodology_id': 'dq_methodologies_tbl',
    'authority_type': 'dq_coding_scheme_authority_types_tbl',
    # Data Quality - Dimensions
    'nature_code': 'dq_nature_dimensions_tbl',
    'origin_code': 'dq_origin_dimensions_tbl',
    'treatment_code': 'dq_treatment_dimensions_tbl',
    # Dataset derivation (self-reference)
    'upstream_derivation_id': 'dq_datasets_tbl',
    # Reference data
    'country_code': 'refdata_countries_tbl',
    'currency_code': 'refdata_currencies_tbl',
}

# Table classification colors and stereotypes
TABLE_STYLES = {
    'temporal': {'stereotype': '<<temporal>>', 'color': '#C6F0D8'},
    'artefact': {'stereotype': '', 'color': '#F7E5FF'},
    'junction': {'stereotype': '<<junction>>', 'color': '#ECECEC'},
    'current_state': {'stereotype': '', 'color': '#F7E5FF'},
}


@dataclass
class Relationship:
    """Relationship between two tables."""
    from_table: str
    to_table: str
    cardinality: str  # e.g., "||--o{" for one-to-many
    label: str


@dataclass
class Warning:
    """Convention violation warning."""
    file: str
    line: int
    code: str
    message: str

    def __str__(self):
        return f"WARNING [{self.code}] {self.file}:{self.line}\n  {self.message}"


@dataclass
class Column:
    """Table column definition."""
    name: str
    type: str
    nullable: bool = True
    is_pk: bool = False
    is_fk: bool = False
    is_unique: bool = False
    default: Optional[str] = None
    references: Optional[dict] = None


@dataclass
class Index:
    """Index definition."""
    name: str
    columns: list
    unique: bool = False
    partial: Optional[str] = None


@dataclass
class Table:
    """Table definition."""
    name: str
    component: str
    entity: str
    classification: str
    columns: list = field(default_factory=list)
    primary_key: dict = field(default_factory=dict)
    indexes: list = field(default_factory=list)
    constraints: list = field(default_factory=list)
    source_file: str = ""
    description: str = ""

    @property
    def stereotype(self):
        return TABLE_STYLES.get(self.classification, {}).get('stereotype', '')

    @property
    def color(self):
        return TABLE_STYLES.get(self.classification, {}).get('color', '#FFFFFF')


@dataclass
class Function:
    """Function definition."""
    name: str
    source_file: str
    line: int


@dataclass
class View:
    """View definition."""
    name: str
    source_file: str
    line: int


@dataclass
class ParseResult:
    """Result of parsing SQL files."""
    tables: list = field(default_factory=list)
    functions: list = field(default_factory=list)
    warnings: list = field(default_factory=list)


class SQLParser:
    """Parses SQL CREATE statements and validates conventions."""

    def __init__(self, warn: bool = True, ignore_file: Optional[Path] = None):
        self.warn = warn
        self.warnings = []
        self.tables = {}
        self.functions = {}
        self.views = {}
        self.drop_tables = set()
        self.drop_functions = set()
        self.drop_views = set()
        self.unique_columns = {}  # table_name -> set of column names
        self.table_descriptions = {}  # table_name -> description text
        self.relationships = []  # List of Relationship objects
        self.ignore_rules = []  # List of (code_pattern, name_pattern) tuples
        if ignore_file:
            self._load_ignore_file(ignore_file)

    def _load_ignore_file(self, ignore_file: Path) -> None:
        """Load validation ignore rules from file.

        Format: WARNING_CODE entity_pattern
        - Lines starting with # are comments
        - Empty lines are skipped
        - Patterns support * wildcard (e.g., dq_*_artefact_tbl)
        """
        if not ignore_file.exists():
            return

        with open(ignore_file, 'r') as f:
            for line in f:
                line = line.strip()
                # Skip comments and empty lines
                if not line or line.startswith('#'):
                    continue

                parts = line.split(None, 1)  # Split on whitespace, max 2 parts
                if len(parts) == 2:
                    code_pattern, name_pattern = parts
                    self.ignore_rules.append((code_pattern, name_pattern))

    def _is_warning_ignored(self, code: str, entity_name: str) -> bool:
        """Check if a warning should be ignored based on ignore rules."""
        import fnmatch
        for code_pattern, name_pattern in self.ignore_rules:
            if fnmatch.fnmatch(code, code_pattern) and fnmatch.fnmatch(entity_name, name_pattern):
                return True
        return False

    def parse_create_dir(self, create_dir: Path) -> None:
        """Parse all SQL files in the create directory."""
        for sql_file in sorted(create_dir.rglob('*_create.sql')):
            self._parse_create_file(sql_file)

    def parse_drop_dir(self, drop_dir: Path) -> None:
        """Parse all SQL files in the drop directory."""
        for sql_file in sorted(drop_dir.rglob('*_drop.sql')):
            self._parse_drop_file(sql_file)

    def _parse_create_file(self, file_path: Path) -> None:
        """Parse a single CREATE SQL file."""
        relative_path = str(file_path.relative_to(file_path.parent.parent.parent))
        content = file_path.read_text()
        lines = content.split('\n')

        # Extract table descriptions (comments before CREATE TABLE)
        self._extract_table_descriptions(content)

        # Extract tables
        self._extract_tables(content, lines, relative_path)

        # Extract unique indexes
        self._extract_unique_indexes(content, relative_path)

        # Extract functions
        self._extract_functions(content, lines, relative_path)

        # Extract views (regular and materialized)
        self._extract_views(content, lines, relative_path)

    def _parse_drop_file(self, file_path: Path) -> None:
        """Parse a single DROP SQL file to track what should be dropped."""
        content = file_path.read_text()

        # Track dropped tables (with or without schema prefix)
        for match in re.finditer(rf'drop\s+table\s+if\s+exists\s+(?:"?{SCHEMA_PATTERN}"?\.)?"?(\w+)"?', content, re.IGNORECASE):
            self.drop_tables.add(match.group(1))

        # Track dropped functions (with or without schema prefix)
        for match in re.finditer(rf'drop\s+function\s+if\s+exists\s+(?:{SCHEMA_PATTERN}\.)?(\w+)', content, re.IGNORECASE):
            self.drop_functions.add(match.group(1))

        # Track dropped views (with or without schema prefix)
        for match in re.finditer(rf'drop\s+(?:materialized\s+)?view\s+if\s+exists\s+(?:"?{SCHEMA_PATTERN}"?\.)?"?(\w+)"?', content, re.IGNORECASE):
            self.drop_views.add(match.group(1))

    def _extract_table_descriptions(self, content: str) -> None:
        """Extract table descriptions from comments before CREATE TABLE.

        Supports two comment formats:
        1. Block style: -- ===...=== / description lines / -- ===...===
        2. JavaDoc style: /** ... */
        """
        # Pattern 1: -- === block comments
        # Match: -- ==== / content lines / -- ==== followed by CREATE TABLE
        block_pattern = re.compile(
            r'-- =+\s*\n'                          # Opening delimiter
            r'((?:-- [^\n]*\n)+)'                  # Content lines (captured)
            r'-- =+\s*\n'                          # Closing delimiter
            r'\s*'                                  # Optional whitespace
            rf'create\s+table\s+if\s+not\s+exists\s+(?:"?{SCHEMA_PATTERN}"?\.)?"?(\w+)"?',  # Table name
            re.IGNORECASE
        )

        for match in block_pattern.finditer(content):
            comment_block = match.group(1)
            table_name = match.group(2)

            # Extract description lines (remove -- prefix and clean up)
            lines = []
            for line in comment_block.split('\n'):
                line = line.strip()
                if line.startswith('-- '):
                    text = line[3:].strip()
                    # Skip delimiter lines and empty lines
                    if text and not text.startswith('=') and not text.startswith('Table:'):
                        lines.append(text)
                elif line == '--':
                    continue

            if lines:
                self.table_descriptions[table_name] = '\n'.join(lines)

        # Pattern 2: /** ... */ JavaDoc style
        javadoc_pattern = re.compile(
            r'/\*\*\s*\n'                          # Opening /**
            r'((?:\s*\*[^\n]*\n)+)'                # Content lines (captured)
            r'\s*\*/\s*\n'                         # Closing */
            r'(?:set\s+schema[^\n]*\n)?'           # Optional set schema
            r'\s*'                                  # Optional whitespace
            r'(?:-- [^\n]*\n)*'                    # Optional comment lines
            r'\s*'                                  # Optional whitespace
            rf'create\s+table\s+if\s+not\s+exists\s+(?:"?{SCHEMA_PATTERN}"?\.)?"?(\w+)"?',  # Table name
            re.IGNORECASE
        )

        for match in javadoc_pattern.finditer(content):
            comment_block = match.group(1)
            table_name = match.group(2)

            # Skip if we already have a description (block style takes precedence)
            if table_name in self.table_descriptions:
                continue

            # Extract description lines (remove * prefix and clean up)
            lines = []
            for line in comment_block.split('\n'):
                line = line.strip()
                if line.startswith('* '):
                    text = line[2:].strip()
                    # Skip title lines (usually first non-empty line)
                    if text and not lines:
                        # First line is often the title - skip generic titles
                        if 'Table' in text or 'Artefact' in text:
                            continue
                    if text:
                        lines.append(text)
                elif line == '*':
                    continue

            if lines:
                self.table_descriptions[table_name] = '\n'.join(lines)

    def _extract_tables(self, content: str, lines: list, file_path: str) -> None:
        """Extract table definitions from SQL content."""
        # Pattern for CREATE TABLE (with or without schema prefix)
        table_pattern = re.compile(
            rf'create\s+table\s+if\s+not\s+exists\s+(?:"?{SCHEMA_PATTERN}"?\.)?"?(\w+)"?\s*\((.*?)\);',
            re.IGNORECASE | re.DOTALL
        )

        for match in table_pattern.finditer(content):
            table_name = match.group(1)
            table_body = match.group(2)

            # Find line number
            start_pos = match.start()
            line_num = content[:start_pos].count('\n') + 1

            # Parse table
            table = self._parse_table(table_name, table_body, file_path, line_num)
            if table:
                self.tables[table_name] = table

    def _extract_unique_indexes(self, content: str, file_path: str) -> None:
        """Extract unique index definitions to identify unique columns.

        Handles both single-column and composite unique indexes.
        For composite indexes, all columns in the index are marked as unique.
        """
        # Pattern: create unique index ... on "schema"."table_name" (col1, col2, ...)
        # Captures table name and the entire column list (which may have multiple columns)
        unique_idx_pattern = re.compile(
            rf'create\s+unique\s+index\s+if\s+not\s+exists\s+\w+\s+'
            rf'on\s+"?{SCHEMA_PATTERN}"?\."?(\w+)"?\s*\(([^)]+)\)',
            re.IGNORECASE
        )

        for match in unique_idx_pattern.finditer(content):
            table_name = match.group(1)
            columns_str = match.group(2)

            # Parse column list - handles "col1, col2" or "col1" or "col1, col2, col3"
            # Also handles quoted column names and expressions with WHERE clauses
            # Strip any WHERE clause first
            columns_part = columns_str.split(' where ')[0].split(' WHERE ')[0]
            column_names = [col.strip().strip('"') for col in columns_part.split(',')]

            if table_name not in self.unique_columns:
                self.unique_columns[table_name] = set()
            for col_name in column_names:
                # Skip empty strings and expressions (those containing parentheses)
                if col_name and '(' not in col_name:
                    self.unique_columns[table_name].add(col_name)

    def _parse_table(self, name: str, body: str, file_path: str, line_num: int) -> Optional[Table]:
        """Parse a table definition."""
        # Detect component from name
        component = self._detect_component(name, file_path, line_num)

        # Extract entity name (remove component prefix and _tbl suffix)
        entity = name
        for prefix in COMPONENT_PREFIXES:
            if name.startswith(prefix):
                entity = name[len(prefix):]
                break
        if entity.endswith('_tbl'):
            entity = entity[:-4]

        # Validate table naming
        self._validate_table_naming(name, file_path, line_num)

        # Parse columns
        columns = self._parse_columns(body)

        # Detect classification
        classification = self._detect_classification(columns, name)

        # Extract primary key
        pk = self._extract_primary_key(body, columns)

        # Validate temporal requirements
        if classification == 'temporal':
            self._validate_temporal_table(name, columns, body, file_path, line_num)

        # Validate tenant-aware requirements (all temporal and artefact tables should be tenant-aware)
        if classification in ('temporal', 'artefact'):
            self._validate_tenant_aware_table(name, columns, body, pk, file_path, line_num)

        # Get description from extracted comments
        description = self.table_descriptions.get(name, '')

        table = Table(
            name=name,
            component=component,
            entity=entity,
            classification=classification,
            columns=columns,
            primary_key=pk,
            source_file=file_path,
            description=description
        )

        return table

    def _detect_component(self, name: str, file_path: str, line_num: int) -> str:
        """Detect component from table name prefix."""
        for prefix, info in COMPONENT_PREFIXES.items():
            if name.startswith(prefix):
                return info['name']

        # Unknown prefix - add warning
        self._add_warning(file_path, line_num, 'NAMING_010',
                          f"Unknown component prefix in table '{name}'",
                          entity_name=name)
        return 'unknown'

    def _validate_table_naming(self, name: str, file_path: str, line_num: int) -> None:
        """Validate table naming conventions."""
        # Check _tbl suffix
        if not name.endswith('_tbl'):
            self._add_warning(file_path, line_num, 'NAMING_001',
                              f"Table '{name}' missing '_tbl' suffix",
                              entity_name=name)

        # Check for component prefix
        has_prefix = any(name.startswith(p) for p in COMPONENT_PREFIXES)
        if not has_prefix:
            self._add_warning(file_path, line_num, 'NAMING_002',
                              f"Table '{name}' missing component prefix",
                              entity_name=name)

        # Check plural entity name (simple heuristic)
        entity_part = name
        for prefix in COMPONENT_PREFIXES:
            if name.startswith(prefix):
                entity_part = name[len(prefix):]
                break
        if entity_part.endswith('_tbl'):
            entity_part = entity_part[:-4]

        # Common singular patterns that should be plural
        singular_patterns = ['_info', '_stat']
        is_likely_singular = (
            not entity_part.endswith('s') and
            not any(entity_part.endswith(p) for p in singular_patterns) and
            not entity_part.endswith('data')
        )
        # Skip this check for now - too many false positives

    def _parse_columns(self, body: str) -> list:
        """Parse column definitions from table body."""
        columns = []

        # Split by comma, but be careful with nested parentheses
        # Simple approach: extract lines that look like column definitions
        lines = body.split('\n')

        for line in lines:
            line = line.strip().rstrip(',')
            if not line:
                continue

            # Skip constraints
            if any(line.upper().startswith(kw) for kw in
                   ['PRIMARY KEY', 'EXCLUDE', 'CHECK', 'FOREIGN KEY', 'CONSTRAINT', 'UNIQUE']):
                continue

            # Try to parse as column - handle multi-word types like "timestamp with time zone"
            # Pattern: "column_name" type_with_possible_modifiers [NOT NULL] [DEFAULT ...]
            col_match = re.match(
                r'"?(\w+)"?\s+'  # Column name
                r'([\w\s]+?(?:\s*\([^)]*\))?(?:\[\])?)'  # Type (including multi-word)
                r'(?:\s+(?:NOT\s+NULL|NULL|DEFAULT|PRIMARY|REFERENCES|CHECK).*)?$',  # Optional modifiers
                line,
                re.IGNORECASE
            )
            if col_match:
                col_name = col_match.group(1)
                col_type = col_match.group(2).strip().lower()

                # Simplify common types for diagram readability
                type_simplifications = {
                    'timestamp with time zone': 'timestamptz',
                    'character varying': 'varchar',
                    'double precision': 'double',
                }
                for long_type, short_type in type_simplifications.items():
                    if col_type == long_type:
                        col_type = short_type
                        break

                nullable = 'not null' not in line.lower()
                default = None
                default_match = re.search(r'default\s+([^,\n]+)', line, re.IGNORECASE)
                if default_match:
                    default = default_match.group(1).strip()

                columns.append(Column(
                    name=col_name,
                    type=col_type,
                    nullable=nullable,
                    default=default
                ))

        return columns

    def _detect_classification(self, columns: list, table_name: str) -> str:
        """Detect table classification based on columns."""
        col_names = {c.name for c in columns}

        has_temporal = 'valid_from' in col_names and 'valid_to' in col_names
        has_dataset_id = 'dataset_id' in col_names

        # Artefact tables
        if '_artefact_' in table_name or table_name.endswith('_artefact_tbl'):
            return 'artefact'

        # Junction tables: have temporal + multiple _id columns + no 'id' column
        # These are many-to-many relationship tables
        if has_temporal:
            fk_like = sum(1 for c in columns if c.name.endswith('_id'))
            has_single_id = 'id' in col_names
            # Junction: multiple FK columns, no single 'id', relatively few other columns
            if fk_like >= 2 and not has_single_id:
                # Count non-FK, non-temporal columns
                other_cols = sum(1 for c in columns
                                 if not c.name.endswith('_id')
                                 and c.name not in ('valid_from', 'valid_to', 'version'))
                # Typical junction: assigned_by, assigned_at, change_reason_code, change_commentary
                if other_cols <= 5:
                    return 'junction'

        # Temporal tables
        if has_temporal:
            return 'temporal'

        # Non-temporal with dataset_id is artefact-like
        if has_dataset_id:
            return 'artefact'

        return 'current_state'

    def _extract_primary_key(self, body: str, columns: list) -> dict:
        """Extract primary key definition."""
        pk_match = re.search(r'primary\s+key\s*\(([^)]+)\)', body, re.IGNORECASE)
        if pk_match:
            pk_col_names = [c.strip().strip('"') for c in pk_match.group(1).split(',')]
            # Mark columns as PK and build PK column list with types
            pk_columns = []
            for col in columns:
                if col.name in pk_col_names:
                    col.is_pk = True
                    pk_columns.append({'name': col.name, 'type': col.type})
            # Sort by order in PK definition
            pk_columns.sort(key=lambda c: pk_col_names.index(c['name']))
            return {
                'columns': pk_columns,
                'composite': len(pk_columns) > 1
            }
        return {'columns': [], 'composite': False}

    def _validate_temporal_table(self, name: str, columns: list, body: str,
                                  file_path: str, line_num: int) -> None:
        """Validate temporal table requirements."""
        col_names = {c.name for c in columns}

        # Check required temporal columns
        if 'valid_from' not in col_names:
            self._add_warning(file_path, line_num, 'TEMPORAL_001',
                              f"Temporal table '{name}' missing 'valid_from' column",
                              entity_name=name)

        if 'valid_to' not in col_names:
            self._add_warning(file_path, line_num, 'TEMPORAL_002',
                              f"Temporal table '{name}' missing 'valid_to' column",
                              entity_name=name)

        # Check EXCLUDE constraint
        if 'exclude using gist' not in body.lower():
            self._add_warning(file_path, line_num, 'TEMPORAL_003',
                              f"Temporal table '{name}' missing EXCLUDE USING gist constraint",
                              entity_name=name)

        # Check version column
        if 'version' not in col_names:
            self._add_warning(file_path, line_num, 'TEMPORAL_004',
                              f"Temporal table '{name}' missing 'version' column",
                              entity_name=name)

        # Check modified_by column (required on all temporal tables)
        if 'modified_by' not in col_names:
            self._add_warning(file_path, line_num, 'TEMPORAL_005',
                              f"Temporal table '{name}' missing 'modified_by' column",
                              entity_name=name)

        # Check change_reason_code (skip for junction tables and permissions)
        if 'junction' not in name and 'permission' not in name:
            if 'change_reason_code' not in col_names:
                self._add_warning(file_path, line_num, 'TEMPORAL_006',
                                  f"Temporal table '{name}' missing 'change_reason_code' column",
                                  entity_name=name)

    def _validate_tenant_aware_table(self, name: str, columns: list, body: str,
                                      pk: dict, file_path: str, line_num: int) -> None:
        """Validate tenant-aware table requirements.

        Multi-tenant tables must have:
        - tenant_id column
        - tenant_id in primary key
        - tenant_id in EXCLUDE constraint (for temporal tables)
        """
        col_names = {c.name for c in columns}

        # Check tenant_id column exists
        if 'tenant_id' not in col_names:
            self._add_warning(file_path, line_num, 'TENANT_001',
                              f"Table '{name}' missing 'tenant_id' column for multi-tenancy",
                              entity_name=name)
            return  # Can't check other tenant requirements without tenant_id

        # Check tenant_id is in primary key
        pk_cols = {c['name'] for c in pk.get('columns', [])}
        if 'tenant_id' not in pk_cols:
            self._add_warning(file_path, line_num, 'TENANT_002',
                              f"Table '{name}' missing 'tenant_id' in primary key",
                              entity_name=name)

        # Check tenant_id is in EXCLUDE constraint (for temporal tables with EXCLUDE)
        if 'exclude using gist' in body.lower():
            if 'tenant_id with =' not in body.lower():
                self._add_warning(file_path, line_num, 'TENANT_003',
                                  f"Table '{name}' missing 'tenant_id WITH =' in EXCLUDE constraint",
                                  entity_name=name)

    def _extract_functions(self, content: str, lines: list, file_path: str) -> None:
        """Extract function definitions from SQL content."""
        # Match functions in metadata, production, or public schemas
        func_pattern = re.compile(
            rf'create\s+or\s+replace\s+function\s+{SCHEMA_PATTERN}\.(\w+)',
            re.IGNORECASE
        )

        for match in func_pattern.finditer(content):
            func_name = match.group(1)
            start_pos = match.start()
            line_num = content[:start_pos].count('\n') + 1

            self.functions[func_name] = Function(
                name=func_name,
                source_file=file_path,
                line=line_num
            )

    def _extract_views(self, content: str, lines: list, file_path: str) -> None:
        """Extract view definitions from SQL content.

        Handles both regular views and materialized views (including those
        created with dynamic SQL inside DO $$ blocks).
        """
        # Pattern for regular views: CREATE [OR REPLACE] VIEW schema.name
        view_pattern = re.compile(
            rf'create\s+(?:or\s+replace\s+)?(?:materialized\s+)?view\s+'
            rf'(?:if\s+not\s+exists\s+)?'
            rf'"?{SCHEMA_PATTERN}"?\."?(\w+)"?',
            re.IGNORECASE
        )

        for match in view_pattern.finditer(content):
            view_name = match.group(1)
            start_pos = match.start()
            line_num = content[:start_pos].count('\n') + 1

            self.views[view_name] = View(
                name=view_name,
                source_file=file_path,
                line=line_num
            )

    def apply_column_markings(self) -> None:
        """Apply unique and FK markings to columns after all tables are parsed."""
        for table_name, table in self.tables.items():
            # Get unique columns for this table
            unique_cols = self.unique_columns.get(table_name, set())

            # Determine which column names could be the entity's own ID
            # e.g., for assets_images_tbl -> image_id, images_id
            entity_own_ids = set()
            entity_singular = table.entity.rstrip('s')
            entity_own_ids.add(entity_singular + '_id')  # e.g., image_id
            entity_own_ids.add(table.entity + '_id')      # e.g., images_id
            entity_own_ids.add('id')                      # generic id

            for col in table.columns:
                # Mark unique columns
                if col.name in unique_cols:
                    col.is_unique = True

                # Mark FK columns based on naming conventions
                # Skip the entity's own ID columns
                if col.name in entity_own_ids:
                    continue

                if col.name in FK_COLUMNS:
                    # Special case: country_code is only FK in specific tables
                    if col.name == 'country_code' and 'session' in table_name:
                        continue  # Not a FK in sessions
                    col.is_fk = True
                # Also mark *_id columns as FK
                elif col.name.endswith('_id'):
                    col.is_fk = True

    def validate_drop_completeness(self) -> None:
        """Check that all created objects have corresponding drop statements."""
        # Check tables
        for table_name, table in self.tables.items():
            if table_name not in self.drop_tables:
                self._add_warning(table.source_file, 0, 'DROP_001',
                                  f"Table '{table_name}' created but not found in drop scripts",
                                  entity_name=table_name)

        # Check functions
        for func_name, func in self.functions.items():
            if func_name not in self.drop_functions:
                self._add_warning(func.source_file, func.line, 'DROP_002',
                                  f"Function '{func_name}' created but not found in drop scripts",
                                  entity_name=func_name)

        # Check views
        for view_name, view in self.views.items():
            if view_name not in self.drop_views:
                self._add_warning(view.source_file, view.line, 'DROP_003',
                                  f"View '{view_name}' created but not found in drop scripts",
                                  entity_name=view_name)

    def validate_function_naming(self) -> None:
        """Check that functions follow the naming convention.

        Valid patterns:
        - {component}_{entities}_{action}_fn (e.g., dq_catalogs_upsert_fn)
        - {component}_{entity}_fn (e.g., utility_infinity_timestamp_fn)

        Valid actions: insert, notify, upsert, assign, validate, lookup, populate
        """
        # Functions that don't need to follow component prefix pattern
        exempt_prefixes = ['ores_utility_', 'ores_seed_', 'ores_refdata_validate_']

        # Known valid function name patterns (regex)
        valid_action_suffixes = [
            r'_insert_fn$',
            r'_notify_fn$',
            r'_upsert_fn$',
            r'_assign_fn$',
            r'_validate_fn$',
            r'_lookup_fn$',
            r'_populate_fn$',
            r'_fn$',  # Generic function suffix
        ]

        for func_name, func in self.functions.items():
            # Skip functions with exempt prefixes
            if any(func_name.startswith(prefix) for prefix in exempt_prefixes):
                continue

            # Check that function ends with _fn
            if not func_name.endswith('_fn'):
                self._add_warning(func.source_file, func.line, 'NAME_001',
                                  f"Function '{func_name}' should end with '_fn' suffix",
                                  entity_name=func_name)
                continue

            # Check that function has a recognized component prefix
            has_component = any(func_name.startswith(prefix)
                                for prefix in COMPONENT_PREFIXES.keys())
            if not has_component:
                self._add_warning(func.source_file, func.line, 'NAME_002',
                                  f"Function '{func_name}' should start with a component prefix "
                                  f"(e.g., dq_, iam_, refdata_)",
                                  entity_name=func_name)

    def validate_view_naming(self) -> None:
        """Check that views follow the naming convention.

        Valid pattern: {component}_{entities}_{qualifier}_vw
        All views must end with '_vw' suffix.
        """
        for view_name, view in self.views.items():
            # Check that view ends with _vw
            if not view_name.endswith('_vw'):
                self._add_warning(view.source_file, view.line, 'NAME_003',
                                  f"View '{view_name}' should end with '_vw' suffix",
                                  entity_name=view_name)
                continue

            # Check that view has a recognized component prefix
            has_component = any(view_name.startswith(prefix)
                                for prefix in COMPONENT_PREFIXES.keys())
            if not has_component:
                self._add_warning(view.source_file, view.line, 'NAME_004',
                                  f"View '{view_name}' should start with a component prefix "
                                  f"(e.g., dq_, iam_, telemetry_)",
                                  entity_name=view_name)

    def _add_warning(self, file_path: str, line: int, code: str, message: str,
                     entity_name: str = '') -> None:
        """Add a warning if warnings are enabled and not ignored."""
        if not self.warn:
            return

        # Check if this warning should be ignored
        if entity_name and self._is_warning_ignored(code, entity_name):
            return

        warning = Warning(file=file_path, line=line, code=code, message=message)
        self.warnings.append(warning)
        print(str(warning), file=sys.stderr)

    def detect_relationships(self) -> None:
        """Detect relationships between tables based on FK columns.

        Infers relationships from column naming conventions:
        - Columns in FK_TARGET_TABLES map to specific tables
        - Junction tables have relationships to both parent tables
        - Artefact tables have dataset_id -> dq_datasets_tbl relationships
        """
        for table_name, table in self.tables.items():
            for col in table.columns:
                # Check if column maps to a known target table
                target_table = FK_TARGET_TABLES.get(col.name)

                if target_table and target_table in self.tables:
                    # Skip self-references (except for specific FK columns like derivation)
                    if target_table == table_name and col.name != 'upstream_derivation_id':
                        continue

                    # Determine cardinality and label based on context
                    cardinality = '||--o{'  # Default: one-to-many

                    if table.classification == 'junction':
                        label = self._infer_relationship_label(target_table, table_name, col.name)
                    elif col.name == 'account_id' and 'login_info' in table_name:
                        # Special case: one-to-one for login_info
                        cardinality = '||--o|'
                        label = 'has'
                    elif 'artefact' in table_name and col.name == 'dataset_id':
                        # Artefact tables: dataset contains artefacts
                        label = 'contains'
                    else:
                        label = self._infer_relationship_label(target_table, table_name, col.name)

                    # Create relationship (from parent to child)
                    rel = Relationship(
                        from_table=target_table,
                        to_table=table_name,
                        cardinality=cardinality,
                        label=label
                    )

                    # Avoid duplicates
                    if not any(r.from_table == rel.from_table and r.to_table == rel.to_table
                               for r in self.relationships):
                        self.relationships.append(rel)

    def _infer_relationship_label(self, from_table: str, to_table: str, col_name: str = '') -> str:
        """Infer a meaningful label for the relationship."""
        # Change reason relationships - audit trail
        if col_name == 'change_reason_code':
            return 'reason for'

        # Category relationships
        if col_name == 'category_code' and 'change_reason_categor' in from_table:
            return 'contains'

        # Session-related
        if 'session' in to_table and 'account' in from_table:
            return 'creates'

        # Account roles (junction)
        if 'account_role' in to_table:
            if 'account' in from_table:
                return 'assigned'
            elif 'role' in from_table:
                return 'granted to'

        # Role permissions (junction)
        if 'role_permission' in to_table:
            if 'permission' in from_table:
                return 'included in'
            elif 'role' in from_table:
                return 'has'

        # Image tags (junction)
        if 'image_tag' in to_table:
            if 'image' in from_table:
                return 'tagged with'
            elif 'tag' in from_table:
                return 'applied to'

        # Telemetry logs
        if 'telemetry_log' in to_table:
            return 'generates'

        # Coding scheme relationships
        if 'coding_scheme' in from_table and 'authority' not in from_table:
            if 'dataset' in to_table:
                return 'identifies'
            else:
                return 'identifies'

        # Authority type classifies coding schemes
        if 'authority_type' in from_table:
            return 'classifies'

        # Catalog contains datasets
        if 'catalog' in from_table and 'dataset' in to_table:
            return 'groups'

        # Domain contains subject areas, groups datasets
        if 'data_domain' in from_table:
            if 'subject_area' in to_table:
                return 'contains'
            elif 'dataset' in to_table or 'coding_scheme' in to_table:
                return 'groups'

        # Subject area relationships
        if 'subject_area' in from_table:
            if 'coding_scheme' in to_table:
                return 'defines'
            elif 'dataset' in to_table:
                return 'classifies'

        # Methodology applied to datasets
        if 'methodolog' in from_table:
            return 'applied to'

        # Dimension relationships (tables named with plural: dimensions)
        if 'dimension' in from_table:
            if 'nature' in from_table:
                return 'describes nature'
            elif 'origin' in from_table:
                return 'describes origin'
            elif 'treatment' in from_table:
                return 'describes treatment'

        # Dataset dependencies
        if 'dataset' in from_table and 'dependenc' in to_table:
            return 'depends on'

        # Dataset publications
        if 'dataset' in from_table and 'publication' in to_table:
            return 'published as'

        # Dataset self-reference (derivation)
        if col_name == 'upstream_derivation_id':
            return 'derives from'

        # Default
        return 'has'

    def _get_dq_subpackage(self, table_name: str) -> tuple:
        """Determine the DQ sub-package for a table based on naming patterns.

        Returns (package_name, description, color, order) tuple.
        """
        # Artefact/staging tables - import tables for data ingestion
        if table_name.endswith('_artefact_tbl'):
            return ('dq_staging', 'Data Staging', '#F3E5F5', 4.1)

        # Dataset management tables
        dataset_tables = [
            'datasets_tbl', 'dataset_bundles_tbl', 'dataset_bundle_members_tbl',
            'dataset_dependencies_tbl', 'dataset_publications_tbl', 'bundle_publications_tbl'
        ]
        if any(table_name.endswith(t) for t in dataset_tables):
            return ('dq_datasets', 'Dataset Management', '#E3F2FD', 4.2)

        # Methodology and dimension tables
        methodology_tables = [
            'methodologies_tbl', 'nature_dimensions_tbl', 'origin_dimensions_tbl',
            'treatment_dimensions_tbl'
        ]
        if any(table_name.endswith(t) for t in methodology_tables):
            return ('dq_methodology', 'Methodology', '#E8F5E9', 4.3)

        # Core metadata tables (default for remaining dq tables)
        return ('dq_metadata', 'Data Catalog', '#E1F5FE', 4.4)

    def get_model(self) -> dict:
        """Generate the JSON model."""
        # Apply column markings (unique, FK) before generating model
        self.apply_column_markings()

        # Detect relationships between tables
        self.detect_relationships()

        # Group tables by component, with special handling for dq sub-packages
        packages = {}
        for table in self.tables.values():
            comp = table.component

            # Split dq package into sub-packages for better diagram layout
            if comp == 'dq':
                pkg_name, pkg_desc, pkg_color, pkg_order = self._get_dq_subpackage(table.name)
                if pkg_name not in packages:
                    packages[pkg_name] = {
                        'name': pkg_name,
                        'description': pkg_desc,
                        'color': pkg_color,
                        'order': pkg_order,
                        'tables': []
                    }
                comp = pkg_name
            elif comp not in packages:
                comp_info = next(
                    (info for prefix, info in COMPONENT_PREFIXES.items()
                     if info['name'] == comp),
                    {'name': comp, 'description': comp.title(), 'color': '#FFFFFF', 'order': 99}
                )
                packages[comp] = {
                    'name': comp,
                    'description': comp_info['description'],
                    'color': comp_info['color'],
                    'order': comp_info.get('order', 99),
                    'tables': []
                }

            # Order columns for display (handles is_pk for temporal columns)
            ordered_columns = self._order_columns_for_display(table.columns, table.classification)

            # Convert table to dict
            table_dict = {
                'name': table.name,
                'component': table.component,
                'entity': table.entity,
                'classification': table.classification,
                'stereotype': table.stereotype,
                'color': table.color,
                'primary_key': self._build_pk_for_model(table.primary_key, table.columns, table.classification),
                'columns': [asdict(c) for c in ordered_columns],
                'indexes': [asdict(i) for i in table.indexes],
                'source_file': table.source_file,
                'description': self._format_description(table.description)
            }
            packages[comp]['tables'].append(table_dict)

        # Sort packages by order (not alphabetically), then tables alphabetically
        sorted_packages = sorted(packages.values(), key=lambda p: p['order'])
        for pkg in sorted_packages:
            pkg['tables'] = sorted(pkg['tables'], key=lambda t: t['name'])

        return {
            'generated_at': datetime.now(timezone.utc).isoformat().replace('+00:00', 'Z'),
            'packages': sorted_packages,
            'relationships': [asdict(r) for r in self.relationships],
            'warnings': [asdict(w) for w in self.warnings]
        }

    def _order_columns_for_display(self, columns: list, classification: str) -> list:
        """Order columns for diagram display and set is_pk for display purposes.

        Order: non-PK columns first, then temporal at end.
        PK columns (except temporal for junction) are shown in PK section, not here.
        """
        temporal_cols = {'valid_from', 'valid_to'}

        # Create copies to avoid modifying originals
        result = []
        for col in columns:
            col_copy = Column(
                name=col.name,
                type=col.type,
                nullable=col.nullable,
                is_pk=col.is_pk,
                is_fk=col.is_fk,
                is_unique=col.is_unique,
                default=col.default,
                references=col.references
            )

            # For display: mark temporal columns as non-PK so they appear in columns section
            # Exception: for junction tables, valid_from stays as PK (shown in PK section)
            if col.name in temporal_cols:
                if classification == 'junction' and col.name == 'valid_from':
                    # valid_from stays as PK for junction tables (shown in PK section)
                    pass
                else:
                    # valid_to always shown in columns, valid_from for non-junction too
                    col_copy.is_pk = False

            result.append(col_copy)

        # Order: non-PK first, then temporal (valid_from, valid_to)
        non_pk_non_temporal = [c for c in result if not c.is_pk and c.name not in temporal_cols]
        temporal = [c for c in result if c.name in temporal_cols and not c.is_pk]

        return non_pk_non_temporal + temporal

    def _format_description(self, description: str) -> str:
        """Format description for PlantUML note rendering.

        Each line needs proper indentation for the note block.
        """
        if not description:
            return ''

        lines = description.split('\n')
        # First line doesn't need extra indent (it follows 'note right of')
        # Subsequent lines need 4 spaces of indent
        formatted_lines = [lines[0]]
        for line in lines[1:]:
            formatted_lines.append('    ' + line)
        return '\n'.join(formatted_lines)

    def _build_pk_for_model(self, primary_key: dict, columns: list, classification: str) -> dict:
        """Build primary key for model.

        For junction tables: include valid_from in PK display.
        For other tables: exclude temporal columns from PK display.
        """
        pk_columns = primary_key.get('columns', [])

        if classification == 'junction':
            # For junction: show all PK columns except valid_to
            display_pk_cols = [c.copy() for c in pk_columns if c['name'] != 'valid_to']
        else:
            # For other tables: exclude both valid_from and valid_to
            temporal_cols = {'valid_from', 'valid_to'}
            display_pk_cols = [c.copy() for c in pk_columns if c['name'] not in temporal_cols]

        # Check if any columns need <<PK, FK>> marker
        for col in columns:
            if col.is_pk and col.is_fk:
                for pk_col in display_pk_cols:
                    if pk_col['name'] == col.name:
                        pk_col['is_fk'] = True

        return {
            'columns': display_pk_cols,
            'composite': len(pk_columns) > 1
        }


def main():
    parser = argparse.ArgumentParser(
        description='Parse SQL schema and generate PlantUML ER model'
    )
    parser.add_argument('--create-dir', required=True,
                        help='Directory containing CREATE SQL files')
    parser.add_argument('--drop-dir', required=True,
                        help='Directory containing DROP SQL files')
    parser.add_argument('--output', '-o',
                        help='Output JSON model file (omit for --validate-only)')
    parser.add_argument('--ignore-file',
                        help='File containing validation warnings to ignore')
    parser.add_argument('--warn', action='store_true', default=True,
                        help='Print warnings to stderr (default: true)')
    parser.add_argument('--no-warn', action='store_true',
                        help='Suppress warnings')
    parser.add_argument('--strict', action='store_true',
                        help='Exit with code 1 if any warnings')
    parser.add_argument('--validate-only', action='store_true',
                        help='Only validate, do not generate model')

    args = parser.parse_args()

    warn = args.warn and not args.no_warn
    ignore_file = Path(args.ignore_file) if args.ignore_file else None

    create_dir = Path(args.create_dir)
    drop_dir = Path(args.drop_dir)

    if not create_dir.exists():
        print(f"Error: Create directory not found: {create_dir}", file=sys.stderr)
        sys.exit(1)

    if not drop_dir.exists():
        print(f"Error: Drop directory not found: {drop_dir}", file=sys.stderr)
        sys.exit(1)

    # Parse SQL files
    sql_parser = SQLParser(warn=warn, ignore_file=ignore_file)
    sql_parser.parse_create_dir(create_dir)
    sql_parser.parse_drop_dir(drop_dir)

    # Validate drop completeness
    sql_parser.validate_drop_completeness()

    # Validate function naming conventions
    sql_parser.validate_function_naming()

    # Validate view naming conventions
    sql_parser.validate_view_naming()

    # Print summary
    print(f"\n=== Validation Summary ===", file=sys.stderr)
    print(f"Tables parsed: {len(sql_parser.tables)}", file=sys.stderr)
    print(f"Functions parsed: {len(sql_parser.functions)}", file=sys.stderr)
    print(f"Views parsed: {len(sql_parser.views)}", file=sys.stderr)
    print(f"Warnings: {len(sql_parser.warnings)}", file=sys.stderr)

    # Generate model (unless validate-only)
    if not args.validate_only and args.output:
        model = sql_parser.get_model()
        output_path = Path(args.output)
        output_path.parent.mkdir(parents=True, exist_ok=True)
        with open(output_path, 'w') as f:
            json.dump(model, f, indent=2)
        print(f"Model written to: {output_path}", file=sys.stderr)

    # Exit with error if strict mode and warnings
    if args.strict and sql_parser.warnings:
        sys.exit(1)


if __name__ == '__main__':
    main()
