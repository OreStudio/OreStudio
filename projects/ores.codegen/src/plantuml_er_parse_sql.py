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
import os
import re
import sys
from dataclasses import dataclass, field, asdict
from datetime import datetime, timezone
from pathlib import Path
from typing import Optional


# Known component prefixes and their descriptions
# Order determines package display order in diagram
COMPONENT_PREFIXES = {
    'iam_': {'name': 'iam', 'description': 'Identity & Access Management', 'color': '#E8F4FD', 'order': 1},
    'assets_': {'name': 'assets', 'description': 'Digital Assets', 'color': '#F3E5F5', 'order': 2},
    'refdata_': {'name': 'refdata', 'description': 'Reference Data', 'color': '#FFF3E0', 'order': 3},
    'dq_': {'name': 'dq', 'description': 'Data Quality', 'color': '#E1F5FE', 'order': 4},
    'variability_': {'name': 'variability', 'description': 'Feature Flags', 'color': '#E8F5E9', 'order': 5},
    'telemetry_': {'name': 'telemetry', 'description': 'Telemetry & Logging', 'color': '#FCE4EC', 'order': 6},
    'geo_': {'name': 'geo', 'description': 'Geolocation', 'color': '#FFF9C4', 'order': 7},
    'utility_': {'name': 'utility', 'description': 'Utility Functions', 'color': '#ECEFF1', 'order': 8},
    'admin_': {'name': 'admin', 'description': 'Administration', 'color': '#E0E0E0', 'order': 9},
}

# Columns that are known FK references by name
FK_COLUMNS = {
    'change_reason_code',  # References change reason codes
    'coding_scheme_code',  # References coding schemes
    'account_id',          # References iam_accounts_tbl
    'role_id',             # References iam_roles_tbl
    'permission_id',       # References iam_permissions_tbl
    'image_id',            # References assets_images_tbl
    'tag_id',              # References assets_tags_tbl
    'dataset_id',          # References dq_datasets_tbl
    'country_code',        # References refdata_countries_tbl (in some contexts)
}

# Table classification colors and stereotypes
TABLE_STYLES = {
    'temporal': {'stereotype': '<<temporal>>', 'color': '#C6F0D8'},
    'artefact': {'stereotype': '', 'color': '#F7E5FF'},
    'junction': {'stereotype': '<<junction>>', 'color': '#ECECEC'},
    'current_state': {'stereotype': '', 'color': '#F7E5FF'},
}


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
class ParseResult:
    """Result of parsing SQL files."""
    tables: list = field(default_factory=list)
    functions: list = field(default_factory=list)
    warnings: list = field(default_factory=list)


class SQLParser:
    """Parses SQL CREATE statements and validates conventions."""

    def __init__(self, warn: bool = True):
        self.warn = warn
        self.warnings = []
        self.tables = {}
        self.functions = {}
        self.drop_tables = set()
        self.drop_functions = set()
        self.unique_columns = {}  # table_name -> set of column names
        self.table_descriptions = {}  # table_name -> description text

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

    def _parse_drop_file(self, file_path: Path) -> None:
        """Parse a single DROP SQL file to track what should be dropped."""
        content = file_path.read_text()

        # Track dropped tables
        for match in re.finditer(r'drop\s+table\s+if\s+exists\s+"?ores"?\."?(\w+)"?', content, re.IGNORECASE):
            self.drop_tables.add(match.group(1))

        # Track dropped functions
        for match in re.finditer(r'drop\s+function\s+if\s+exists\s+ores\.(\w+)', content, re.IGNORECASE):
            self.drop_functions.add(match.group(1))

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
            r'create\s+table\s+if\s+not\s+exists\s+"?ores"?\."?(\w+)"?',  # Table name
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
            r'create\s+table\s+if\s+not\s+exists\s+"?ores"?\."?(\w+)"?',  # Table name
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
        # Pattern for CREATE TABLE
        table_pattern = re.compile(
            r'create\s+table\s+if\s+not\s+exists\s+"?ores"?\."?(\w+)"?\s*\((.*?)\);',
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
        """Extract unique index definitions to identify unique columns."""
        # Pattern: create unique index ... on "ores"."table_name" (column_name)
        unique_idx_pattern = re.compile(
            r'create\s+unique\s+index\s+if\s+not\s+exists\s+\w+\s+'
            r'on\s+"?ores"?\."?(\w+)"?\s*\((\w+)\)',
            re.IGNORECASE
        )

        for match in unique_idx_pattern.finditer(content):
            table_name = match.group(1)
            column_name = match.group(2)

            if table_name not in self.unique_columns:
                self.unique_columns[table_name] = set()
            self.unique_columns[table_name].add(column_name)

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
                          f"Unknown component prefix in table '{name}'")
        return 'unknown'

    def _validate_table_naming(self, name: str, file_path: str, line_num: int) -> None:
        """Validate table naming conventions."""
        # Check _tbl suffix
        if not name.endswith('_tbl'):
            self._add_warning(file_path, line_num, 'NAMING_001',
                              f"Table '{name}' missing '_tbl' suffix")

        # Check for component prefix
        has_prefix = any(name.startswith(p) for p in COMPONENT_PREFIXES)
        if not has_prefix:
            self._add_warning(file_path, line_num, 'NAMING_002',
                              f"Table '{name}' missing component prefix")

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
                              f"Temporal table '{name}' missing 'valid_from' column")

        if 'valid_to' not in col_names:
            self._add_warning(file_path, line_num, 'TEMPORAL_002',
                              f"Temporal table '{name}' missing 'valid_to' column")

        # Check EXCLUDE constraint
        if 'exclude using gist' not in body.lower():
            self._add_warning(file_path, line_num, 'TEMPORAL_003',
                              f"Temporal table '{name}' missing EXCLUDE USING gist constraint")

        # Check version column
        if 'version' not in col_names:
            self._add_warning(file_path, line_num, 'TEMPORAL_004',
                              f"Temporal table '{name}' missing 'version' column")

        # Check audit columns (skip for junction tables and permissions)
        if 'junction' not in name and 'permission' not in name:
            if 'modified_by' not in col_names:
                self._add_warning(file_path, line_num, 'TEMPORAL_005',
                                  f"Temporal table '{name}' missing 'modified_by' column")

            if 'change_reason_code' not in col_names:
                self._add_warning(file_path, line_num, 'TEMPORAL_006',
                                  f"Temporal table '{name}' missing 'change_reason_code' column")

    def _extract_functions(self, content: str, lines: list, file_path: str) -> None:
        """Extract function definitions from SQL content."""
        func_pattern = re.compile(
            r'create\s+or\s+replace\s+function\s+ores\.(\w+)',
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
                                  f"Table '{table_name}' created but not found in drop scripts")

        # Check functions
        for func_name, func in self.functions.items():
            if func_name not in self.drop_functions:
                self._add_warning(func.source_file, func.line, 'DROP_002',
                                  f"Function '{func_name}' created but not found in drop scripts")

    def _add_warning(self, file_path: str, line: int, code: str, message: str) -> None:
        """Add a warning if warnings are enabled."""
        if self.warn:
            warning = Warning(file=file_path, line=line, code=code, message=message)
            self.warnings.append(warning)
            print(str(warning), file=sys.stderr)

    def get_model(self) -> dict:
        """Generate the JSON model."""
        # Apply column markings (unique, FK) before generating model
        self.apply_column_markings()

        # Group tables by component
        packages = {}
        for table in self.tables.values():
            comp = table.component
            if comp not in packages:
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

    create_dir = Path(args.create_dir)
    drop_dir = Path(args.drop_dir)

    if not create_dir.exists():
        print(f"Error: Create directory not found: {create_dir}", file=sys.stderr)
        sys.exit(1)

    if not drop_dir.exists():
        print(f"Error: Drop directory not found: {drop_dir}", file=sys.stderr)
        sys.exit(1)

    # Parse SQL files
    sql_parser = SQLParser(warn=warn)
    sql_parser.parse_create_dir(create_dir)
    sql_parser.parse_drop_dir(drop_dir)

    # Validate drop completeness
    sql_parser.validate_drop_completeness()

    # Print summary
    print(f"\n=== Validation Summary ===", file=sys.stderr)
    print(f"Tables parsed: {len(sql_parser.tables)}", file=sys.stderr)
    print(f"Functions parsed: {len(sql_parser.functions)}", file=sys.stderr)
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
