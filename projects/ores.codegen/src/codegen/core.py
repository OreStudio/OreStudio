"""
Simple code generator that loads data and applies templates.
"""
import json
import re
import os
import random
from pathlib import Path
import pystache
from datetime import datetime

_MODELINE_RE = re.compile(r"^\*{3}\s+\S+\.(\S+)\s+:modeline:\s*$")
_CODEC_VALUE_RE = re.compile(r"^:masd\.codec\.value:\s+(.+?)\s*$")
_FACET_HEADING_RE = re.compile(r"^\*\* (.+?) :(\w+):$")
_FACET_PROP_KEY_RE = re.compile(r"^:(\w[\w-]*):\s+(.*?)\s*$")
# Archetype cells in the facet catalogue are org-roam links to the archetype
# doc: [[id:UUID][template_name.mustache]]. Generation needs the bare template
# name (the display text); the link target wires the doc graph for the site.
_ORG_ID_LINK_RE = re.compile(r"\[\[id:[^\]]+\]\[(.+?)\]\]")


def _unlink_org(cell):
    """Return the display text of an org-roam id link, or the cell unchanged."""
    m = _ORG_ID_LINK_RE.search(cell)
    return m.group(1) if m else cell
_ORG_TYPE_RE = re.compile(r"^#\+type:\s*(\S+)\s*$", re.MULTILINE | re.IGNORECASE)

# Maps #+type: frontmatter values to model-type strings.
_ORG_TYPE_TO_MODEL_TYPE = {
    "ores.codegen.entity":           "domain_entity",
    "ores.codegen.table":            "table",
    "ores.codegen.junction":         "junction",
    "ores.codegen.component":        "component",
    "ores.codegen.field_group":      "field_group",
    "ores.codegen.lookup_entity":    "enum",
    "ores.codegen.service_registry": "service_registry",
    "ores.codegen.dataset":          "dataset",
}


def _read_org_type(model_path):
    """Return the model-type string for an org file by reading its #+type: header.

    Returns None if the file carries no #+type: header (suffix fallback applies).
    Raises ValueError if #+type: is present but not a recognised codegen type.
    Raises OSError if the file cannot be read.
    Only the first 4096 bytes are scanned (frontmatter is always at the top).
    """
    with open(model_path, encoding="utf-8", errors="replace") as fh:
        head = fh.read(4096)
    m = _ORG_TYPE_RE.search(head)
    if not m:
        return None
    raw = m.group(1)
    model_type = _ORG_TYPE_TO_MODEL_TYPE.get(raw)
    if model_type is None:
        raise ValueError(
            f"{model_path}: unrecognised #+type: {raw!r}. "
            f"Known types: {sorted(_ORG_TYPE_TO_MODEL_TYPE)}"
        )
    return model_type


def load_data(data_dir):
    """
    Load all data files from the data directory.

    Args:
        data_dir (str): Path to the data directory

    Returns:
        dict: Dictionary containing loaded data
    """
    data = {}

    # Load JSON files
    for json_file in Path(data_dir).glob("*.json"):
        with open(json_file, 'r', encoding='utf-8') as f:
            key = json_file.stem  # Use filename without extension as key
            data[key] = json.load(f)

    # Load text files
    for txt_file in Path(data_dir).glob("*.txt"):
        with open(txt_file, 'r', encoding='utf-8') as f:
            key = txt_file.stem  # Use filename without extension as key
            data[key] = f.read()

    # Load modelines from the org source (modeline.org is the sole source).
    modeline_org = Path(data_dir) / "modeline.org"
    if modeline_org.exists():
        data['modelines'] = _load_modelines_from_org(modeline_org)

    return data


def _load_modelines_from_org(path):
    """Parse a MASD-style modeline.org and return a {name: content} dict."""
    result = {}
    lines = Path(path).read_text(encoding="utf-8").splitlines()
    current_name = None
    for line in lines:
        m = _MODELINE_RE.match(line)
        if m:
            current_name = m.group(1)
            continue
        if current_name:
            cv = _CODEC_VALUE_RE.match(line.strip())
            if cv:
                result[current_name] = cv.group(1)
                current_name = None  # reset after first content attribute
    expected = {"sql", "c++", "cmake"}
    missing = expected - result.keys()
    if missing:
        raise ValueError(
            f"modeline.org: missing modeline(s): {sorted(missing)}"
        )
    return result


def format_comment_block(text, lang='sql'):
    """
    Format text as a comment block with language-specific syntax.

    Args:
        text (str): Text to format as comments
        lang (str): Programming language for comment syntax

    Returns:
        str: Formatted comment block
    """
    # Define comment formats for different languages
    comment_formats = {
        'sql': {'prefix': ' * ', 'suffix': '', 'start': '/*', 'end': ' */'},
        'c++': {'prefix': ' * ', 'suffix': '', 'start': '/*', 'end': ' */'},
        'cmake': {'prefix': '# ', 'suffix': '', 'start': '#', 'end': ''},
        'plantuml': {'prefix': "' ", 'suffix': '', 'start': "'", 'end': ''},
        'python': {'prefix': '# ', 'suffix': '', 'start': '"""', 'end': '"""'},
        'javascript': {'prefix': ' * ', 'suffix': '', 'start': '/**', 'end': ' */'},
    }

    # Get the format for the specified language
    fmt = comment_formats.get(lang, comment_formats['sql'])  # Default to SQL

    # Split the text into lines
    lines = text.split('\n')

    # Add the prefix to each line
    formatted_lines = []
    for line in lines:
        # Only add prefix if line is not empty
        if line.strip():
            formatted_line = f"{fmt['prefix']}{line}"
        else:
            # For empty lines within the comment block, just add the prefix with asterisk
            formatted_line = fmt['prefix'].rstrip()  # Just " * " becomes " *" for empty lines
        formatted_lines.append(formatted_line)

    # Combine everything with proper start and end markers
    result = f"{fmt['start']}\n" + '\n'.join(formatted_lines) + f"\n{fmt['end']}"

    return result


def generate_license_with_header(license_text, modeline_info, lang='sql'):
    """
    Generate a license comment block with modeline and copyright header.

    Args:
        license_text (str): Raw license text
        modeline_info (str): Modeline information
        lang (str): Language for comment formatting

    Returns:
        str: Complete license comment block with header
    """
    # Get current year
    current_year = datetime.now().year

    # Create the content lines without prefixes
    content_lines = [
        f" -*- {modeline_info} -*-",
        "",
        f"Copyright (C) {current_year} Marco Craveiro <marco.craveiro@gmail.com>",
        "",
    ]

    # Add the license text lines
    license_lines = license_text.split('\n')
    content_lines.extend(license_lines)

    # Define comment formats for different languages
    comment_formats = {
        'sql': {'prefix': ' * ', 'suffix': '', 'start': '/*', 'end': ' */'},
        'c++': {'prefix': ' * ', 'suffix': '', 'start': '/*', 'end': ' */'},
        'cmake': {'prefix': '# ', 'suffix': '', 'start': '#', 'end': ''},
        'plantuml': {'prefix': "' ", 'suffix': '', 'start': "'", 'end': ''},
        'python': {'prefix': '# ', 'suffix': '', 'start': '"""', 'end': '"""'},
        'javascript': {'prefix': ' * ', 'suffix': '', 'start': '/**', 'end': ' */'},
    }

    # Get the format for the specified language
    fmt = comment_formats.get(lang, comment_formats['sql'])  # Default to SQL

    # Add the prefix to each content line
    formatted_lines = []
    for i, line in enumerate(content_lines):
        if line.startswith(" -*-"):  # Special handling for modeline - skip processing
            continue
        elif line.strip():  # Non-empty line
            # Special handling: the copyright line should have the standard prefix like other content lines
            formatted_line = f"{fmt['prefix']}{line}"
        else:  # Empty line
            # Use prefix stripped of trailing space (e.g. ' * ' -> ' *', '# ' -> '#', "' " -> "'")
            formatted_line = fmt['prefix'].rstrip()
        formatted_lines.append(formatted_line)

    # Combine everything with proper start and end markers, including the modeline
    end_part = f"\n{fmt['end']}" if fmt['end'] else ""
    result = f"{fmt['start']} -*- {modeline_info} -*-\n" + '\n'.join(formatted_lines) + end_part

    return result


def render_template(template_path, data):
    """
    Render a mustache template with the provided data.

    Args:
        template_path (str): Path to the template file
        data (dict): Data to use for rendering

    Returns:
        str: Rendered template content
    """
    with open(template_path, 'r', encoding='utf-8') as f:
        template_content = f.read()

    # Add utility functions to the data context
    extended_data = data.copy()
    extended_data['generate_flag_svg'] = generate_flag_svg

    return pystache.render(template_content, extended_data)


def get_template_mappings():
    """
    Define the mapping between model filenames and their corresponding templates.

    Returns:
        dict: Mapping of model filenames to lists of template names
    """
    return {
        "model.json": ["sql_batch_execute.mustache"],
        "catalogs.json": ["sql_catalog_populate.mustache"],
        "country_currency.json": ["sql_flag_populate.mustache", "sql_currency_populate.mustache", "sql_country_populate.mustache"],
        "country_currency_flags.json": ["sql_flag_populate.mustache"],  # Keep for backward compatibility
        "datasets.json": ["sql_dataset_populate.mustache", "sql_dataset_dependency_populate.mustache"],
        "methodologies.json": ["sql_methodology_populate.mustache"],
        "tags.json": ["sql_tag_populate.mustache"]
    }


def get_schema_template_mappings():
    """
    Define the mapping for entity schema templates.

    Returns:
        list: List of tuples (template_name, output_suffix) for schema generation
    """
    return [
        ("sql_schema_table_create.mustache", "_create.sql"),
        ("sql_schema_notify_trigger.mustache", "_notify_trigger.sql"),
        ("sql_schema_artefact_create.mustache", "_artefact_create.sql"),
        ("sql_populate_function_refdata.mustache", "_population_functions.sql"),
    ]


# ---------------------------------------------------------------------------
# LEGACY: filename-suffix model classification.
#
# These is_*_model(model_filename) predicates infer a model's kind from its
# filename suffix (e.g. _table.org, _junction.org, _component.org). This is
# legacy: filenames must NOT carry type information — the source of truth is
# the document's #+type: frontmatter (see get_model_type / _read_org_type and
# the _ORG_TYPE_RE map in manifest.py). A filename-based scheme also produces
# false positives — an entity legitimately named e.g. gmm_component would be
# misread as a component model by a naive _component.org suffix match.
#
# get_model_type() consults #+type: first and only falls back to these
# predicates when no recognised type header is present (legacy/JSON models).
# New callers must classify via get_model_type(filename, path); these
# predicates are retained only for that fallback and should be removed once
# all models carry a #+type:.
# ---------------------------------------------------------------------------
def is_entity_schema_model(model_filename):
    """
    Check if a model file is an entity schema model.

    Two forms are accepted:
    - ``*_entity.json`` (legacy JSON model)
    - ``*_lookup_entity.org`` (literate org-mode model — bi-temporal
      lookup entities sharing the JSON-side ``entity`` root key)

    Args:
        model_filename (str): The model filename

    Returns:
        bool: True if this is an entity schema model
    """
    return (
        model_filename.endswith("_entity.json")
        or model_filename.endswith("_lookup_entity.org")
    )


def is_domain_entity_model(model_filename):
    """
    Check if a model file is a domain entity model.

    Three forms are accepted:
    - ``*_domain_entity.json`` (legacy JSON model, C++ only)
    - ``*_entity.org`` (POC literate org model, unified C++ + SQL)
    - ``ores.<component>.<entity>.org`` (current literate org model,
      co-located under projects/ores.<component>/modeling/)

    Other org-mode model kinds use a discriminating suffix
    (``_field_group.org``, etc.); those are excluded from the generic
    ``ores.<component>.<entity>.org`` match so they route to their own
    predicates instead.

    Args:
        model_filename (str): The model filename

    Returns:
        bool: True if this is a domain entity model
    """
    basename = os.path.basename(model_filename)
    _other_org_kinds = (
        "_field_group.org", "_junction.org", "_table.org",
        "_lookup_entity.org", "service_registry.org",
    )
    if model_filename.endswith("_domain_entity.json"):
        return True
    # Exclusions checked before the generic _entity.org / ores.*.org match,
    # so kinds whose suffix overlaps with _entity.org (e.g. _lookup_entity.org)
    # route to their own predicate.
    if any(model_filename.endswith(s) for s in _other_org_kinds):
        return False
    if model_filename.endswith("_entity.org"):
        return True
    if basename.startswith("ores.") and basename.endswith(".org"):
        return True
    return False


def is_junction_model(model_filename):
    """
    Check if a model file is a junction table model.

    Args:
        model_filename (str): The model filename

    Returns:
        bool: True if this is a junction table model
    """
    return (
        model_filename.endswith("_junction.json")
        or model_filename.endswith("_junction.org")
    )


def is_enum_model(model_filename):
    """
    Check if a model file is an enum model.

    Args:
        model_filename (str): The model filename

    Returns:
        bool: True if this is an enum model
    """
    return model_filename.endswith("_enum.json")


def is_component_model(model_filename):
    """
    Check if a model file is a component scaffold model.

    Args:
        model_filename (str): The model filename

    Returns:
        bool: True if this is a component model
    """
    return (
        model_filename.endswith("_component.json")
        or model_filename.endswith("component_overview.org")
    )


def is_service_registry_model(model_filename):
    """
    Check if a model file is a service registry model.

    Args:
        model_filename (str): The model filename

    Returns:
        bool: True if this is a service registry model
    """
    return (
        model_filename.endswith("_service_registry.json")
        or model_filename.endswith("service_registry.org")
    )


def is_field_group_model(model_filename):
    """
    Check if a model file is a field-group model.

    Field-group models describe plain C++ structs that group related
    fields from a parent entity to reduce per-struct field count (e.g.
    for rfl::Flatten<T> composition to avoid MSVC C1202).  They have no
    primary key, no audit columns, no DB table, and no repository layer.

    Args:
        model_filename (str): The model filename

    Returns:
        bool: True if this is a field-group model
    """
    return (
        model_filename.endswith("_field_group.json")
        or model_filename.endswith("_field_group.org")
    )


def is_table_model(model_filename):
    """
    Check if a model file is a unified table model.

    Table models use the unified 'table' root key and are rendered by
    sql_schema_create.mustache via the 'sql' profile.  They replace the
    older '_entity.json' / sql_schema_table_create.mustache pipeline.

    Args:
        model_filename (str): The model filename

    Returns:
        bool: True if this is a table model
    """
    return (
        model_filename.endswith("_table.json")
        or model_filename.endswith("_table.org")
    )


def get_model_type(model_filename, model_path=None):
    """
    Determine the model type for a model file.

    For .org files the #+type: frontmatter key is checked first (via
    _read_org_type); the filename-suffix fallback is used when no recognised
    #+type: is present, keeping backward compatibility with legacy filenames.

    Args:
        model_filename (str): The model filename (basename).
        model_path (str or Path, optional): Full path to the file; required for
            frontmatter detection on .org files.  When omitted, only filename
            suffix detection is used.

    Returns:
        str: The model type ('domain_entity', 'junction', 'enum', 'schema',
             'table', 'component', 'field_group', 'service_registry', or 'unknown')

    Raises:
        ValueError: if the file contains a #+type: header with an unrecognised value.
        OSError: if model_path is given but cannot be read.
    """
    if model_path is not None and str(model_filename).endswith('.org'):
        org_type = _read_org_type(model_path)
        if org_type is not None:
            return org_type

    if is_domain_entity_model(model_filename):
        return 'domain_entity'
    elif is_junction_model(model_filename):
        return 'junction'
    elif is_field_group_model(model_filename):
        return 'field_group'
    elif is_table_model(model_filename):
        return 'table'
    elif is_service_registry_model(model_filename):
        return 'service_registry'
    elif is_component_model(model_filename):
        return 'component'
    elif is_enum_model(model_filename):
        return 'enum'
    elif is_entity_schema_model(model_filename):
        return 'schema'
    return 'unknown'


def _parse_facet_table(lines, pos):
    """Parse an org-mode table starting at lines[pos]. Returns (rows, next_pos)."""
    rows = []
    while pos < len(lines):
        line = lines[pos].strip()
        if not line.startswith("|"):
            break
        if re.match(r"^\|[-+|]+\|?$", line):
            pos += 1
            continue
        cells = [c.strip() for c in line.strip("|").split("|")]
        rows.append(cells)
        pos += 1
    return rows, pos


def _load_profiles_from_org(path):
    """Parse facet_catalogue.org and return the profiles dict."""
    lines = Path(path).read_text(encoding="utf-8").splitlines()
    profiles = {}
    i = 0
    n = len(lines)
    while i < n:
        m = _FACET_HEADING_RE.match(lines[i])
        if not m:
            i += 1
            continue
        name = m.group(1)
        i += 1
        props = {}
        if i < n and lines[i].strip() == ":PROPERTIES:":
            i += 1
            while i < n and lines[i].strip() != ":END:":
                pm = _FACET_PROP_KEY_RE.match(lines[i].strip())
                if pm:
                    props[pm.group(1)] = pm.group(2)
                i += 1
            i += 1
        if "description" not in props or "model_types" not in props:
            print(f"Warning: profile '{name}' missing required property "
                  f"(description/model_types) — skipped", file=__import__('sys').stderr)
            continue
        profile = {
            "description": props["description"],
            "model_types": props["model_types"].split(),
        }
        if "includes" in props:
            profile["includes"] = props["includes"].split()
        else:
            j = i
            while j < n and not lines[j].strip():
                j += 1
            if j < n and lines[j].strip().startswith("|"):
                rows, _ = _parse_facet_table(lines, j)
                if rows:
                    headers = [h.lower() for h in rows[0]]
                    has_mt_col = "model types" in headers
                    templates = []
                    for row in rows[1:]:
                        entry = {"template": _unlink_org(row[0]), "output": row[1]}
                        if has_mt_col and len(row) > 2 and row[2]:
                            entry["model_types"] = row[2].split()
                        templates.append(entry)
                    profile["templates"] = templates
        profiles[name] = profile
    return profiles


def load_profiles(base_dir):
    """Load profile definitions from facet_catalogue.org."""
    catalogue = Path(base_dir) / "library" / "facet_catalogue.org"
    if not catalogue.exists():
        return {}
    return _load_profiles_from_org(catalogue)


def snake_to_pascal(snake_str):
    """
    Convert a snake_case string to PascalCase.

    Args:
        snake_str (str): Snake case string (e.g., "dataset_bundle")

    Returns:
        str: PascalCase string (e.g., "DatasetBundle")
    """
    return ''.join(word.capitalize() for word in snake_str.split('_'))


def resolve_output_path(output_pattern, model_data, model_type):
    """
    Resolve placeholders in an output path pattern.

    Args:
        output_pattern (str): Output path pattern with placeholders
        model_data (dict): The loaded model data
        model_type (str): The model type ('domain_entity', 'junction', 'enum', etc.)

    Returns:
        str: Resolved output path
    """
    result = output_pattern

    # Extract values based on model type
    if model_type == 'domain_entity' and 'domain_entity' in model_data:
        entity = model_data['domain_entity']
        component = entity.get('component', 'unknown')
        subcomponent = entity.get('subcomponent', '')
        if subcomponent:
            component_include = f"{component}.{subcomponent}"
            component_dir = f"ores.{component}/{subcomponent}"
            component_core = f"{component}.core"
            component_core_dir = f"ores.{component}/core"
        else:
            component_include = entity.get('component_include', component)
            component_dir = f"ores.{component}"
            component_core = entity.get('component_core', component)
            component_core_dir = f"ores.{component}"
        entity_singular = entity.get('entity_singular', 'unknown')
        entity_plural = entity.get('entity_plural', entity_singular + 's')
        entity_pascal = snake_to_pascal(entity_singular)

        generator_facet_name = entity.get('generator_facet_name', 'generators')

        result = result.replace('{component_dir}', component_dir)
        result = result.replace('{component_core_dir}', component_core_dir)
        result = result.replace('{component_include}', component_include)
        result = result.replace('{component_core}', component_core)
        result = result.replace('{component}', component)
        result = result.replace('{entity_plural}', entity_plural)
        result = result.replace('{entity}', entity_singular)
        result = result.replace('{EntityPascal}', entity_pascal)
        result = result.replace('{generator_facet_name}', generator_facet_name)

    elif model_type == 'junction' and 'junction' in model_data:
        junction = model_data['junction']
        component = junction.get('component', 'unknown')
        junction_name = junction.get('name', 'unknown')
        name_singular = junction.get('name_singular', junction_name.rstrip('s'))
        entity_pascal = snake_to_pascal(name_singular)

        result = result.replace('{component}', component)
        result = result.replace('{junction_name}', junction_name)
        result = result.replace('{entity}', name_singular)
        result = result.replace('{EntityPascal}', entity_pascal)

    elif model_type == 'field_group' and 'field_group' in model_data:
        fg = model_data['field_group']
        component = fg.get('component', 'unknown')
        subcomponent = fg.get('subcomponent', '')
        if subcomponent:
            component_include = f"{component}.{subcomponent}"
            component_dir = f"ores.{component}/{subcomponent}"
        else:
            component_include = fg.get('component_include', component)
            component_dir = f"ores.{component}"
        entity_singular = fg.get('entity_singular', 'unknown')
        entity_pascal = snake_to_pascal(entity_singular)

        result = result.replace('{component_dir}', component_dir)
        result = result.replace('{component_include}', component_include)
        result = result.replace('{component}', component)
        result = result.replace('{entity}', entity_singular)
        result = result.replace('{EntityPascal}', entity_pascal)

    elif model_type == 'enum' and 'enum' in model_data:
        enum = model_data['enum']
        component = enum.get('component', 'unknown')
        enum_name = enum.get('name', 'unknown')

        result = result.replace('{component}', component)
        result = result.replace('{enum_name}', enum_name)

    elif model_type == 'component' and 'component' in model_data:
        component = model_data['component']
        name = component.get('name', 'unknown')
        full_name = component.get('full_name', f'ores.{name}')
        # {component_dir} is the on-disk component root relative to projects/
        # (nested layout after the product-group regroup, e.g. ores.refdata/api),
        # while {component_full} stays the dotted include namespace
        # (ores.refdata.api). 'dir' is injected by resolve_targets from the model
        # location; fall back to the dotted name for non-regrouped components.
        component_dir = component.get('dir', full_name)

        result = result.replace('{component_dir}', component_dir)
        result = result.replace('{component}', name)
        result = result.replace('{component_full}', full_name)

    elif model_type == 'table' and 'table' in model_data:
        table = model_data['table']
        component = table.get('component', 'unknown')
        entity_singular = table.get('entity_singular', 'unknown')
        entity_plural = table.get('entity_plural', entity_singular + 's')
        entity_pascal = snake_to_pascal(entity_singular)

        result = result.replace('{component}', component)
        result = result.replace('{entity}', entity_singular)
        result = result.replace('{entity_plural}', entity_plural)
        result = result.replace('{EntityPascal}', entity_pascal)

    elif model_type == 'service_registry':
        # Service registry output paths are fixed — no placeholder substitution needed.
        pass

    elif model_type == 'dataset' and 'dataset' in model_data:
        # Populate/seed outputs live under populate/{dataset}/ and are prefixed
        # by the dataset's model_name (e.g. solvaris_country_populate.sql); the
        # master include is {prefix}_populate.sql.
        dataset = model_data['dataset']
        name = dataset.get('name', 'unknown')
        result = result.replace('{dataset}', name)
        result = result.replace('{prefix}', dataset.get('prefix', name))

    elif 'schema' in model_data:
        schema = model_data['schema']
        component = schema.get('component', 'unknown')
        entity_singular = schema.get('entity_singular', 'unknown')
        entity_plural = schema.get('entity_plural', entity_singular + 's')
        entity_pascal = snake_to_pascal(entity_singular)

        result = result.replace('{component}', component)
        result = result.replace('{entity}', entity_singular)
        result = result.replace('{entity_plural}', entity_plural)
        result = result.replace('{EntityPascal}', entity_pascal)

    elif 'entity' in model_data:
        # Handle entity schema models (files ending with _entity.json)
        entity = model_data['entity']
        component = entity.get('component', 'unknown')
        subcomponent = entity.get('subcomponent', '')
        if subcomponent:
            component_include = f"{component}.{subcomponent}"
            component_dir = f"ores.{component}/{subcomponent}"
            component_core = f"{component}.core"
            component_core_dir = f"ores.{component}/core"
        else:
            component_include = entity.get('component_include', component)
            component_dir = f"ores.{component}"
            component_core = entity.get('component_core', component)
            component_core_dir = f"ores.{component}"
        entity_singular = entity.get('entity_singular', 'unknown')
        entity_plural = entity.get('entity_plural', entity_singular + 's')
        entity_pascal = snake_to_pascal(entity_singular)

        result = result.replace('{component_dir}', component_dir)
        result = result.replace('{component_core_dir}', component_core_dir)
        result = result.replace('{component_include}', component_include)
        result = result.replace('{component_core}', component_core)
        result = result.replace('{component}', component)
        result = result.replace('{entity}', entity_singular)
        result = result.replace('{entity_plural}', entity_plural)
        result = result.replace('{EntityPascal}', entity_pascal)

    return result


def resolve_profile_templates(profile_name, profiles, model_type=None, resolved=None):
    """
    Resolve all templates for a profile, including any included profiles.

    Args:
        profile_name (str): Name of the profile to resolve
        profiles (dict): Dictionary of all profile definitions
        model_type (str): The model type for filtering templates
        resolved (set): Set of already resolved profile names (for cycle detection)

    Returns:
        list: List of template info dicts with 'template' and optional 'output' keys
    """
    if resolved is None:
        resolved = set()

    if profile_name in resolved:
        return []  # Avoid infinite recursion

    if profile_name not in profiles:
        print(f"Warning: Unknown profile '{profile_name}'")
        return []

    resolved.add(profile_name)
    profile = profiles[profile_name]
    templates = []

    # Process templates - handle both old string format and new object format
    for tmpl in profile.get('templates', []):
        if isinstance(tmpl, str):
            # Old format: just template name
            templates.append({'template': tmpl})
        elif isinstance(tmpl, dict):
            # New format: object with template and output
            # Check if template is compatible with model type
            tmpl_model_types = tmpl.get('model_types')
            if tmpl_model_types and model_type and model_type not in tmpl_model_types:
                continue  # Skip this template - not compatible with model type
            templates.append(tmpl)

    # Resolve included profiles
    for included in profile.get('includes', []):
        templates.extend(resolve_profile_templates(included, profiles, model_type, resolved))

    return templates


def validate_profile_for_model(profile_name, profiles, model_type):
    """
    Check if a profile is compatible with a model type.

    Args:
        profile_name (str): Name of the profile
        profiles (dict): Dictionary of all profile definitions
        model_type (str): The model type

    Returns:
        tuple: (is_valid, error_message)
    """
    if profile_name not in profiles:
        return False, f"Unknown profile: {profile_name}"

    profile = profiles[profile_name]
    model_types = profile.get('model_types', [])

    if model_type not in model_types:
        return False, (f"Profile '{profile_name}' is not compatible with model type '{model_type}'. "
                      f"Supported types: {', '.join(model_types)}")

    # Also check included profiles
    for included in profile.get('includes', []):
        is_valid, error = validate_profile_for_model(included, profiles, model_type)
        if not is_valid:
            return False, error

    return True, None


def list_profiles(profiles):
    """
    Print a formatted list of available profiles.

    Args:
        profiles (dict): Dictionary of profile definitions
    """
    print("Available profiles:")
    print()
    for name, profile in sorted(profiles.items()):
        description = profile.get('description', 'No description')
        model_types = ', '.join(profile.get('model_types', []))
        template_count = len(profile.get('templates', []))
        includes = profile.get('includes', [])

        print(f"  {name}")
        print(f"    Description:  {description}")
        print(f"    Model types:  {model_types}")
        if includes:
            print(f"    Includes:     {', '.join(includes)}")
        else:
            print(f"    Templates:    {template_count}")
        print()


def normalise_sql_table_context(table):
    """Compute the derived render fields the unified SQL schema template
    (``sql_schema_create.mustache``) expects on its ``table`` context.

    Shared by both SQL inputs — native ``table`` models and the ``table``
    context projected from a unified ``domain_entity`` model — so a single set
    of rules governs coding-scheme flags, validation-function scope, check
    constraints, and comma-handling markers. Mutates ``table`` in place."""
    if 'columns' in table:
        _mark_last_item(table['columns'])
    if 'indexes' in table:
        _mark_last_item(table['indexes'])
    if 'check_constraints' in table:
        _mark_last_item(table['check_constraints'])
    if 'insert_trigger' in table and 'validations' in table['insert_trigger']:
        _mark_last_item(table['insert_trigger']['validations'])
    # Pre-render the description as a SQL comment block: prefix every line with
    # '-- ' so multi-line prose stays valid SQL, and emit it unescaped (the
    # template uses a triple-stache) so apostrophes are not HTML-escaped.
    description = table.get('description', '') or ''
    table['description_comment'] = '\n'.join(
        f'-- {line}' if line.strip() else '--'
        for line in description.split('\n')
    ) if description else '--'
    # Precompute coding_scheme boolean flags
    coding_scheme = table['coding_scheme']
    table['has_coding_scheme'] = (coding_scheme == 'required')
    table['has_nullable_coding_scheme'] = (coding_scheme == 'nullable')
    table['has_any_coding_scheme'] = coding_scheme in ('required', 'nullable')
    table['has_image_id'] = bool(table.get('image_id', False))
    table['has_tenant_id'] = bool(table.get('has_tenant_id', True))
    # Pre-render check constraints as a single string to avoid Mustache
    # whitespace issues when inserting them after the standard checks.
    raw_checks = table.get('check_constraints', [])
    if raw_checks:
        table['has_check_constraints'] = True
        lines = [f'    check ({c["expression"]})' for c in raw_checks]
        table['sql_check_constraints'] = ',\n'.join(lines)
    else:
        table['has_check_constraints'] = False
        table['sql_check_constraints'] = ''
    # Precompute tenant-scope flags for the validation function
    if 'validation_fn' in table:
        scope = table['validation_fn']['tenant_scope']
        table['validation_fn']['scope_system'] = (scope == 'system')
        table['validation_fn']['scope_both'] = (scope == 'both')
        table['validation_fn']['scope_tenant'] = (scope == 'tenant')
        if 'order_by' not in table['validation_fn']:
            table['validation_fn']['order_by'] = table['primary_key']['column']


def get_domain_entity_template_mappings():
    """
    Define the mapping for domain entity schema templates.

    Returns:
        list: List of tuples (template_name, output_suffix) for domain entity generation
    """
    return [
        # End-state: the entity pathway renders SQL through the single unified
        # bi-temporal table template. The `table` render context is projected
        # from the domain_entity model (see the domain-entity normalisation in
        # generate_code), so there is exactly one SQL schema template to maintain.
        ("sql_schema_create.mustache", "_create.sql"),
    ]


def get_junction_template_mappings():
    """
    Define the mapping for junction table schema templates.

    Returns:
        list: List of tuples (template_name, output_suffix) for junction table generation
    """
    return [
        ("sql_schema_junction_create.mustache", "_create.sql"),
    ]


def get_qt_domain_entity_template_mappings():
    """
    Define the mapping for Qt domain entity templates.

    Returns:
        list: List of tuples (template_name, output_dir, output_suffix) for Qt generation
    """
    return [
        # Client model facet
        ("cpp_qt_client_model.hpp.mustache", "include/ores.qt", "Model.hpp"),
        ("cpp_qt_client_model.cpp.mustache", "src", "Model.cpp"),
        # MDI window facet
        ("cpp_qt_mdi_window.hpp.mustache", "include/ores.qt", "MdiWindow.hpp"),
        ("cpp_qt_mdi_window.cpp.mustache", "src", "MdiWindow.cpp"),
        # Detail dialog facet
        ("cpp_qt_detail_dialog.hpp.mustache", "include/ores.qt", "DetailDialog.hpp"),
        ("cpp_qt_detail_dialog.cpp.mustache", "src", "DetailDialog.cpp"),
        ("qt_detail_dialog_ui.mustache", "ui", "DetailDialog.ui"),
        # History dialog facet
        ("cpp_qt_history_dialog.hpp.mustache", "include/ores.qt", "HistoryDialog.hpp"),
        ("cpp_qt_history_dialog.cpp.mustache", "src", "HistoryDialog.cpp"),
        ("qt_history_dialog_ui.mustache", "ui", "HistoryDialog.ui"),
        # Controller facet
        ("cpp_qt_controller.hpp.mustache", "include/ores.qt", "Controller.hpp"),
        ("cpp_qt_controller.cpp.mustache", "src", "Controller.cpp"),
    ]


def get_cpp_domain_entity_template_mappings():
    """
    Define the mapping for C++ domain entity templates.

    Returns:
        list: List of tuples (template_name, output_dir, output_suffix) for C++ generation
    """
    return [
        # Class definition facet
        ("cpp_domain_type_class.hpp.mustache", "include/{component}/domain", ".hpp"),
        # JSON I/O facet
        ("cpp_domain_type_json_io.hpp.mustache", "include/{component}/domain", "_json_io.hpp"),
        ("cpp_domain_type_json_io.cpp.mustache", "src/domain", "_json_io.cpp"),
        # Table facet
        ("cpp_domain_type_table.hpp.mustache", "include/{component}/domain", "_table.hpp"),
        ("cpp_domain_type_table.cpp.mustache", "src/domain", "_table.cpp"),
        ("cpp_domain_type_table_io.hpp.mustache", "include/{component}/domain", "_table_io.hpp"),
        ("cpp_domain_type_table_io.cpp.mustache", "src/domain", "_table_io.cpp"),
        # Generator facet
        ("cpp_domain_type_generator.hpp.mustache", "include/{component}/generators", "_generator.hpp"),
        ("cpp_domain_type_generator.cpp.mustache", "src/generators", "_generator.cpp"),
        # Repository entity facet
        ("cpp_domain_type_entity.hpp.mustache", "include/{component}/repository", "_entity.hpp"),
        ("cpp_domain_type_entity.cpp.mustache", "src/repository", "_entity.cpp"),
        # Repository mapper facet
        ("cpp_domain_type_mapper.hpp.mustache", "include/{component}/repository", "_mapper.hpp"),
        ("cpp_domain_type_mapper.cpp.mustache", "src/repository", "_mapper.cpp"),
        # Repository CRUD facet
        ("cpp_domain_type_repository.hpp.mustache", "include/{component}/repository", "_repository.hpp"),
        ("cpp_domain_type_repository.cpp.mustache", "src/repository", "_repository.cpp"),
        # Service facet
        ("cpp_service.hpp.mustache", "include/{component}/service", "_service.hpp"),
        ("cpp_service.cpp.mustache", "src/service", "_service.cpp"),
        # Protocol facet (header-only; cpp_protocol.cpp retired — protocol is
        # NATS header-only, see B8)
        ("cpp_protocol.hpp.mustache", "include/{component}/messaging", "_protocol.hpp"),
    ]


def get_cpp_junction_template_mappings():
    """
    Define the mapping for C++ junction table templates.

    Junction tables generate domain, JSON I/O, table I/O, entity, mapper,
    repository, and generator files. They do not generate service or protocol
    files (those are only for domain entities).

    Returns:
        list: List of tuples (template_name, output_dir, output_suffix) for C++ generation
    """
    return [
        # Class definition facet
        ("cpp_domain_type_class.hpp.mustache", "include/{component}/domain", ".hpp"),
        # JSON I/O facet
        ("cpp_domain_type_json_io.hpp.mustache", "include/{component}/domain", "_json_io.hpp"),
        ("cpp_domain_type_json_io.cpp.mustache", "src/domain", "_json_io.cpp"),
        # Table facet
        ("cpp_domain_type_table.hpp.mustache", "include/{component}/domain", "_table.hpp"),
        ("cpp_domain_type_table.cpp.mustache", "src/domain", "_table.cpp"),
        ("cpp_domain_type_table_io.hpp.mustache", "include/{component}/domain", "_table_io.hpp"),
        ("cpp_domain_type_table_io.cpp.mustache", "src/domain", "_table_io.cpp"),
        # Generator facet
        ("cpp_domain_type_generator.hpp.mustache", "include/{component}/generators", "_generator.hpp"),
        ("cpp_domain_type_generator.cpp.mustache", "src/generators", "_generator.cpp"),
        # Repository entity facet
        ("cpp_domain_type_entity.hpp.mustache", "include/{component}/repository", "_entity.hpp"),
        ("cpp_domain_type_entity.cpp.mustache", "src/repository", "_entity.cpp"),
        # Repository mapper facet
        ("cpp_domain_type_mapper.hpp.mustache", "include/{component}/repository", "_mapper.hpp"),
        ("cpp_domain_type_mapper.cpp.mustache", "src/repository", "_mapper.cpp"),
        # Repository CRUD facet
        ("cpp_domain_type_repository.hpp.mustache", "include/{component}/repository", "_repository.hpp"),
        ("cpp_domain_type_repository.cpp.mustache", "src/repository", "_repository.cpp"),
    ]


def get_enum_template_mappings():
    """
    Define the mapping for enum schema templates.

    Returns:
        list: List of tuples (template_name, output_suffix) for enum generation
    """
    return [
        ("cpp_enum.hpp.mustache", ".hpp"),
    ]


_PASTE_MARKER_RE = re.compile(r"[ \t]*<<paste:([0-9A-Fa-f-]+)>>[ \t]*\n?")


def _substitute_paste_markers(text, data):
    """Replace ``<<paste:KIND_UUID>>`` markers with concatenated bodies of
    every block in the current entity that ``:implements`` that kind UUID.

    Org-mode entity models carry an ``implementations`` dict keyed by kind
    UUID under ``domain_entity``. For each marker, look up the matching
    list of code bodies, join them with a blank line, and substitute.

    Missing kinds produce an empty substitution and the entire marker line
    (including its newline) is collapsed, leaving no trace in the output.
    This makes templates safe to include markers that no current entity
    implements."""
    de = data.get("domain_entity") or {}
    impls = de.get("implementations") or {}

    def replace(match):
        kind = match.group(1)
        blocks = impls.get(kind)
        if not blocks:
            return ""
        return "\n\n".join(b.rstrip() for b in blocks) + "\n"

    return _PASTE_MARKER_RE.sub(replace, text)


def load_model(model_path):
    """
    Load a model from the specified path.

    Dispatches on file extension: ``.org`` files go through the org-mode
    loader, anything else (``.json``) is parsed as JSON.

    Args:
        model_path (str or Path): Path to the model file

    Returns:
        dict: The loaded model data
    """
    path_str = str(model_path)
    if path_str.endswith('.org'):
        # Local import avoids a circular dependency at module load time.
        from .org_loader import (
            load_org_model,
            load_org_field_group_model,
            load_org_junction_model,
            load_org_table_model,
            load_org_lookup_entity_model,
            load_org_service_registry_model,
            load_org_component_model,
            load_org_component_overview_model,
            load_org_dataset_model,
        )
        # Prefer #+type: frontmatter over filename suffix.
        org_type = _read_org_type(model_path)
        if org_type == 'dataset':
            return load_org_dataset_model(model_path)
        if org_type == 'field_group':
            return load_org_field_group_model(model_path)
        if org_type == 'junction':
            return load_org_junction_model(model_path)
        if org_type == 'table':
            return load_org_table_model(model_path)
        if org_type == 'enum':
            return load_org_lookup_entity_model(model_path)
        if org_type == 'service_registry':
            return load_org_service_registry_model(model_path)
        if org_type == 'component':
            if path_str.endswith('component_overview.org'):
                return load_org_component_overview_model(model_path)
            return load_org_component_model(model_path)
        if org_type == 'domain_entity':
            return load_org_model(model_path)

        # Fallback: no recognised #+type: — use filename suffix (legacy).
        if path_str.endswith('_field_group.org'):
            return load_org_field_group_model(model_path)
        if path_str.endswith('_junction.org'):
            return load_org_junction_model(model_path)
        if path_str.endswith('_table.org'):
            return load_org_table_model(model_path)
        if path_str.endswith('_lookup_entity.org'):
            return load_org_lookup_entity_model(model_path)
        if path_str.endswith('service_registry.org'):
            return load_org_service_registry_model(model_path)
        if path_str.endswith('component_overview.org'):
            return load_org_component_overview_model(model_path)
        if path_str.endswith('_component.org'):
            return load_org_component_model(model_path)
        return load_org_model(model_path)
    with open(model_path, 'r', encoding='utf-8') as f:
        return json.load(f)


def get_relative_path(abs_path, base_path):
    """
    Get the relative path of abs_path from base_path.

    Args:
        abs_path (Path): Absolute path
        base_path (Path): Base path to calculate relative path from

    Returns:
        str: Relative path string
    """
    try:
        return str(abs_path.relative_to(base_path))
    except ValueError:
        # If abs_path is not within base_path, return the full path
        return str(abs_path)


# Diverse pool of currency defaults based on common patterns
CURRENCY_DEFAULTS_POOL = [
    {'symbol': '$', 'fraction_symbol': '¢', 'fractions_per_unit': 100, 'rounding_type': 'Closest', 'rounding_precision': 2, 'format': '$#,##0.00', 'asset_class': 'fiat', 'market_tier': 'emerging'},
    {'symbol': '€', 'fraction_symbol': 'c', 'fractions_per_unit': 100, 'rounding_type': 'Closest', 'rounding_precision': 2, 'format': '€#,##0.00', 'asset_class': 'fiat', 'market_tier': 'g10'},
    {'symbol': '£', 'fraction_symbol': 'p', 'fractions_per_unit': 100, 'rounding_type': 'Closest', 'rounding_precision': 2, 'format': '£#,##0.00', 'asset_class': 'fiat', 'market_tier': 'g10'},
    {'symbol': '¥', 'fraction_symbol': '', 'fractions_per_unit': 0, 'rounding_type': 'Closest', 'rounding_precision': 0, 'format': '¥#,##0', 'asset_class': 'fiat', 'market_tier': 'emerging'},
    {'symbol': 'kr', 'fraction_symbol': 'ø', 'fractions_per_unit': 100, 'rounding_type': 'Closest', 'rounding_precision': 2, 'format': 'kr #,##0.00', 'asset_class': 'fiat', 'market_tier': 'g10'},
    {'symbol': 'zł', 'fraction_symbol': 'gr', 'fractions_per_unit': 100, 'rounding_type': 'Closest', 'rounding_precision': 2, 'format': '#,##0.00 zł', 'asset_class': 'fiat', 'market_tier': 'emerging'},
    {'symbol': '₹', 'fraction_symbol': 'p', 'fractions_per_unit': 100, 'rounding_type': 'Closest', 'rounding_precision': 2, 'format': '₹#,##0.00', 'asset_class': 'fiat', 'market_tier': 'emerging'},
    {'symbol': 'د.إ', 'fraction_symbol': 'ف', 'fractions_per_unit': 100, 'rounding_type': 'Closest', 'rounding_precision': 2, 'format': 'د.إ#,##0.00', 'asset_class': 'fiat', 'market_tier': 'emerging'},
    {'symbol': 'R', 'fraction_symbol': 'c', 'fractions_per_unit': 100, 'rounding_type': 'Closest', 'rounding_precision': 2, 'format': 'R#,##0.00', 'asset_class': 'fiat', 'market_tier': 'emerging'},
    {'symbol': 'ƒ', 'fraction_symbol': '¢', 'fractions_per_unit': 100, 'rounding_type': 'Closest', 'rounding_precision': 2, 'format': 'ƒ#,##0.00', 'asset_class': 'fiat', 'market_tier': 'emerging'},
    {'symbol': 'د.ك', 'fraction_symbol': 'ف', 'fractions_per_unit': 1000, 'rounding_type': 'Closest', 'rounding_precision': 3, 'format': 'د.ك#,##0.000', 'asset_class': 'fiat', 'market_tier': 'emerging'},
    {'symbol': 'S/', 'fraction_symbol': '¢', 'fractions_per_unit': 100, 'rounding_type': 'Closest', 'rounding_precision': 2, 'format': 'S/#,##0.00', 'asset_class': 'fiat', 'market_tier': 'emerging'},
    {'symbol': '฿', 'fraction_symbol': 'ส', 'fractions_per_unit': 100, 'rounding_type': 'Closest', 'rounding_precision': 2, 'format': '฿#,##0.00', 'asset_class': 'fiat', 'market_tier': 'emerging'},
]


def _mark_last_item(data_list):
    """
    Mark the last item in a list of dictionaries with a 'last' flag.

    Args:
        data_list (list): List to process
    """
    if isinstance(data_list, list) and data_list:
        # Only add if it's a list of dictionaries
        if isinstance(data_list[-1], dict):
            data_list[-1]['last'] = True


def _format_description_as_comment(description):
    """
    Format a multi-line description as SQL comment block content.

    Adds ' * ' prefix to each line after the first, handling empty lines
    as ' *' (just asterisk).

    Args:
        description (str): Multi-line description text

    Returns:
        str: Formatted description with comment prefixes
    """
    if not description:
        return description

    lines = description.split('\n')
    formatted_lines = []
    for i, line in enumerate(lines):
        if i == 0:
            # First line doesn't get prefix (it follows the title line)
            formatted_lines.append(line)
        elif line.strip():
            # Non-empty lines get ' * ' prefix
            formatted_lines.append(' * ' + line)
        else:
            # Empty lines get just ' *'
            formatted_lines.append(' *')
    return '\n'.join(formatted_lines)


def _prepare_table_display(cpp_section, uuid_columns=None, optional_columns=None):
    """
    Prepare table_display items by adding iterator_var and is_uuid to each item.

    Mustache can't access parent context variables from within a loop,
    so we add the iterator_var to each table_display item. We also flag
    UUID columns so the table template can wrap them with to_string(), and
    optional columns so the template can unwrap them before streaming to
    libfort (which has no operator<< for std::optional).

    Args:
        cpp_section (dict): The 'cpp' section of the model
        uuid_columns (set): Set of column names that are UUID type
        optional_columns (set): Set of column names that are std::optional<T>
    """
    if 'table_display' not in cpp_section:
        return

    uuid_columns = uuid_columns or set()
    optional_columns = optional_columns or set()
    iter_var = cpp_section.get('iterator_var', 'e')
    has_uuid = False
    has_optional = False
    for item in cpp_section['table_display']:
        item['iter_var'] = iter_var
        item['is_uuid'] = item['column'] in uuid_columns
        item['is_optional'] = item['column'] in optional_columns
        if item['is_uuid']:
            has_uuid = True
        if item['is_optional']:
            has_optional = True
    cpp_section['has_uuid_table_display'] = has_uuid
    cpp_section['has_optional_table_display'] = has_optional


def _format_detail_for_doxygen(detail):
    """
    Format a multi-line detail string for doxygen comments.

    Adds '     * ' prefix to continuation lines.

    Args:
        detail (str): Multi-line detail text

    Returns:
        str: Formatted detail with proper doxygen prefixes
    """
    if not detail or '\n' not in detail:
        return detail

    lines = detail.split('\n')
    formatted_lines = [lines[0]]  # First line as-is
    for line in lines[1:]:
        if line.strip():
            formatted_lines.append('     * ' + line)
        else:
            formatted_lines.append('     *')
    return '\n'.join(formatted_lines)


def _format_columns_for_doxygen(columns):
    """
    Format detail fields in columns for doxygen comments.

    Args:
        columns (list): List of column dictionaries
    """
    if not columns:
        return

    for col in columns:
        if 'detail' in col:
            col['detail'] = _format_detail_for_doxygen(col['detail'])


def generate_from_model(model_path, data_dir, templates_dir, output_dir, is_processing_batch=False, prefix=None, target_template=None, target_output=None):
    """
    Generate output files from a model using the appropriate templates.

    Args:
        model_path (str or Path): Path to the model file
        data_dir (Path): Path to the data directory
        templates_dir (Path): Path to the templates directory
        output_dir (Path): Path to the output directory
        is_processing_batch (bool): Flag to indicate if we're already processing a batch
        prefix (str): Optional prefix for output filenames
        target_template (str): Optional override for the template to use
        target_output (str): Optional override for the output filename
    """
    # Load the model
    model = load_model(model_path)
    model_filename = Path(model_path).name

    # Special handling for model.json - generate all dependent models first
    if model_filename == "model.json" and not is_processing_batch:
        print(f"Processing overall model: {model_filename}...")
        
        # Get prefix from overall model
        prefix = model.get("model_name")

        # Extract model directory
        model_dir = Path(model_path).parent

        # Generate all dependent files listed in the model manifest
        for file_entry in model.get("files", []):
            dependent_model_filename = file_entry.get("model")
            template_filename = file_entry.get("template")
            output_filename = file_entry.get("name")

            if dependent_model_filename and template_filename:
                dependent_model_path = model_dir / dependent_model_filename

                if dependent_model_path.exists():
                    print(f"Generating dependent item: {output_filename} (from {dependent_model_filename})")
                    generate_from_model(
                        dependent_model_path, 
                        data_dir, 
                        templates_dir, 
                        output_dir, 
                        is_processing_batch=True, 
                        prefix=prefix,
                        target_template=template_filename,
                        target_output=output_filename
                    )
                else:
                    print(f"Warning: Dependent model not found: {dependent_model_path}")

        # After generating all dependencies, now generate the overall model file itself
        is_processing_batch = True

    # Get template mappings
    template_map = get_template_mappings()

    # Classify by the document #+type: (the source of truth), falling back to
    # filename-suffix detection only when no path/type is available. Filenames
    # must NOT carry type information — e.g. an entity legitimately named
    # gmm_component would otherwise be misread as a component model via the
    # _component.org suffix.
    model_type = get_model_type(model_filename, model_path)
    is_schema_model = model_type == 'schema'
    is_domain_entity = model_type == 'domain_entity'
    is_junction = model_type == 'junction'
    is_enum = model_type == 'enum'
    is_component = model_type == 'component'
    is_service_registry = model_type == 'service_registry'
    is_field_group = model_type == 'field_group'
    is_table = model_type == 'table'

    # Check for C++ generation flag (--cpp or cpp_ prefix in target_template)
    generate_cpp = target_template and target_template.startswith('cpp_') and not target_template.startswith('cpp_qt_')
    # Check for Qt generation flag (qt_ or cpp_qt_ prefix in target_template)
    generate_qt = target_template and (target_template.startswith('qt_') or target_template.startswith('cpp_qt_'))

    # Determine which templates to process
    if target_template:
        templates_to_process = [target_template]
    elif is_domain_entity:
        # Domain entity models use a specific template
        templates_to_process = [t[0] for t in get_domain_entity_template_mappings()]
    elif is_junction:
        # Junction table models use a specific template
        templates_to_process = [t[0] for t in get_junction_template_mappings()]
    elif is_enum:
        # Enum models use a specific template
        templates_to_process = [t[0] for t in get_enum_template_mappings()]
    elif is_field_group:
        # Field-group models must be used via the field-group profile (no default templates)
        if target_template:
            templates_to_process = [target_template]
        else:
            print(f"Field-group model '{model_filename}' requires --profile field-group")
            return
    elif is_component:
        # Component scaffold models must be used via a profile (no default templates)
        if target_template:
            templates_to_process = [target_template]
        else:
            print(f"Component model '{model_filename}' requires --profile component")
            return
    elif is_service_registry:
        # Service registry models must be used via a profile (no default templates)
        if target_template:
            templates_to_process = [target_template]
        else:
            print(f"Service registry model '{model_filename}' requires --profile service-registry")
            return
    elif is_table:
        # Table models must be used via a profile (e.g. --profile sql)
        if target_template:
            templates_to_process = [target_template]
        else:
            print(f"Table model '{model_filename}' requires --profile sql")
            return
    elif is_schema_model:
        # Entity schema models use a different template set
        templates_to_process = [t[0] for t in get_schema_template_mappings()]
    elif model_filename in template_map:
        templates_to_process = template_map[model_filename]
    else:
        print(f"No templates found for model: {model_filename}")
        return

    # Load library data
    data = load_data(data_dir)
    
    # Add prefix to data context if available
    if prefix:
        data['model_name'] = prefix

    # Load sibling models (other JSON files in the same directory)
    # Only load files that are recognized as models in our mappings
    model_dir = Path(model_path).parent
    known_model_filenames = set(get_template_mappings().keys())
    known_model_filenames.add("model.json")

    for json_file in model_dir.glob("*.json"):
        if json_file.name in known_model_filenames:
            key = json_file.stem
            if key not in data:
                data[key] = load_model(json_file)

    # Identify specific datasets for cross-referencing in templates
    if 'datasets' in data:
        for ds in data['datasets']:
            if ds.get('subject_area_name') == 'Currencies':
                data['currencies_dataset'] = ds
            elif ds.get('subject_area_name') == 'Country Flags':
                data['flags_dataset'] = ds
            elif ds.get('subject_area_name') == 'Countries':
                data['countries_dataset'] = ds

    # Generate SQL license with modeline and copyright header
    if 'licence-GPL-v3' in data and 'modelines' in data:
        # Get the SQL modeline
        sql_modeline = data['modelines'].get('sql', '')
        # Generate the SQL license
        sql_license = generate_license_with_header(
            data['licence-GPL-v3'],
            sql_modeline,
            'sql'
        )
        # Add to data for use in templates
        data['sql_license'] = sql_license
        # Also add the modeline separately if needed
        data['sql_modeline'] = sql_modeline

        # Get the C++ modeline and generate C++ license
        cpp_modeline = data['modelines'].get('c++', '')
        cpp_license = generate_license_with_header(
            data['licence-GPL-v3'],
            cpp_modeline,
            'c++'
        )
        data['cpp_license'] = cpp_license
        data['cpp_modeline'] = cpp_modeline

        # Get the CMake modeline and generate CMake license
        cmake_modeline = data['modelines'].get('cmake', '')
        if cmake_modeline:
            cmake_license = generate_license_with_header(
                data['licence-GPL-v3'],
                cmake_modeline,
                'cmake'
            )
            data['cmake_license'] = cmake_license
            data['cmake_modeline'] = cmake_modeline

    # Add the model data to the template data
    # Use the model filename (without extension) as the key
    model_key = Path(model_path).stem
    data[model_key] = model
    
    # If the model is a list, mark the last item for Mustache templates
    _mark_last_item(data[model_key])

    # Handle file references in the model data (e.g., steps_file pointing to methodology.txt)
    model_dir = Path(model_path).parent
    _resolve_file_references(data[model_key], model_dir, data)

    # For manifest.json, copy methodologies to top level for template access
    if model_key == 'manifest' and isinstance(data[model_key], dict):
        if 'methodologies' in data[model_key]:
            data['methodologies'] = data[model_key]['methodologies']

    # Special processing for country_currency model to generate SVG flags
    if model_key in ['country_currency', 'country_currency_flags']:
        # Process each country currency to generate SVG flag data
        processed_data = []
        for i, item in enumerate(data[model_key]):
            # Create a copy of the item and add the generated SVG
            processed_item = item.copy()
            processed_item['generated_svg'] = generate_flag_svg(item.get('country_code', ''))
            
            # Select a default from the pool based on the item index for diversity
            pool_index = i % len(CURRENCY_DEFAULTS_POOL)
            defaults = CURRENCY_DEFAULTS_POOL[pool_index]
            
            # Add hardcoded defaults for missing fields
            processed_item.setdefault('currency_symbol', defaults['symbol'])
            processed_item.setdefault('fraction_symbol', defaults['fraction_symbol'])
            processed_item.setdefault('fractions_per_unit', defaults['fractions_per_unit'])
            processed_item.setdefault('rounding_type', defaults['rounding_type'])
            processed_item.setdefault('rounding_precision', defaults['rounding_precision'])
            processed_item.setdefault('format', defaults['format'])
            processed_item.setdefault('asset_class', defaults['asset_class'])
            
            # Add country specific defaults
            country_code = item.get('country_code', 'XX')
            country_name = item.get('country_name', 'Unknown')
            processed_item.setdefault('country_alpha3', f"X{country_code}")
            # Use a deterministic numeric code based on the alpha2 code
            numeric_base = sum(ord(c) for c in country_code) + 1000
            processed_item.setdefault('country_numeric', numeric_base)
            processed_item.setdefault('country_official_name', f"Republic of {country_name}")
            
            # Pre-calculate lowercase country code for template use
            if 'country_code' in processed_item:
                processed_item['country_code_lower'] = processed_item['country_code'].lower()
            
            processed_data.append(processed_item)
        
        # Mark the last item for Mustache templates
        _mark_last_item(processed_data)
            
        # Store the processed data under the original key for templates to use
        data[model_key] = processed_data

    # Special processing for datasets model to handle dependencies
    if model_key == 'datasets':
        for ds in data[model_key]:
            if 'dependencies' in ds:
                # Transform simple string list to objects for Mustache
                ds['dataset_dependencies'] = []
                for dep_code in ds['dependencies']:
                    ds['dataset_dependencies'].append({
                        'parent_code': ds['code'],
                        'dependency_code': dep_code,
                        'role': 'visual_assets'  # Default role
                    })
                # Mark last for SQL formatting if needed
                _mark_last_item(ds['dataset_dependencies'])

    # Special processing for component scaffold models
    if is_component and isinstance(model, dict) and 'component' in model:
        component = model['component']
        name = component.get('name', 'unknown')
        full_name = component.get('full_name', f'ores.{name}')
        component['full_name_upper'] = full_name.replace('.', '_').upper()
        component['name_upper'] = name.replace('.', '_').upper()
        component['namespace'] = full_name.replace('.', '::')
        # Derive split-component names (e.g. name="refdata.core" -> base="refdata")
        name_parts = name.split('.')
        base_name = '.'.join(name_parts[:-1]) if len(name_parts) > 1 else name_parts[0]
        component['base_name'] = base_name
        component['segment'] = name_parts[-1] if len(name_parts) > 1 else ''

        full_parts = full_name.split('.')
        if len(full_parts) > 1 and len(name_parts) > 1:
            base_full = '.'.join(full_parts[:-1])
            component['api_full_name'] = f'{base_full}.api'
            component['core_full_name'] = f'{base_full}.core'
            component['service_full_name'] = f'{base_full}.service'
        else:
            component['api_full_name'] = f'ores.{base_name}.api'
            component['core_full_name'] = f'ores.{base_name}.core'
            component['service_full_name'] = f'ores.{base_name}.service'
        data['component'] = component

    # Special processing for service registry models
    if is_service_registry and isinstance(model, dict) and 'service_registry' in model:
        service_registry = model['service_registry']
        services = service_registry.get('services', [])
        # Mark last service for comma/semicolon handling in templates
        _mark_last_item(services)
        # Propagate psql_var into nested grant lists so templates can reference
        # the parent service variable from within a nested loop (Mustache has
        # no parent-context access, so we add the field to each nested item).
        for svc in services:
            psql_var = svc.get('psql_var', '')
            for item in svc.get('dml_prefixes', []):
                item['psql_var'] = psql_var
            for item in svc.get('select_tables', []):
                item['psql_var'] = psql_var
            for item in svc.get('select_prefixes', []):
                item['psql_var'] = psql_var
        data['service_registry'] = service_registry

    # Special processing for unified table models
    if is_table and isinstance(model, dict) and 'table' in model:
        normalise_sql_table_context(model['table'])
        data['table'] = model['table']

    # Special processing for entity schema models
    if is_schema_model and isinstance(model, dict) and 'entity' in model:
        entity = model['entity']
        # Mark last item in columns list for proper comma handling
        if 'columns' in entity:
            _mark_last_item(entity['columns'])
        # Mark last item in indexes list
        if 'indexes' in entity:
            _mark_last_item(entity['indexes'])
        # Mark last item in artefact_indexes list
        if 'artefact_indexes' in entity:
            _mark_last_item(entity['artefact_indexes'])
        # Derive component paths from component + subcomponent
        if 'component' in entity:
            component = entity['component']
            entity['component_upper'] = component.upper()
            subcomponent = entity.get('subcomponent', '')
            if subcomponent:
                entity['component_include'] = f"{component}.{subcomponent}"
                entity['component_dir'] = f"ores.{component}/{subcomponent}"
                entity['component_core'] = f"{component}.core"
                entity['component_core_dir'] = f"ores.{component}/core"
            else:
                entity.setdefault('component_include', component)
                entity['component_dir'] = f"ores.{component}"
                entity.setdefault('component_core', component)
                entity['component_core_dir'] = f"ores.{component}"
            entity['component_include_upper'] = (
                entity['component_include'].replace('.', '_').upper()
            )
            entity['component_core_upper'] = (
                entity['component_core'].replace('.', '_').upper()
            )
        # Store entity at top level for easier template access
        data['entity'] = entity

        # Add image linking configuration if defined in entity model
        if 'image_linking' in entity:
            data['image_linking'] = entity['image_linking']

    # Special processing for domain entity models
    if is_domain_entity and isinstance(model, dict) and 'domain_entity' in model:
        domain_entity = model['domain_entity']
        # Project the unified entity model onto the shared SQL `table` context
        # and normalise it exactly like a native table model, so the entity
        # pathway renders through the single sql_schema_create.mustache template.
        from .org_loader import domain_entity_to_table_context  # deferred to avoid circular import
        sql_table = domain_entity_to_table_context(domain_entity)['table']
        normalise_sql_table_context(sql_table)
        data['table'] = sql_table
        # Get iterator_var from cpp section for column processing
        iter_var = domain_entity.get('cpp', {}).get('iterator_var', 'e')
        if 'columns' in domain_entity:
            _mark_last_item(domain_entity['columns'])
            _format_columns_for_doxygen(domain_entity['columns'])
            # Add type flags and iterator_var for protocol serialization
            for col in domain_entity['columns']:
                # image_id is rendered into SQL via the has_image_id flag (so it
                # lands after coding_scheme_code, the canonical column order). It
                # stays in the column list for C++ generation; the SQL columns
                # loop skips it via this guard to avoid emitting it twice.
                col['is_image_id'] = (
                    col.get('name') == 'image_id' and domain_entity.get('has_image_id', False)
                )
                col['is_int'] = col.get('type') == 'integer' or col.get('cpp_type') == 'int'
                is_uuid_type = col.get('type') == 'uuid' or 'boost::uuids::uuid' in col.get('cpp_type', '')
                is_timestamp_type = col.get('type') in ('timestamp', 'timestamptz')
                is_enum_type = col.get('is_enum', False)
                is_already_optional = (
                    col.get('cpp_type', '').startswith('std::optional<')
                    and not is_uuid_type
                )
                col['is_already_optional'] = is_already_optional
                col['is_uuid'] = is_uuid_type and not col.get('nullable', False)
                col['is_optional_uuid'] = is_uuid_type and col.get('nullable', False)
                col['is_optional_timestamp'] = is_timestamp_type and col.get('nullable', False)
                col['is_enum'] = is_enum_type and not col.get('nullable', False)
                col['is_nullable_string'] = (
                    col.get('nullable', False)
                    and not is_uuid_type
                    and not is_timestamp_type
                    and not is_enum_type
                    and not is_already_optional
                )
                col['is_simple'] = (
                    not col.get('nullable', False)
                    and not is_uuid_type
                    and not is_enum_type
                    and not is_already_optional
                )
                # Non-nullable plain std::string columns without an explicit
                # generator_expr have no safe struct-level default (unlike
                # bool/int below) — an empty string often fails a `<> ''`
                # check constraint. Flag them so the generator template can
                # fall back to a synthetic value.
                col['is_plain_string'] = (
                    col['is_simple'] and col.get('cpp_type') == 'std::string'
                    and not col.get('generator_expr')
                )
                # Supply a safe default for non-nullable scalar types that
                # would otherwise leave the domain struct with an
                # indeterminate value. Nullable fields wrap in optional so
                # they default to nullopt; only bool/int need an explicit
                # default. The model may override via default_value.
                if not col.get('default_value') and col['is_simple']:
                    cpp_type = col.get('cpp_type', '')
                    if cpp_type == 'bool':
                        col['default_value'] = 'false'
                    elif cpp_type == 'int':
                        col['default_value'] = '0'
                col['iter_var'] = iter_var
        # Field-group contract: detect identity/audit group annotations and
        # mark each column so templates can emit nested-struct form.
        identity_group_value = domain_entity.get('domain_identity_group', '')
        audit_group_value = domain_entity.get('domain_audit_group', '')
        has_identity_group = bool(identity_group_value)
        has_audit_group = bool(audit_group_value)
        domain_entity['has_identity_group'] = has_identity_group
        domain_entity['has_audit_group'] = has_audit_group
        if has_identity_group:
            parts = identity_group_value.split('.')
            if len(parts) != 3:
                raise ValueError(
                    f"domain_identity_group must be a 3-part dotted name "
                    f"(e.g. 'ores.trading.instrument_identity'), got: "
                    f"'{identity_group_value}' ({len(parts)} parts)"
                )
            # e.g. ores.trading.instrument_identity → 'instrument_identity'
            domain_entity['identity_group_type'] = parts[-1]
        if has_audit_group:
            parts = audit_group_value.split('.')
            if len(parts) != 3:
                raise ValueError(
                    f"domain_audit_group must be a 3-part dotted name "
                    f"(e.g. 'ores.dq.audit_record'), got: "
                    f"'{audit_group_value}' ({len(parts)} parts)"
                )
            # e.g. ores.dq.audit_record → 'ores::dq::domain::audit_record'
            domain_entity['audit_group_qualified'] = (
                f"{parts[0]}::{parts[1]}::domain::{parts[2]}"
            )
        for col in domain_entity.get('columns', []):
            col['is_identity_group_column'] = (
                has_identity_group and col.get('group', '') == 'identity'
            )
        # Auto-inject identity/audit group headers into cpp.includes.domain so
        # models only need to list their own direct (non-group-field) includes.
        if has_identity_group or has_audit_group:
            cpp = domain_entity.setdefault('cpp', {})
            includes_dict = cpp.setdefault('includes', {})
            existing_domain = list(includes_dict.get('domain', []))
            injected = []
            if has_audit_group:
                parts = audit_group_value.split('.')
                injected.append(f'"{parts[0]}.{parts[1]}.api/domain/{parts[2]}.hpp"')
            if has_identity_group:
                parts = identity_group_value.split('.')
                injected.append(f'"{parts[0]}.{parts[1]}.api/domain/{parts[2]}.hpp"')
            includes_dict['domain'] = sorted(injected) + existing_domain
        if 'natural_keys' in domain_entity:
            _mark_last_item(domain_entity['natural_keys'])
            # Add iterator_var and is_uuid/is_int to natural_keys for protocol serialization
            for key in domain_entity['natural_keys']:
                key['iter_var'] = iter_var
                key['is_uuid'] = key.get('type') == 'uuid' or 'boost::uuids::uuid' in key.get('cpp_type', '')
                key['is_int'] = key.get('cpp_type', '') in ('int', 'long', 'std::size_t') or key.get('type', '') == 'integer'
                key['is_timestamp'] = ('time_point' in key.get('cpp_type', '') or
                                       key.get('type', '') in ('timestamp', 'timestamptz', 'timestamp with time zone'))
                key['is_date'] = (key.get('cpp_type', '') == 'std::chrono::year_month_day' or
                                  key.get('type', '') == 'date')
            nks = domain_entity['natural_keys']
            domain_entity['has_multiple_natural_keys'] = len(nks) > 1
            # Flag: UUID-PK entities with text natural keys need an idx counter in the generator
            domain_entity['has_text_natural_keys'] = any(
                not k.get('is_uuid') and not k.get('is_int')
                and not k.get('is_timestamp') and not k.get('is_date')
                for k in nks
            )
            domain_entity['has_enum_columns'] = any(c.get('is_enum') for c in domain_entity.get('columns', []))
            domain_entity['has_date_natural_keys'] = any(k.get('is_date') for k in nks)
            domain_entity['has_date_or_timestamp_natural_keys'] = any(
                k.get('is_date') or k.get('is_timestamp') for k in nks
            )
            domain_entity['needs_counter'] = (
                domain_entity.get('primary_key', {}).get('is_text', False)
                or domain_entity['has_text_natural_keys']
            )
            if len(nks) > 1:
                domain_entity['natural_keys_composite_columns'] = ', '.join(nk['column'] for nk in nks)
                if 'natural_keys_composite_name' not in domain_entity:
                    domain_entity['natural_keys_composite_name'] = '_'.join(nk['column'] for nk in nks)
        if 'indexes' in domain_entity:
            _mark_last_item(domain_entity['indexes'])
        if 'validations' in domain_entity:
            _mark_last_item(domain_entity['validations'])
        # Format description as comment block lines (for SQL)
        if 'description' in domain_entity:
            domain_entity['description_formatted'] = _format_description_as_comment(domain_entity['description'])
            # Split description into lines for C++ doxygen comments
            domain_entity['description_lines'] = domain_entity['description'].split('\n')
        # Derive component paths from component + subcomponent
        if 'component' in domain_entity:
            component = domain_entity['component']
            domain_entity['component_upper'] = component.upper()
            subcomponent = domain_entity.get('subcomponent', '')
            if subcomponent:
                domain_entity['component_include'] = f"{component}.{subcomponent}"
                domain_entity['component_dir'] = f"ores.{component}/{subcomponent}"
                domain_entity['component_core'] = f"{component}.core"
                domain_entity['component_core_dir'] = f"ores.{component}/core"
            else:
                domain_entity.setdefault('component_include', component)
                domain_entity['component_dir'] = f"ores.{component}"
                domain_entity.setdefault('component_core', component)
                domain_entity['component_core_dir'] = f"ores.{component}"
            domain_entity['component_include_upper'] = (
                domain_entity['component_include'].replace('.', '_').upper()
            )
            domain_entity['component_core_upper'] = (
                domain_entity['component_core'].replace('.', '_').upper()
            )
        if 'entity_singular' in domain_entity:
            domain_entity['entity_singular_upper'] = domain_entity['entity_singular'].upper()
            # Human-readable version (last word, e.g., "dataset_bundle" -> "bundle")
            words = domain_entity['entity_singular'].split('_')
            domain_entity['entity_singular_words'] = words[-1] if words else domain_entity['entity_singular']
            # PascalCase versions for Qt class names (e.g., "dataset_bundle" -> "DatasetBundle")
            domain_entity['entity_pascal'] = ''.join(w.capitalize() for w in words)
            domain_entity['entity_snake'] = domain_entity['entity_singular']
            domain_entity['entity_upper'] = domain_entity['entity_singular'].upper()
            # Short versions (last word only, e.g., "dataset_bundle" -> "Bundle")
            domain_entity['entity_pascal_short'] = words[-1].capitalize() if words else domain_entity['entity_singular'].capitalize()
            # Fallback plural: naive +s (overridden below when entity_plural is present)
            domain_entity['entity_pascal_short_plural'] = domain_entity['entity_pascal_short'] + 's'
        if 'entity_plural' in domain_entity:
            # Derive from entity_plural last word to get correct irregular plurals
            # (e.g. country→countries→Countries, book_status→book_statuses→Statuses)
            plural_words = domain_entity['entity_plural'].split('_')
            domain_entity['entity_pascal_short_plural'] = (
                plural_words[-1].capitalize() if plural_words else domain_entity['entity_plural'].capitalize()
            )
            domain_entity['entity_plural_upper'] = domain_entity['entity_plural'].upper()
        if 'entity_title' in domain_entity:
            domain_entity['entity_title_lower'] = domain_entity['entity_title'].lower()
        # Prepare table display items for C++ templates
        if 'cpp' in domain_entity:
            # Collect UUID column names for table display
            uuid_columns = set()
            optional_columns = set()
            if 'primary_key' in domain_entity and domain_entity['primary_key'].get('is_uuid'):
                uuid_columns.add(domain_entity['primary_key']['column'])
            if 'natural_keys' in domain_entity:
                for key in domain_entity['natural_keys']:
                    if key.get('is_uuid'):
                        uuid_columns.add(key['column'])
            if 'columns' in domain_entity:
                for col in domain_entity['columns']:
                    if col.get('is_uuid') or col.get('is_optional_uuid'):
                        uuid_columns.add(col['name'])
                    if col.get('nullable', False) and not col.get('is_uuid') and not col.get('is_nullable_string'):
                        optional_columns.add(col['name'])
            _prepare_table_display(domain_entity['cpp'], uuid_columns, optional_columns)
        # Copy repository section fields to top level for template access
        if 'repository' in domain_entity:
            for key, value in domain_entity['repository'].items():
                domain_entity[key] = value
        # Set defaults for messaging handler knobs if not provided by entity model.
        # Entities override via ** Repository section; these cover the common cases.
        pk = domain_entity.get('primary_key', {})
        pk_col = pk.get('column', 'id')
        pk_type = pk.get('type', 'uuid')
        domain_entity.setdefault(
            'delete_request_id_field',
            pk_col + 's' if pk_type == 'text' else 'ids')
        domain_entity.setdefault('history_request_id_field', pk_col)
        domain_entity.setdefault('single_delete', False)
        # Compute index_name_prefix: use sql.index_prefix when set, else entity_plural
        sql_section = domain_entity.get('sql', {})
        domain_entity['index_name_prefix'] = sql_section.get(
            'index_prefix', domain_entity.get('entity_plural', 'unknown'))
        # Compute has_tenant_in_pk: tenant_id is in the primary key when has_tenant_id
        # is set but neither system_scope nor nullable_tenant_id overrides the PK.
        has_tenant_id = domain_entity.get('has_tenant_id', False)
        domain_entity['has_tenant_in_pk'] = (
            has_tenant_id
            and not sql_section.get('system_scope', False)
            and not sql_section.get('nullable_tenant_id', False)
            and not sql_section.get('hypertable', False)
        )
        # Hypertable: suppress GIST and version locking; add create_hypertable block.
        if sql_section.get('hypertable', False):
            sql_section['hypertable'] = True
        # Bi-temporal soft-update/soft-delete trigger pattern (hypertable entities).
        sql_section['bitemporal_soft_update'] = (
            sql_section.get('bitemporal_trigger', '') == 'soft_update_delete'
        )
        # GIST exclusion: suppressed for hypertables (incompatible); active otherwise
        # for standard temporal entities with has_tenant_id.
        domain_entity['has_gist_exclusion'] = (
            not sql_section.get('hypertable', False)
            and sql_section.get('gist_exclusion', True)
        )
        # Audit columns (modified_by, performed_by, change_reason_code, change_commentary,
        # version): suppressed for hypertable time-series entities via #+no_audit_columns.
        domain_entity['has_audit_columns'] = not sql_section.get('no_audit_columns', False)
        # Mark last items in new iterable sql sub-sections for template rendering
        if 'fk_copy_validations' in sql_section:
            _mark_last_item(sql_section['fk_copy_validations'])
            for fkc in sql_section['fk_copy_validations']:
                if 'declare_vars' in fkc:
                    _mark_last_item(fkc['declare_vars'])
                if 'copy_empty' in fkc:
                    _mark_last_item(fkc['copy_empty'])
        if 'soft_fk_validations' in sql_section:
            _mark_last_item(sql_section['soft_fk_validations'])
        if 'text_code_validations' in sql_section:
            _mark_last_item(sql_section['text_code_validations'])
        if 'extra_delete_sets' in sql_section:
            _mark_last_item(sql_section['extra_delete_sets'])
        # Add computed properties for primary key type detection
        if 'primary_key' in domain_entity:
            pk = domain_entity['primary_key']
            pk_type = pk.get('type', 'uuid')
            pk['is_uuid'] = pk_type == 'uuid'
            pk['is_text'] = pk_type == 'text'
            if pk['is_uuid'] and 'uuid_check_fn' not in pk:
                pk['uuid_check_fn'] = 'ores_utility_nil_uuid_fn()'
            # Ensure cpp_type is set with sensible defaults
            if 'cpp_type' not in pk:
                if pk['is_uuid']:
                    pk['cpp_type'] = 'boost::uuids::uuid'
                else:
                    pk['cpp_type'] = 'std::string'
        # Process Qt-specific fields
        if 'qt' in domain_entity:
            qt = domain_entity['qt']
            # Auto-derive include paths and domain class from the domain entity
            # so models don't need to spell them out. Models may still override
            # by setting these fields explicitly.
            entity_singular = domain_entity.get('entity_singular', '')
            component = domain_entity.get('component', '')
            _subcomponent = domain_entity.get('subcomponent', '')
            _derived_component_include = (
                f'{component}.{_subcomponent}' if _subcomponent else component)
            component_include = domain_entity.get(
                'component_include', _derived_component_include)
            if 'domain_include' not in qt and entity_singular and component_include:
                qt['domain_include'] = (
                    f'ores.{component_include}/domain/{entity_singular}.hpp')
            if 'protocol_include' not in qt and entity_singular and component_include:
                qt['protocol_include'] = (
                    f'ores.{component_include}/messaging/{entity_singular}_protocol.hpp')
            if 'domain_class' not in qt and entity_singular and component:
                qt['domain_class'] = f'{component}::domain::{entity_singular}'
            if 'changed_event_class' not in qt and entity_singular and component:
                qt['changed_event_class'] = (
                    f'{component}::eventing::{entity_singular}_changed_event')
            if 'changed_event_include' not in qt and entity_singular and component_include:
                qt['changed_event_include'] = (
                    f'ores.{component_include}/eventing/{entity_singular}_changed_event.hpp')
            # Mark last item in columns for template iteration
            if 'columns' in qt:
                _mark_last_item(qt['columns'])
                # Compute has_description_column flag
                qt['has_description_column'] = any(
                    c.get('enum_name') == 'Description'
                    for c in qt['columns']
                )
                # Cross-reference qt columns with domain columns to flag optionals:
                # when the underlying domain column is std::optional<std::string>, the
                # Qt model needs to unwrap before QString::fromStdString.
                domain_col_types = {
                    c.get('name'): c.get('cpp_type', '')
                    for c in domain_entity.get('columns', [])
                }
                for idx, qt_col in enumerate(qt['columns']):
                    field = qt_col.get('field')
                    cpp_type = domain_col_types.get(field, '')
                    if qt_col.get('is_string') and cpp_type.startswith('std::optional<'):
                        qt_col['is_optional_string'] = True
                        qt_col['is_string'] = False
                    # Auto-assign column index for badge resolver calls
                    qt_col.setdefault('column_index', idx)
                    # Default column_style when not specified
                    if 'column_style' not in qt_col:
                        if qt_col.get('is_badge'):
                            qt_col['column_style'] = 'cs::badge_centered'
                        elif qt_col.get('is_int'):
                            qt_col['column_style'] = 'cs::mono_center'
                        else:
                            qt_col['column_style'] = 'cs::text_left'
                # Compute has_badge_columns flag
                qt['has_badge_columns'] = any(
                    c.get('is_badge') for c in qt['columns']
                )
            # Add iterator variable reference for templates
            qt['item_var'] = qt.get('item_var', 'item')
            # Auto-generate default detail_fields if not provided
            if 'detail_fields' not in qt:
                key_field = qt.get('key_field', 'code')
                column_names = {c.get('name') for c in domain_entity.get('columns', [])}
                fields = [
                    {'field': key_field, 'label': key_field.replace('_', ' ').title(),
                     'widget': 'codeEdit',
                     'type': 'line_edit', 'is_key': True, 'is_required': True,
                     'placeholder': 'Enter ' + domain_entity.get('entity_singular_words', 'item') + ' ' + key_field.replace('_', ' ')},
                    {'field': 'name', 'label': 'Name', 'widget': 'nameEdit',
                     'type': 'line_edit', 'is_required': True,
                     'placeholder': 'Enter display name'},
                ]
                if 'description' in column_names:
                    fields.append(
                        {'field': 'description', 'label': 'Description', 'widget': 'descriptionEdit',
                         'type': 'text_edit',
                         'placeholder': 'Enter a description'})
                qt['detail_fields'] = fields
            # Compute per-field flags for template iteration
            detail_fields = qt['detail_fields']
            required_fields = []
            required_dynamic_combo_fields = []
            domain_col_types = {
                c.get('name'): c.get('cpp_type', '')
                for c in domain_entity.get('columns', [])
            }
            for i, f in enumerate(detail_fields):
                f['is_line_edit'] = f.get('type') == 'line_edit'
                f['is_text_edit'] = f.get('type') in ('text_edit', 'plain_text_edit')
                f['is_static_combo'] = f.get('type') == 'static_combo'
                f['is_dynamic_combo'] = f.get('type') == 'dynamic_combo'
                f['is_check_box'] = f.get('type') == 'check_box'
                f['is_spin_box'] = f.get('type') == 'spin_box'
                field_cpp = domain_col_types.get(f.get('field'), '')
                f['is_optional_string'] = (
                    field_cpp.startswith('std::optional<std::string>')
                    and (f['is_line_edit'] or f['is_text_edit'])
                )
                # UUID type detection — needed for boost::uuids::to_string() conversions
                _is_any_uuid = 'boost::uuids::uuid' in field_cpp
                f['is_optional_uuid'] = 'std::optional<boost::uuids::uuid>' in field_cpp
                f['is_uuid'] = _is_any_uuid and not f['is_optional_uuid']
                # Tri-state checkbox for optional<bool>; normal two-state
                # for plain bool. Nullable spin box uses minimum as sentinel.
                f['is_tristate'] = (
                    f['is_check_box']
                    and field_cpp.startswith('std::optional<bool>')
                )
                f['is_nullable_int'] = (
                    f['is_spin_box']
                    and field_cpp.startswith('std::optional<int>')
                )
                f['is_double'] = (
                    f['is_line_edit']
                    and field_cpp in ('double', 'float')
                )
                # Default spin box range (overridable via model)
                if f['is_spin_box']:
                    f.setdefault('spin_min', -1 if f['is_nullable_int'] else 0)
                    f.setdefault('spin_max', 9999)
                f['_is_first'] = (i == 0)
                f['_is_last'] = (i == len(detail_fields) - 1)
                f['_row_index'] = i
                if not f.get('is_key'):
                    f['is_key'] = False
                if not f.get('is_required'):
                    f['is_required'] = False
                # Derive value_widget for history dialog (e.g. codeEdit->codeValue, nameCombo->nameValue)
                widget = f.get('widget', f['field'] + 'Edit')
                f['value_widget'] = widget.replace('Edit', 'Value').replace('Combo', 'Value')
                # Derive label_widget for detail dialog form labels (e.g. code -> labelCode)
                f['label_widget'] = 'label' + snake_to_pascal(f.get('field', ''))
                if f.get('is_required') and (f.get('is_line_edit') or f.get('is_static_combo')):
                    required_fields.append({
                        'field': f['field'],
                        'widget': f['widget'],
                        '_is_last': False,
                    })
                if f.get('is_required') and f.get('is_dynamic_combo'):
                    required_dynamic_combo_fields.append({
                        'field': f['field'],
                        'widget': f['widget'],
                        '_is_last': False,
                    })
            if required_fields:
                required_fields[-1]['_is_last'] = True
            if required_dynamic_combo_fields:
                required_dynamic_combo_fields[-1]['_is_last'] = True
            qt['required_fields'] = required_fields
            qt['required_dynamic_combo_fields'] = required_dynamic_combo_fields
            # Expose the key field's widget name for setCreateMode
            key_field_data = next((f for f in detail_fields if f.get('is_key')), None)
            qt['key_widget'] = key_field_data['widget'] if key_field_data else 'codeEdit'
            # Default has_pagination to False if not set
            qt['has_pagination'] = qt.get('has_pagination', False)
            qt['has_text_edit_fields'] = any(
                f.get('type') in ('text_edit', 'plain_text_edit') for f in detail_fields
            )
            qt['has_combo_fields'] = any(
                f.get('type') in ('static_combo', 'dynamic_combo') for f in detail_fields
            )
            qt['has_dynamic_combo_fields'] = any(
                f.get('type') == 'dynamic_combo' for f in detail_fields
            )
            qt['has_static_combo_fields'] = any(
                f.get('type') == 'static_combo' for f in detail_fields
            )
            qt['has_uuid_detail_fields'] = any(
                f.get('is_uuid') or f.get('is_optional_uuid') for f in detail_fields
            )
            # Delete request id field: protocol generates 'ids' for UUID PK and
            # '{pk_column}s' for text PK (matching cpp_protocol.hpp.mustache line 53).
            if qt.get('has_uuid_primary_key', False):
                qt.setdefault('delete_request_id_field', 'ids')
                qt['delete_request_id_is_plural'] = (
                    qt['delete_request_id_field'] != 'id')
            else:
                pk_col = domain_entity.get('primary_key', {}).get('column', '')
                if pk_col and 'delete_request_id_field' not in qt:
                    qt['delete_request_id_field'] = f'{pk_col}s'
                if 'delete_request_id_field' in qt:
                    qt.setdefault('delete_request_id_is_plural', True)
            # History response data field: protocol always uses 'history'.
            qt.setdefault('history_response_data_field', 'history')
            # Determine if the Qt key field is a UUID (needs to_string wrapping).
            # A key field is UUID when has_uuid_primary_key is true AND the key_field
            # matches the primary key column (i.e. the key field IS the UUID PK, not a
            # separate natural-key string like unit_code).
            pk_col_name = domain_entity.get('primary_key', {}).get('column', '')
            key_field_name = qt.get('key_field', '')
            key_field_is_uuid = (
                qt.get('has_uuid_primary_key', False) and
                key_field_name == pk_col_name
            )
            qt['key_field_is_uuid'] = key_field_is_uuid
            if key_field_is_uuid:
                qt['key_to_string_prefix'] = 'boost::uuids::to_string('
                qt['key_to_string_suffix'] = ')'
            else:
                qt['key_to_string_prefix'] = ''
                qt['key_to_string_suffix'] = ''
            qt['metadata_start_row'] = len(detail_fields)
            qt['metadata_start_row_plus_1'] = len(detail_fields) + 1
            qt['metadata_start_row_plus_2'] = len(detail_fields) + 2
            qt['metadata_start_row_plus_3'] = len(detail_fields) + 3
        # Add generator facet name with default (trade uses 'generator', refdata uses 'generators')
        domain_entity.setdefault('generator_facet_name', 'generators')
        domain_entity['generator_facet_name_upper'] = domain_entity['generator_facet_name'].upper()
        # Compute whether any columns are UUID-typed (for mapper includes)
        has_uuid_cols = any(
            col.get('is_uuid') or col.get('is_optional_uuid')
            for col in domain_entity.get('columns', [])
        )
        has_uuid_nat_keys = any(
            key.get('is_uuid')
            for key in domain_entity.get('natural_keys', [])
        )
        domain_entity['has_uuid_columns'] = (
            has_uuid_cols or has_uuid_nat_keys
            or domain_entity.get('primary_key', {}).get('is_uuid', False)
            or domain_entity.get('has_workspace_id', False)
        )
        data['domain_entity'] = domain_entity

    # Special processing for junction models
    if is_junction and isinstance(model, dict) and 'junction' in model:
        junction = model['junction']
        # Get iterator_var from cpp section for column processing
        iter_var = junction.get('cpp', {}).get('iterator_var', 'm')
        if 'columns' in junction:
            _mark_last_item(junction['columns'])
            _format_columns_for_doxygen(junction['columns'])
            # Add type flags and iterator_var for protocol serialization
            for col in junction['columns']:
                col['is_int'] = col.get('type') == 'integer' or col.get('cpp_type') == 'int'
                is_uuid_type = col.get('type') == 'uuid' or 'boost::uuids::uuid' in col.get('cpp_type', '')
                col['is_uuid'] = is_uuid_type and not col.get('nullable', False)
                col['is_optional_uuid'] = is_uuid_type and col.get('nullable', False)
                col['iter_var'] = iter_var
        # Add lowercase versions and UUID flags for left/right columns
        if 'left' in junction:
            if 'column_title' in junction['left']:
                junction['left']['column_title_lower'] = junction['left']['column_title'].lower()
            junction['left']['is_uuid'] = junction['left'].get('type') == 'uuid'
        if 'right' in junction:
            if 'column_title' in junction['right']:
                junction['right']['column_title_lower'] = junction['right']['column_title'].lower()
            junction['right']['is_uuid'] = junction['right'].get('type') == 'uuid'
        junction['has_uuid_left_or_right'] = (
            junction.get('left', {}).get('is_uuid', False) or
            junction.get('right', {}).get('is_uuid', False)
        )
        # Format description as comment block lines (for SQL)
        if 'description' in junction:
            junction['description_formatted'] = _format_description_as_comment(junction['description'])
            # Split description into lines for C++ doxygen comments
            junction['description_lines'] = junction['description'].split('\n')
        # Add uppercase versions for C++ include guards
        if 'component' in junction:
            junction['component_upper'] = junction['component'].upper()
        if 'name_singular' in junction:
            junction['name_singular_upper'] = junction['name_singular'].upper()
            # Human-readable version - use explicit value or derive from last word
            if 'name_singular_words' not in junction:
                words = junction['name_singular'].split('_')
                junction['name_singular_words'] = words[-1] if words else junction['name_singular']
        if 'name' in junction:
            junction['name_upper'] = junction['name'].upper()
        if 'name_title' in junction:
            junction['name_title_lower'] = junction['name_title'].lower()
        # Prepare table display items for C++ templates
        if 'cpp' in junction:
            # Collect UUID column names for table display
            uuid_columns = set()
            if junction.get('left', {}).get('is_uuid'):
                uuid_columns.add(junction['left']['column'])
            if junction.get('right', {}).get('is_uuid'):
                uuid_columns.add(junction['right']['column'])
            if 'columns' in junction:
                for col in junction['columns']:
                    if col.get('is_uuid'):
                        uuid_columns.add(col['name'])
            _prepare_table_display(junction['cpp'], uuid_columns)
        # Copy repository section fields to top level for template access
        if 'repository' in junction:
            for key, value in junction['repository'].items():
                junction[key] = value
        data['junction'] = junction

    # Special processing for field-group models
    if is_field_group and isinstance(model, dict) and 'field_group' in model:
        fg = model['field_group']
        # Split description into lines for C++ doxygen comments. The ' * '
        # prefix is baked in here (rstripped on blank lines) so the emitted
        # comment block carries no trailing whitespace.
        if fg.get('description'):
            fg['description_lines'] = [
                (' * ' + line).rstrip()
                for line in fg['description'].split('\n')
            ]
        # Derive include-guard and path components from component + subcomponent
        component = fg.get('component', 'unknown')
        subcomponent = fg.get('subcomponent', '')
        if subcomponent:
            fg['component_include'] = f"{component}.{subcomponent}"
            fg['component_dir'] = f"ores.{component}/{subcomponent}"
        else:
            fg.setdefault('component_include', component)
            fg['component_dir'] = f"ores.{component}"
        fg['component_include_upper'] = fg['component_include'].replace('.', '_').upper()
        # Compute include-guard suffix from entity_singular
        if 'entity_singular' in fg:
            fg['entity_singular_upper'] = fg['entity_singular'].upper()
        # Mark the last field so the template can omit the separator blank
        # line after it (no stray blank before the closing brace).
        if fg.get('fields'):
            fg['fields'][-1]['last'] = True
        data['field_group'] = fg

    # Special processing for enum models
    if is_enum and isinstance(model, dict) and 'enum' in model:
        enum = model['enum']
        enum_name = enum.get('name', 'unknown')
        # Mark last value for comma handling in template
        # Also add enum_name and is_sentinel to each value for case statements
        if 'values' in enum:
            _mark_last_item(enum['values'])
            for val in enum['values']:
                val['enum_name'] = enum_name
                # Mark sentinel values explicitly for template logic
                comment = val.get('comment', '').lower()
                val['is_sentinel'] = 'sentinel' in comment
        # Split description into lines for C++ doxygen comments
        if 'description' in enum:
            enum['description_lines'] = enum['description'].split('\n')
        # Add uppercase versions for C++ include guards
        if 'component' in enum:
            enum['component_upper'] = enum['component'].upper()
        if 'name' in enum:
            enum['name_upper'] = enum['name'].upper()
        data['enum'] = enum

    # Find the git directory to calculate relative paths
    current_path = Path.cwd()
    git_path = None
    search_path = current_path
    while search_path.parent != search_path:  # Stop at root directory
        if (search_path / '.git').exists():
            git_path = search_path
            break
        search_path = search_path.parent

    # If no git directory found, use the current directory as base
    if git_path is None:
        git_path = current_path

    # Process each associated template
    for template_name in templates_to_process:
        template_path = templates_dir / template_name
        if not template_path.exists():
            print(f"Template not found: {template_path}")
            continue

        # Render the template with the combined data
        rendered_content = render_template(template_path, data)

        # Post-render: substitute <<paste:UUID>> markers with implementations
        # gathered from the entity model (org-mode literate fragment mechanism).
        rendered_content = _substitute_paste_markers(rendered_content, data)

        # Determine output filename
        if target_output:
            output_filename = target_output
        elif generate_cpp and is_domain_entity and 'domain_entity' in data:
            # C++ generation for domain entity
            domain_entity = data['domain_entity']
            component = domain_entity.get('component', 'unknown')
            entity_singular = domain_entity.get('entity_singular', 'unknown')
            # Find the mapping for this template
            cpp_mappings = get_cpp_domain_entity_template_mappings()
            mapping = next(((t, d, s) for t, d, s in cpp_mappings if t == template_name), None)
            if mapping:
                output_dir_pattern, suffix = mapping[1], mapping[2]
                # Replace {component} placeholder
                sub_dir = output_dir_pattern.replace('{component}', f'ores.{component}')
                output_filename = f"{sub_dir}/{entity_singular}{suffix}"
            else:
                output_filename = f"{entity_singular}.hpp"
        elif generate_cpp and is_junction and 'junction' in data:
            # C++ generation for junction
            junction = data['junction']
            component = junction.get('component', 'unknown')
            name_singular = junction.get('name_singular', 'unknown')
            # Find the mapping for this template
            cpp_mappings = get_cpp_junction_template_mappings()
            mapping = next(((t, d, s) for t, d, s in cpp_mappings if t == template_name), None)
            if mapping:
                output_dir_pattern, suffix = mapping[1], mapping[2]
                # Replace {component} placeholder
                sub_dir = output_dir_pattern.replace('{component}', f'ores.{component}')
                output_filename = f"{sub_dir}/{name_singular}{suffix}"
            else:
                output_filename = f"{name_singular}.hpp"
        elif generate_qt and is_domain_entity and 'domain_entity' in data and 'qt' in data['domain_entity']:
            # Qt generation for domain entity — only when the model has a ** Qt section.
            domain_entity = data['domain_entity']
            entity_pascal = domain_entity.get('entity_pascal', 'Unknown')
            # Find the mapping for this template
            qt_mappings = get_qt_domain_entity_template_mappings()
            mapping = next(((t, d, s) for t, d, s in qt_mappings if t == template_name), None)
            if mapping:
                sub_dir, suffix = mapping[1], mapping[2]
                # Client model uses "Client" prefix
                if 'client_model' in template_name:
                    output_filename = f"{sub_dir}/Client{entity_pascal}{suffix}"
                else:
                    output_filename = f"{sub_dir}/{entity_pascal}{suffix}"
            else:
                output_filename = f"{entity_pascal}.hpp"
        elif is_domain_entity and 'domain_entity' in data:
            # For domain entity models, derive filename from domain_entity definition
            # Use entity_singular for filename (table/indexes/functions use entity_plural)
            domain_entity = data['domain_entity']
            component = domain_entity.get('component', 'unknown')
            entity_singular = domain_entity.get('entity_singular', 'unknown')
            domain_entity_mappings = get_domain_entity_template_mappings()
            suffix = next((s for t, s in domain_entity_mappings if t == template_name), '_create.sql')
            output_filename = f"{component}_{entity_singular}{suffix}"
        elif is_junction and 'junction' in data:
            # For junction table models, derive filename from junction definition
            # Use name_singular for filename (table/indexes/functions use name)
            junction = data['junction']
            component = junction.get('component', 'unknown')
            name_singular = junction.get('name_singular', 'unknown')
            junction_mappings = get_junction_template_mappings()
            suffix = next((s for t, s in junction_mappings if t == template_name), '_create.sql')
            output_filename = f"{component}_{name_singular}{suffix}"
        elif is_enum and 'enum' in data:
            # For enum models, derive filename from enum definition
            enum = data['enum']
            enum_name = enum.get('name', 'unknown')
            enum_mappings = get_enum_template_mappings()
            suffix = next((s for t, s in enum_mappings if t == template_name), '.hpp')
            output_filename = f"{enum_name}{suffix}"
        elif is_schema_model and 'entity' in data:
            # For entity schema models, derive filename from entity definition
            entity = data['entity']
            component = entity.get('component', 'unknown')
            entity_plural = entity.get('entity_plural', 'unknown')
            # Find the suffix for this template
            schema_mappings = get_schema_template_mappings()
            suffix = next((s for t, s in schema_mappings if t == template_name), '_create.sql')
            # Artefact tables and population functions use 'dq_' prefix
            if 'artefact' in template_name or 'populate_function' in template_name:
                output_filename = f"dq_{entity_plural}{suffix}"
            else:
                output_filename = f"{component}_{entity_plural}{suffix}"
        else:
            output_ext = '.sql' if template_name.endswith('.mustache') else ''
            output_filename = template_name.replace('.mustache', output_ext)

        # Apply prefix if provided, replacing 'sql_' with prefix + '_'
        # Skip prefix handling for schema/domain_entity/junction/enum models (they use entity-based naming)
        if prefix and not is_schema_model and not is_domain_entity and not is_junction and not is_enum:
            # Special case: the master include is {prefix}_populate.sql — the
            # convention every other populate/<dir>/<dir>_populate.sql master
            # follows (and the name catalogues_populate.sql \ir's).
            if template_name == 'sql_batch_execute.mustache':
                output_filename = f"{prefix}_populate.sql"
            elif output_filename.startswith('sql_'):
                output_filename = f"{prefix}_{output_filename[4:]}"
            elif not output_filename.startswith(f"{prefix}_"):
                output_filename = f"{prefix}_{output_filename}"

        output_path = output_dir / output_filename

        # Create parent directories if needed (for C++ templates with subdirectories)
        output_path.parent.mkdir(parents=True, exist_ok=True)

        # Write output to file
        with open(output_path, 'w', encoding='utf-8') as f:
            f.write(rendered_content)

        # Calculate and show relative path
        relative_path = get_relative_path(output_path.resolve(), git_path)
        print(f"Generated {relative_path}")


def generate_flag_svg(country_code_num):
    """
    Generate a deterministic SVG flag based on a country code number.

    Args:
        country_code_num (int or str): A number representing the country code

    Returns:
        str: SVG string for the flag
    """
    # Convert to integer if it's a string
    if isinstance(country_code_num, str):
        # Convert string like "AL" to a number for deterministic generation
        num = 0
        for char in country_code_num.upper():
            num = num * 100 + ord(char)  # Use ASCII values to create a unique number
    else:
        num = int(country_code_num)

    # Use the number to deterministically generate colors and patterns
    # Set seed to ensure deterministic output for the same input
    random.seed(num)

    # Generate random but deterministic colors based on the seed
    r1, g1, b1 = random.randint(0, 255), random.randint(0, 255), random.randint(0, 255)
    r2, g2, b2 = random.randint(0, 255), random.randint(0, 255), random.randint(0, 255)
    r3, g3, b3 = random.randint(0, 255), random.randint(0, 255), random.randint(0, 255)

    # Choose a flag pattern based on the number
    pattern_choice = num % 4

    if pattern_choice == 0:
        # Horizontal stripes
        svg = f'''<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="160" y="0" fill="#{r1:02x}{g1:02x}{b1:02x}"/>
  <rect width="640" height="160" y="160" fill="#{r2:02x}{g2:02x}{b2:02x}"/>
  <rect width="640" height="160" y="320" fill="#{r3:02x}{g3:02x}{b3:02x}"/>
</svg>'''
    elif pattern_choice == 1:
        # Vertical stripes
        svg = f'''<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#{r1:02x}{g1:02x}{b1:02x}"/>
  <rect width="213.33" height="480" x="213.33" fill="#{r2:02x}{g2:02x}{b2:02x}"/>
  <rect width="213.34" height="480" x="426.66" fill="#{r3:02x}{g3:02x}{b3:02x}"/>
</svg>'''
    elif pattern_choice == 2:
        # Diagonal pattern
        svg = f'''<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="480" fill="#{r1:02x}{g1:02x}{b1:02x}"/>
  <polygon points="0,0 200,0 640,480 440,480" fill="#{r2:02x}{g2:02x}{b2:02x}"/>
  <polygon points="440,0 640,0 640,200 600,240 560,280 520,320 480,360 440,400 400,440 400,480 240,480 240,440 200,400 160,360 120,320 80,280 40,240 0,200 0,0" fill="#{r3:02x}{g3:02x}{b3:02x}"/>
</svg>'''
    else:
        # Central emblem pattern
        svg = f'''<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="480" fill="#{r1:02x}{g1:02x}{b1:02x}"/>
  <circle cx="320" cy="240" r="80" fill="#{r2:02x}{g2:02x}{b2:02x}"/>
  <rect x="280" y="160" width="80" height="160" fill="#{r3:02x}{g3:02x}{b3:02x}"/>
  <rect x="240" y="200" width="160" height="80" fill="#{r3:02x}{g3:02x}{b3:02x}"/>
</svg>'''

    return svg


def _resolve_file_references(model_data, model_dir, global_data):
    """
    Resolve file references in the model data by reading content from external files.

    Args:
        model_data (dict or list): The model data that may contain file references
        model_dir (Path): Directory where the model file is located
        global_data (dict): Global data dictionary to update with resolved content
    """
    if isinstance(model_data, dict):
        # Collect keys to process to avoid modifying dict during iteration
        keys_to_process = []
        for key, value in model_data.items():
            if key.endswith('_file') and isinstance(value, str):
                keys_to_process.append(key)
            elif isinstance(value, (dict, list)):
                # Recursively resolve references in nested structures
                _resolve_file_references(value, model_dir, global_data)

        # Process the collected keys
        for key in keys_to_process:
            value = model_data[key]
            # This is a file reference, read the content
            file_path = model_dir / value
            if file_path.exists():
                with open(file_path, 'r', encoding='utf-8') as f:
                    # Replace the _file key with content under the base key
                    base_key = key[:-5]  # Remove '_file' suffix
                    model_data[base_key] = f.read()
                    # Remove the _file reference
                    del model_data[key]
            else:
                print(f"Warning: Referenced file not found: {file_path}")
    elif isinstance(model_data, list):
        for item in model_data:
            if isinstance(item, (dict, list)):
                _resolve_file_references(item, model_dir, global_data)


