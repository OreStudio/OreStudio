"""
Simple code generator that loads data and applies templates.
"""
import copy
import functools
import json
import re
import os
import random
from pathlib import Path
from typing import Any
import pystache
from datetime import datetime

_MODELINE_RE = re.compile(r"^\*{3}\s+\S+\.(\S+)\s+:modeline:\s*$")
_CODEC_VALUE_RE = re.compile(r"^:masd\.codec\.value:\s+(.+?)\s*$")
_ORG_TYPE_RE = re.compile(r"^#\+type:\s*(\S+)\s*$", re.MULTILINE | re.IGNORECASE)

# Templates whose context needs a directory-scanned `files` list injected
# (see the component['files'] assignment below) rather than pure model data.
_COMPONENT_FILES_TEMPLATES = {
    "cmake_component_files_src.mustache",
    "cmake_component_files_tests.mustache",
}

# Maps #+type: frontmatter values to model-type strings.
_ORG_TYPE_TO_MODEL_TYPE = {
    "ores.codegen.entity":           "domain_entity",
    "ores.codegen.junction":         "junction",
    "ores.codegen.component":        "component",
    "ores.codegen.field_group":      "field_group",
    "ores.codegen.lookup_entity":    "schema",
    "ores.codegen.service_registry": "service_registry",
    "ores.codegen.dataset":          "dataset",
    "ores.codegen.oresmd_quote_type": "oresmd_quote_type",
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
    # _table.org has no model_type of its own anymore (that type was
    # retired), but keeping it excluded here means a stray file with
    # that suffix still falls through to 'unknown' rather than being
    # silently misclassified as domain_entity by the generic
    # ores.*.org pattern below.
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
             'component', 'field_group', 'service_registry', or 'unknown')

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
    elif is_service_registry_model(model_filename):
        return 'service_registry'
    elif is_component_model(model_filename):
        return 'component'
    elif is_enum_model(model_filename):
        return 'enum'
    elif is_entity_schema_model(model_filename):
        return 'schema'
    return 'unknown'


def snake_to_pascal(snake_str):
    """
    Convert a snake_case string to PascalCase.

    Args:
        snake_str (str): Snake case string (e.g., "dataset_bundle")

    Returns:
        str: PascalCase string (e.g., "DatasetBundle")
    """
    return ''.join(word.capitalize() for word in snake_str.split('_'))


def compute_view_groups(detail_fields):
    """Group a fully-enriched Qt detail_fields list by their optional
    view_group cell into Qt detail-dialog tabs (see
    codegen_input_org_schema.org), preserving first-appearance order both
    within a group and across groups.

    Fields with no view_group all land in one implicit "General" group; in
    that case (no entity field ever set view_group) the single group
    reproduces the exact legacy single-tab widget names/title -- including
    the group box's "Basic Information" title, distinct from the tab's own
    "General" title -- byte-for-byte, so adding view_group support has zero
    effect on any entity that doesn't use it. This is the mechanism behind
    the backward-compatibility guarantee: adding view_group values to one
    entity's model cannot change any other entity's generated .ui.

    Each returned group dict also gets a '_group_row_index' set on every one
    of its own detail_fields entries (0-based within that group, for the
    per-tab QFormLayout's row= attribute -- distinct from the field's own
    global '_row_index', which numbers it within the whole flat list and is
    used elsewhere, e.g. the history dialog's single flat form).

    Grouping identity uses the same normalized key as the derived Qt widget
    names (lowercase, non-alnum stripped), not the raw view_group string --
    two rows differing only by case or incidental whitespace (e.g.
    "Rounding" vs "rounding ") must fold into the same tab, or they'd
    silently derive identical widget names for two distinct groups, and
    uic would reject the generated .ui as a duplicate-widget name. The
    first-seen raw string is kept as the display title.
    """
    uses_view_group = any(f.get('view_group') for f in detail_fields)
    groups: dict[str, list] = {}
    display_names: dict[str, str] = {}
    for f in detail_fields:
        group_name = f.get('view_group') or 'General'
        group_key = re.sub(r'[^0-9a-zA-Z]+', '_', group_name).strip('_').lower() or 'general'
        if group_key not in groups:
            groups[group_key] = []
            display_names[group_key] = group_name
        groups[group_key].append(f)

    view_groups = []
    for group_key, group_fields in groups.items():
        group_name = display_names[group_key]
        for gi, f in enumerate(group_fields):
            f['_group_row_index'] = gi
        if not uses_view_group:
            view_groups.append({
                'name': group_name,
                'detail_fields': group_fields,
                'tab_widget_name': 'generalTab',
                'tab_layout_name': 'generalLayout',
                'group_box_name': 'basicInfoGroup',
                'group_box_title': 'Basic Information',
                'form_layout_name': 'formLayout',
                'spacer_name': 'verticalSpacer',
            })
            continue
        group_pascal = snake_to_pascal(group_key)
        group_camel = group_pascal[0].lower() + group_pascal[1:] if group_pascal else 'general'
        view_groups.append({
            'name': group_name,
            'detail_fields': group_fields,
            'tab_widget_name': group_camel + 'Tab',
            'tab_layout_name': group_camel + 'Layout',
            'group_box_name': group_camel + 'Group',
            'group_box_title': group_name,
            'form_layout_name': group_camel + 'FormLayout',
            # Distinct per tab -- uic flattens every named widget into one
            # Ui_* member namespace regardless of nesting, so reusing
            # "verticalSpacer" across tabs would collide.
            'spacer_name': group_camel + 'Spacer',
        })

    view_groups[0]['_is_first'] = True
    for vg in view_groups[1:]:
        vg['_is_first'] = False
    return view_groups


def _component_path_vars(entity):
    """Derive the component/subcomponent path placeholders
    (``component_dir``, ``component_include``, ``component_core``, ...)
    shared by ``domain_entity`` and ``junction`` output-path resolution,
    from a dict carrying ``component``/``subcomponent``/``component_include``/
    ``component_core``/``component_service``/``generator_facet_name``/
    ``cached_by`` keys (whichever of those the model set).

    Returns:
        dict: placeholder name -> resolved value.
    """
    component = entity.get('component', 'unknown')
    # qt_component: override for entities whose Qt UI lives in a
    # different ores.qt.* project than the entity's own component (e.g.
    # a variability system_setting entity whose UI is grouped into
    # ores.qt.iam alongside accounts/roles/tenants rather than getting
    # its own ores.qt.variability project). Defaults to component so
    # every entity that doesn't set this keeps generating into
    # ores.qt.{component} as before.
    qt_component = entity.get('qt_component', component)
    subcomponent = entity.get('subcomponent', '')
    if subcomponent:
        component_include = f"{component}.{subcomponent}"
        component_dir = f"ores.{component}/{subcomponent}"
        component_core = f"{component}.core"
        component_core_dir = f"ores.{component}/core"
        component_service = f"{component}.service"
        component_service_dir = f"ores.{component}/service"
    else:
        component_include = entity.get('component_include', component)
        component_dir = f"ores.{component}"
        component_core = entity.get('component_core', component)
        component_core_dir = f"ores.{component}"
        component_service = entity.get('component_service', component)
        component_service_dir = f"ores.{component}"

    generator_facet_name = entity.get('generator_facet_name', 'generators')

    # cached_by: the consumer component a nats-event-cache archetype's
    # output belongs to (e.g. party is defined in refdata but its
    # generated cache compiles into iam), distinct from the entity's
    # own component used by every other facet above. Accepts the same
    # "component.subcomponent" dotted form component_include already
    # uses (e.g. cached_by: refdata.client) for a consumer whose cache
    # doesn't land in its default "core" subcomponent -- no separate
    # model flag; cache_component/cache_subcomponent below are purely
    # derived, like every other _upper/_dir variable in this function.
    cached_by_raw = entity.get('cached_by', component)
    if '.' in cached_by_raw:
        cache_component, cache_subcomponent = cached_by_raw.split('.', 1)
    else:
        cache_component, cache_subcomponent = cached_by_raw, 'core'
    cache_component_dir = f"ores.{cache_component}"

    return {
        'component': component,
        'qt_component': qt_component,
        'component_dir': component_dir,
        'component_core_dir': component_core_dir,
        'component_service_dir': component_service_dir,
        'component_include': component_include,
        'component_core': component_core,
        'component_service': component_service,
        'cache_component_dir': cache_component_dir,
        'cache_component': cache_component,
        'cache_subcomponent': cache_subcomponent,
        'generator_facet_name': generator_facet_name,
    }


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
        path_vars = _component_path_vars(entity)
        entity_singular = entity.get('entity_singular', 'unknown')
        entity_plural = entity.get('entity_plural', entity_singular + 's')
        entity_pascal = snake_to_pascal(entity_singular)

        for placeholder, value in path_vars.items():
            result = result.replace('{' + placeholder + '}', value)
        result = result.replace('{entity_plural}', entity_plural)
        result = result.replace('{entity}', entity_singular)
        result = result.replace('{EntityPascal}', entity_pascal)

    elif model_type == 'junction' and 'junction' in model_data:
        junction = model_data['junction']
        path_vars = _component_path_vars(junction)
        junction_name = junction.get('name', 'unknown')
        name_singular = junction.get('name_singular', junction_name.rstrip('s'))
        entity_pascal = snake_to_pascal(name_singular)

        for placeholder, value in path_vars.items():
            result = result.replace('{' + placeholder + '}', value)
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

    elif model_type == 'schema' and 'entity' in model_data:
        # Lookup-entity models (#+type: ores.codegen.lookup_entity) load into
        # the 'entity' key but route through the 'schema' model_type, sharing
        # sql_schema_table_create.mustache's output path shape.
        entity = model_data['entity']
        path_vars = _component_path_vars(entity)
        entity_singular = entity.get('entity_singular', 'unknown')
        entity_plural = entity.get('entity_plural', entity_singular + 's')
        entity_pascal = snake_to_pascal(entity_singular)

        for placeholder, value in path_vars.items():
            result = result.replace('{' + placeholder + '}', value)
        result = result.replace('{entity_plural}', entity_plural)
        result = result.replace('{entity}', entity_singular)
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

    Only consulted by the legacy target_template=None dispatch path in
    generate_from_model (unreachable from the live physical-space
    codegen entity CLI, which always resolves and passes an explicit
    target_template/target_output). The domain_entity SQL archetype is
    sql_schema_domain_entity_create.mustache, resolved via the
    ores.sql.schema.domain_entity_create physical-space node, not
    through this table.

    Returns:
        list: List of tuples (template_name, output_suffix) for domain entity generation
    """
    return []


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
    UUID under ``domain_entity`` (or ``junction``, for junction models).
    For each marker, look up the matching list of code bodies, join them
    with a blank line, and substitute.

    Missing kinds produce an empty substitution and the entire marker line
    (including its newline) is collapsed, leaving no trace in the output.
    This makes templates safe to include markers that no current entity
    implements.

    UUIDs are matched case-insensitively: a template marker and a model's
    :implements property are free-standing text authored independently
    (often by different sessions), and a UUID is canonically
    case-insensitive -- a case mismatch between the two must not silently
    drop an author's paste block, which a case-sensitive dict lookup would
    do without any error or warning."""
    de = data.get("domain_entity") or data.get("junction") or {}
    impls = de.get("implementations") or {}
    impls_lower = {k.lower(): v for k, v in impls.items()}

    def replace(match):
        kind = match.group(1).lower()
        blocks = impls_lower.get(kind)
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
            load_org_lookup_entity_model,
            load_org_oresmd_quote_type_model,
            load_org_service_registry_model,
            load_org_component_model,
            load_org_component_overview_model,
            load_org_dataset_model,
        )
        # Prefer #+type: frontmatter over filename suffix.
        org_type = _read_org_type(model_path)
        if org_type == 'dataset':
            return load_org_dataset_model(model_path)
        if org_type == 'oresmd_quote_type':
            return load_org_oresmd_quote_type_model(model_path)
        if org_type == 'field_group':
            return load_org_field_group_model(model_path)
        if org_type == 'junction':
            return load_org_junction_model(model_path)
        if org_type == 'schema':
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


def _prepare_table_display(cpp_section, uuid_columns=None, optional_columns=None, bool_columns=None):
    """
    Prepare table_display items by adding iterator_var and is_uuid to each item.

    Mustache can't access parent context variables from within a loop,
    so we add the iterator_var to each table_display item. We also flag
    UUID columns so the table template can wrap them with to_string(),
    optional columns so the template can unwrap them before streaming to
    libfort (which has no operator<< for std::optional), and non-optional
    bool columns so the template can render "true"/"false" instead of
    libfort integer-promoting a raw bool to 0/1.

    Args:
        cpp_section (dict): The 'cpp' section of the model
        uuid_columns (set): Set of column names that are UUID type
        optional_columns (set): Set of column names that are std::optional<T>
        bool_columns (set): Set of column names that are plain bool
    """
    if 'table_display' not in cpp_section:
        return

    uuid_columns = uuid_columns or set()
    optional_columns = optional_columns or set()
    bool_columns = bool_columns or set()
    iter_var = cpp_section.get('iterator_var', 'e')
    has_uuid = False
    has_optional = False
    for item in cpp_section['table_display']:
        item['iter_var'] = iter_var
        item['is_uuid'] = item['column'] in uuid_columns
        item['is_optional'] = item['column'] in optional_columns
        item['is_bool'] = item['column'] in bool_columns and item['column'] not in optional_columns
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


def validate_read_for_cache(domain_entity):
    """
    Validate and default the read_for_cache messaging flag: a bulk
    unpaginated read of a tenant's active entities, used to warm
    client-side caches. Requires tenant scoping.

    Args:
        domain_entity (dict): mutated in place; defaults read_for_cache
            to False if unset.

    Raises:
        ValueError: if read_for_cache is set without has_tenant_id.
    """
    if domain_entity.get('read_for_cache') and not domain_entity.get('has_tenant_id'):
        raise ValueError(
            f"{domain_entity.get('entity_singular', '?')}: read_for_cache requires has_tenant_id")
    domain_entity.setdefault('read_for_cache', False)


def validate_cached_by(domain_entity):
    """
    Validate the cached_by messaging flag: the consumer component a
    generated nats-event-cache lives in (e.g. party is defined in refdata
    but cached_by: iam moves its generated cache into ores.iam). Requires
    read_for_cache, since the generated cache warms/reloads itself via
    that RPC.

    Args:
        domain_entity (dict): not mutated; cached_by has no default (its
            absence simply means no nats-event-cache archetype applies).

    Raises:
        ValueError: if cached_by is set without read_for_cache.
    """
    if domain_entity.get('cached_by') and not domain_entity.get('read_for_cache'):
        raise ValueError(
            f"{domain_entity.get('entity_singular', '?')}: cached_by requires read_for_cache")


def validate_cache_aux_type(domain_entity):
    """
    Validate the cache_aux_type messaging flag: the C++ type name of a
    nats-event-cache's optional aux index (e.g. party's parent/child
    children_map), supplied via the entity's own paste-point blocks.
    Requires cached_by, since an aux index with no generated cache to
    attach it to is meaningless.

    Args:
        domain_entity (dict): not mutated; cache_aux_type has no default
            (its absence simply means the generated cache's AuxIndex
            stays std::monostate).

    Raises:
        ValueError: if cache_aux_type is set without cached_by.
    """
    if domain_entity.get('cache_aux_type') and not domain_entity.get('cached_by'):
        raise ValueError(
            f"{domain_entity.get('entity_singular', '?')}: cache_aux_type requires cached_by")


def validate_rls_isolation(domain_entity):
    """
    Validate the rls_party_isolation sql flag: the AS RESTRICTIVE party
    policy is emitted inside the tenant-isolation block (it ANDs with the
    permissive tenant policy), so party isolation without tenant isolation
    would silently emit no RLS at all for the entity.

    Args:
        domain_entity (dict): not mutated.

    Raises:
        ValueError: if rls_party_isolation is set without rls_tenant_isolation.
    """
    sql_section = domain_entity.get('sql', {})
    if sql_section.get('rls_party_isolation') and not sql_section.get('rls_tenant_isolation'):
        raise ValueError(
            f"{domain_entity.get('entity_singular', '?')}: rls_party_isolation requires "
            f"rls_tenant_isolation")


def validate_explorer_interface(domain_entity):
    """
    Validate the qt.explorer_interface knob: the name of an abstract
    interface a generated Controller additionally implements, for a
    cross-component explorer window that needs to drive openEdit/
    openHistory without linking against this entity's concrete Controller
    header. Requires has_explorer_api, since that's what generates the
    openEdit/openHistory methods the interface's pure virtuals need — a
    Controller with the extra base class but no has_explorer_api would
    inherit unimplemented pure virtuals and fail to compile as an
    abstract class, far from the actual misconfiguration.

    Args:
        domain_entity (dict): not mutated; qt.explorer_interface has no
            default (its absence simply means no extra interface applies).

    Raises:
        ValueError: if qt.explorer_interface is set without qt.has_explorer_api.
    """
    qt = domain_entity.get('qt', {})
    if qt.get('explorer_interface') and not qt.get('has_explorer_api'):
        raise ValueError(
            f"{domain_entity.get('entity_singular', '?')}: "
            f"qt.explorer_interface requires qt.has_explorer_api")


def has_as_of_combo_fields(detail_fields):
    """
    Whether any dynamic_combo detail field declares combo_as_of_fetch_fn --
    an `_at_timepoint(ClientManager*, QString)` sibling of its normal
    combo_fetch_fn, so a read-only/historical view of this entity resolves
    that combo's options as-of the entity's own recorded_at, not against
    the current (possibly since-renamed or deleted) lookup list. See the
    As-of lookup resolution codegen facet story.

    Args:
        detail_fields (list[dict]): the qt.detail_fields list; not mutated.

    Returns:
        bool: gates the datetime.hpp include and the
            setX()/setReadOnly() re-populate calls in the Qt detail-dialog
            template.
    """
    return any(
        f.get('combo_as_of_fetch_fn') for f in detail_fields
        if f.get('type') == 'dynamic_combo'
    )
def validate_parent_scoped_list(domain_entity):
    """
    Validate the qt.has_parent_scoped_list knob: scopes a
    has_readonly_paginated_list list window's get-request to an owning
    parent key (e.g. calendar_dates scoped by calendar_code). Requires
    both companion fields, since the parent key belongs to a different
    entity and neither can be derived -- an unset parent_key_param in
    particular would render a nameless member/parameter (e.g. `QString
    _;`) instead of failing fast here.

    Args:
        domain_entity (dict): not mutated; has_parent_scoped_list has no
            default of its own here (qt['has_parent_scoped_list'] is
            defaulted separately, after this validation runs).

    Raises:
        ValueError: if qt.has_parent_scoped_list is set without both
            qt.parent_key_field and qt.parent_key_param.
    """
    qt = domain_entity.get('qt', {})
    if qt.get('has_parent_scoped_list') and not (
            qt.get('parent_key_field') and qt.get('parent_key_param')):
        raise ValueError(
            f"{domain_entity.get('entity_singular', '?')}: "
            f"qt.has_parent_scoped_list requires both "
            f"qt.parent_key_field and qt.parent_key_param")


def _projects_dir_from(model_path: Path) -> Path:
    """The ``projects/`` directory that owns the given entity model org."""
    for parent in Path(model_path).resolve().parents:
        if parent.name == 'projects':
            return parent
    return Path(model_path).resolve().parent


@functools.lru_cache(maxsize=None)
def _parent_entity_info(org_path: Path | None) -> dict[str, Any] | None:
    """Raw model metadata of a soft-FK parent entity (no enrichment).

    Loaded once per parent org per process (cached): the eventing-test
    template's per-FK seeding blocks need the parent's entity name,
    generator facet, audit-group status, component and mandatory-FK
    list. Returns None when the org cannot be loaded (cross-component or
    non-codegen tables resolve to no modeling org and are skipped by the
    caller).
    """
    if org_path is None:
        return None
    try:
        raw = load_model(str(org_path))
    except Exception:
        return None
    de = raw.get('domain_entity') or {}
    return {
        'entity_singular': de.get('entity_singular'),
        'generator_facet_name': de.get('generator_facet_name'),
        'has_audit_group': bool(de.get('domain_audit_group')),
        'has_identity_group': bool(de.get('domain_identity_group')),
        'seed_country_sentinel': bool(de.get('seed_country_sentinel')),
        'component': de.get('component'),
        'mandatory_fks': [
            f for f in de.get('foreign_keys') or [] if not f.get('nullable', False)
        ],
    }


def _plan_required_seeds(mfks, parent_var, org_by_table, component, path):
    """Plan the ordered seed actions a written parent row needs first.

    Each non-nullable soft FK of the parent row must reference an active
    row of its own, so seeding a parent row means seeding (and patching)
    every mandatory ancestor up the FK chain before the parent's insert
    runs. Returns a flat, write-ordered action list; every item seeds one
    ancestor entity, and items deeper in the chain precede the item of
    the entity that references them. ``parent_var`` names the variable
    the seeded entity will be patched into; ``path`` is the set of tables
    already on the current chain (cycle guard for self-referential
    hierarchies). Skipped like the direct-parent resolution: nullable
    FKs, unresolvable tables (no modeling org), parties (the direct
    parent's party branch is the single party-seeding mechanism) and
    cross-component parents.
    """
    items = []
    for mfk in mfks:
        grandparent = _parent_entity_info(
            (org_by_table.get(mfk.get('table')) or {}).get('org'))
        if not grandparent or not grandparent['entity_singular']:
            continue
        if grandparent['entity_singular'] == 'party':
            continue
        if grandparent['component'] != component:
            continue
        if mfk.get('table') in path:
            continue
        var = mfk['column'] + '_parent'
        items.extend(_plan_required_seeds(
            grandparent['mandatory_fks'], var, org_by_table, component,
            path | {mfk.get('table')}))
        items.append({
            'var': var,
            'column': mfk['column'],
            'table': mfk['table'],
            'parent_var': parent_var,
            'parent_entity_singular': grandparent['entity_singular'],
            'parent_generator_facet_name': (
                grandparent['generator_facet_name'] or 'generators'),
            'target_column': mfk.get('target_column'),
            'parent_has_audit_group': grandparent['has_audit_group'],
        })
    return items


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

    # Check for C++ generation flag (--cpp or cpp_ prefix in target_template)
    generate_cpp = target_template and target_template.startswith('cpp_') and not target_template.startswith('cpp_qt_')
    # Check for Qt generation flag (qt_ or cpp_qt_ prefix in target_template)
    generate_qt = target_template and (target_template.startswith('qt_') or target_template.startswith('cpp_qt_'))

    # A junction or domain_entity is only ever eligible for ores.cpp.qt
    # generation via its own ** Qt drawer -- the facet's #+model_types:
    # admits every junction/domain_entity at the routing layer
    # (physical_space.py has no visibility into a model's body, only its
    # frontmatter), so a model with no Qt drawer must fail fast here
    # rather than silently emit a qt-less render (empty class names,
    # mangled macro guards, blank #include "" -- both model kinds have
    # exhibited this before their Qt drawer became mandatory).
    if is_junction and generate_qt and not model.get('junction', {}).get('qt'):
        print(f"{model_filename}: junction has no ** Qt drawer -- "
              f"{target_template} needs one (see ores.refdata.calendar_date.org "
              f"for a worked example)")
        return 1
    if is_domain_entity and generate_qt and not model.get('domain_entity', {}).get('qt'):
        print(f"{model_filename}: domain_entity has no ** Qt drawer -- "
              f"{target_template} needs one (see ores.refdata.calendar.org "
              f"for a worked example)")
        return 1

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
        # Field-group models must be used via an address (e.g. --address ores.cpp.field-group; no default templates)
        if target_template:
            templates_to_process = [target_template]
        else:
            print(f"Field-group model '{model_filename}' requires --address ores.cpp.field-group")
            return
    elif is_component:
        # Component scaffold models must be used via an address (no default templates)
        if target_template:
            templates_to_process = [target_template]
        else:
            print(f"Component model '{model_filename}' requires --address ores.cpp.component")
            return
    elif is_service_registry:
        # Service registry models must be used via an address (no default templates)
        if target_template:
            templates_to_process = [target_template]
        else:
            print(f"Service registry model '{model_filename}' requires --address ores.shell.service")
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

    # --- oresmd quote-type models ---
    # Project the batch/single spec dicts onto the top-level keys the
    # oresmd_enums.hpp template consumes. The batch manifest loads as
    # {"oresmd_quote_types": [...]}; a single spec loads as
    # {"oresmd_quote_type": {...}}. The template's preamble guard renders
    # once via the first_asset_class marker; each spec's quote_types list
    # gets last-item markers for comma handling.
    if model_type == 'oresmd_quote_type' and isinstance(model, dict):
        specs = list(model.get('oresmd_quote_types') or [])
        if not specs and model.get('oresmd_quote_type'):
            specs = [model['oresmd_quote_type']]
        for spec in specs:
            _mark_last_item(spec.get('quote_types') or [])
            _mark_last_item(spec.get('index_family') or [])
        # first_asset_class: marks the leading spec so the template can emit
        # one-time content after the first per-class enum (the shared
        # index_family enum sits between ir and credit in the hand-crafted
        # file); the preamble guard uses the top-level marker below. The
        # outer list also gets a last marker for templates that iterate the
        # specs themselves (identifiers/requirements emit std::variant
        # argument lists and must not render a trailing comma).
        if specs:
            specs[0]['first_asset_class'] = True
            _mark_last_item(specs)
        data['oresmd_quote_types'] = specs
        data['oresmd_quote_type'] = {'first_asset_class': True}
        # Variant-ordered list for the header templates: the hand-crafted
        # market_data_identifier.hpp/market_data_requirement.hpp list their
        # structs fx-first, which differs from the enum generation order
        # (ir first) declared by the manifest's Spec files table. Specs
        # without a variant_order (single-spec runs) keep manifest order.
        variant_specs = [s for s in specs if s.get('variant_order') is not None]
        variant_specs.sort(key=lambda s: s['variant_order'])
        for s in specs:
            if s.get('variant_order') is None:
                variant_specs.append(s)
        if variant_specs:
            _mark_last_item(variant_specs)
            # variant_first: the to_uri()/std::visit branches open with
            # `if constexpr` on the variant-ordered list's first member and
            # `} else if constexpr` on the rest (first_asset_class marks
            # the manifest-ordered list, ir first, for the enums).
            variant_specs[0]['variant_first'] = True
        data['oresmd_variant_specs'] = variant_specs
        # Parser/projections/resolver shaping. The parser template keys
        # per-class specials off the spec's own asset class name
        # ({{#fx}}, {{#ir}}); the projections template renders each class's
        # ore_type/ore_<ac>_metric switches from the quote types table (the
        # ore_metric cases grouped by metric preserving first-appearance
        # order -- the hand-crafted switches group e.g. spot/fwd under
        # PRICE before dividend under RATE); the resolver derives each
        # field's fill strategy from the Fields table.
        for s in specs:
            ac = s['asset_class']
            s[ac] = True
            v = s.get('validate', '')
            s['validate_function_call'] = v in ('function', 'delegate_function')
            s['validate_inline_delegate'] = v == 'inline_delegate'
            s['validate_inline'] = v == 'inline'
            qts = s.get('quote_types') or []
            groups: list[dict[str, Any]] = []
            first_seen: dict[str, int] = {}
            for row in qts:
                m = row.get('ore_metric', '')
                if m not in first_seen:
                    first_seen[m] = len(groups)
                    # The projections template renders each metric case as
                    # `return ore_metric_spec::<constant>;`; the constant name is
                    # the ORE METRIC value in snake_case (RATE -> rate), mirroring
                    # the hand-written ore_metric_spec block in the template.
                    groups.append({'metric': m, 'constant': m.lower(), 'names': []})
                groups[first_seen[m]]['names'].append(row['enum_name'])
            data[f'{ac}_ore'] = {
                'asset_class': ac,
                'quote_types': qts,
                'metric_groups': groups,
                'ore_type_default': qts[0].get('ore_type', '') if qts else '',
                'ore_metric_default': qts[0].get('ore_metric', '') if qts else '',
                # ore_constant names the ore_type_spec constant each enum maps to
                # (the Ore constant column of the Quote types table); the forward
                # ore_type() switches and the inverse dispatcher both reference
                # these constants, so a rename stays in sync in both directions.
                'ore_constant_default': qts[0].get('ore_constant', '') if qts else '',
                'ore_metric_constant_default': (qts[0].get('ore_metric', '').lower() if qts else ''),
            }
        # Parse/resolve function definitions run in the hand-crafted files'
        # own order (fx, ir, equity, credit, correlation, inflation,
        # commodity), which differs from variant order -- declared per spec
        # via the manifest's parse_order column. Specs without one (single-
        # spec runs) keep manifest order.
        parse_specs = [s for s in specs if s.get('parse_order') is not None]
        parse_specs.sort(key=lambda s: s['parse_order'])
        for s in specs:
            if s.get('parse_order') is None:
                parse_specs.append(s)
        if parse_specs:
            data['oresmd_parse_specs'] = parse_specs
        # The validate_<ac>() functions bracket the static
        # validate_no_ir_only_keys() helper in the hand-crafted file:
        # the explicit-reject validators (fx, ir) precede it, the
        # delegating validator (equity) follows it.
        if any(s.get('validate') == 'function' for s in parse_specs):
            data['oresmd_validate_pre'] = [
                s for s in parse_specs if s.get('validate') == 'function'
            ]
        if any(s.get('validate') == 'delegate_function' for s in parse_specs):
            data['oresmd_validate_post'] = [
                s for s in parse_specs if s.get('validate') == 'delegate_function'
            ]
        # Per-class test lists: the test templates render each asset class's
        # Test cases rows at fixed positions in file order (the hand-crafted
        # files interleave classes and comment blocks), so every table
        # becomes a top-level list named <asset_class>_<table>, e.g.
        # ir_round_trip, fx_rejection, equity_projections.
        for s in specs:
            for kind, rows in s.get('test_cases', {}).items():
                data[f"{s['asset_class']}_{kind}"] = rows
        # The hand-crafted test files interleave static content between
        # rows of a single spec table; split those lists so the templates
        # can emit each run at its fixed position. ir's Projections table
        # holds the design-doc worked examples (first three rows) and then
        # the story's new quote types, with a section comment between them
        # in the projections tests file. equity's Rejection rows bracket
        # the static parse_equity_with_point test in the parser tests file.
        ir_proj = data.get('ir_projections', [])
        if len(ir_proj) > 3:
            data['ir_projections_worked_examples'] = ir_proj[:3]
            data['ir_projections_new_quote_types'] = ir_proj[3:]
        eq_rej = data.get('equity_rejection', [])
        if len(eq_rej) > 1:
            data['equity_rejection'] = eq_rej[:1]
            data['equity_rejection_tail'] = eq_rej[1:]
        # The hand-crafted projections tests file wraps the discount worked
        # example's long URI as two string literals on the parse() line.
        # clang-format's canonical re-break of the unbroken line differs
        # (break after parse(), string split at "oresmd://ir/"), so emit the
        # two pieces and let clang-format keep the manual wrap -- both lines
        # fit within the column limit, so the file is a format fixed point.
        for row in data.get('ir_projections_worked_examples', []):
            if row.get('description') == 'discount_quote_key_matches_worked_example':
                head, rest = row['uri'].split('&quote=', 1)
                # The split consumed '&quote='; re-attach the '&' to the head
                # and 'quote=' to the tail so the two literals concatenate
                # back to the exact URI.
                row['uri_head'] = head + '&'
                row['uri_tail'] = 'quote=' + rest
                row['uri_split'] = True

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

        # The ores.cmake.component.files_{src,tests} archetypes emit an
        # explicit, checked-in `set(files ...)` list -- the idiomatic
        # CMake alternative to file(GLOB_RECURSE ...). Unlike every other
        # component field above, this one can't come from the model (a
        # source file list isn't modeled data); it's a directory scan
        # against this render's own output_dir, which for these two
        # archetypes IS the src/ or tests/ dir being listed.
        if target_template in _COMPONENT_FILES_TEMPLATES:
            component['files'] = sorted(
                p.relative_to(output_dir).as_posix()
                for p in Path(output_dir).rglob('*.cpp') if p.is_file())
            # Qt AUTOMOC components additionally need their headers listed
            # (Qt needs to see Q_OBJECT declarations); harmless, unused
            # HEADERS for a non-Qt component with an empty/no include/ dir.
            include_dir = Path(output_dir).parent / 'include'
            headers = sorted(
                p.relative_to(include_dir).as_posix()
                for p in include_dir.rglob('*.hpp') if p.is_file()
            ) if include_dir.is_dir() else []
            component['headers'] = headers
            component['has_headers'] = bool(headers)

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
            # 'name' is now the full binary name (e.g. ores.iam.service);
            # 'component' is the short key some templates need (e.g. the
            # env-var-mirror generator), always psql_var with its "_service"
            # suffix stripped -- psql_var is always "<component>_service".
            if psql_var.endswith('_service'):
                svc['component'] = psql_var[: -len('_service')]
            elif psql_var:
                svc['component'] = psql_var
        data['service_registry'] = service_registry

        # Some entries have no DB-access aspect (no :psql_var:) -- e.g. a
        # deployment-only process with no NATS-domain-service role. Templates
        # that join services with a trailing comma/semicolon via 'last' (see
        # _mark_last_item above) need a *separate* last-marked list scoped to
        # only the DB-access-bearing subset, or the 'last' flag could land on
        # a skipped entry and leave a dangling trailing comma. copy.deepcopy
        # avoids mutating the shared services list's own 'last' flags.
        db_services = copy.deepcopy([s for s in services if s.get('psql_var')])
        _mark_last_item(db_services)
        data['service_registry_db_services'] = db_services

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
        # and normalise it with the same rules a native table model used to
        # get, so sql_schema_domain_entity_create.mustache's {{table.*}}
        # fields are populated identically either way.
        from .org_loader import domain_entity_to_table_context  # deferred to avoid circular import
        sql_table = domain_entity_to_table_context(domain_entity)['table']
        normalise_sql_table_context(sql_table)
        data['table'] = sql_table
        # normalise_sql_table_context computes tenant-scope flags onto its own
        # (shallow-copied) validation_fn dict; sync them back onto domain_entity
        # so the domain-entity SQL template — which reads domain_entity.validation_fn
        # directly — sees scope_system/scope_both/scope_tenant too.
        if 'validation_fn' in sql_table:
            domain_entity['validation_fn'] = sql_table['validation_fn']
        # Same sync, for the artefact-table archetype (sql_schema_domain_
        # entity_artefact_create.mustache), which also reads domain_entity.*
        # directly rather than through this table-context projection.
        for flag in (
            'has_coding_scheme', 'has_nullable_coding_scheme',
            'has_any_coding_scheme', 'has_image_id',
        ):
            if flag in sql_table:
                domain_entity[flag] = sql_table[flag]
        # Some entities (e.g. refdata's country) predate this generic
        # coding_scheme feature and declare their own coding_scheme_code
        # column by hand under `* Columns`. Suppress the auto-emitted
        # CREATE TABLE column in that case -- it would duplicate the
        # explicit one -- but keep has_any_coding_scheme (and thus the FK
        # validation block) intact, since a hand-declared column still
        # wants the generic existence check unless the model supplies its
        # own validation_fn for it.
        has_manual_coding_scheme_column = any(
            c.get('name') == 'coding_scheme_code'
            for c in domain_entity.get('columns', [])
        )
        if has_manual_coding_scheme_column:
            domain_entity['has_coding_scheme'] = False
            domain_entity['has_nullable_coding_scheme'] = False
        if 'artefact_indexes' in domain_entity:
            _mark_last_item(domain_entity['artefact_indexes'])
        if any(
            v.get('cardinality_limit_table')
            for v in domain_entity.get('insert_trigger', {}).get('validations', [])
        ):
            domain_entity.setdefault('sql', {})['has_cardinality_limit_validations'] = True
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
                is_timestamp_type = col.get('type') in (
                    'timestamp', 'timestamptz', 'timestamp with time zone'
                )
                is_enum_type = col.get('is_enum', False)
                is_already_optional = (
                    col.get('cpp_type', '').startswith('std::optional<')
                    and not is_uuid_type
                    and not is_timestamp_type
                )
                col['is_already_optional'] = is_already_optional
                col['is_uuid'] = is_uuid_type and not col.get('nullable', False)
                col['is_optional_uuid'] = is_uuid_type and col.get('nullable', False)
                col['is_optional_timestamp'] = is_timestamp_type and col.get('nullable', False)
                # A required (NOT NULL) plain timestamp column needs the same
                # entity-layer std::string representation natural-key timestamps
                # already get -- sqlgen cannot serialise a raw
                # std::chrono::time_point, and is_simple's raw-cpp_type passthrough
                # (below) doesn't know about timestamps at all. Without this, a
                # required, non-natural-key timestamp column silently falls
                # through to is_simple and fails at compile time inside sqlgen's
                # transpilation layer.
                col['is_required_timestamp'] = is_timestamp_type and not col.get('nullable', False)
                col['is_enum'] = is_enum_type and not col.get('nullable', False)
                col['is_nullable_string'] = (
                    col.get('nullable', False)
                    and not is_uuid_type
                    and not is_timestamp_type
                    and not is_enum_type
                    and not is_already_optional
                    and col.get('cpp_type') == 'std::string'
                )
                # A nullable numeric (or bool) column whose domain member is
                # the plain scalar: the entity layer represents it as
                # std::optional<{cpp_type}> and the mapper maps NULL to the
                # zero sentinel, mirroring the string/uuid/timestamp idioms.
                # is_nullable_string must NOT claim these columns (it would
                # emit std::optional<std::string> for a numeric entity member).
                col['is_nullable_numeric'] = (
                    col.get('nullable', False)
                    and not is_uuid_type
                    and not is_timestamp_type
                    and not is_enum_type
                    and not is_already_optional
                    and col.get('cpp_type') in (
                        'int', 'std::int64_t', 'std::uint64_t', 'double', 'float', 'bool'
                    )
                )
                col['is_simple'] = (
                    not col.get('nullable', False)
                    and not is_uuid_type
                    and not is_timestamp_type
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
                # A plain-string column declared :unique: must produce a
                # distinct value per synthetic row. The generator template
                # appends the process-counter suffix (the same mechanism
                # text natural keys use) unless the model opts out via
                # :no_generator_suffix:. The raw cpp_type check mirrors
                # is_plain_string but tolerates an explicit generator_expr,
                # which is_plain_string deliberately excludes -- a unique
                # column WITH a generator expression collides just as
                # surely as one without (faker-derived bases are not
                # distinct), so both need the suffix.
                col['requires_unique_suffix'] = (
                    col.get('unique') is True
                    and col.get('cpp_type') == 'std::string'
                    and not col.get('no_generator_suffix', False)
                )
                # Render-type flags for templates (e.g. the history field
                # mapper) that must render an existing value to string.
                # Deliberately derived from the RAW cpp_type string, not
                # from is_uuid/is_nullable_string/is_already_optional/
                # is_plain_string above — those flags exist for the
                # repository/SQL layer's own nullable-to-optional promotion
                # logic, which the domain class template
                # (cpp_domain_type_class.hpp.mustache) does NOT apply: it
                # emits {{{cpp_type}}} verbatim, so a model with
                # ":nullable: true" but an explicit ":cpp_type: std::string"
                # override gets a plain std::string field, not
                # std::optional<std::string> — the render_* flags must match
                # that same ground truth, not the derived nullable flags.
                _render_cpp_type = (col.get('cpp_type') or '').strip()
                col['render_is_string'] = _render_cpp_type == 'std::string'
                col['render_is_optional_string'] = _render_cpp_type == 'std::optional<std::string>'
                col['render_is_bool'] = _render_cpp_type == 'bool'
                col['render_is_optional_bool'] = _render_cpp_type == 'std::optional<bool>'
                col['render_is_int'] = _render_cpp_type == 'int'
                col['render_is_optional_int'] = _render_cpp_type == 'std::optional<int>'
                col['render_is_double'] = _render_cpp_type == 'double'
                col['render_is_optional_double'] = _render_cpp_type == 'std::optional<double>'
                col['render_is_uuid'] = _render_cpp_type == 'boost::uuids::uuid'
                col['render_is_optional_uuid'] = (
                    _render_cpp_type == 'std::optional<boost::uuids::uuid>'
                )
                col['render_is_timestamp'] = (
                    'time_point' in _render_cpp_type and not _render_cpp_type.startswith('std::optional<')
                )
                col['render_is_optional_timestamp'] = (
                    _render_cpp_type.startswith('std::optional<') and 'time_point' in _render_cpp_type
                )
                # Derived from the raw is_enum flag, not the nullable-narrowed
                # col['is_enum'] above -- render_* flags must match the
                # domain struct's actual field type (see the module
                # docstring), and an enum column stays an enum type in the
                # domain struct regardless of nullability.
                col['render_is_enum'] = is_enum_type
                # Mechanical title-case label for templates that render a
                # human-readable field name (e.g. the history field mapper)
                # without depending on the Qt profile's curated "Detail
                # Fields" table, which is about widget wiring, not just
                # labels, and isn't guaranteed present for every entity.
                col['render_label'] = ' '.join(
                    word.upper() if word.lower() in ('id', 'iso', 'fx')
                    else word.capitalize()
                    for word in col.get('name', '').split('_')
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
            # Unconditional on natural_keys (unlike the other has_* flags
            # below, which are computed only inside the natural_keys block):
            # an entity can have enum columns with a surrogate-UUID-only
            # primary key and no natural keys at all (e.g.
            # market_data_generation_config), and templates like the
            # history field mapper need this flag regardless.
            domain_entity['has_enum_columns'] = any(
                c.get('is_enum') for c in domain_entity['columns']
            )
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
        # Primary-key columns live in a separate 'primary_key' dict, not in
        # 'columns' (see org_loader._parse_columns), so they need the same
        # flag set independently -- otherwise repository-layer helpers like
        # value_log_fields below would reference a flat field that the
        # identity-grouped domain struct no longer has.
        pk_dict = domain_entity.get('primary_key', {})
        for col in [pk_dict] + list(pk_dict.get('columns', [])):
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
                key['render_label'] = ' '.join(
                    word.upper() if word.lower() in ('id', 'iso', 'fx')
                    else word.capitalize()
                    for word in key.get('column', '').split('_')
                )
            nks = domain_entity['natural_keys']
            domain_entity['has_multiple_natural_keys'] = len(nks) > 1
            # Flag: UUID-PK entities with text natural keys need an idx counter in the generator
            domain_entity['has_text_natural_keys'] = any(
                not k.get('is_uuid') and not k.get('is_int')
                and not k.get('is_timestamp') and not k.get('is_date')
                for k in nks
            )
            domain_entity['has_date_natural_keys'] = any(k.get('is_date') for k in nks)
            domain_entity['has_date_or_timestamp_natural_keys'] = any(
                k.get('is_date') or k.get('is_timestamp') for k in nks
            )
            if len(nks) > 1:
                domain_entity['natural_keys_composite_columns'] = ', '.join(nk['column'] for nk in nks)
                if 'natural_keys_composite_name' not in domain_entity:
                    domain_entity['natural_keys_composite_name'] = '_'.join(nk['column'] for nk in nks)
        else:
            # No natural keys at all (e.g. currency_pair): has_text_natural_keys
            # defaults False so needs_counter below falls back to the primary
            # key check alone.
            domain_entity.setdefault('has_text_natural_keys', False)
        domain_entity['unique_keys'] = [
            {'column': col['name']} for col in domain_entity.get('columns', [])
            if col.get('unique') is True
        ]
        # A required (NOT NULL) plain timestamp column needs the same
        # ores.platform/time/datetime.hpp + <chrono>/<format>/<sstream>
        # includes a timestamp natural key needs -- broaden the flag rather
        # than require a timestamp natural key to also be present. The same
        # holds for a nullable timestamp whose domain member is a plain
        # time_point: the mapper's sentinel idiom compares against
        # std::chrono::system_clock::time_point{} and renders via
        # datetime::to_db_string.
        domain_entity['has_date_or_timestamp_natural_keys'] = (
            domain_entity.get('has_date_or_timestamp_natural_keys', False)
            or any(c.get('is_required_timestamp') for c in domain_entity.get('columns', []))
            or any(
                c.get('is_optional_timestamp') and c.get('render_is_timestamp')
                for c in domain_entity.get('columns', [])
            )
        )
        # Check primary_key's raw 'type' rather than the derived 'is_text'
        # flag: that flag isn't computed until later in this function, so
        # reading it here would always see False (order-dependency bug —
        # only ever surfaced once a text-PK entity supplied a custom
        # generator, triggering the uniqueness-suffix/counter code path).
        # Also moved outside the 'natural_keys' presence check above: an
        # entity with a text PK and no natural keys at all (e.g.
        # currency_pair) still needs the counter, but previously this whole
        # computation only ran when natural_keys existed.
        domain_entity['has_unique_suffix_columns'] = any(
            c.get('requires_unique_suffix') for c in domain_entity.get('columns', [])
        )
        # needs_counter drives the process-static counter the synthetic
        # generator suffixes onto text natural keys and text primary keys.
        # A :unique: plain-string column needs the counter too: its value
        # must differ per row, and a faker-derived base is a process
        # constant, so without the suffix the second write of the entity in
        # one process collides on the unique index.
        domain_entity['needs_counter'] = (
            domain_entity.get('primary_key', {}).get('type') == 'text'
            or domain_entity['has_text_natural_keys']
            or domain_entity['has_unique_suffix_columns']
        )
        if 'indexes' in domain_entity:
            _mark_last_item(domain_entity['indexes'])
        if 'validations' in domain_entity:
            _mark_last_item(domain_entity['validations'])
        # Format description as comment block lines (for SQL)
        if 'description' in domain_entity:
            domain_entity['description_formatted'] = _format_description_as_comment(domain_entity['description'])
            # Split description into lines for C++ doxygen comments
            domain_entity['description_lines'] = domain_entity['description'].split('\n')
            # Single physical line for templates that embed the description in
            # a one-line SQL comment (e.g. sql_schema_domain_entity_artefact_
            # create.mustache's "-- {{description}} - Artefact Table --"): a
            # raw multi-line description would otherwise break out of the
            # comment mid-line.
            domain_entity['description_oneline'] = ' '.join(
                domain_entity['description'].split()
            )
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
                domain_entity['component_service'] = f"{component}.service"
                domain_entity['component_service_dir'] = f"ores.{component}/service"
            else:
                domain_entity.setdefault('component_include', component)
                domain_entity['component_dir'] = f"ores.{component}"
                domain_entity.setdefault('component_core', component)
                domain_entity['component_core_dir'] = f"ores.{component}"
                domain_entity.setdefault('component_service', component)
                domain_entity['component_service_dir'] = f"ores.{component}"
            domain_entity['component_include_upper'] = (
                domain_entity['component_include'].replace('.', '_').upper()
            )
            domain_entity['component_core_upper'] = (
                domain_entity['component_core'].replace('.', '_').upper()
            )
            domain_entity['component_service_upper'] = (
                domain_entity['component_service'].replace('.', '_').upper()
            )
            # cached_by: the consumer component a nats-event-cache
            # archetype's output belongs to (see resolve_output_path);
            # exposed to templates alongside the entity's own component.
            # Accepts the same "component.subcomponent" dotted form
            # component_include already uses, for a consumer whose cache
            # doesn't land in its default "core" subcomponent -- no
            # separate model flag; both variables below are purely
            # derived, like every other _upper variable in this block.
            cached_by_raw = domain_entity.get('cached_by', component)
            if '.' in cached_by_raw:
                cache_component, cache_subcomponent = cached_by_raw.split('.', 1)
            else:
                cache_component, cache_subcomponent = cached_by_raw, 'core'
            domain_entity['cache_component'] = cache_component
            domain_entity['cache_component_upper'] = cache_component.upper()
            domain_entity['cache_subcomponent'] = cache_subcomponent
            domain_entity['cache_subcomponent_upper'] = cache_subcomponent.upper()
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
            # Human-readable spaced versions for doc comments and UI
            # (e.g. "pricing_engine_types" -> "pricing engine types" / "Pricing Engine Types")
            de_plural_spaced = domain_entity['entity_plural'].replace('_', ' ')
            domain_entity['entity_plural_words'] = de_plural_spaced
            domain_entity['entity_plural_words_cap'] = de_plural_spaced.title()
        if 'entity_title' in domain_entity:
            domain_entity['entity_title_lower'] = domain_entity['entity_title'].lower()
        # Prepare table display items for C++ templates
        if 'cpp' in domain_entity:
            # Collect UUID column names for table display
            uuid_columns = set()
            optional_columns = set()
            bool_columns = set()
            if 'primary_key' in domain_entity and domain_entity['primary_key'].get('is_uuid'):
                uuid_columns.add(domain_entity['primary_key']['column'])
            if 'natural_keys' in domain_entity:
                for key in domain_entity['natural_keys']:
                    if key.get('is_uuid'):
                        uuid_columns.add(key['column'])
            if 'columns' in domain_entity:
                for col in domain_entity['columns']:
                    # Optional-ness must come from the raw cpp_type, not the
                    # SQL-nullability flags: the domain class template emits
                    # {{{cpp_type}}} verbatim, so a ":nullable: true" column
                    # whose author left cpp_type as a plain scalar (e.g.
                    # "int" or a plain time_point) is a plain member and
                    # streams as-is; only an explicit std::optional<...>
                    # cpp_type needs the opt_str() wrapper (see the
                    # render_* flag comment above).
                    _render_cpp_type = (col.get('cpp_type') or '').strip()
                    if (
                        'boost::uuids::uuid' in _render_cpp_type
                        and not _render_cpp_type.startswith('std::optional<')
                    ):
                        uuid_columns.add(col['name'])
                    if _render_cpp_type.startswith('std::optional<'):
                        optional_columns.add(col['name'])
                    if _render_cpp_type == 'bool':
                        bool_columns.add(col['name'])
            _prepare_table_display(domain_entity['cpp'], uuid_columns, optional_columns, bool_columns)
        # Copy repository section fields to top level for template access
        if 'repository' in domain_entity:
            for key, value in domain_entity['repository'].items():
                domain_entity[key] = value
        # Tenant read-scope: 'shared' entities (system-tenant-seeded reference
        # data every tenant may read, e.g. DQ governance taxonomy already
        # covered by the matching *_read_policy in dq_rls_policies_create.sql,
        # which allows own-tenant OR system-tenant rows) rely on RLS alone for
        # SELECT queries -- adding a narrower app-level tenant_id filter on
        # top would defeat the policy's system-tenant fallback (exactly the
        # bug this flag fixes). Mutations (insert/update/delete) always stay
        # tenant-scoped regardless of this flag.
        domain_entity['read_tenant_filtered'] = (
            domain_entity.get('has_tenant_id', False)
            and domain_entity.get('tenant_read_scope', 'tenant') != 'shared')
        # System-tenant read scope: entities marked :system_tenant_visible:
        # read their own tenant's rows PLUS platform rows seeded under the
        # system tenant (e.g. compute apps shared across all tenants). The
        # union is spliced into every read where-clause via tenant_where /
        # sys_decl (see cpp_domain_type_repository.cpp.mustache); mutations
        # (insert/update/delete) always stay own-tenant scoped regardless.
        # Incompatible with tenant_read_scope: shared -- that scope removes
        # the app-level tenant filter entirely (RLS governs), which would
        # leave sys declared but unused.
        domain_entity['system_tenant_visible'] = bool(
            domain_entity.get('system_tenant_visible', False))
        if domain_entity['system_tenant_visible']:
            domain_entity['tenant_where'] = (
                '("tenant_id"_c == tid || "tenant_id"_c == sys)')
            # Leading newline: the template inlines {{sys_decl}} at the end
            # of the tid declaration line, so an empty value must render
            # nothing at all (no stray blank line) and a non-empty value
            # starts on its own line.
            domain_entity['sys_decl'] = (
                '\n    static const std::string sys('
                'ores::database::service::tenant_context::system_tenant_id);')
        else:
            domain_entity['tenant_where'] = '"tenant_id"_c == tid'
            domain_entity['sys_decl'] = ''
        # Set defaults for messaging handler knobs if not provided by entity model.
        # Entities override via ** Repository section; these cover the common cases.
        pk = domain_entity.get('primary_key', {})
        pk_col = pk.get('column', 'id')
        pk_type = pk.get('type', 'uuid')
        domain_entity.setdefault(
            'delete_request_id_field',
            pk_col + 's' if pk_type == 'text' else 'ids')
        domain_entity.setdefault('history_request_id_field', pk_col)
        # Composite primary keys: history_request_id_field/delete_request_id_field
        # above only ever cover the FIRST pk column (back-compat single-column
        # mirroring in _primary_key_dict()). A multi-column text pk (e.g.
        # subject_area's name+domain_name) needs every remaining column passed
        # to the service call too, or history()/remove() silently look up (or
        # delete!) rows keyed on a partial, ambiguous key. These extra-arg
        # lists are empty for every single-column-pk entity, so the nats-
        # handler template's use of them is a no-op there.
        pk_extra_cols = (pk.get('columns') or [])[1:]
        domain_entity.setdefault(
            'history_request_extra_args',
            [{'name': c['column']} for c in pk_extra_cols])
        domain_entity.setdefault(
            'delete_request_extra_args',
            [{'name': c['column'] + 's'} for c in pk_extra_cols])
        domain_entity.setdefault('single_delete', False)
        validate_read_for_cache(domain_entity)
        validate_cached_by(domain_entity)
        validate_cache_aux_type(domain_entity)
        # Derive paged list-by-foreign-key NATS operations (protocol/handler/
        # registrar) from any foreign key opted in via :list_by: true. The
        # repository/service methods themselves are generated directly off
        # domain_entity['foreign_keys'] (see repository/service archetypes).
        entity_plural_short = domain_entity.get('entity_plural_short', domain_entity.get('entity_plural'))
        domain_entity['extra_list_requests'] = [
            {
                'name_suffix': f"by_{fk['column']}",
                'nats_suffix': f"list_by_{fk['column']}",
                'filter_column': fk['column'],
                'service_method': f"list_{entity_plural_short}_by_{fk['column']}",
                'count_service_method': f"count_{entity_plural_short}_by_{fk['column']}",
                'default_limit': int(fk.get('list_by_default_limit', 100)),
            }
            for fk in domain_entity.get('foreign_keys', [])
            if fk.get('list_by')
        ]
        # A list_by facet orders by the primary key by default; a foreign key
        # may override this via :list_by_order_by: "<column> [desc]" (e.g. a
        # time-series entity ordering its "latest N" query by its own
        # timestamp column instead of an unordered-with-respect-to-time UUID
        # primary key).
        for fk in domain_entity.get('foreign_keys', []):
            if not fk.get('list_by'):
                continue
            order_by_spec = fk.get('list_by_order_by', pk_col).split()
            fk['list_by_order_column'] = order_by_spec[0]
            fk['list_by_order_desc'] = (
                len(order_by_spec) > 1 and order_by_spec[1].lower() == 'desc')
        # Soft-FK parent resolution for eventing-integration-test seeding: a
        # child write whose mandatory soft FK references another entity is
        # rejected by the parent's existence-check trigger unless an active
        # parent row already exists, so the integration test must seed one
        # (see the eventing-integration-test archetype template). Resolve
        # each FK's :table: to the parent entity's model metadata -- RAW
        # load_model() output, not this enrichment -- so the template can
        # emit per-FK seeding code. Skipped for: nullable FKs (their
        # generators emit nullopt, which the trigger's check skips), tables
        # with no modeling org (cross-component or non-codegen tables), and
        # cross-component parents (the template's includes assume a
        # same-component parent).
        fks = domain_entity.get('foreign_keys') or []
        if fks:
            from .org_loader import _entity_org_by_table
            org_by_table = _entity_org_by_table(_projects_dir_from(model_path))
            for fk in fks:
                if fk.get('nullable'):
                    continue
                parent = _parent_entity_info(
                    (org_by_table.get(fk.get('table')) or {}).get('org'))
                if not parent or not parent['entity_singular']:
                    continue
                if parent['component'] != domain_entity.get('component'):
                    continue
                fk['parent_entity_singular'] = parent['entity_singular']
                fk['parent_generator_facet_name'] = (
                    parent['generator_facet_name'] or 'generators')
                fk['parent_is_party'] = parent['entity_singular'] == 'party'
                fk['parent_has_audit_group'] = parent['has_audit_group']
                fk['parent_has_identity_group'] = parent['has_identity_group']
                fk['parent_seed_country_sentinel'] = parent['seed_country_sentinel']
                # The parent may itself have a mandatory party_id FK (e.g.
                # portfolio -- session-set in production): the template then
                # seeds a party too, so the parent's own insert passes its
                # trigger. The party table is resolved via the same table
                # scan (its entity_singular is 'party').
                fk['parent_requires_party'] = any(
                    not mfk.get('nullable')
                    and (org_by_table.get(mfk.get('table')) or {}).get(
                        'entity_singular') == 'party'
                    for mfk in parent['mandatory_fks'])
                # The parent may itself have mandatory soft-FK parents of
                # its own (e.g. currency_pair's base/quote legs ->
                # currency): the child's eventing test must seed those too,
                # or the parent's own insert fails its existence check
                # before the child is even written. Party is excluded: the
                # parent_requires_party branch above already seeds it, so
                # exactly one mechanism emits the party. Cross-component
                # and unresolvable parents are skipped like the level
                # above. The plan closes the FK chain transitively (an
                # ancestor with mandatory FKs of its own -- app_version's
                # app_id, say -- seeds them first), not just one level.
                fk['parent_required_fks'] = _plan_required_seeds(
                    parent['mandatory_fks'], fk['column'] + '_parent',
                    org_by_table, domain_entity.get('component'),
                    set())
                # Whether the FK column sits inside the child's identity
                # group (trading entities) or is a flat domain field
                # (refdata) -- set here, after the columns loop above has
                # stamped is_identity_group_column on every column.
                fk['is_identity_group_column'] = any(
                    c.get('name') == fk.get('column')
                    and c.get('is_identity_group_column', False)
                    for c in domain_entity.get('columns', []) or [])
            domain_entity['seed_party'] = any(
                fk.get('parent_is_party') or fk.get('parent_requires_party')
                for fk in fks)
            domain_entity['seed_parent_country_sentinel'] = any(
                fk.get('parent_seed_country_sentinel') for fk in fks)
        # Compute index_name_prefix: use sql.index_prefix when set, else entity_plural
        sql_section = domain_entity.get('sql', {})
        validate_rls_isolation(domain_entity)
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
        # Compute the base name every SQL identifier for this entity is composed
        # from (table, insert/notify/delete functions, triggers, delete rule).
        # PostgreSQL silently truncates identifiers to 63 bytes, so a base
        # longer than 63 - longest_suffix makes several derived identifiers
        # collapse onto one identical truncated string: the 66-char base of
        # ores.synthetic's ir_curve_generation_config_process_parameter_value
        # entity made its table, insert/notify functions and triggers all
        # truncate to the same name, so the last `create or replace` silently
        # replaced the earlier ones and the insert trigger that fills
        # valid_from/valid_to no longer existed. The entity model's
        # :tablename: (minus the trailing _tbl) is the explicit base when set
        # -- the documented contract for the generated table's name, which the
        # C++ sqlgen entity template already honours via {{sql.tablename}} --
        # with the conventional composition as fallback; the truncation
        # safety net below applies in both cases so a suffix always survives.
        base_candidates = [4, 10, 11, 9, 12]  # _tbl, _insert_fn, _insert_trg, _delete_fn, _delete_rule
        if domain_entity.get('generate_touch_function'):
            base_candidates.append(17)  # _touch_version_fn
        if domain_entity.get('has_parent_id'):
            base_candidates.append(13)  # _hierarchy_fn
        longest_suffix = max(base_candidates)
        table_name = sql_section.get('tablename', '')
        if table_name.endswith('_tbl'):
            sql_name_base = table_name[:-4]
        else:
            sql_name_base = table_name or (
                f"{domain_entity.get('product', 'ores')}_"
                f"{domain_entity.get('component', 'unknown')}_"
                f"{domain_entity.get('entity_plural', 'unknown')}"
            )
        if len(sql_name_base) + longest_suffix > 63:
            sql_name_base = sql_name_base[:63 - longest_suffix]
        domain_entity['sql_name_base'] = sql_name_base
        # RLS policy names are composed from the short table base
        # (market_series_tbl_tenant_isolation_policy), the dominant
        # hand-written shape, while sql_name_base carries the full
        # <product>_<component>_ prefix used by every other identifier.
        rls_table_base = sql_name_base
        prefix = (
            f"{domain_entity.get('product', 'ores')}_"
            f"{domain_entity.get('component', 'unknown')}_"
        )
        if rls_table_base.startswith(prefix):
            rls_table_base = rls_table_base[len(prefix):]
        # Policy names append the isolation suffix to rls_table_base
        # (_tbl_tenant_isolation_policy / _tbl_party_isolation_policy, up to
        # 28 chars) in the create/drop templates. The budget above covers
        # sql_name_base's own suffixes but not these; an over-long policy name
        # would hit the same silent 63-byte truncation. Only reachable when no
        # prefix was stripped (a :tablename: not following the
        # <product>_<component>_ convention) and RLS is opted into.
        longest_rls_suffix = len('_tbl_tenant_isolation_policy')
        if (sql_section.get('rls_tenant_isolation')
                and len(rls_table_base) + longest_rls_suffix > 63):
            rls_table_base = rls_table_base[:63 - longest_rls_suffix]
        domain_entity['rls_table_base'] = rls_table_base
        # GIST exclusion: suppressed for hypertables (incompatible); active otherwise
        # for standard temporal entities with has_tenant_id.
        domain_entity['has_gist_exclusion'] = (
            not sql_section.get('hypertable', False)
            and sql_section.get('gist_exclusion', True)
        )
        # Audit columns (modified_by, performed_by, change_reason_code, change_commentary,
        # version): suppressed for hypertable time-series entities via #+no_audit_columns.
        domain_entity['has_audit_columns'] = not sql_section.get('no_audit_columns', False)
        # change_reason_code is validated automatically by the has_audit_columns block
        # below; suppress that auto-emission when the model's own insert_trigger
        # validations table already declares an explicit row for it, to avoid
        # emitting the NEW.change_reason_code assignment twice.
        domain_entity['change_reason_code_declared'] = any(
            v.get('column') == 'change_reason_code'
            for v in domain_entity.get('insert_trigger', {}).get('validations', [])
        )
        # The Validations table only declares (column, validation_function);
        # it doesn't repeat nullability, so cross-reference each validated
        # column against its own Columns/Natural-keys declaration and
        # propagate 'nullable' onto the validation row -- the SQL template's
        # {{#nullable}} guard (skip validation when NEW.<col> is null) reads
        # this flag, and without it every validation was silently emitted
        # unconditionally, rejecting a legitimate null on an otherwise
        # nullable column.
        _nullable_by_column = {
            col.get('name', col.get('column')): bool(col.get('nullable', False))
            for col in (domain_entity.get('columns', []) or [])
            + (domain_entity.get('natural_keys', []) or [])
            + (domain_entity.get('primary_key', {}).get('columns', []) or [])
        }
        for v in domain_entity.get('insert_trigger', {}).get('validations', []):
            v['nullable'] = _nullable_by_column.get(v.get('column'), False)
        # Mark last items in new iterable sql sub-sections for template rendering
        if 'fk_copy_validations' in sql_section:
            _mark_last_item(sql_section['fk_copy_validations'])
            for fkc in sql_section['fk_copy_validations']:
                if 'declare_vars' in fkc:
                    _mark_last_item(fkc['declare_vars'])
                if 'copy_empty' in fkc:
                    _mark_last_item(fkc['copy_empty'])
        if 'foreign_keys' in domain_entity:
            _mark_last_item(domain_entity['foreign_keys'])
        if 'text_code_validations' in sql_section:
            _mark_last_item(sql_section['text_code_validations'])
        if 'extra_delete_sets' in sql_section:
            _mark_last_item(sql_section['extra_delete_sets'])
        # Add computed properties for primary key type detection. Applied to
        # both the top-level (back-compat, first-flagged-column) scalar dict
        # and, identically, to each entry of primary_key['columns'] -- a
        # compound key's SQL template rendering (sql_schema_domain_entity_
        # create.mustache) loops that list, so every column needs the same
        # is_uuid/is_text/uuid_check_fn/cpp_type/render_label flags the
        # single-column case already relied on as scalars.
        def _enrich_primary_key_field(field):
            field_type = field.get('type', 'uuid')
            field['is_uuid'] = field_type == 'uuid'
            field['is_text'] = field_type == 'text'
            if field['is_uuid'] and 'uuid_check_fn' not in field:
                field['uuid_check_fn'] = 'ores_utility_nil_uuid_fn()'
            if 'cpp_type' not in field:
                field['cpp_type'] = (
                    'boost::uuids::uuid' if field['is_uuid'] else 'std::string'
                )
            # Mechanical title-case label — see the identical column-level
            # 'render_label' computation below for why this doesn't reuse
            # 'description' (long prose, not a UI-sized label).
            field['render_label'] = ' '.join(
                word.upper() if word.lower() in ('id', 'iso', 'fx')
                else word.capitalize()
                for word in field.get('column', '').split('_')
            )

        if 'primary_key' in domain_entity:
            pk = domain_entity['primary_key']
            _enrich_primary_key_field(pk)
            pk_columns = pk.get('columns', [])
            for field in pk_columns:
                _enrich_primary_key_field(field)
            _mark_last_item(pk_columns)
            pk['is_compound'] = len(pk_columns) > 1
            # Every key column beyond the back-compat first (pk itself,
            # scalar-projected above) -- the generator template needs
            # these to synthesize a value for each, the same way it
            # already does for natural_keys.
            pk['extra_columns'] = pk_columns[1:]
            pk['column_list'] = ', '.join(c['column'] for c in pk_columns)
            pk['index_suffix'] = '_'.join(c['column'] for c in pk_columns)
            pk['gist_column_clause'] = '\n        '.join(
                f"{c['column']} WITH =," for c in pk_columns
            )
            pk['where_new'] = ' and '.join(
                f"{c['column']} = NEW.{c['column']}" for c in pk_columns
            )
            pk['where_old'] = ' and '.join(
                f"{c['column']} = OLD.{c['column']}" for c in pk_columns
            )
            # Repository-layer helpers: a compound key's where()/signature/
            # log-statement text is built once here as a single token,
            # rather than as nested mustache loops repeated across every
            # tenant/workspace-filtered branch in the repository templates.
            # sqlgen's where() only composes && at compile time over a
            # fixed set of columns, so this joins cleanly for both the
            # single-column (back-compat) and compound cases.
            pk['where_and'] = ' && '.join(
                f'"{c["column"]}"_c == {c["column"]}' for c in pk_columns
            )
            pk['order_by_cols'] = ', '.join(
                f'"{c["column"]}"_c' for c in pk_columns
            )
            pk['params'] = ', '.join(
                f'const std::string& {c["column"]}' for c in pk_columns
            )
            pk['args'] = ', '.join(c['column'] for c in pk_columns)
            # Complete stream expressions (leading string literal, trailing
            # bare value -- not a string fragment), so templates splice
            # them in as `<< {{{primary_key.log_fields}}}` with no extra
            # quoting on either side.
            pk['log_fields'] = '"' + ' << " '.join(
                f'{c["column"]}: " << {c["column"]}' for c in pk_columns
            )
            pk['value_log_fields'] = '"' + ' << " '.join(
                f'{c["column"]}: " << v.'
                + ('identity.' if c.get('is_identity_group_column') else '')
                + c["column"]
                for c in pk_columns
            )
            # Batch (vector) overloads: sqlgen has no tuple/composite IN, so
            # the query fetches a per-column .in() candidate superset (a
            # cross-product over-fetch for compound keys) and the exact
            # requested key-tuples are then filtered in C++ -- see
            # getml/sqlgen#107 and the "Support compound primary keys in
            # repository templates" task for why.
            pk['batch_params'] = ', '.join(
                f'const std::vector<std::string>& {c["column"]}s' for c in pk_columns
            )
            pk['batch_args'] = ', '.join(f"{c['column']}s" for c in pk_columns)
            pk['batch_empty_check'] = ' || '.join(
                f"{c['column']}s.empty()" for c in pk_columns
            )
            pk['batch_in_and'] = ' && '.join(
                f'"{c["column"]}"_c.in({c["column"]}s)' for c in pk_columns
            )
            pk['batch_tuple_type'] = ', '.join('std::string' for _ in pk_columns)
            pk['batch_requested_insert'] = (
                'requested.emplace(' +
                ', '.join(f"{c['column']}s[i]" for c in pk_columns) +
                ');'
            )
            pk['batch_requested_size_check'] = pk_columns[0]['column'] + 's.size()'
            pk['batch_tuple_from_item'] = ', '.join(
                f"item.{c['column']}" for c in pk_columns
            )
            # Every batch loop is bounded by the first column's vector size
            # alone and indexes the rest unchecked -- guard against a caller
            # (e.g. a NATS request decoded with independently-sized vector
            # fields) passing mismatched lengths, which would otherwise be
            # an out-of-bounds read/UB rather than a caught error.
            pk['batch_size_mismatch_check'] = ' || '.join(
                f"{c['column']}s.size() != {pk_columns[0]['column']}s.size()"
                for c in pk_columns[1:]
            )
            # Compound-key batch remove cannot use the over-fetch/filter
            # trick (a DELETE already executed can't be filtered after the
            # fact, and a per-column cross-product .in() would delete rows
            # outside the requested tuples) -- it loops single-tuple
            # removes instead.
            pk['batch_loop_args'] = ', '.join(
                f"{c['column']}s[i]" for c in pk_columns
            )
            # Notify-trigger helpers: emit one changed_<column> variable per
            # key column (not just the first) so a compound key's change
            # notification can distinguish two rows that share only their
            # leading key column (e.g. subject_area's name across domains).
            pk['notify_declarations'] = '\n    '.join(
                f"changed_{c['column']} {c['type']};" for c in pk_columns
            )
            pk['notify_assign_old'] = '\n        '.join(
                f"changed_{c['column']} := OLD.{c['column']};" for c in pk_columns
            )
            pk['notify_assign_new'] = '\n        '.join(
                f"changed_{c['column']} := NEW.{c['column']};" for c in pk_columns
            )
            pk['notify_id_array'] = ', '.join(
                f"changed_{c['column']}" for c in pk_columns
            )
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
            # Cross-plugin export macro: opt-in via has_export_macro, for entities
            # whose generated Qt classes are constructed from another qt/* plugin's
            # shared library (built with -fvisibility=hidden) and therefore need a
            # dllexport-style annotation to resolve at load time.
            if qt.get('has_export_macro') and component:
                if 'export_macro' not in qt:
                    qt['export_macro'] = f'ORES_QT_{component.upper()}_EXPORT'
                if 'export_header' not in qt:
                    qt['export_header'] = f'{component.capitalize()}Export.hpp'
            # Mark last item in columns for template iteration
            qt.setdefault('qt_settings_version', 1)
            if 'columns' in qt:
                _mark_last_item(qt['columns'])
                # Compute has_description_column flag
                qt['has_description_column'] = any(
                    c.get('enum_name') == 'Description'
                    for c in qt['columns']
                )
                # Columns hidden from the list view by default: any column
                # explicitly marked hidden_by_default in the org model, plus
                # Description (a long free-text field, hidden for every
                # entity that has one — a generic rule, not a per-entity
                # exception).
                seen_enum_names = set()
                hidden_columns = []
                for c in qt['columns']:
                    if c.get('enum_name') in seen_enum_names:
                        continue
                    if c.get('hidden_by_default') or c.get('enum_name') == 'Description':
                        hidden_columns.append(c)
                        seen_enum_names.add(c.get('enum_name'))
                if hidden_columns:
                    _mark_last_item(hidden_columns)
                qt['hidden_columns'] = hidden_columns
                # Cross-reference qt columns with domain columns to flag optionals:
                # when the underlying domain column is std::optional<std::string>, the
                # Qt model needs to unwrap before QString::fromStdString.
                domain_col_types = {
                    c.get('name'): c.get('cpp_type', '')
                    for c in domain_entity.get('columns', [])
                }
                # Columns that also carry a DecorationRole icon (per the
                # "Icon columns (Qt model)" table) need icon-aware rendering
                # even though they display text too — otherwise they fall
                # through to the text_left default, which paints the icon
                # via Qt's own QStyledItemDelegate path using the view's
                # blanket iconSize() directly instead of the height-
                # constrained, aspect-preserving sizing every other icon
                # column gets. Harmless when a view has only one icon shape
                # (e.g. currency's own single flag column), but visibly
                # wrong the moment a view mixes shapes (e.g. currency_pair's
                # narrow BaseCurrency/QuoteCurrency next to its wide
                # composited PairCode column).
                icon_column_names = {
                    ic.get('column') for ic in qt.get('icon_columns', [])
                }
                if qt.get('flag_icon_column'):
                    icon_column_names.add(qt['flag_icon_column'])
                for idx, qt_col in enumerate(qt['columns']):
                    field = qt_col.get('field')
                    cpp_type = domain_col_types.get(field, '')
                    if qt_col.get('is_string') and cpp_type.startswith('std::optional<'):
                        qt_col['is_optional_string'] = True
                        qt_col['is_string'] = False
                    if qt_col.get('is_int') and cpp_type.startswith('std::optional<'):
                        qt_col['is_optional_int'] = True
                        qt_col['is_int'] = False
                    if qt_col.get('is_double') and cpp_type.startswith('std::optional<'):
                        qt_col['is_optional_double'] = True
                        qt_col['is_double'] = False
                    # Auto-assign column index for badge resolver calls
                    qt_col.setdefault('column_index', idx)
                    # Default column_style when not specified. self_colour
                    # (e.g. badge_definition's own background_colour/
                    # text_colour columns) shares badge_centered's pill
                    # rendering but not is_badge's BadgeCache-lookup
                    # resolver — see has_self_colour_columns below.
                    if 'column_style' not in qt_col:
                        if qt_col.get('is_badge') or qt_col.get('self_colour'):
                            qt_col['column_style'] = 'cs::badge_centered'
                        elif qt_col.get('enum_name') in icon_column_names:
                            qt_col['column_style'] = 'cs::icon_text_left'
                        elif qt_col.get('is_int') or qt_col.get('is_optional_int'):
                            qt_col['column_style'] = 'cs::mono_center'
                        else:
                            qt_col['column_style'] = 'cs::text_left'
                # Compute has_badge_columns flag — also true when a detail
                # field's static_combo uses the badge system (needs the same
                # BadgeCache threaded to the controller even with no badge
                # list columns), not just list-view is_badge columns.
                qt['has_badge_columns'] = (
                    any(c.get('is_badge') for c in qt['columns'])
                    or qt.get('has_combo_badge_source', False)
                )
                qt['has_date_columns'] = any(c.get('is_date') for c in qt['columns'])
                # self_colour columns (e.g. badge_definition's own
                # background_colour/text_colour) render badge_centered too,
                # but the column's own value IS the colour — no
                # BadgeCache lookup needed, so this stays separate from
                # has_badge_columns (which gates BadgeCache wiring).
                qt['has_self_colour_columns'] = any(
                    c.get('self_colour') for c in qt['columns']
                )
                # EntityItemDelegate (icon_centered/icon_text_left sizing,
                # badge_centered rendering) is needed whenever the list view
                # has ANY icon, badge, or self-colour column — not just
                # badge columns. Gating it on has_badge_columns alone left
                # icon-only entities (e.g. country, with just its own flag
                # column) rendering that icon through Qt's default item-view
                # path, which uses decorationSize directly instead of the
                # aspect-correct, height-constrained sizing the delegate
                # provides — the same class of bug fixed for currency_pair's
                # mixed icon/text columns, just not yet visibly wrong for a
                # single-icon-column view. EntityItemDelegate's constructor
                # doesn't require a BadgeCache, so this is safe to widen
                # independently of badgeCache_ (which stays gated on
                # has_badge_columns, since only badge columns need it).
                qt['needs_item_delegate'] = (
                    qt['has_badge_columns'] or qt.get('has_any_flag_icon', False)
                    or qt['has_self_colour_columns']
                )
            # has_explorer_api opts a controller into the public
            # openAdd()/openEdit()/openHistory() surface a sibling explorer
            # window (e.g. PortfolioExplorer, OrgExplorer) needs to drive it
            # from outside. parent_entity_singular is an optional companion:
            # when set, the controller additionally gets
            # openAddWithParent(boost::uuids::uuid parent<Pascal>Id) for
            # explorers that create this entity nested under a parent node.
            parent_entity_singular = qt.get('parent_entity_singular')
            qt['has_parent_relationship'] = bool(
                qt.get('has_explorer_api') and parent_entity_singular
            )
            if qt['has_parent_relationship']:
                parent_pascal = snake_to_pascal(parent_entity_singular)
                qt['parent_entity_pascal'] = parent_pascal
                qt.setdefault('parent_id_field_camel', f'parent{parent_pascal}Id')
                qt.setdefault('parent_id_field', f'parent_{parent_entity_singular}_id')
            # explorer_interface is an optional companion to has_explorer_api:
            # when a cross-component explorer window (e.g. OrgExplorerMdiWindow
            # in ores.qt.trading) needs to drive this controller's openEdit/
            # openHistory without linking against its concrete header, the
            # entity model names an abstract interface (hand-authored
            # elsewhere, e.g. ores.qt.api/IBusinessUnitBrowser.hpp) that the
            # generated Controller additionally implements. Same-component
            # explorers (PortfolioExplorer/OrgExplorer for Book, both in
            # ores.qt.trading) need no such interface — leave unset.
            validate_explorer_interface(domain_entity)
            qt['has_explorer_interface'] = bool(qt.get('explorer_interface'))
            validate_parent_scoped_list(domain_entity)
            # Add iterator variable reference for templates
            qt['item_var'] = qt.get('item_var', 'item')
            # Auto-generate default detail_fields if not provided. The
            # default shape is the code+name+description lookup form: a
            # key row plus a display-name row, each gated on the column
            # existing. An entity whose key field IS name (compute app)
            # has no separate display name, so the key row alone carries
            # it, named after the field like every other row; emitting a
            # second nameEdit would bind two widgets to one column. An
            # entity with no name column at all (compute batch, result,
            # ...) gets no display-name row either -- the name row must
            # not be emitted unconditionally, the way description is
            # gated below, or the dialog binds a phantom member.
            if 'detail_fields' not in qt:
                key_field = qt.get('key_field', 'code')
                column_names = {c.get('name') for c in domain_entity.get('columns', [])}
                # A display name carried as the natural key (refdata's
                # code-keyed lookups, e.g. rounding_type) is a real
                # member too, but the parser catalogues natural keys
                # under natural_keys, not columns -- include them or the
                # name row below is gated off for every such lookup.
                column_names.update(
                    nk.get('column')
                    for nk in domain_entity.get('natural_keys', [])
                    if nk.get('column'))
                key_is_name = key_field == 'name'
                fields = [
                    {'field': key_field, 'label': key_field.replace('_', ' ').title(),
                     'widget': 'nameEdit' if key_is_name else 'codeEdit',
                     'type': 'line_edit', 'is_key': True, 'is_required': True,
                     'placeholder': 'Enter ' + domain_entity.get('entity_singular_words', 'item') + ' ' + key_field.replace('_', ' ')},
                ]
                if not key_is_name and 'name' in column_names:
                    fields.append(
                        {'field': 'name', 'label': 'Name', 'widget': 'nameEdit',
                         'type': 'line_edit', 'is_required': True,
                         'placeholder': 'Enter display name'})
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
            date_fields = []
            domain_col_types = {
                c.get('name'): c.get('cpp_type', '')
                for c in domain_entity.get('columns', [])
            }
            for nk in domain_entity.get('natural_keys', []):
                if nk.get('column'):
                    domain_col_types.setdefault(nk['column'], nk.get('cpp_type', ''))
            _pk = domain_entity.get('primary_key')
            if _pk and _pk.get('column'):
                domain_col_types.setdefault(_pk['column'], _pk.get('cpp_type', ''))
            for i, f in enumerate(detail_fields):
                f['is_line_edit'] = f.get('type') == 'line_edit'
                f['is_text_edit'] = f.get('type') in ('text_edit', 'plain_text_edit')
                f['is_static_combo'] = f.get('type') == 'static_combo'
                f['is_dynamic_combo'] = f.get('type') == 'dynamic_combo'
                if (f['is_static_combo'] or f['is_dynamic_combo']) and f.get('badge_key'):
                    # Promote to the marker class whose closed-box paintEvent
                    # renders the current selection as a badge pill too, not
                    # just the popup rows apply_combo_badges() already
                    # colours -- same shape as combo_widget_class for a
                    # currency flagged_combo. Applies equally to a fixed
                    # (static_combo) or DB-backed (dynamic_combo) badge list.
                    f.setdefault('combo_widget_class', 'ores::qt::OreBadgeComboBox')
                    f.setdefault('combo_widget_extends', 'QComboBox')
                    f.setdefault('combo_widget_header', 'ores.qt/OreBadgeComboBox.hpp')
                if f['is_dynamic_combo'] and f.get('combo_fetch_fn'):
                    field_pascal = snake_to_pascal(f.get('field', ''))
                    f.setdefault('combo_setter_pascal', field_pascal)
                    f.setdefault('combo_items_member', field_pascal[0].lower() + field_pascal[1:])
                # A flagged_combo is a single-select combo asynchronously
                # populated from a plain code list (e.g. fetch_currency_codes),
                # with an optional per-item flag icon. Generalises the
                # hand-duplicated pattern in ~10 trading forms
                # (OreCurrencyComboBox + apply_flag_icons); distinct from
                # is_dynamic_combo (struct-shaped code/description/
                # display_order lookups, e.g. rounding_type) since a plain
                # code list has no description/sort-order fields to show.
                f['is_flagged_combo'] = f.get('type') == 'flagged_combo'
                if f['is_flagged_combo']:
                    f.setdefault('combo_allow_blank', False)
                    flag_source = f.get('flag_source', 'currency')
                    f['flag_source_pascal'] = snake_to_pascal(flag_source)
                    # currency_pair icons are twice as wide as a single flag
                    # (two composited flags) -- squeezing one into the
                    # square box single_flag_icon_size() gives every other
                    # flag source would squash it. currency_pair_icon_size()
                    # keeps the same height (so it still matches every other
                    # flag-bearing widget in the app) but reserves the extra
                    # width.
                    f['flag_icon_size_expr'] = ('currency_pair_icon_size()'
                                                if flag_source == 'currency_pair'
                                                else 'single_flag_icon_size()')
                    # A currency flagged_combo has a ready-made shared
                    # implementation (setup_currency_combo) and promoted
                    # widget class (OreCurrencyComboBox, for uic's standard
                    # combo-popup sizing/positioning) -- reuse both instead
                    # of falling back to the generic per-entity inlined
                    # fetch and a plain QComboBox. Other flag sources
                    # (country, business_centre) have no such helper yet
                    # and keep the generic inline path.
                    if flag_source == 'currency':
                        f.setdefault('combo_helper', 'setup_currency_combo')
                        f.setdefault('combo_widget_class', 'ores::qt::OreCurrencyComboBox')
                        f.setdefault('combo_widget_extends', 'QComboBox')
                        f.setdefault('combo_widget_header', 'ores.qt/OreCurrencyComboBox.hpp')
                f['is_check_box'] = f.get('type') == 'check_box'
                f['is_spin_box'] = f.get('type') == 'spin_box'
                # Colour swatch button (QColorDialog-backed) — see
                # ColourSwatchHelper.hpp. Distinct from is_badge/combo
                # fields: a colour field's own value IS the colour, not a
                # code resolved against a separate badge_definition.
                f['is_colour'] = f.get('type') == 'colour'
                if f['is_colour']:
                    f.setdefault('placeholder', '#rrggbb')
                field_cpp = domain_col_types.get(f.get('field'), '')
                f['is_optional_string'] = (
                    field_cpp.startswith('std::optional<std::string>')
                    and (f['is_line_edit'] or f['is_text_edit'] or f['is_flagged_combo']
                         or f['is_static_combo'] or f['is_dynamic_combo'])
                )
                # UUID type detection — needed for boost::uuids::to_string() conversions
                _is_any_uuid = 'boost::uuids::uuid' in field_cpp
                f['is_optional_uuid'] = 'std::optional<boost::uuids::uuid>' in field_cpp
                f['is_uuid'] = _is_any_uuid and not f['is_optional_uuid']
                # Self-referencing dynamic combo (e.g. party's own
                # parent_party_id, fetched from the party list itself):
                # the fetched item's own id can coincide with the
                # currently-edited entity's own id, which must be excluded
                # from the combo -- both to prevent self-parenting and
                # because a self-referencing row breaks the recursive
                # hierarchy CTE (UNION ALL never dedupes, so it loops
                # forever). Detected purely by combo_domain_type matching
                # this entity's own domain_class, no extra model property
                # needed.
                f['is_self_referencing_combo'] = (
                    f.get('combo_domain_type') and
                    f.get('combo_domain_type') == qt.get('domain_class')
                )
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
                # Nullable double edits with an optional<double> field:
                # empty text reads back as nullopt, non-empty as the
                # parsed value (mirrors is_nullable_int's spin-box
                # sentinel).
                f['is_optional_double'] = (
                    f['is_line_edit']
                    and field_cpp == 'std::optional<double>'
                )
                # A plain QLineEdit editing an ISO-8601 date
                # ("YYYY-MM-DD"), converted to/from
                # std::chrono::year_month_day via ores.platform's
                # datetime helpers -- no dedicated QDateEdit widget, to
                # keep this facet's widget vocabulary small; add one if
                # a future entity needs a real date picker.
                f['is_date'] = (
                    f['is_line_edit']
                    and field_cpp == 'std::chrono::year_month_day'
                )
                if f['is_date']:
                    date_fields.append({'field': f['field'], 'widget': f['widget']})
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
                # immutable: an identity-defining field that isn't the
                # primary key itself (e.g. currency_pair's base_currency/
                # quote_currency) but must never change after creation --
                # locked the same way as is_key: editable while
                # createMode_ is true, disabled and excluded from
                # update...FromUi() otherwise.
                f['is_immutable'] = bool(f.get('immutable'))
                f['is_locked_after_create'] = f['is_key'] or f['is_immutable']
                # Derive value_widget for history dialog (e.g. codeEdit->codeValue, nameCombo->nameValue)
                widget = f.get('widget', f['field'] + 'Edit')
                f['value_widget'] = widget.replace('Edit', 'Value').replace('Combo', 'Value')
                # Derive label_widget for detail dialog form labels (e.g. code -> labelCode)
                f['label_widget'] = 'label' + snake_to_pascal(f.get('field', ''))
                # Derive field_pascal for generated method names (e.g. base_currency -> BaseCurrency)
                f['field_pascal'] = snake_to_pascal(f.get('field', ''))
                if f.get('is_required') and f.get('is_line_edit'):
                    required_fields.append({
                        'field': f['field'],
                        'widget': f['widget'],
                        '_is_last': False,
                    })
                # static_combo/dynamic_combo/flagged_combo are all QComboBox-backed
                # (currentIndex() >= 0), not QLineEdit-backed (.text()) -- a
                # required static_combo field misclassified here would generate a
                # ui_->{{widget}}->text() call that doesn't compile against QComboBox.
                if f.get('is_required') and (f.get('is_dynamic_combo') or
                                              f.get('is_flagged_combo') or
                                              f.get('is_static_combo')):
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
            qt['date_fields'] = date_fields
            # Expose the key field's widget name for setCreateMode
            key_field_data = next((f for f in detail_fields if f.get('is_key')), None)
            qt['key_widget'] = key_field_data['widget'] if key_field_data else 'codeEdit'
            # onCodeChanged() only makes sense (and is only ever connected)
            # for an is_key field that's a QLineEdit -- a combo-widget key
            # (e.g. currency_pair_convention's pair_code) wires to the
            # generic onFieldChanged() like any other combo instead. Gate
            # the method's generation so it doesn't sit as unreachable dead
            # code for entities whose key isn't a line_edit.
            qt['has_line_edit_key'] = bool(
                key_field_data and key_field_data.get('is_line_edit'))
            # Every field locked after create (is_key or immutable):
            # setCreateMode() disables each by its own widget kind
            # (setReadOnly for a text field, setEnabled(false) for a
            # combo) rather than assuming every locked field is the
            # single QLineEdit key_widget above.
            locked_fields = [
                {
                    'widget': f['widget'],
                    'is_line_edit': f['is_line_edit'],
                    'is_text_edit': f['is_text_edit'],
                    'is_static_combo': f['is_static_combo'],
                    'is_dynamic_combo': f['is_dynamic_combo'],
                    'is_flagged_combo': f['is_flagged_combo'],
                    'is_check_box': f['is_check_box'],
                    'is_colour': f['is_colour'],
                    # The primary key's own line_edit is never
                    # user-editable when the entity has a UUID primary
                    # key: setCreateMode() auto-generates it
                    # (has_uuid_primary_key's random_generator() block
                    # below), so it must stay read-only even while
                    # createMode is true -- not just after create like
                    # every other locked field. Must match on the field
                    # actually being the entity's own UUID primary key
                    # column (like key_field_is_uuid below), not merely
                    # "some is_key field on an entity that happens to
                    # have a UUID primary key" -- otherwise a natural/
                    # business key field (e.g. party's short_code, which
                    # is user-entered, not generated) on an entity that
                    # separately has a UUID id gets wrongly locked for
                    # the entire lifetime of the Create dialog instead of
                    # just after create.
                    'is_auto_generated_key': (
                        f['is_key'] and qt.get('has_uuid_primary_key', False) and
                        f.get('field') == domain_entity.get('primary_key', {}).get('column', '')
                    ),
                }
                for f in detail_fields if f['is_locked_after_create']
            ]
            qt['locked_fields'] = locked_fields
            qt['has_locked_fields'] = bool(locked_fields)
            # Default has_pagination to False if not set
            qt['has_pagination'] = qt.get('has_pagination', False)
            # Default has_readonly_paginated_list to False if not set
            qt['has_readonly_paginated_list'] = qt.get('has_readonly_paginated_list', False)
            # Default has_parent_scoped_list to False if not set. Paired
            # with parent_key_field (the protocol request field, e.g.
            # calendar_code) and parent_key_param (the C++ member/
            # parameter name) -- both required when the knob is set
            # (enforced by validate_parent_scoped_list above), since the
            # parent key belongs to a different entity and there's
            # nothing on this entity to derive either from. Distinct from
            # parent_entity_singular/has_parent_relationship above, which
            # solves a different problem (pre-filling a foreign key on
            # create for a still-full-CRUD entity, not filtering a
            # read-only list's get-request).
            qt['has_parent_scoped_list'] = qt.get('has_parent_scoped_list', False)
            qt['has_text_edit_fields'] = any(
                f.get('type') in ('text_edit', 'plain_text_edit') for f in detail_fields
            )
            qt['has_combo_fields'] = any(
                f.get('type') in ('static_combo', 'dynamic_combo', 'flagged_combo')
                for f in detail_fields
            )
            qt['has_dynamic_combo_fields'] = any(
                f.get('type') == 'dynamic_combo' for f in detail_fields
            )
            qt['has_flagged_combo_fields'] = any(
                f.get('type') == 'flagged_combo' for f in detail_fields
            )
            qt['has_static_combo_fields'] = any(
                f.get('type') == 'static_combo' for f in detail_fields
            )
            qt['has_colour_fields'] = any(
                f.get('type') == 'colour' for f in detail_fields
            )
            # Deduplicated <customwidgets> entries for any promoted combo
            # widget class a detail field resolved to above (e.g. every
            # currency-flag combo needs the same OreCurrencyComboBox
            # registration once, however many currency fields the entity
            # has). Computed here, not in org_loader.py: it depends on
            # combo_widget_class values that is_flagged_combo/is_dynamic_combo
            # handling above defaults in, which runs after org_loader.py.
            seen_widget_classes = set()
            combo_customs = []
            for f in detail_fields:
                cls = f.get('combo_widget_class')
                if not cls or cls in seen_widget_classes:
                    continue
                seen_widget_classes.add(cls)
                combo_customs.append({
                    'class': cls,
                    'extends': f.get('combo_widget_extends', 'QComboBox'),
                    'header': f.get('combo_widget_header', ''),
                })
            if combo_customs:
                qt['combo_widget_customs'] = combo_customs
            # Whether any static_combo or dynamic_combo detail field renders
            # its items as badges (via the badge system, apply_combo_badges
            # resolves purely off (badge_key, item text) at paint time — it
            # doesn't care how the combo was populated) — gates BadgeCache
            # wiring into the detail dialog itself (has_badge_columns only
            # wires it into the list/MDI window).
            qt['has_combo_badge_source'] = any(
                f.get('badge_key') for f in detail_fields
                if f.get('type') in ('static_combo', 'dynamic_combo')
            )
            # Gates the datetime.hpp include and the setX()/setReadOnly()
            # re-populate calls below -- see has_as_of_combo_fields's own
            # docstring for what combo_as_of_fetch_fn is for.
            qt['has_as_of_combo_fields'] = has_as_of_combo_fields(detail_fields)
            qt['has_uuid_detail_fields'] = any(
                f.get('is_uuid') or f.get('is_optional_uuid') for f in detail_fields
            )
            qt['has_date_detail_fields'] = any(f.get('is_date') for f in detail_fields)
            # A `party_id` natural key that is *not* exposed as a Detail
            # field is implicit-from-session by convention (see book/
            # portfolio) -- setCreateMode must still populate it from the
            # active party, or every row created via this dialog gets a
            # nil party_id silently accepted by the (equally nil-tolerant)
            # domain type. Only fires for a genuine UUID party_id; a
            # party_id shown in the Detail form is user-editable instead
            # and handled by the normal field-binding code, not here.
            party_id_field = next(
                (nk for nk in domain_entity.get('natural_keys', [])
                 if nk.get('column') == 'party_id'
                 and 'boost::uuids::uuid' in nk.get('cpp_type', '')),
                None
            )
            detail_field_names = {f.get('field') for f in detail_fields}
            qt['hidden_party_id_on_create'] = bool(
                party_id_field and 'party_id' not in detail_field_names
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
            # History response data field: most protocols use 'history', but
            # some older ones (predating that convention) name it after the
            # entity's plural collection instead (e.g. cds_convention's
            # get_cds_convention_history_response.cds_conventions) -- those
            # entities set :history_response_data_field: explicitly in their
            # model to override this default.
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
            # Group detail_fields by their optional view_group cell into Qt
            # detail-dialog tabs (see codegen_input_org_schema.org). Computed
            # here, after detail_fields is fully finalized (both the
            # org-provided and auto-generated-above cases), not in
            # org_loader.py, so it uses the same fully-enriched field dicts
            # the .ui template already renders per-field widgets from.
            qt['view_groups'] = compute_view_groups(detail_fields)
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
        # Dedicated ground-truth counterpart to has_uuid_columns for the
        # history field mapper's own include guard: has_uuid_columns is
        # derived from is_uuid/is_optional_uuid, which factor in `nullable`
        # (a repository/SQL-layer promotion decision) rather than the raw
        # cpp_type the render_is_uuid/render_is_optional_uuid flags key off
        # — reusing has_uuid_columns here would silently drift out of sync
        # with the mapper's actual dispatch the same way the mapper's
        # rendering itself once drifted from is_nullable_string (see the
        # history-field-mapper task's Notes).
        has_render_uuid_cols = any(
            col.get('render_is_uuid') or col.get('render_is_optional_uuid')
            for col in domain_entity.get('columns', [])
        )
        domain_entity['has_render_uuid_columns'] = (
            has_render_uuid_cols or has_uuid_nat_keys
            or domain_entity.get('primary_key', {}).get('is_uuid', False)
        )
        # cpp_service.hpp's <boost/uuid/uuid.hpp> gate: needed by both the
        # existing has_parent_id hierarchy methods and the additive
        # service_find_by_uuid overloads (find_X/remove_X/get_X_history).
        domain_entity['has_uuid_include'] = (
            domain_entity.get('has_parent_id', False)
            or domain_entity.get('service_find_by_uuid', False)
            or domain_entity.get('service_find_by_code', {}).get('parent_column')
            or any(
                fk.get('list_by_uuid')
                for fk in domain_entity.get('foreign_keys', [])
            )
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
                col['is_date'] = (col.get('type') == 'date' or
                                  col.get('cpp_type') == 'std::chrono::year_month_day')
                col['iter_var'] = iter_var
        # Add lowercase versions and UUID/date flags for left/right columns
        if 'left' in junction:
            if 'column_title' in junction['left']:
                junction['left']['column_title_lower'] = junction['left']['column_title'].lower()
            junction['left']['is_uuid'] = junction['left'].get('type') == 'uuid'
            junction['left']['is_date'] = (
                junction['left'].get('type') == 'date' or
                junction['left'].get('cpp_type') == 'std::chrono::year_month_day'
            )
        if 'right' in junction:
            if 'column_title' in junction['right']:
                junction['right']['column_title_lower'] = junction['right']['column_title'].lower()
            junction['right']['is_uuid'] = junction['right'].get('type') == 'uuid'
            junction['right']['is_date'] = (
                junction['right'].get('type') == 'date' or
                junction['right'].get('cpp_type') == 'std::chrono::year_month_day'
            )
        junction['has_uuid_left_or_right'] = (
            junction.get('left', {}).get('is_uuid', False) or
            junction.get('right', {}).get('is_uuid', False)
        )
        junction['has_date_left_or_right_or_column'] = (
            junction.get('left', {}).get('is_date', False) or
            junction.get('right', {}).get('is_date', False) or
            any(c.get('is_date') for c in junction.get('columns', []))
        )
        # Mirrors domain_entity's needs_counter: only declare the
        # generator's counter/idx local when some generator_expr actually
        # references it -- otherwise it's an unused variable for every
        # junction whose left/right/columns codes don't need a uniqueness
        # suffix (e.g. badge_mapping).
        junction['needs_counter'] = any(
            'idx' in (side.get('generator_expr') or '')
            for side in (junction.get('left', {}), junction.get('right', {}))
        ) or any(
            'idx' in (col.get('generator_expr') or '')
            for col in junction.get('columns', [])
        )
        # Format description as comment block lines (for SQL)
        if 'description' in junction:
            junction['description_formatted'] = _format_description_as_comment(junction['description'])
            # Split description into lines for C++ doxygen comments
            junction['description_lines'] = junction['description'].split('\n')
        # Add uppercase versions for C++ include guards
        if 'component' in junction:
            junction['component_upper'] = junction['component'].upper()
            # Derive component_include/component_core/... for #include lines
            # and C++ namespacing, mirroring the domain_entity branch above
            # (see _component_path_vars, shared with resolve_output_path's
            # equivalent output-path derivation).
            junction.update(_component_path_vars(junction))
            junction['component_include_upper'] = (
                junction['component_include'].replace('.', '_').upper()
            )
            junction['component_core_upper'] = (
                junction['component_core'].replace('.', '_').upper()
            )
            junction['component_service_upper'] = (
                junction['component_service'].replace('.', '_').upper()
            )
            junction['cache_component_upper'] = (
                junction['cache_component'].replace('.', '_').upper()
            )
            junction['cache_subcomponent_upper'] = junction['cache_subcomponent'].upper()
            junction['generator_facet_name_upper'] = (
                junction['generator_facet_name'].upper()
            )
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
            # Collect UUID/optional/bool column names for table display,
            # mirroring the domain_entity branch above (fort::char_table's
            # operator<< can't stream std::optional<non-string> directly --
            # bool needs the same true/false ternary treatment).
            uuid_columns = set()
            optional_columns = set()
            bool_columns = set()
            if junction.get('left', {}).get('is_uuid'):
                uuid_columns.add(junction['left']['column'])
            if junction.get('right', {}).get('is_uuid'):
                uuid_columns.add(junction['right']['column'])
            if 'columns' in junction:
                for col in junction['columns']:
                    if col.get('is_uuid') or col.get('is_optional_uuid'):
                        uuid_columns.add(col['name'])
                    # A column needs opt_str() wrapping (fort::char_table
                    # has no operator<< for std::optional<T>) only when its
                    # cpp_type is genuinely std::optional<...> -- a nullable
                    # column whose author left cpp_type as a plain (non-
                    # optional) type streams as-is, same as domain_entity's
                    # is_nullable_string carve-out.
                    if (col.get('cpp_type') or '').strip().startswith('std::optional<') \
                            and not col.get('is_optional_uuid'):
                        optional_columns.add(col['name'])
                    if (col.get('cpp_type') or '').strip() == 'bool':
                        bool_columns.add(col['name'])
            _prepare_table_display(junction['cpp'], uuid_columns, optional_columns, bool_columns)
        # Copy repository section fields to top level for template access
        if 'repository' in junction:
            for key, value in junction['repository'].items():
                junction[key] = value
        data['junction'] = junction
        # A junction with a ** Qt drawer renders through the ores.cpp.qt
        # facet exactly like a domain_entity: org_loader.py's
        # load_org_junction_model already aliased the entity_singular/
        # entity_plural/entity_pascal/... family (and repository.
        # entity_plural_short) onto this same dict when it parsed the
        # drawer, so the (already fully-enriched, component_include and
        # all) junction dict can stand in for 'domain_entity' as-is --
        # no separate enrichment pass needed, the domain_entity-only
        # block above never runs for a junction model.
        if generate_qt and 'qt' in junction:
            data['domain_entity'] = junction

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


