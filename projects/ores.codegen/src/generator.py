"""
Simple code generator that loads data and applies templates.
"""
import json
import os
import random
from pathlib import Path
import pystache
from datetime import datetime


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

    # Process modelines to create a lookup dictionary
    if 'modeline' in data:
        modeline_lookup = {}
        for item in data['modeline']:
            modeline_lookup[item['name']] = item['content']
        data['modelines'] = modeline_lookup

    return data


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
            formatted_line = " *"  # Just the asterisk for empty lines within the comment block
        formatted_lines.append(formatted_line)

    # Combine everything with proper start and end markers, including the modeline
    result = f"{fmt['start']} -*- {modeline_info} -*-\n" + '\n'.join(formatted_lines) + f"\n{fmt['end']}"

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


def is_entity_schema_model(model_filename):
    """
    Check if a model file is an entity schema model.

    Args:
        model_filename (str): The model filename

    Returns:
        bool: True if this is an entity schema model
    """
    return model_filename.endswith("_entity.json")


def is_entity_data_model(model_filename):
    """
    Check if a model file is an entity data model (for populate scripts).

    Args:
        model_filename (str): The model filename

    Returns:
        bool: True if this is an entity data model
    """
    return model_filename.endswith("_data.json")


def is_domain_entity_model(model_filename):
    """
    Check if a model file is a domain entity model.

    Args:
        model_filename (str): The model filename

    Returns:
        bool: True if this is a domain entity model
    """
    return model_filename.endswith("_domain_entity.json")


def is_junction_model(model_filename):
    """
    Check if a model file is a junction table model.

    Args:
        model_filename (str): The model filename

    Returns:
        bool: True if this is a junction table model
    """
    return model_filename.endswith("_junction.json")


def is_enum_model(model_filename):
    """
    Check if a model file is an enum model.

    Args:
        model_filename (str): The model filename

    Returns:
        bool: True if this is an enum model
    """
    return model_filename.endswith("_enum.json")


def get_model_type(model_filename):
    """
    Determine the model type from the filename.

    Args:
        model_filename (str): The model filename

    Returns:
        str: The model type ('domain_entity', 'junction', 'enum', 'schema', 'data', or 'unknown')
    """
    if is_domain_entity_model(model_filename):
        return 'domain_entity'
    elif is_junction_model(model_filename):
        return 'junction'
    elif is_enum_model(model_filename):
        return 'enum'
    elif is_entity_schema_model(model_filename):
        return 'schema'
    elif is_entity_data_model(model_filename):
        return 'data'
    return 'unknown'


def load_profiles(base_dir):
    """
    Load profile definitions from profiles.json.

    Args:
        base_dir (Path): Base directory of the codegen project

    Returns:
        dict: Dictionary of profile definitions
    """
    profiles_path = base_dir / "library" / "profiles.json"
    if not profiles_path.exists():
        return {}

    with open(profiles_path, 'r', encoding='utf-8') as f:
        data = json.load(f)
    return data.get('profiles', {})


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
        entity_singular = entity.get('entity_singular', 'unknown')
        entity_plural = entity.get('entity_plural', entity_singular + 's')
        entity_pascal = snake_to_pascal(entity_singular)

        result = result.replace('{component}', component)
        result = result.replace('{entity}', entity_singular)
        result = result.replace('{entity_plural}', entity_plural)
        result = result.replace('{EntityPascal}', entity_pascal)

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

    elif model_type == 'enum' and 'enum' in model_data:
        enum = model_data['enum']
        component = enum.get('component', 'unknown')
        enum_name = enum.get('name', 'unknown')

        result = result.replace('{component}', component)
        result = result.replace('{enum_name}', enum_name)

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
        entity_singular = entity.get('entity_singular', 'unknown')
        entity_plural = entity.get('entity_plural', entity_singular + 's')
        entity_pascal = snake_to_pascal(entity_singular)

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


def get_domain_entity_template_mappings():
    """
    Define the mapping for domain entity schema templates.

    Returns:
        list: List of tuples (template_name, output_suffix) for domain entity generation
    """
    return [
        ("sql_schema_domain_entity_create.mustache", "_create.sql"),
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
        # Protocol facet
        ("cpp_protocol.hpp.mustache", "include/{component}/messaging", "_protocol.hpp"),
        ("cpp_protocol.cpp.mustache", "src/messaging", "_protocol.cpp"),
    ]


def get_cpp_junction_template_mappings():
    """
    Define the mapping for C++ junction table templates.

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
        # Protocol facet
        ("cpp_protocol.hpp.mustache", "include/{component}/messaging", "_protocol.hpp"),
        ("cpp_protocol.cpp.mustache", "src/messaging", "_protocol.cpp"),
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


def get_populate_template_mappings():
    """
    Define the mapping for entity populate templates (per-dataset).

    Returns:
        list: List of tuples (template_name, output_prefix, output_suffix) for populate generation
    """
    return [
        ("sql_populate_refdata.mustache", "fpml_", "_artefact_populate.sql"),
        ("sql_dataset_refdata.mustache", "fpml_", "_dataset_populate.sql"),
    ]


def get_non_iso_currency_template_mappings():
    """
    Define the mapping for non-ISO currency populate templates.

    Non-ISO currencies use the shared dq_currencies_artefact_tbl instead of
    having their own entity-specific artefact table.

    Returns:
        list: List of tuples (template_name, output_prefix, output_suffix) for populate generation
    """
    return [
        ("sql_non_iso_currency_populate.mustache", "fpml_", "_artefact_populate.sql"),
        ("sql_dataset_refdata.mustache", "fpml_", "_dataset_populate.sql"),
    ]


def load_model(model_path):
    """
    Load a model from the specified path.

    Args:
        model_path (str or Path): Path to the model file

    Returns:
        dict: The loaded model data
    """
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
    {'symbol': '$', 'fraction_symbol': '¢', 'fractions_per_unit': 100, 'rounding_type': 'Closest', 'rounding_precision': 2, 'format': '$#,##0.00', 'currency_type': 'fiat.emerging'},
    {'symbol': '€', 'fraction_symbol': 'c', 'fractions_per_unit': 100, 'rounding_type': 'Closest', 'rounding_precision': 2, 'format': '€#,##0.00', 'currency_type': 'fiat.major'},
    {'symbol': '£', 'fraction_symbol': 'p', 'fractions_per_unit': 100, 'rounding_type': 'Closest', 'rounding_precision': 2, 'format': '£#,##0.00', 'currency_type': 'fiat.major'},
    {'symbol': '¥', 'fraction_symbol': '', 'fractions_per_unit': 0, 'rounding_type': 'Closest', 'rounding_precision': 0, 'format': '¥#,##0', 'currency_type': 'fiat.emerging'},
    {'symbol': 'kr', 'fraction_symbol': 'ø', 'fractions_per_unit': 100, 'rounding_type': 'Closest', 'rounding_precision': 2, 'format': 'kr #,##0.00', 'currency_type': 'fiat.major'},
    {'symbol': 'zł', 'fraction_symbol': 'gr', 'fractions_per_unit': 100, 'rounding_type': 'Closest', 'rounding_precision': 2, 'format': '#,##0.00 zł', 'currency_type': 'fiat.emerging'},
    {'symbol': '₹', 'fraction_symbol': 'p', 'fractions_per_unit': 100, 'rounding_type': 'Closest', 'rounding_precision': 2, 'format': '₹#,##0.00', 'currency_type': 'fiat.emerging'},
    {'symbol': 'د.إ', 'fraction_symbol': 'ف', 'fractions_per_unit': 100, 'rounding_type': 'Closest', 'rounding_precision': 2, 'format': 'د.إ#,##0.00', 'currency_type': 'fiat.emerging'},
    {'symbol': 'R', 'fraction_symbol': 'c', 'fractions_per_unit': 100, 'rounding_type': 'Closest', 'rounding_precision': 2, 'format': 'R#,##0.00', 'currency_type': 'fiat.emerging'},
    {'symbol': 'ƒ', 'fraction_symbol': '¢', 'fractions_per_unit': 100, 'rounding_type': 'Closest', 'rounding_precision': 2, 'format': 'ƒ#,##0.00', 'currency_type': 'fiat.emerging'},
    {'symbol': 'د.ك', 'fraction_symbol': 'ف', 'fractions_per_unit': 1000, 'rounding_type': 'Closest', 'rounding_precision': 3, 'format': 'د.ك#,##0.000', 'currency_type': 'fiat.emerging'},
    {'symbol': 'S/', 'fraction_symbol': '¢', 'fractions_per_unit': 100, 'rounding_type': 'Closest', 'rounding_precision': 2, 'format': 'S/#,##0.00', 'currency_type': 'fiat.emerging'},
    {'symbol': '฿', 'fraction_symbol': 'ส', 'fractions_per_unit': 100, 'rounding_type': 'Closest', 'rounding_precision': 2, 'format': '฿#,##0.00', 'currency_type': 'fiat.emerging'},
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


def _prepare_table_display(cpp_section):
    """
    Prepare table_display items by adding iterator_var to each item.

    Mustache can't access parent context variables from within a loop,
    so we add the iterator_var to each table_display item.

    Args:
        cpp_section (dict): The 'cpp' section of the model
    """
    if 'table_display' not in cpp_section:
        return

    iter_var = cpp_section.get('iterator_var', 'e')
    for item in cpp_section['table_display']:
        item['iter_var'] = iter_var


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

    # Check if this is an entity schema model or data model
    is_schema_model = is_entity_schema_model(model_filename)
    is_data_model = is_entity_data_model(model_filename)
    is_domain_entity = is_domain_entity_model(model_filename)
    is_junction = is_junction_model(model_filename)
    is_enum = is_enum_model(model_filename)

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
    elif is_schema_model:
        # Entity schema models use a different template set
        templates_to_process = [t[0] for t in get_schema_template_mappings()]
    elif is_data_model:
        # Entity data models use populate templates
        # Non-ISO currencies use a special template that populates dq_currencies_artefact_tbl
        if model.get('uses_shared_currency_table'):
            templates_to_process = [t[0] for t in get_non_iso_currency_template_mappings()]
        else:
            templates_to_process = [t[0] for t in get_populate_template_mappings()]
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
            processed_item.setdefault('currency_type', defaults['currency_type'])
            
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
        # Store entity at top level for easier template access
        data['entity'] = entity

        # Add image linking configuration if defined in entity model
        if 'image_linking' in entity:
            data['image_linking'] = entity['image_linking']

    # Special processing for domain entity models
    if is_domain_entity and isinstance(model, dict) and 'domain_entity' in model:
        domain_entity = model['domain_entity']
        # Get iterator_var from cpp section for column processing
        iter_var = domain_entity.get('cpp', {}).get('iterator_var', 'e')
        if 'columns' in domain_entity:
            _mark_last_item(domain_entity['columns'])
            _format_columns_for_doxygen(domain_entity['columns'])
            # Add is_int flag and iterator_var for protocol serialization
            for col in domain_entity['columns']:
                col['is_int'] = col.get('type') == 'integer' or col.get('cpp_type') == 'int'
                col['iter_var'] = iter_var
        if 'natural_keys' in domain_entity:
            _mark_last_item(domain_entity['natural_keys'])
            # Add iterator_var to natural_keys for protocol serialization
            for key in domain_entity['natural_keys']:
                key['iter_var'] = iter_var
        # Format description as comment block lines (for SQL)
        if 'description' in domain_entity:
            domain_entity['description_formatted'] = _format_description_as_comment(domain_entity['description'])
            # Split description into lines for C++ doxygen comments
            domain_entity['description_lines'] = domain_entity['description'].split('\n')
        # Add uppercase versions for C++ include guards
        if 'component' in domain_entity:
            domain_entity['component_upper'] = domain_entity['component'].upper()
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
            domain_entity['entity_pascal_short_plural'] = domain_entity['entity_pascal_short'] + 's'
        if 'entity_plural' in domain_entity:
            domain_entity['entity_plural_upper'] = domain_entity['entity_plural'].upper()
        if 'entity_title' in domain_entity:
            domain_entity['entity_title_lower'] = domain_entity['entity_title'].lower()
        # Prepare table display items for C++ templates
        if 'cpp' in domain_entity:
            _prepare_table_display(domain_entity['cpp'])
        # Copy repository section fields to top level for template access
        if 'repository' in domain_entity:
            for key, value in domain_entity['repository'].items():
                domain_entity[key] = value
        # Add computed properties for primary key type detection
        if 'primary_key' in domain_entity:
            pk = domain_entity['primary_key']
            pk_type = pk.get('type', 'uuid')
            pk['is_uuid'] = pk_type == 'uuid'
            pk['is_text'] = pk_type == 'text'
            # Ensure cpp_type is set with sensible defaults
            if 'cpp_type' not in pk:
                if pk['is_uuid']:
                    pk['cpp_type'] = 'boost::uuids::uuid'
                else:
                    pk['cpp_type'] = 'std::string'
        # Process Qt-specific fields
        if 'qt' in domain_entity:
            qt = domain_entity['qt']
            # Mark last item in columns for template iteration
            if 'columns' in qt:
                _mark_last_item(qt['columns'])
            # Add iterator variable reference for templates
            qt['item_var'] = qt.get('item_var', 'item')
        data['domain_entity'] = domain_entity

    # Special processing for junction models
    if is_junction and isinstance(model, dict) and 'junction' in model:
        junction = model['junction']
        # Get iterator_var from cpp section for column processing
        iter_var = junction.get('cpp', {}).get('iterator_var', 'm')
        if 'columns' in junction:
            _mark_last_item(junction['columns'])
            _format_columns_for_doxygen(junction['columns'])
            # Add is_int flag and iterator_var for protocol serialization
            for col in junction['columns']:
                col['is_int'] = col.get('type') == 'integer' or col.get('cpp_type') == 'int'
                col['iter_var'] = iter_var
        # Add lowercase versions for left/right columns
        if 'left' in junction and 'column_title' in junction['left']:
            junction['left']['column_title_lower'] = junction['left']['column_title'].lower()
        if 'right' in junction and 'column_title' in junction['right']:
            junction['right']['column_title_lower'] = junction['right']['column_title'].lower()
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
            _prepare_table_display(junction['cpp'])
        # Copy repository section fields to top level for template access
        if 'repository' in junction:
            for key, value in junction['repository'].items():
                junction[key] = value
        data['junction'] = junction

    # Special processing for enum models
    if is_enum and isinstance(model, dict) and 'enum' in model:
        enum = model['enum']
        enum_name = enum.get('name', 'unknown')
        # Mark last value for comma handling in template
        # Also add enum_name to each value for case statements
        if 'values' in enum:
            _mark_last_item(enum['values'])
            for val in enum['values']:
                val['enum_name'] = enum_name
        # Split description into lines for C++ doxygen comments
        if 'description' in enum:
            enum['description_lines'] = enum['description'].split('\n')
        # Add uppercase versions for C++ include guards
        if 'component' in enum:
            enum['component_upper'] = enum['component'].upper()
        if 'name' in enum:
            enum['name_upper'] = enum['name'].upper()
        data['enum'] = enum

    # Special processing for entity data models (populate scripts)
    if is_data_model and isinstance(model, dict):
        # New format: model has 'dataset', 'entity' and 'items' keys (per-coding-scheme)
        if 'dataset' in model and 'entity' in model and 'items' in model:
            data['dataset'] = model['dataset']
            data['entity'] = model['entity']
            items = model['items']

            # Escape single quotes in string fields for SQL
            for item in items:
                for key, value in item.items():
                    if isinstance(value, str):
                        item[key] = value.replace("'", "''")

            # Also escape dataset and entity descriptions
            if 'description' in data['dataset'] and isinstance(data['dataset']['description'], str):
                data['dataset']['description'] = data['dataset']['description'].replace("'", "''")
            if 'description' in data['entity'] and isinstance(data['entity']['description'], str):
                data['entity']['description'] = data['entity']['description'].replace("'", "''")

            # Mark last item for comma handling
            _mark_last_item(items)
            data['items'] = items

            # Add image linking configuration if defined in entity model
            if 'image_linking' in data['entity']:
                data['image_linking'] = data['entity']['image_linking']

            # Add shared table configuration for entities that use existing tables
            # (e.g., non-ISO currencies use dq_currencies_artefact_tbl)
            if 'shared_table_config' in model:
                data['shared_table_config'] = model['shared_table_config']
        # Legacy format: model has 'entity' and 'items' keys (per-entity)
        elif 'entity' in model and 'items' in model:
            data['entity'] = model['entity']
            items = model['items']

            # Escape single quotes in string fields for SQL
            for item in items:
                for key, value in item.items():
                    if isinstance(value, str):
                        item[key] = value.replace("'", "''")

            # Also escape entity description
            if 'description' in data['entity'] and isinstance(data['entity']['description'], str):
                data['entity']['description'] = data['entity']['description'].replace("'", "''")

            # Mark last item for comma handling
            _mark_last_item(items)
            data['items'] = items
            data['coding_schemes'] = model.get('coding_schemes', [])
        else:
            # Old format: Find the entity plural name from the data (it's the first key that's not 'coding_schemes')
            entity_plural = None
            items = None
            for key, value in model.items():
                if key != 'coding_schemes' and isinstance(value, list):
                    entity_plural = key
                    items = value
                    break

            if entity_plural and items:
                # Try to load the corresponding entity schema file
                entity_file = Path(model_path).parent / f"{entity_plural}_entity.json"
                if entity_file.exists():
                    entity_model = load_model(entity_file)
                    if 'entity' in entity_model:
                        data['entity'] = entity_model['entity']
                else:
                    # Create minimal entity info from the data
                    entity_singular = entity_plural[:-1] if entity_plural.endswith('s') else entity_plural
                    data['entity'] = {
                        'entity_singular': entity_singular,
                        'entity_plural': entity_plural,
                        'component': 'refdata'
                    }

                # Escape single quotes in string fields for SQL
                for item in items:
                    for key, value in item.items():
                        if isinstance(value, str):
                            item[key] = value.replace("'", "''")

                # Mark last item for comma handling
                _mark_last_item(items)
                data['items'] = items
                data['coding_schemes'] = model.get('coding_schemes', [])

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
        elif generate_qt and is_domain_entity and 'domain_entity' in data:
            # Qt generation for domain entity
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
        elif is_data_model and 'entity' in data:
            # For entity data models, derive filename from dataset or entity definition
            # Find the prefix and suffix for this template
            # Check both standard and non-ISO currency mappings
            populate_mappings = get_populate_template_mappings()
            non_iso_mappings = get_non_iso_currency_template_mappings()
            all_mappings = populate_mappings + non_iso_mappings
            mapping = next(((t, p, s) for t, p, s in all_mappings if t == template_name), None)
            if mapping:
                file_prefix, suffix = mapping[1], mapping[2]
            else:
                file_prefix, suffix = 'refdata_', '_populate.sql'

            # Use dataset code for filename if available (e.g., fpml.entity_type -> entity_type)
            if 'dataset' in data:
                dataset_code = data['dataset'].get('code', 'unknown')
                # Remove fpml. prefix and use as base name
                base_name = dataset_code.replace('fpml.', '').replace('.', '_')
            else:
                # Fall back to entity_plural for legacy data models
                base_name = data['entity'].get('entity_plural', 'unknown')

            output_filename = f"{file_prefix}{base_name}{suffix}"
        else:
            output_ext = '.sql' if template_name.endswith('.mustache') else ''
            output_filename = template_name.replace('.mustache', output_ext)

        # Apply prefix if provided, replacing 'sql_' with prefix + '_'
        # Skip prefix handling for schema/domain_entity/junction/enum models (they use entity-based naming)
        if prefix and not is_schema_model and not is_domain_entity and not is_junction and not is_enum:
            # Special case: master include file should be just {prefix}.sql
            if template_name == 'sql_batch_execute.mustache':
                output_filename = f"{prefix}.sql"
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


def main():
    """Main function to run the code generator."""
    import sys
    import argparse

    parser = argparse.ArgumentParser(description='Generate code from models using templates')
    parser.add_argument('model_path', nargs='?', default=None, help='Path to the model file')
    parser.add_argument('output_dir', nargs='?', default=None, help='Output directory (default: output/)')
    parser.add_argument('--template', '-t', dest='target_template',
                        help='Specific template to use (overrides default template selection)')
    parser.add_argument('--output', '-o', dest='target_output',
                        help='Specific output filename (overrides default naming)')
    parser.add_argument('--profile', '-p', dest='profile',
                        help='Generate all templates in a profile (e.g., qt, sql, protocol)')
    parser.add_argument('--list-profiles', action='store_true',
                        help='List available profiles and exit')

    args = parser.parse_args()

    # Define paths
    base_dir = Path(__file__).parent.parent
    data_dir = base_dir / "library" / "data"
    templates_dir = base_dir / "library" / "templates"

    # Load profiles
    profiles = load_profiles(base_dir)

    # Handle --list-profiles
    if args.list_profiles:
        list_profiles(profiles)
        sys.exit(0)

    # Require model_path if not listing profiles
    if not args.model_path:
        parser.error("model_path is required unless using --list-profiles")

    # Use provided output directory or default to 'output'
    if args.output_dir:
        output_dir = Path(args.output_dir)
    else:
        output_dir = base_dir / "output"

    # Create output directory if it doesn't exist
    output_dir.mkdir(parents=True, exist_ok=True)

    # Handle profile-based generation
    if args.profile:
        if args.target_template:
            parser.error("Cannot use --template and --profile together")

        # Determine model type
        model_filename = os.path.basename(args.model_path)
        model_type = get_model_type(model_filename)

        # Validate profile compatibility
        is_valid, error = validate_profile_for_model(args.profile, profiles, model_type)
        if not is_valid:
            print(f"Error: {error}")
            sys.exit(1)

        # Load model data to resolve output paths
        with open(args.model_path, 'r', encoding='utf-8') as f:
            model_data = json.load(f)

        # Resolve all templates in the profile (with model type filtering)
        templates = resolve_profile_templates(args.profile, profiles, model_type)
        if not templates:
            print(f"Error: Profile '{args.profile}' has no templates")
            sys.exit(1)

        print(f"Generating {len(templates)} templates from profile '{args.profile}'...")

        # Determine project root (parent of projects/ores.codegen)
        project_root = base_dir.parent.parent

        # Generate each template
        for tmpl_info in templates:
            template_name = tmpl_info.get('template') if isinstance(tmpl_info, dict) else tmpl_info
            output_pattern = tmpl_info.get('output') if isinstance(tmpl_info, dict) else None

            # Resolve output path if pattern is provided
            if output_pattern:
                resolved_output = resolve_output_path(output_pattern, model_data, model_type)
                full_output_path = project_root / resolved_output

                # Create output directory if needed
                full_output_path.parent.mkdir(parents=True, exist_ok=True)

                generate_from_model(args.model_path, data_dir, templates_dir, full_output_path.parent,
                                    is_processing_batch=False,
                                    target_template=template_name,
                                    target_output=full_output_path.name)
            else:
                # Fall back to default output directory
                generate_from_model(args.model_path, data_dir, templates_dir, output_dir,
                                    is_processing_batch=False,
                                    target_template=template_name,
                                    target_output=args.target_output)
    else:
        # Single template or default generation
        generate_from_model(args.model_path, data_dir, templates_dir, output_dir,
                            is_processing_batch=False,
                            target_template=args.target_template,
                            target_output=args.target_output)


if __name__ == "__main__":
    main()