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
    {'symbol': '$', 'fraction_symbol': '¢', 'fractions_per_unit': 100, 'rounding_type': 'standard', 'rounding_precision': 2, 'format': '$#,##0.00', 'currency_type': 'fiat.emerging'},
    {'symbol': '€', 'fraction_symbol': 'c', 'fractions_per_unit': 100, 'rounding_type': 'standard', 'rounding_precision': 2, 'format': '€#,##0.00', 'currency_type': 'fiat.major'},
    {'symbol': '£', 'fraction_symbol': 'p', 'fractions_per_unit': 100, 'rounding_type': 'standard', 'rounding_precision': 2, 'format': '£#,##0.00', 'currency_type': 'fiat.major'},
    {'symbol': '¥', 'fraction_symbol': '', 'fractions_per_unit': 0, 'rounding_type': 'standard', 'rounding_precision': 0, 'format': '¥#,##0', 'currency_type': 'fiat.emerging'},
    {'symbol': 'kr', 'fraction_symbol': 'ø', 'fractions_per_unit': 100, 'rounding_type': 'standard', 'rounding_precision': 2, 'format': 'kr #,##0.00', 'currency_type': 'fiat.major'},
    {'symbol': 'zł', 'fraction_symbol': 'gr', 'fractions_per_unit': 100, 'rounding_type': 'standard', 'rounding_precision': 2, 'format': '#,##0.00 zł', 'currency_type': 'fiat.emerging'},
    {'symbol': '₹', 'fraction_symbol': 'p', 'fractions_per_unit': 100, 'rounding_type': 'standard', 'rounding_precision': 2, 'format': '₹#,##0.00', 'currency_type': 'fiat.emerging'},
    {'symbol': 'د.إ', 'fraction_symbol': 'ف', 'fractions_per_unit': 100, 'rounding_type': 'standard', 'rounding_precision': 2, 'format': 'د.إ#,##0.00', 'currency_type': 'fiat.emerging'},
    {'symbol': 'R', 'fraction_symbol': 'c', 'fractions_per_unit': 100, 'rounding_type': 'standard', 'rounding_precision': 2, 'format': 'R#,##0.00', 'currency_type': 'fiat.emerging'},
    {'symbol': 'ƒ', 'fraction_symbol': '¢', 'fractions_per_unit': 100, 'rounding_type': 'standard', 'rounding_precision': 2, 'format': 'ƒ#,##0.00', 'currency_type': 'fiat.emerging'},
    {'symbol': 'د.ك', 'fraction_symbol': 'ف', 'fractions_per_unit': 1000, 'rounding_type': 'standard', 'rounding_precision': 3, 'format': 'د.ك#,##0.000', 'currency_type': 'fiat.emerging'},
    {'symbol': 'S/', 'fraction_symbol': '¢', 'fractions_per_unit': 100, 'rounding_type': 'standard', 'rounding_precision': 2, 'format': 'S/#,##0.00', 'currency_type': 'fiat.emerging'},
    {'symbol': '฿', 'fraction_symbol': 'ส', 'fractions_per_unit': 100, 'rounding_type': 'standard', 'rounding_precision': 2, 'format': '฿#,##0.00', 'currency_type': 'fiat.emerging'},
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

    # Determine which templates to process
    if target_template:
        templates_to_process = [target_template]
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

    # Generate enhanced license with modeline and copyright header
    if 'licence-GPL-v3' in data and 'modelines' in data:
        # Get the SQL modeline
        sql_modeline = data['modelines'].get('sql', '')
        # Generate the enhanced license
        enhanced_license = generate_license_with_header(
            data['licence-GPL-v3'],
            sql_modeline,
            'sql'
        )
        # Add to data for use in templates
        data['enhanced_license'] = enhanced_license
        # Also add the modeline separately if needed
        data['sql_modeline'] = sql_modeline

    # Add the model data to the template data
    # Use the model filename (without extension) as the key
    model_key = Path(model_path).stem
    data[model_key] = model
    
    # If the model is a list, mark the last item for Mustache templates
    _mark_last_item(data[model_key])

    # Handle file references in the model data (e.g., steps_file pointing to methodology.txt)
    model_dir = Path(model_path).parent
    _resolve_file_references(data[model_key], model_dir, data)

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
        # Skip prefix handling for schema models (they use entity-based naming)
        if prefix and not is_schema_model:
            # Special case: master include file should be just {prefix}.sql
            if template_name == 'sql_batch_execute.mustache':
                output_filename = f"{prefix}.sql"
            elif output_filename.startswith('sql_'):
                output_filename = f"{prefix}_{output_filename[4:]}"
            elif not output_filename.startswith(f"{prefix}_"):
                output_filename = f"{prefix}_{output_filename}"

        output_path = output_dir / output_filename

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

    # Check if a model path was provided as command-line argument
    if len(sys.argv) < 2:
        print("Usage: python generator.py <model_path> [output_dir]")
        print("Example: python generator.py models/slovaris/catalogs.json")
        print("Example with custom output: python generator.py models/slovaris/catalogs.json custom_output/")
        return

    model_path = sys.argv[1]

    # Define paths
    base_dir = Path(__file__).parent.parent
    data_dir = base_dir / "library" / "data"
    templates_dir = base_dir / "library" / "templates"

    # Use provided output directory or default to 'output'
    if len(sys.argv) > 2:
        output_dir = Path(sys.argv[2])
    else:
        output_dir = base_dir / "output"

    # Create output directory if it doesn't exist
    output_dir.mkdir(parents=True, exist_ok=True)

    # Generate from the specified model
    generate_from_model(model_path, data_dir, templates_dir, output_dir, is_processing_batch=False)


if __name__ == "__main__":
    main()