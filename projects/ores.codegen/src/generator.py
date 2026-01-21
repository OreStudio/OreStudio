"""
Simple code generator that loads data and applies templates.
"""
import json
import os
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
        "batch_execution.json": ["sql_batch_execute.mustache"],
        "catalogs.json": ["sql_catalog_populate.mustache"],
        "country_currency.json": ["sql_flag_populate.mustache"],  # Generate flags from country_currency
        "country_currency_flags.json": ["sql_flag_populate.mustache"],  # Keep for backward compatibility
        "datasets.json": ["sql_dataset_populate.mustache"],
        "methodologies.json": ["sql_methodology_populate.mustache"],
        "tags.json": ["sql_tag_populate.mustache"]
    }


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


def generate_from_model(model_path, data_dir, templates_dir, output_dir, is_processing_batch=False):
    """
    Generate output files from a model using the appropriate templates.

    Args:
        model_path (str or Path): Path to the model file
        data_dir (Path): Path to the data directory
        templates_dir (Path): Path to the templates directory
        output_dir (Path): Path to the output directory
        is_processing_batch (bool): Flag to indicate if we're already processing a batch to avoid recursion
    """
    # Load the model
    model = load_model(model_path)
    model_filename = Path(model_path).name

    # Special handling for batch_execution.json - generate all dependent models first
    if model_filename == "batch_execution.json" and not is_processing_batch:
        print("Processing batch execution model...")

        # Extract model directory
        model_dir = Path(model_path).parent

        # Map from output filenames to input model filenames
        output_to_model_map = {
            "sql_catalog_populate.sql": "catalogs.json",
            "sql_methodology_populate.sql": "methodologies.json",
            "sql_dataset_populate.sql": "datasets.json",
            "sql_tag_populate.sql": "tags.json",
            "sql_flag_populate.sql": "country_currency.json"
        }

        # Generate all dependent files listed in the batch execution model
        for file_entry in model.get("files", []):
            output_filename = file_entry.get("name")
            if output_filename in output_to_model_map:
                model_filename_for_output = output_to_model_map[output_filename]
                dependent_model_path = model_dir / model_filename_for_output

                if dependent_model_path.exists():
                    print(f"Generating dependent model: {dependent_model_path}")
                    generate_from_model(dependent_model_path, data_dir, templates_dir, output_dir, is_processing_batch=True)
                else:
                    print(f"Warning: Dependent model not found: {dependent_model_path}")

        # After generating all dependencies, now generate the batch execution file itself
        # Continue with the original batch execution generation process
        # But skip the recursive processing by setting is_processing_batch=True
        is_processing_batch = True

    # Get template mappings
    template_map = get_template_mappings()

    # Check if this model has associated templates
    if model_filename not in template_map:
        print(f"No templates found for model: {model_filename}")
        return

    # Load library data
    data = load_data(data_dir)

    # Generate enhanced license with modeline and copyright
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

    # Handle file references in the model data (e.g., steps_file pointing to methodology.txt)
    model_dir = Path(model_path).parent
    _resolve_file_references(data[model_key], model_dir, data)

    # Special processing for country_currency model to generate SVG flags
    if model_key in ['country_currency', 'country_currency_flags']:
        # Process each country currency to generate SVG flag data
        processed_data = []
        for item in data[model_key]:
            # Create a copy of the item and add the generated SVG
            processed_item = item.copy()
            processed_item['generated_svg'] = generate_flag_svg(item.get('country_code', ''))
            processed_data.append(processed_item)
        # Store the processed data under the original key for templates to use
        data[model_key] = processed_data

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
    for template_name in template_map[model_filename]:
        template_path = templates_dir / template_name
        if not template_path.exists():
            print(f"Template not found: {template_path}")
            continue

        # Render the template with the combined data
        rendered_content = render_template(template_path, data)

        # Determine output filename (replace .mustache with appropriate extension)
        output_ext = '.sql' if template_name.endswith('.mustache') else ''
        output_filename = template_name.replace('.mustache', output_ext)
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
    import random

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