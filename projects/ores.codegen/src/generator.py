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
    
    return pystache.render(template_content, data)


def get_template_mappings():
    """
    Define the mapping between model filenames and their corresponding templates.

    Returns:
        dict: Mapping of model filenames to lists of template names
    """
    return {
        "catalogs.json": ["sql_catalog_populate.mustache"]
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


def generate_from_model(model_path, data_dir, templates_dir, output_dir):
    """
    Generate output files from a model using the appropriate templates.

    Args:
        model_path (str or Path): Path to the model file
        data_dir (Path): Path to the data directory
        templates_dir (Path): Path to the templates directory
        output_dir (Path): Path to the output directory
    """
    # Load the model
    model = load_model(model_path)
    model_filename = Path(model_path).name

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

        print(f"Generated {output_path}")
        print("Content preview:")
        print(rendered_content[:500] + "..." if len(rendered_content) > 500 else rendered_content)


def main():
    """Main function to run the code generator."""
    import sys

    # Check if a model path was provided as command-line argument
    if len(sys.argv) < 2:
        print("Usage: python generator.py <model_path>")
        print("Example: python generator.py models/slovaris/catalogs.json")
        return

    model_path = sys.argv[1]

    # Define paths
    base_dir = Path(__file__).parent.parent
    data_dir = base_dir / "library" / "data"
    templates_dir = base_dir / "library" / "templates"
    output_dir = base_dir / "output"

    # Create output directory if it doesn't exist
    output_dir.mkdir(exist_ok=True)

    # Generate from the specified model
    generate_from_model(model_path, data_dir, templates_dir, output_dir)


if __name__ == "__main__":
    main()