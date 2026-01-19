import os
import random
import sys

# Configuration
OUTPUT_DIR = "fake_flags"

# A diverse palette of colors to ensure uniqueness
COLORS = [
    "#FF5733", "#33FF57", "#3357FF", "#F1C40F", "#8E44AD",
    "#1ABC9C", "#E67E22", "#2ECC71", "#3498DB", "#E74C3C",
    "#17202A", "#D5DBDB", "#FBEEE6", "#1B4F72", "#145A32",
    "#512E5F", "#78281F", "#0E6251", "#1A5276", "#1D8348",
    "#FF00FF", "#00FFFF", "#FFFF00", "#000000", "#FFFFFF"
]

def generate_svg(filename, content):
    """Wraps SVG content in a standard 4:3 aspect ratio container."""
    header = '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 600 450">'
    footer = '</svg>'
    if not os.path.exists(OUTPUT_DIR):
        os.makedirs(OUTPUT_DIR)
    with open(os.path.join(OUTPUT_DIR, filename), "w") as f:
        f.write(header + content + footer)

def create_flags(num_to_generate):
    for i in range(num_to_generate):
        # Select 3 unique colors for this specific flag
        choice_colors = random.sample(COLORS, 3)
        c1, c2, c3 = choice_colors

        # Determine pattern (cycling through 6 distinct types)
        pattern_type = i % 6
        filename = f"fake_flag_{i+1:03d}.svg"
        svg_content = ""

        if pattern_type == 0:
            # Pattern: 5-Vertical Stripes
            w = 600 / 5
            svg_content = f'''
                <rect width="{w}" height="450" fill="{c1}"/>
                <rect x="{w}" width="{w}" height="450" fill="{c2}"/>
                <rect x="{w*2}" width="{w}" height="450" fill="{c3}"/>
                <rect x="{w*3}" width="{w}" height="450" fill="{c2}"/>
                <rect x="{w*4}" width="{w}" height="450" fill="{c1}"/>'''

        elif pattern_type == 1:
            # Pattern: Diagonal "X" (Saltire) with a center circle
            svg_content = f'''
                <rect width="600" height="450" fill="{c1}"/>
                <path d="M0,0 L600,450 M600,0 L0,450" stroke="{c2}" stroke-width="60"/>
                <circle cx="300" cy="225" r="60" fill="{c3}"/>'''

        elif pattern_type == 2:
            # Pattern: Triple Nested Rectangles
            svg_content = f'''
                <rect width="600" height="450" fill="{c1}"/>
                <rect x="100" y="75" width="400" height="300" fill="{c2}"/>
                <rect x="200" y="150" width="200" height="150" fill="{c3}"/>'''

        elif pattern_type == 3:
            # Pattern: Large Central Diamond
            svg_content = f'''
                <rect width="600" height="450" fill="{c1}"/>
                <polygon points="300,50 550,225 300,400 50,225" fill="{c2}"/>
                <rect x="275" y="200" width="50" height="50" fill="{c3}"/>'''

        elif pattern_type == 4:
            # Pattern: Top and Bottom Chevrons
            svg_content = f'''
                <rect width="600" height="450" fill="{c1}"/>
                <polyline points="0,0 300,100 600,0" fill="none" stroke="{c2}" stroke-width="80"/>
                <polyline points="0,450 300,350 600,450" fill="none" stroke="{c3}" stroke-width="80"/>'''

        elif pattern_type == 5:
            # Pattern: Vertical tricolor with a horizontal bar across the middle
            svg_content = f'''
                <rect width="200" height="450" fill="{c1}"/>
                <rect x="200" width="200" height="450" fill="{c2}"/>
                <rect x="400" width="200" height="450" fill="{c3}"/>
                <rect y="175" width="600" height="100" fill="{c1}" fill-opacity="0.5" stroke="{c1}" stroke-width="5"/>'''

        generate_svg(filename, svg_content)

if __name__ == "__main__":
    # Get N from command line arguments, default to 50
    try:
        n = int(sys.argv[1]) if len(sys.argv) > 1 else 50
    except ValueError:
        print("Please provide a valid integer for the number of flags.")
        sys.exit(1)

    create_flags(n)
    print(f"Generated {n} unique fake flags in './{OUTPUT_DIR}'")
