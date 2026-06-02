"""ores.seeder — database-level synthetic test-data generator.

This module is intentionally thin. The MVP just delegates to
``ores.codegen.generator`` via the shell script ``seeder.sh``,
which calls ``generator.py`` directly. The Python module is the
forward-compatible home for future synthesised-flavour code that
reads a codegen entity overview + Faker hints and emits a
Python-Faker script.

See ``projects/ores.seeder/modeling/component_overview.org`` for
the full design.
"""
