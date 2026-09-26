# -*- mode: cmake; cmake-tab-width: 4; indent-tabs-mode: nil -*-
#
# Hand-written: this module has no modeling/component_overview.org, so
# regenerate_cmake_component_files.py has nothing to read for it.
set(files
    "app/commands/workflow/workflow_instance_commands.cpp"
    "app/commands/workflow/workflow_step_commands.cpp"
)

set(HEADERS
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/workflow/workflow_instance_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/workflow/workflow_step_commands.hpp"
)
