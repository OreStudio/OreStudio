#include "ores.shell/app/commands/assets/assets_commands.hpp"
#include "ores.shell/app/commands/assets/image_commands.hpp"
#include "ores.shell/app/commands/assets/image_tag_commands.hpp"
#include "ores.shell/app/commands/assets/tag_commands.hpp"

namespace ores::shell::app::commands {

void assets_commands::register_commands(cli::Menu& root_menu,
                                        ores::nats::service::nats_client& session) {
    image_commands::register_commands(root_menu, session);
    image_tag_commands::register_commands(root_menu, session);
    tag_commands::register_commands(root_menu, session);
}

}
