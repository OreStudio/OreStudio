#ifndef ORES_SHELL_APP_COMMANDS_ASSETS_ASSETS_COMMANDS_HPP
#define ORES_SHELL_APP_COMMANDS_ASSETS_ASSETS_COMMANDS_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.nats/service/nats_client.hpp"

namespace cli {

class Menu;

}

namespace ores::shell::app::commands {

/**
 * @brief Registers every assets command unit.
 *
 * The units are generated, one per model that opts in through its profile,
 * and each owns a submenu. This aggregator is the one hand-written file in
 * the part: it is the list the host calls, and no facet emits the list.
 */
class assets_commands {
private:
    inline static std::string_view logger_name = "ores.shell.app.commands.assets.assets_commands";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    static void register_commands(cli::Menu& root_menu, ores::nats::service::nats_client& session);
};

}

#endif
