/*
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */
#ifndef ORES_SHELL_APP_COMMANDS_REPORTING_PRICING_ENGINES_COMMANDS_HPP
#define ORES_SHELL_APP_COMMANDS_REPORTING_PRICING_ENGINES_COMMANDS_HPP

#include "ores.nats/service/nats_client.hpp"
#include "ores.ore.api/messaging/ore_document_protocol.hpp"
#include <cli/cli.h>
#include <ostream>
#include <string>
#include <vector>

namespace ores::shell::app::commands {

/**
 * @brief A pricing engine configuration as the ORE document ORE reads.
 *
 * The ORE service writes the document, so the command sends the request the
 * configuration's screen sends and writes the answer where the person asked for
 * it: a file, or standard output, so it can be piped into ORE.
 */
class pricing_engines_commands {
public:
    static void register_commands(cli::Menu& root_menu, ores::nats::service::nats_client& session);

    /**
     * @brief Writes one configuration's ORE document.
     *
     * The configuration is named by the id its screen shows; the document is
     * written from the reporting configuration that record fills, so the two
     * are joined here rather than left to the person to find.
     */
    static void process_export(std::ostream& out,
                               ores::nats::service::nats_client& session,
                               const std::vector<std::string>& args);

    /**
     * @brief Writes the document where the person asked for it: to a file, or
     * to standard output, so it can be piped into ORE.
     *
     * @return Whether it was written.
     */
    static bool write_document(std::ostream& out,
                               const std::string& path,
                               const ores::ore::messaging::ore_document& file);
};

}

#endif
