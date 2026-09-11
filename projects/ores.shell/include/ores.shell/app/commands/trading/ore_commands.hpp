/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
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
 *
 */
#ifndef ORES_SHELL_APP_COMMANDS_TRADING_ORE_COMMANDS_HPP
#define ORES_SHELL_APP_COMMANDS_TRADING_ORE_COMMANDS_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <chrono>
#include <ostream>
#include <string>

namespace cli {

class Menu;

}

namespace ores::shell::app::commands {

/**
 * @brief Drives the ORE XML to database to XML path from the shell.
 *
 * The three steps of that path already exist as services; this unit adds
 * the verbs that reach them. Upload packs a directory of ORE documents
 * into the ore-imports bucket over HTTP, import starts the
 * ore_import_workflow and waits for its single step, and export asks the
 * trading service for a portfolio's trades, reconstructs the ORE XML from
 * them and writes it to disk.
 *
 * The verbs share one request id, because the import handler reads the
 * tarball from the key that id names. A script supplies it in
 * $ORE_REQUEST_ID, as the other .ores scripts supply their ids.
 */
class ore_commands {
private:
    inline static std::string_view logger_name = "ores.shell.app.commands.trading.ore_commands";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Register the ORE import and export commands.
     *
     * Creates the ore submenu with upload, import and export.
     */
    static void register_commands(cli::Menu& root_menu, ores::nats::service::nats_client& session);

    /**
     * @brief Pack a directory and upload it to the ore-imports bucket.
     *
     * Generates a request id when @p request_id is empty and prints the
     * effective one, so an interactive caller can feed it to import. The
     * object key is "{request_id}.tar.gz", as the import handler expects.
     * Marks command failure on an unreadable directory or a failed upload.
     */
    static void process_upload(std::ostream& out,
                               const std::string& src_dir,
                               const std::string& request_id);

    /**
     * @brief Start the ORE import workflow and wait for its outcome.
     *
     * The handler dispatches the workflow and replies at once with the
     * instance id, so the outcome is read from the workflow steps, not
     * from the reply. The reply's item errors are printed when the
     * import ran synchronously. Marks command failure when the workflow
     * fails or the wait times out.
     *
     * @param party_id Owning party; the logged-in account's default party
     *                 when empty.
     * @param choices_file Path to a JSON-serialised import_choices; the
     *                     server's defaults when empty, which is every
     *                     field except the party.
     * @param parent_portfolio_name Name of the portfolio that wraps the
     *                 imported books. Overrides the choices file. An
     *                 upload with no portfolio directory above its books
     *                 needs a name here, because a book cannot stand
     *                 without a portfolio.
     */
    static void process_import(std::ostream& out,
                               ores::nats::service::nats_client& session,
                               const std::string& request_id,
                               const std::string& party_id,
                               const std::string& choices_file,
                               const std::string& parent_portfolio_name,
                               std::chrono::seconds timeout);

    /**
     * @brief Request a portfolio export and write the XML to a file.
     *
     * The reply carries trades and instruments, not XML, so the XML is
     * reconstructed here through exporter::export_portfolio. Marks
     * command failure on a failed request or an unwritable file.
     *
     * @param node_id Book, portfolio or business unit UUID; every trade
     *                visible to the tenant when empty.
     */
    static void process_export(std::ostream& out,
                               ores::nats::service::nats_client& session,
                               const std::string& output_file,
                               const std::string& node_id,
                               std::uint32_t limit);
};

}

#endif
