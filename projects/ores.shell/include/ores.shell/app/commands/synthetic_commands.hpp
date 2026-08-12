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
#ifndef ORES_SHELL_APP_COMMANDS_SYNTHETIC_COMMANDS_HPP
#define ORES_SHELL_APP_COMMANDS_SYNTHETIC_COMMANDS_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.shell/app/command_args.hpp"
#include "ores.synthetic.api/domain/binding_mode.hpp"
#include "ores.synthetic.api/domain/scope.hpp"
#include "ores.synthetic.api/messaging/generate_organisation_protocol.hpp"
#include <optional>
#include <string>
#include <vector>

namespace cli {

class Menu;

}

namespace ores::shell::app::commands {

/**
 * @brief Commands for synthetic data generation and the market
 * simulator.
 *
 * Wraps the synthetic organisation generator the tenant provisioning
 * wizard uses for its synthetic data source. Every generation knob is
 * an optional flag carrying the wizard's default; the response prints
 * the actual seed used so a random run can be reproduced.
 *
 * Also exposes market simulator operations scriptably: listing the
 * folder tree, market_data_generation_configs and running feeds,
 * starting/stopping individual feeds or whole folder subtrees, and
 * validating vintage data availability -- the same operations the Qt
 * Market Simulator window performs, without the GUI. Every feed verb
 * works for any asset class: the shell never enumerates feed kinds.
 * All requests are authenticated and inherit the viewer's context, so
 * RLS and scope-based filtering apply exactly as they do in the
 * client.
 */
class synthetic_commands {
private:
    inline static std::string_view logger_name = "ores.shell.app.commands.synthetic_commands";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Register synthetic-data commands.
     *
     * Creates the synthetic submenu with the generate operation and the
     * market simulator command group (list folders/configs/feeds,
     * start/stop folder/feed, validate-vintage).
     */
    static void register_commands(cli::Menu& root_menu, ores::nats::service::nats_client& session);

    /**
     * @brief Generate a synthetic organisation:
     * synthetic generate [--country GB|US] [--party-count N]
     * [--party-max-depth N] [--counterparty-count N]
     * [--counterparty-max-depth N] [--portfolio-leaf-count N]
     * [--portfolio-max-depth N] [--books-per-portfolio N]
     * [--business-unit-count N] [--business-unit-max-depth N]
     * [--contacts-per-party N] [--contacts-per-counterparty N]
     * [--no-addresses] [--no-identifiers] [--seed N].
     */
    static void process_generate(std::ostream& out,
                                 ores::nats::service::nats_client& session,
                                 const std::vector<std::string>& args);

    /**
     * @brief The generation knob flag specs, for commands that embed
     * synthetic generation (provision tenant).
     */
    static std::vector<flag_spec> generate_flag_specs();

    /**
     * @brief Build a generation request from parsed flags, reporting
     * the offending flag on validation failure.
     */
    static std::optional<ores::synthetic::messaging::generate_organisation_request>
    build_generate_request(std::ostream& out, const parsed_args& parsed);

    /**
     * @brief Execute a generation request, printing the entity counts
     * and the actual seed used. Marks command failure on error.
     *
     * @return true on success.
     */
    static bool generate(std::ostream& out,
                         ores::nats::service::nats_client& session,
                         const ores::synthetic::messaging::generate_organisation_request& req);

    /**
     * @brief List folders: synthetic list folders
     * [--config-id=<collection-id>] [--name=<folder-token>].
     *
     * Prints the folder hierarchy (root > collection > asset class >
     * instrument type) visible to the logged-in party. --config-id
     * narrows the listing to the collection folder representing that
     * market_data_generation_config and its descendants; --name narrows
     * it to the subtree(s) rooted at the folder(s) matching the token
     * (exact name or standard codename path, e.g. 2026realistic/fx --
     * see resolve_folder_id). Multi-word names are quoted, as
     * everywhere in the REPL: "synthetic list folders --name
     * \"2026 Realistic\"".
     */
    static void process_list_folders(std::ostream& out,
                                     ores::nats::service::nats_client& session,
                                     const std::vector<std::string>& args);

    /**
     * @brief Execute a folder-list request, printing the hierarchy
     * rooted at the top-level folders (or the subtree selected by
     * @p collection_id / @p folder_name, when non-empty).
     *
     * @return true on success.
     */
    static bool list_folders(std::ostream& out,
                             ores::nats::service::nats_client& session,
                             const std::string& collection_id,
                             const std::string& folder_name);

    /**
     * @brief List generation configs: synthetic list configs
     * [--scope=system|tenant|party].
     *
     * Prints name, id, scope, party_id, binding_mode and enabled status
     * for every market_data_generation_config visible to the logged-in
     * party/tenant; the optional --scope flag further filters by scope
     * level.
     */
    static void process_list_configs(std::ostream& out,
                                     ores::nats::service::nats_client& session,
                                     const std::vector<std::string>& args);

    /**
     * @brief Execute a generation-config list request, printing a row
     * per config, filtered to @p scope_filter when non-null.
     *
     * @return true on success.
     */
    static bool list_configs(std::ostream& out,
                             ores::nats::service::nats_client& session,
                             std::optional<ores::synthetic::domain::scope> scope_filter);

    /**
     * @brief List running feeds: synthetic list feeds.
     *
     * Prints the source_name of every feed currently running in the
     * synthetic service, of every asset class, plus the running count.
     */
    static void process_list_feeds(std::ostream& out,
                                   ores::nats::service::nats_client& session,
                                   const std::vector<std::string>& args);

    /**
     * @brief Execute a running-feeds list request, printing one line
     * per running feed's source_name.
     *
     * @return true on success.
     */
    static bool list_feeds(std::ostream& out,
                           ores::nats::service::nats_client& session);

    /**
     * @brief Start feeds: synthetic start folder <folder-token>
     * | feed <feed-token>.
     *
     * Folder-scoped starts resolve the whole subtree server-side
     * (start_feeds_under_folder_request); feed-scoped starts send the
     * config_id and the server resolves the config, its children, and
     * the refdata context (start_feed_request), exactly as the Qt
     * Market Simulator does. Folder tokens are UUIDs, exact names or
     * codename paths; feed tokens are UUIDs, ore keys or source_names
     * of any asset class (see resolve_folder_id/resolve_feed).
     */
    static void process_start(std::ostream& out,
                              ores::nats::service::nats_client& session,
                              const std::vector<std::string>& args);

    /**
     * @brief Stop feeds: synthetic stop folder <folder-token>
     * | feed <feed-token>.
     *
     * Folder-scoped stops resolve the whole subtree server-side
     * (stop_feeds_under_folder_request); feed-scoped stops send the
     * config_id and the server resolves it to the config's
     * source_name.
     */
    static void process_stop(std::ostream& out,
                             ores::nats::service::nats_client& session,
                             const std::vector<std::string>& args);

    /**
     * @brief Validate vintage data: synthetic validate-vintage
     * <feed-token>.
     *
     * Reports whether the vintage market data the feed's initial spot
     * depends on is available (pass/fail + reason).
     */
    static void process_validate_vintage(std::ostream& out,
                                         ores::nats::service::nats_client& session,
                                         const std::vector<std::string>& args);

    /**
     * @brief Start every feed under a folder subtree, resolved
     * server-side via start_feeds_under_folder_request. @p token is a
     * folder id, name or codename path.
     *
     * @return true on success.
     */
    static bool start_folder(std::ostream& out,
                             ores::nats::service::nats_client& session,
                             const std::string& token);

    /**
     * @brief Start one feed by config_id; the server resolves the
     * config, its children, and the refdata context (mirrors the Qt
     * Market Simulator's per-pair start). @p token is a feed id, ore
     * key or source_name of any asset class.
     *
     * @return true on success.
     */
    static bool start_feed(std::ostream& out,
                           ores::nats::service::nats_client& session,
                           const std::string& token);

    /**
     * @brief Stop every running feed under a folder subtree, resolved
     * server-side via stop_feeds_under_folder_request. @p token is a
     * folder id, name or codename path.
     *
     * @return true on success.
     */
    static bool stop_folder(std::ostream& out,
                            ores::nats::service::nats_client& session,
                            const std::string& token);

    /**
     * @brief Stop one feed by config_id; the server resolves it to the
     * config's source_name. @p token is a feed id, ore key or
     * source_name of any asset class.
     *
     * @return true on success.
     */
    static bool stop_feed(std::ostream& out,
                          ores::nats::service::nats_client& session,
                          const std::string& token);

    /**
     * @brief Report vintage-availability status for one feed, computed
     * live server-side via get_vintage_validity_request. @p token is a
     * feed id, ore key or source_name of any asset class.
     *
     * @return true on success.
     */
    static bool validate_vintage(std::ostream& out,
                                 ores::nats::service::nats_client& session,
                                 const std::string& token);
};

}

#endif
