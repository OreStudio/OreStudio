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
#include "ores.synthetic.api/domain/fx_spot_generation_config.hpp"
#include "ores.synthetic.api/domain/gmm_component.hpp"
#include "ores.synthetic.api/domain/ir_curve_generation_config.hpp"
#include "ores.synthetic.api/domain/ir_curve_generation_config_process_parameter_value.hpp"
#include "ores.synthetic.api/domain/ir_curve_template_entry.hpp"
#include "ores.synthetic.api/domain/market_data_generation_config.hpp"
#include "ores.synthetic.api/domain/scope.hpp"
#include "ores.synthetic.api/domain/yield_curve_process_parameter_definition.hpp"
#include "ores.synthetic.api/domain/yield_curve_process_type.hpp"
#include "ores.synthetic.api/messaging/ir_curve_operations_protocol.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <array>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <optional>
#include <ostream>
#include <string>
#include <vector>

namespace cli {

class Menu;

}

namespace ores::shell::app::commands {

/**
 * @brief One feed's configured state, as the service reports it.
 *
 * A feed is a sub-config of the market data generation config that owns it,
 * of one asset class: an FX spot config or an IR curve config. The rows a
 * class needs beyond its config are its child rows -- GMM components for FX,
 * curve template entries and process parameter values for IR -- and a feed
 * with none of them is partial: enabled and pointed at a producer that has
 * nothing to step. An asset class supplies those rows and the whole listing
 * shares the rest.
 */
struct feed_state {
    /// Which asset class the feed is, as its config family names it.
    std::string asset_class;
    /// The config's own stable name; the name its bindings and its ticks carry.
    std::string source_name;
    /// The config's own id, the key every start and stop request uses.
    std::string config_id;
    /// Whether the config is active and eligible for generation.
    bool enabled = false;
    /// Whether the service starts it when it comes up, rather than by hand.
    bool auto_start = false;
    /// Whether the service is running it now.
    bool running = false;
    /// The child rows the class needs, e.g. "gmm 3".
    std::string child_rows;
    /// True when a row the class needs is missing.
    bool partial = false;
};

/// What a `list feeds` run gathered, every read the listing needs.
struct feed_listing {
    std::vector<synthetic::domain::market_data_generation_config> configs;
    std::vector<synthetic::domain::fx_spot_generation_config> fx_spot;
    std::vector<synthetic::domain::ir_curve_generation_config> ir_curve;
    std::vector<synthetic::domain::gmm_component> gmm_components;
    std::vector<synthetic::domain::ir_curve_template_entry> template_entries;
    std::vector<synthetic::domain::ir_curve_generation_config_process_parameter_value>
        parameter_values;
    std::vector<synthetic::domain::yield_curve_process_parameter_definition> definitions;
    std::vector<synthetic::domain::yield_curve_process_type> process_types;
    std::vector<std::string> running;
};

/**
 * @brief The rows a feed of one asset class needs beyond its own config.
 *
 * The setup flow, its validation and its reporting are shared; this is the
 * whole of what an asset class supplies, so the listing and the preview
 * commands stay kind agnostic.
 */
class feed_assets {
public:
    virtual ~feed_assets() = default;

    /// Every feed of this class, with its own config's state.
    [[nodiscard]] virtual std::vector<feed_state>
    states(const feed_listing& listing, const std::vector<std::string>& running) const = 0;

    /// True when the feed holds every child row its class needs.
    [[nodiscard]] virtual bool complete(const feed_listing& listing,
                                        const feed_state& feed) const = 0;
};

/**
 * @brief Commands for the synthetic market simulator.
 *
 * Exposes market simulator operations scriptably: authoring a whole feed in
 * one pass, listing every configured feed with its state, starting and
 * stopping individual feeds or whole folder subtrees, validating vintage data
 * availability, and previewing what a feed would produce -- the operations the
 * Qt Market Simulator window performs that no entity model states. Every feed
 * verb works for any asset class: an asset class supplies only which child
 * rows its feeds need, and the flow, its validation and its reporting are one.
 * All requests are authenticated and inherit the viewer's context, so RLS and
 * scope-based filtering apply exactly as they do in the client.
 * The entity reads are generated units of their own, under the
 * folders, market_data_generation_configs and related menus.
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
     * Creates the synthetic submenu: list folders/feeds, setup, the preview
     * group (fx-spot, ir-shape, ir-paths), start/stop folder/feed, and
     * validate-vintage. The organisation generator that used to sit beside it
     * is gone; see the component overview for why.
     */
    static void register_commands(cli::Menu& root_menu, ores::nats::service::nats_client& session);

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
     * @brief List feeds and their state: synthetic list feeds.
     *
     * Prints one line per configured feed, of every asset class, whether or
     * not the service is running it: its class, its source name, whether it is
     * enabled, whether it is set to auto-start, whether it is running now, and
     * the child rows it holds. A feed with a missing child row is marked
     * partial, so an incomplete feed is visible rather than absent.
     */
    static void process_list_feeds(std::ostream& out,
                                   ores::nats::service::nats_client& session,
                                   const std::vector<std::string>& args);

    /**
     * @brief Execute the listing: every configured feed, running or not.
     *
     * @return true on success.
     */
    static bool list_feeds(std::ostream& out, ores::nats::service::nats_client& session);

    /**
     * @brief Render one feed's state as the listing shows it.
     *
     * Literal text rather than a table, so the two facts a reader compares
     * -- running and partial -- sit where the eye lands.
     */
    [[nodiscard]] static std::string format_feed_state(const feed_state& feed);

    /**
     * @brief Read every feed and the state the listing renders: the config
     * families, their child rows, and what the service is running.
     */
    [[nodiscard]] static std::optional<feed_listing>
    read_feed_listing(std::ostream& out, ores::nats::service::nats_client& session);

    /**
     * @brief Read the listing and, on top of it, the process catalogue the
     * previews and the setup flow both need.
     *
     * Kept apart from read_feed_listing because the listing renders no
     * catalogue field, so its own reads stay the ones it uses.
     */
    [[nodiscard]] static std::optional<feed_listing>
    read_feed_context(std::ostream& out, ores::nats::service::nats_client& session);

    /// Every feed's state, each class computing partial from its own rows.
    [[nodiscard]] static std::vector<feed_state> feed_states(const feed_listing& listing);

    /**
     * @brief Preview what an FX spot feed would produce:
     * synthetic preview fx-spot <feed>.
     */
    static void process_preview_fx_spot(std::ostream& out,
                                        ores::nats::service::nats_client& session,
                                        const std::vector<std::string>& args);

    /**
     * @brief Preview the curve shape an IR curve feed would publish:
     * synthetic preview ir-shape <feed>.
     */
    static void process_preview_ir_shape(std::ostream& out,
                                         ores::nats::service::nats_client& session,
                                         const std::vector<std::string>& args);

    /**
     * @brief Preview the short-rate paths an IR curve feed would step:
     * synthetic preview ir-paths <feed>.
     */
    static void process_preview_ir_paths(std::ostream& out,
                                         ores::nats::service::nats_client& session,
                                         const std::vector<std::string>& args);

    /**
     * @brief Author a working feed in one pass: synthetic setup --kind <fx|ir>
     * ...
     *
     * Writes the rows a class needs in the order they depend on each other,
     * and refuses before the first write when a reference it needs is
     * missing, naming the missing code.
     */
    static void process_setup(std::ostream& out,
                              ores::nats::service::nats_client& session,
                              const std::vector<std::string>& args);

    /**
     * @brief One row a feed setup writes: the subject the entity's own
     * generated `add` verb sends, and the request body on it.
     *
     * The plan is built before the first row is sent, so what an asset class
     * authors is readable without counting calls, and a refusal names the row
     * that failed.
     */
    struct setup_row {
        std::string label;
        std::string subject;
        std::string body;
    };

    /**
     * @brief The session operations the setup flow needs, so a test can drive
     * the whole flow without a transport.
     *
     * The flow reads the reference context it validates against and sends each
     * planned row on the session that owns the party. Production uses the
     * nats_client; a test supplies its own.
     */
    struct setup_session {
        virtual ~setup_session() = default;
        [[nodiscard]] virtual bool is_logged_in() const = 0;
        [[nodiscard]] virtual std::string party_id() const = 0;
        [[nodiscard]] virtual std::optional<feed_listing> read_context(std::ostream& out) = 0;
        [[nodiscard]] virtual bool send(std::ostream& out, const setup_row& row) = 0;
    };

    /// Author a feed on @p session, as process_setup does for a live client.
    static void
    process_setup(std::ostream& out, setup_session& session, const std::vector<std::string>& args);

    /// The rows an FX feed needs beyond the shared container and folders.
    [[nodiscard]] static std::vector<setup_row>
    plan_fx(const std::string& sub_config_id,
            const std::string& container_id,
            const std::string& folder_id,
            const std::string& source_name,
            const std::string& base,
            const std::string& quote,
            const std::string& process_type,
            std::uint32_t ticks_per_hour,
            double initial_price,
            const std::vector<std::array<double, 3>>& components);

    /// The rows an IR curve feed needs beyond the shared container and folders.
    [[nodiscard]] static std::vector<setup_row> plan_ir(
        const std::string& sub_config_id,
        const std::string& container_id,
        const std::string& folder_id,
        const std::string& source_name,
        const std::vector<synthetic::domain::yield_curve_process_parameter_definition>& definitions,
        const std::string& currency,
        const std::string& index_family,
        const std::string& tenor,
        const std::string& role,
        const std::string& process_type,
        std::uint32_t ticks_per_hour,
        const std::vector<ores::synthetic::messaging::parameter_spec>& parameters,
        const std::vector<std::string>& curve_keys);

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
