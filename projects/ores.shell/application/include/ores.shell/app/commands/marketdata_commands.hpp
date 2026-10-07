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
#ifndef ORES_SHELL_APP_COMMANDS_MARKETDATA_COMMANDS_HPP
#define ORES_SHELL_APP_COMMANDS_MARKETDATA_COMMANDS_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/datum/market_datum.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <cstddef>
#include <expected>
#include <functional>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace cli {

class Menu;

}

namespace ores::shell::app::commands {

/**
 * @brief The ticks a running stream consumes, one batch per call.
 *
 * The command holds a subscription open and reads what arrives, so a test
 * cannot hand it a request and read one answer. This is that seam: the
 * production source reads the NATS subscription's buffer, and a test hands
 * over a fixed batch and then everything is done. The loop owns the CSV
 * file, the screen lines and the count, so a source that yields two ticks
 * exercises all three.
 */
struct tick_source {
    virtual ~tick_source() = default;

    /// The ticks that have arrived since the last call, and whether more can.
    virtual std::pair<std::vector<marketdata::messaging::market_tick>, bool> next() = 0;
};

/**
 * @brief What a finished stream wrote, in the order it wrote it.
 *
 * Rows includes the header, so an empty run still shows the one line that
 * makes its CSV valid.
 */
struct stream_output {
    std::vector<std::string> screen_lines;
    std::vector<std::string> rows;
    std::size_t count = 0;
    /// The CSV path the run could not open, or empty when the file was fine.
    std::string csv_error;
};

/**
 * @brief What one stream watches: the subjects to subscribe to, and the series
 * every arriving tick must belong to.
 *
 * A point URI watches its own key alone and matches every tick on it, so it
 * carries no filter. A series URI has no key of its own, so it watches the
 * party's whole tick subtree and carries the series as a filter.
 */
struct stream_watch {
    std::vector<std::string> subjects;
    std::optional<marketdata::datum::market_datum> series;
};

/**
 * @brief Commands for market data import.
 *
 * Gives ores.shell a non-interactive entry point into
 * import_market_data_request, the same request the Qt
 * ImportTradeDialog sends, so imports can run scripted or at
 * provisioning time without the GUI.
 */
class marketdata_commands {
private:
    inline static std::string_view logger_name = "ores.shell.app.commands.marketdata_commands";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Register market data commands.
     *
     * Creates the marketdata submenu with the import operation.
     */
    static void register_commands(cli::Menu& root_menu, ores::nats::service::nats_client& session);

    /**
     * @brief Import market data: marketdata import [--file <path>]
     * [--fixings <path>] [--source <tag>].
     *
     * Reads the ORE market.txt/fixings.txt file(s) named by --file
     * and --fixings (at least one is required) and sends their
     * content, along with the optional --source tag, as a single
     * import_market_data_request over NATS.
     */
    static void process_import(std::ostream& out,
                               ores::nats::service::nats_client& session,
                               const std::vector<std::string>& args);

    /**
     * @brief Writes the tenant's market data back out as ORE text files.
     *
     * Sends an export_market_data_request over NATS and writes the two bodies
     * it returns to the paths named by --market-data and --fixings, both of
     * which default to the names ORE itself reads.
     */
    static void process_export(std::ostream& out,
                               ores::nats::service::nats_client& session,
                               const std::vector<std::string>& args);

    /**
     * @brief Watch one oresmd on the republished tick stream.
     *
     * Resolves the URI with the oresmd codec and subscribes on the session's
     * own connection: to the datum's ORE key for a point, or to the party's
     * whole tick subtree for a series, whose ticks the series filter keeps.
     * Prints or writes each tick until Ctrl-C.
     */
    static void process_stream(std::ostream& out,
                               ores::nats::service::nats_client& session,
                               const std::vector<std::string>& args);

    /**
     * @brief What a stream for @p uri watches.
     *
     * A point URI resolves to one subject: its datum's canonical ORE key, and
     * no filter. A series URI resolves to the party's whole tick subtree and
     * the series to match, because no NATS wildcard covers exactly the points
     * of one series. The error is the codec's own message.
     */
    [[nodiscard]] static std::expected<stream_watch, std::string>
    stream_subjects(std::string_view tenant_id, std::string_view party_id, std::string_view uri);

    /// True when @p tick's URI names a point of @p series.
    [[nodiscard]] static bool tick_in_series(const marketdata::messaging::market_tick& tick,
                                             const marketdata::datum::market_datum& series);

    /// One tick as the screen renders it: time, point URI, value and source.
    [[nodiscard]] static std::string tick_line(const marketdata::messaging::market_tick& tick);

    /// One tick as the CSV renders it, with its fields escaped.
    [[nodiscard]] static std::string tick_row(const marketdata::messaging::market_tick& tick);

    /**
     * @brief Consume @p source until it says there is no more, rendering to
     * both sinks as it goes.
     *
     * The loop is what a test drives: it appends each screen line and each
     * CSV row in arrival order, writes the file when @p csv_path is set, and
     * returns the count. A source that yields nothing still produces the
     * header row, which is what makes an empty run's file valid. Only ticks
     * @p accept admits are rendered; an empty @p accept admits every tick.
     */
    [[nodiscard]] static stream_output
    run_stream(tick_source& source,
               const std::function<bool()>& cancelled,
               const std::string& csv_path,
               std::ostream& out,
               const std::function<bool(const marketdata::messaging::market_tick&)>& accept = {});
};

}

#endif
