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
#include "ores.shell/app/commands/marketdata_commands.hpp"
#include "ores.marketdata.api/domain/tick_subjects.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.marketdata.core/datum/oresmd_uri_codec.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/buffered_subscription.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.shell/app/command_args.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include "ores.shell/app/commands/marketdata/feed_binding_commands.hpp"
#include "ores.shell/app/commands/marketdata/market_fixing_commands.hpp"
#include "ores.shell/app/commands/marketdata/market_observation_commands.hpp"
#include "ores.shell/app/commands/marketdata/market_series_asset_class_commands.hpp"
#include "ores.shell/app/commands/marketdata/market_series_commands.hpp"
#include "ores.shell/app/commands/marketdata/observation_lineage_commands.hpp"
#include "ores.shell/app/commands/marketdata/oresmd_index_commands.hpp"
#include "ores.shell/app/commands/marketdata/series_classification_rule_commands.hpp"
#include "ores.shell/app/request_helpers.hpp"
#include "ores.shell/app/shell_root_menu.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <chrono>
#include <cli/cli.h>
#include <csignal>
#include <format>
#include <fstream>
#include <functional>
#include <optional>
#include <ostream>
#include <sstream>
#include <string>
#include <thread>
#include <utility>

namespace ores::shell::app::commands {

using namespace logging;
using ores::nats::service::nats_client;

namespace {

// Import and export both carry a whole market.txt/fixings.txt file's worth of
// content in one request; mirror the bundles publish command's generous
// request timeout.
constexpr std::chrono::minutes bulk_transfer_timeout(5);

// A stream has no deadline of its own: it ends when the reader interrupts it.
// The interval only decides how often the subscription's buffer is drained.
constexpr auto stream_poll_interval = std::chrono::milliseconds(200);

// Enough ticks for a reader to catch up after a pause without holding a whole
// session's traffic.
constexpr std::size_t stream_buffer_capacity = 8192;

constexpr std::string_view stream_csv_header = "observation_time,oresmd_uri,value,source";

// Written from the signal handler and read by the loop, so it is the one
// object in the command that a signal may touch.
volatile std::sig_atomic_t interrupted = 0;

extern "C" void on_interrupt(int) {
    interrupted = 1;
}
/// A field as CSV writes it: quoted when it holds a separator, a quote or a
/// line break, with its own quotes doubled.
std::string csv_field(const std::string& text) {
    if (text.find_first_of(",\"\r\n") == std::string::npos)
        return text;
    std::string out;
    out.reserve(text.size() + 2);
    out.push_back('"');
    for (const char c : text)
        out.append(c == '"' ? "\"\"" : std::string(1, c));
    out.push_back('"');
    return out;
}

/**
 * @brief The ticks a live subscription carries, decoded one tick per message.
 *
 * Reads only what arrived since the last call, so a tick is printed once. A
 * watch may hold one subscription per subject it covers, and their messages
 * arrive in whatever order the service published them.
 */
class subscription_tick_source final : public tick_source {
public:
    subscription_tick_source(ores::nats::service::client& transport,
                             const std::vector<std::string>& subjects) {
        for (const auto& subject : subjects) {
            subscriptions_.push_back(
                {transport.subscribe_buffered(subject, stream_buffer_capacity), 0});
        }
    }

    std::pair<std::vector<marketdata::messaging::market_tick>, bool> next() override {
        std::vector<marketdata::messaging::market_tick> ticks;
        for (auto& [subscription, read] : subscriptions_) {
            const auto snapshot = subscription.snapshot();
            for (; read < snapshot.size(); ++read) {
                auto decoded =
                    ores::nats::default_wire_codec().decode<marketdata::messaging::market_tick>(
                        snapshot[read].data);
                if (decoded)
                    ticks.push_back(std::move(*decoded));
            }
        }
        return {std::move(ticks), true};
    }

private:
    std::vector<std::pair<ores::nats::service::buffered_subscription, std::size_t>> subscriptions_;
};

bool write_file(const std::string& path, const std::string& content) {
    std::ofstream file(path);
    if (!file.is_open())
        return false;
    file << content;
    return file.good();
}

std::optional<std::string> read_file(const std::string& path) {
    std::ifstream file(path);
    if (!file.is_open())
        return std::nullopt;

    std::ostringstream contents;
    contents << file.rdbuf();
    return contents.str();
}

}

void marketdata_commands::register_commands(cli::Menu& root_menu, nats_client& session) {
    // Generated per-entity command units. Each projects one market-data
    // entity onto the REPL as a top-level menu of its own, so the commands a
    // user can type cannot drift from the operations the service offers.
    feed_binding_commands::register_commands(root_menu, session);
    market_fixing_commands::register_commands(root_menu, session);
    market_observation_commands::register_commands(root_menu, session);
    market_series_commands::register_commands(root_menu, session);
    market_series_asset_class_commands::register_commands(root_menu, session);
    observation_lineage_commands::register_commands(root_menu, session);
    series_classification_rule_commands::register_commands(root_menu, session);

    // Import has no modelled operation family yet, so it stays hand-written.
    auto marketdata_menu = std::make_unique<cli::Menu>("marketdata");

    marketdata_menu->Insert(
        "import",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_import(std::ref(out), std::ref(session), args);
        },
        "Import ORE market.txt/fixings.txt content via import_market_data_request",
        {"[--file <path>] [--fixings <path>] [--source <tag>] [--duplicates-are-errors]"});

    marketdata_menu->Insert(
        "export",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_export(std::ref(out), std::ref(session), args);
        },
        "Write the tenant's market data back out as ORE market.txt/fixings.txt files",
        {"[--market-data <path>] [--fixings <path>] [--date <YYYY-MM-DD>] [--max-age-days <n>]"});

    marketdata_menu->Insert(
        "backfill-identity",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_backfill_identity(std::ref(out), std::ref(session), args);
        },
        "Re-project the tenant's series identities, writing only the rows that changed",
        {"[--party <party_id>]"});

    marketdata_menu->Insert("ticks",
                            [&session](std::ostream& out, std::vector<std::string> args) {
                                process_ticks(std::ref(out), std::ref(session), args);
                            },
                            "Read the ticks of one series over a span of time, in one call",
                            {"--asset <class> --scope <scope> --instrument <type> --quote <type> "
                             "[--fields <name=value,...>] [--party <id>] [--from <date|instant>] "
                             "[--to <date|instant>] [--coordinate <oresmd-uri>] [--csv <path>]"});

    marketdata_menu->Insert("stream",
                            [&session](std::ostream& out, std::vector<std::string> args) {
                                process_stream(std::ref(out), std::ref(session), args);
                            },
                            "Watch one oresmd on the republished tick stream until Ctrl-C",
                            {"<oresmd-uri> [--csv <path>]"});

    // The codec utility is offline, so it hangs off the same menu but takes
    // no session and answers with no service up.
    oresmd_index_commands::register_verb(*marketdata_menu);

    ores::shell::app::insert_menu(root_menu, std::move(marketdata_menu));
}

void marketdata_commands::process_import(std::ostream& out,
                                         nats_client& session,
                                         const std::vector<std::string>& args) {
    auto parsed = parse_args(args,
                             {{.name = "file", .requires_value = true, .default_value = ""},
                              {.name = "fixings", .requires_value = true, .default_value = ""},
                              {.name = "source", .requires_value = true, .default_value = ""},
                              {.name = "duplicates-are-errors"}});
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }

    const auto& file_path = parsed->flag("file");
    const auto& fixings_path = parsed->flag("fixings");
    if (file_path.empty() && fixings_path.empty()) {
        fail(out) << "Usage: marketdata import [--file <path>] [--fixings <path>] "
                     "[--source <tag>] [--duplicates-are-errors] "
                     "(at least one of --file/--fixings is required)"
                  << std::endl;
        return;
    }

    if (!session.is_logged_in()) {
        fail(out) << "Not logged in." << std::endl;
        return;
    }

    marketdata::messaging::import_market_data_request req;
    if (!file_path.empty()) {
        auto content = read_file(file_path);
        if (!content) {
            fail(out) << "Cannot open file: " << file_path << std::endl;
            return;
        }
        req.market_data_content = std::move(*content);
    }
    if (!fixings_path.empty()) {
        auto content = read_file(fixings_path);
        if (!content) {
            fail(out) << "Cannot open file: " << fixings_path << std::endl;
            return;
        }
        req.fixings_content = std::move(*content);
    }
    req.source = parsed->flag("source");
    req.duplicates_are_errors = parsed->flag_set("duplicates-are-errors");

    BOOST_LOG_SEV(lg(), info) << "Importing market data (file: " << file_path
                              << ", fixings: " << fixings_path << ", source: " << req.source << ")";
    out << "Importing market data..." << std::endl;

    auto result = do_auth_request<marketdata::messaging::import_market_data_response>(
        out, session, std::string(req.nats_subject), req, bulk_transfer_timeout);
    if (!result)
        return;

    if (!result->success) {
        // Sections without duplicate errors are still persisted (see
        // import_service::import) — report their counts too, so a partial
        // import doesn't read as "nothing happened".
        fail(out) << "Failed to import market data: " << result->message << std::endl;
        out << "  (series: " << result->series_count
            << ", observations: " << result->observation_count
            << ", fixings: " << result->fixing_count
            << " — counts reflect content that had no duplicate errors)" << std::endl;
        for (const auto& error : result->errors)
            out << "  ✗ " << error << std::endl;
        return;
    }

    out << "✓ Imported " << result->series_count << " series, " << result->observation_count
        << " observation(s), " << result->fixing_count << " fixing(s): " << result->message
        << std::endl;
    if (!result->warnings.empty()) {
        out << "⚠ " << result->warnings.size() << " warning(s):" << std::endl;
        for (const auto& warning : result->warnings)
            out << "  ⚠ " << warning << std::endl;
    }
    BOOST_LOG_SEV(lg(), info) << "Import succeeded: " << result->series_count << " series, "
                              << result->observation_count << " observations, "
                              << result->fixing_count << " fixings, " << result->warnings.size()
                              << " warning(s).";
}

void marketdata_commands::process_export(std::ostream& out,
                                         nats_client& session,
                                         const std::vector<std::string>& args) {
    auto parsed =
        parse_args(args,
                   {{.name = "market-data", .requires_value = true, .default_value = "market.txt"},
                    {.name = "fixings", .requires_value = true, .default_value = "fixings.txt"},
                    {.name = "date", .requires_value = true, .default_value = ""},
                    {.name = "max-age-days", .requires_value = true, .default_value = ""}});
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }

    if (!session.is_logged_in()) {
        fail(out) << "Not logged in." << std::endl;
        return;
    }

    const auto& market_path = parsed->flag("market-data");
    const auto& fixings_path = parsed->flag("fixings");

    BOOST_LOG_SEV(lg(), info) << "Exporting market data (market-data: " << market_path
                              << ", fixings: " << fixings_path << ")";
    out << "Exporting market data..." << std::endl;

    marketdata::messaging::export_market_data_request req;
    if (!parsed->flag("date").empty())
        req.valuation_date = parsed->flag("date");
    if (!parsed->flag("max-age-days").empty()) {
        try {
            req.max_age_days = std::stoi(parsed->flag("max-age-days"));
        } catch (const std::exception&) {
            fail(out) << "--max-age-days is not a number: " << parsed->flag("max-age-days")
                      << std::endl;
            return;
        }
    }
    auto result = do_auth_request<marketdata::messaging::export_market_data_response>(
        out, session, std::string(req.nats_subject), req, bulk_transfer_timeout);
    if (!result)
        return;

    if (!result->success) {
        fail(out) << "Failed to export market data: " << result->message << std::endl;
        return;
    }

    // Both files are written even when one is empty, because ORE reads both and
    // an absent fixings.txt fails the run -- which is exactly the state the
    // TA002 example ships in, with a fixings.txt of zero lines.
    if (!write_file(market_path, result->market_data_content)) {
        fail(out) << "Cannot write file: " << market_path << std::endl;
        return;
    }
    if (!write_file(fixings_path, result->fixings_content)) {
        fail(out) << "Cannot write file: " << fixings_path << std::endl;
        return;
    }

    out << "✓ Exported " << result->series_count << " series, " << result->observation_count
        << " observation(s), " << result->fixing_count << " fixing(s) to " << market_path << " and "
        << fixings_path << std::endl;
    if (result->carried_forward_count > 0 || result->stale_count > 0)
        out << "  " << result->carried_forward_count
            << " datum(s) carry a value from an earlier day, " << result->stale_count
            << " older than the limit left out of the date." << std::endl;
    BOOST_LOG_SEV(lg(), info) << "Export succeeded: " << result->series_count << " series, "
                              << result->observation_count << " observations, "
                              << result->fixing_count << " fixings.";
}

void marketdata_commands::process_backfill_identity(std::ostream& out,
                                                    nats_client& session,
                                                    const std::vector<std::string>& args) {
    auto parsed =
        parse_args(args, {{.name = "party", .requires_value = true, .default_value = ""}});
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }

    if (!session.is_logged_in()) {
        fail(out) << "Not logged in." << std::endl;
        return;
    }

    const auto& party = parsed->flag("party");
    BOOST_LOG_SEV(lg(), info) << "Re-projecting series identities (party: "
                              << (party.empty() ? "all" : party) << ")";
    out << "Re-projecting series identities..." << std::endl;

    marketdata::messaging::backfill_series_identity_request req;
    req.party_id = party;
    auto result = do_auth_request<marketdata::messaging::backfill_series_identity_response>(
        out, session, std::string(req.nats_subject), req, bulk_transfer_timeout);
    if (!result)
        return;

    if (!result->success) {
        fail(out) << "Failed to re-project the series identities: " << result->message << std::endl;
        return;
    }

    out << "✓ " << result->message << std::endl;
    BOOST_LOG_SEV(lg(), info) << "Identity backfill succeeded: " << result->message;
}

std::expected<stream_watch, std::string> marketdata_commands::stream_subjects(
    std::string_view tenant_id, std::string_view party_id, std::string_view uri) {
    auto datum = marketdata::datum::oresmd_uri_codec::read(uri);
    if (!datum)
        return std::unexpected(datum.error());

    if (!datum->is_series()) {
        // The URI the codec writes is the datum's one spelling, and the subject
        // is made from it, as the ingest loop makes it.
        const auto canonical = marketdata::datum::oresmd_uri_codec::write(*datum);
        if (!canonical)
            return std::unexpected(canonical.error());
        return stream_watch{
            {marketdata::domain::market_tick_subject(tenant_id, party_id, *canonical)},
            std::nullopt};
    }

    // A series is not a datum a producer publishes: it only ever names points,
    // each on a subject of its own, so no NATS wildcard covers exactly one
    // series' points. Watch the party's whole tick subtree and keep the ticks
    // whose series matches.
    return stream_watch{{marketdata::domain::market_tick_subject_all(tenant_id, party_id)}, *datum};
}

bool marketdata_commands::tick_in_series(const marketdata::messaging::market_tick& tick,
                                         const marketdata::datum::market_datum& series) {
    const auto datum = marketdata::datum::oresmd_uri_codec::read(tick.oresmd_uri);
    return datum && marketdata::datum::series_of(*datum) == series;
}

std::string marketdata_commands::tick_line(const marketdata::messaging::market_tick& tick) {
    return std::format("{}  {}  {}  {}",
                       ores::platform::time::datetime::to_iso8601_utc(tick.observation_time),
                       tick.oresmd_uri,
                       tick.value,
                       tick.source);
}

std::string marketdata_commands::tick_row(const marketdata::messaging::market_tick& tick) {
    return std::format(
        "{},{},{},{}",
        csv_field(ores::platform::time::datetime::to_iso8601_utc(tick.observation_time)),
        csv_field(tick.oresmd_uri),
        csv_field(tick.value),
        csv_field(tick.source));
}

stream_output marketdata_commands::run_stream(
    tick_source& source,
    const std::function<bool()>& cancelled,
    const std::string& csv_path,
    std::ostream& out,
    const std::function<bool(const marketdata::messaging::market_tick&)>& accept) {
    stream_output output;
    output.rows.push_back(std::string(stream_csv_header));

    std::ofstream csv;
    if (!csv_path.empty()) {
        csv.open(csv_path);
        if (!csv.is_open())
            output.csv_error = csv_path;
    }
    // The header goes down before anything arrives, so a stream that receives
    // nothing still leaves a file a reader can open.
    if (csv.is_open())
        csv << output.rows.front() << '\n';

    for (;;) {
        auto [ticks, more] = source.next();
        for (const auto& tick : ticks) {
            if (accept && !accept(tick))
                continue;
            output.screen_lines.push_back(tick_line(tick));
            output.rows.push_back(tick_row(tick));
            out << output.screen_lines.back() << std::endl;
            if (csv.is_open())
                csv << output.rows.back() << '\n';
            ++output.count;
        }
        if (csv.is_open())
            csv.flush();
        // A break after a batch rather than before it, so a tick that arrived
        // just as the reader interrupted is still printed.
        if (cancelled() || !more)
            break;
        std::this_thread::sleep_for(stream_poll_interval);
    }

    if (csv.is_open())
        csv.flush();
    return output;
}

std::expected<std::chrono::system_clock::time_point, std::string>
marketdata_commands::parse_span_bound(std::string_view text) {
    std::string wire(text);
    // A date is ten characters, and it means midnight UTC.
    if (wire.size() == 10)
        wire += " 00:00:00Z";
    try {
        return ores::platform::time::datetime::from_iso8601_utc(wire);
    } catch (const std::exception&) {
        return std::unexpected(std::format("'{}' is neither a date (YYYY-MM-DD) nor an instant "
                                           "(YYYY-MM-DD HH:MM:SS[.ffffff]Z).",
                                           text));
    }
}

std::expected<std::vector<marketdata::messaging::series_identity_field>, std::string>
marketdata_commands::parse_identity_fields(std::string_view text) {
    std::vector<marketdata::messaging::series_identity_field> fields;
    while (!text.empty()) {
        const auto end = text.find(',');
        const auto pair = text.substr(0, end);
        text = end == std::string_view::npos ? std::string_view{} : text.substr(end + 1);
        const auto equals = pair.find('=');
        if (equals == std::string_view::npos || equals == 0 || equals + 1 == pair.size())
            return std::unexpected(
                std::format("'{}' is not a field of the form name=value.", pair));
        fields.push_back(
            {std::string(pair.substr(0, equals)), std::string(pair.substr(equals + 1))});
    }
    return fields;
}

std::expected<std::vector<history_tick>, std::string>
marketdata_commands::history_ticks(const marketdata::messaging::get_series_ticks_response& reply) {
    const auto count = reply.values.size();
    if (reply.gaps_us.size() != count || reply.coordinate_indexes.size() != count)
        return std::unexpected("The reply is malformed: its arrays differ in length.");
    std::vector<history_tick> ticks;
    ticks.reserve(count);
    if (count == 0)
        return ticks;
    if (!reply.first_instant)
        return std::unexpected("The reply is malformed: it holds ticks and no first instant.");
    auto at = *reply.first_instant;
    for (std::size_t i = 0; i != count; ++i) {
        const auto index = reply.coordinate_indexes[i];
        if (index < 0 || static_cast<std::size_t>(index) >= reply.coordinates.size())
            return std::unexpected(
                std::format("The reply is malformed: tick {} names coordinate {} "
                            "and the reply holds {}.",
                            i,
                            index,
                            reply.coordinates.size()));
        at += std::chrono::microseconds(reply.gaps_us[i]);
        ticks.push_back({at, reply.coordinates[static_cast<std::size_t>(index)], reply.values[i]});
    }
    return ticks;
}

std::string marketdata_commands::history_tick_line(const history_tick& tick) {
    return std::format("{}  {}  {}",
                       ores::platform::time::datetime::to_iso8601_utc(tick.at),
                       tick.oresmd_uri,
                       tick.value);
}

std::string marketdata_commands::history_tick_row(const history_tick& tick) {
    return std::format("{},{},{}",
                       csv_field(ores::platform::time::datetime::to_iso8601_utc(tick.at)),
                       csv_field(tick.oresmd_uri),
                       csv_field(std::format("{}", tick.value)));
}

void marketdata_commands::process_ticks(std::ostream& out,
                                        nats_client& session,
                                        const std::vector<std::string>& args) {
    auto parsed = parse_args(args,
                             {{.name = "asset", .requires_value = true, .default_value = ""},
                              {.name = "scope", .requires_value = true, .default_value = ""},
                              {.name = "instrument", .requires_value = true, .default_value = ""},
                              {.name = "quote", .requires_value = true, .default_value = ""},
                              {.name = "fields", .requires_value = true, .default_value = ""},
                              {.name = "party", .requires_value = true, .default_value = ""},
                              {.name = "from", .requires_value = true, .default_value = ""},
                              {.name = "to", .requires_value = true, .default_value = ""},
                              {.name = "coordinate", .requires_value = true, .default_value = ""},
                              {.name = "csv", .requires_value = true, .default_value = ""}});
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }
    if (!parsed->positionals.empty() || parsed->flag("asset").empty() ||
        parsed->flag("scope").empty() || parsed->flag("instrument").empty() ||
        parsed->flag("quote").empty()) {
        fail(out) << "Usage: marketdata ticks --asset <class> --scope <scope> "
                     "--instrument <type> --quote <type> [--fields <name=value,...>] "
                     "[--party <id>] [--from <date|instant>] [--to <date|instant>] "
                     "[--coordinate <oresmd-uri>] [--csv <path>]"
                  << std::endl;
        return;
    }
    if (!session.is_logged_in()) {
        fail(out) << "Not logged in." << std::endl;
        return;
    }

    marketdata::messaging::get_series_ticks_request req;
    req.identity.asset = parsed->flag("asset");
    req.identity.scope = parsed->flag("scope");
    req.identity.instrument_type = parsed->flag("instrument");
    req.identity.quote_type = parsed->flag("quote");
    req.identity.party_id = parsed->flag("party");
    const auto fields = parse_identity_fields(parsed->flag("fields"));
    if (!fields) {
        fail(out) << "--fields: " << fields.error() << std::endl;
        return;
    }
    req.identity.fields = *fields;
    if (parsed->flag("from").empty()) {
        req.from_instant = std::chrono::system_clock::now() - std::chrono::minutes(10);
    } else {
        const auto from = parse_span_bound(parsed->flag("from"));
        if (!from) {
            fail(out) << "--from: " << from.error() << std::endl;
            return;
        }
        req.from_instant = *from;
    }
    if (!parsed->flag("to").empty()) {
        const auto to = parse_span_bound(parsed->flag("to"));
        if (!to) {
            fail(out) << "--to: " << to.error() << std::endl;
            return;
        }
        req.to_instant = *to;
    }
    if (!parsed->flag("coordinate").empty())
        req.coordinates.push_back(parsed->flag("coordinate"));

    BOOST_LOG_SEV(lg(), info) << "Reading the ticks of the " << req.identity.instrument_type
                              << " series of " << req.identity.scope;
    auto result = do_auth_request<marketdata::messaging::get_series_ticks_response>(
        out, session, std::string(req.nats_subject), req);
    if (!result)
        return;
    if (result->result.outcome != ores::utility::domain::outcome::ok) {
        fail(out) << "Failed to read the ticks: " << result->result.message << " ("
                  << result->result.code << ")" << std::endl;
        return;
    }

    const auto ticks = history_ticks(*result);
    if (!ticks) {
        fail(out) << ticks.error() << std::endl;
        return;
    }
    for (const auto& tick : *ticks)
        out << history_tick_line(tick) << std::endl;
    out << "✓ " << ticks->size() << " tick(s) of " << result->coordinates.size()
        << " coordinate(s)." << std::endl;

    const auto& csv_path = parsed->flag("csv");
    if (!csv_path.empty()) {
        std::string body = "time,coordinate,value\n";
        for (const auto& tick : *ticks)
            body += history_tick_row(tick) + "\n";
        if (!write_file(csv_path, body)) {
            fail(out) << "Cannot write file: " << csv_path << std::endl;
            return;
        }
        out << "Wrote " << csv_path << std::endl;
    }
}

void marketdata_commands::process_stream(std::ostream& out,
                                         nats_client& session,
                                         const std::vector<std::string>& args) {
    auto parsed = parse_args(args, {{.name = "csv", .requires_value = true, .default_value = ""}});
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }
    if (parsed->positionals.size() != 1) {
        fail(out) << "Usage: marketdata stream <oresmd-uri> [--csv <path>]" << std::endl;
        return;
    }
    if (!session.is_logged_in()) {
        fail(out) << "Not logged in." << std::endl;
        return;
    }

    const auto& uri = parsed->positionals[0];
    const auto& csv_path = parsed->flag("csv");
    const auto& auth = session.auth();

    auto watch = stream_subjects(auth.tenant_id, auth.default_party_id, uri);
    if (!watch) {
        // The codec's own message names what the URI got wrong, which a raw
        // subject error would not.
        fail(out) << watch.error() << std::endl;
        return;
    }

    for (const auto& subject : watch->subjects)
        out << "Watching '" << subject << "' for " << uri << "; Ctrl-C to stop." << std::endl;
    if (watch->series)
        out << "Keeping only ticks of this series." << std::endl;
    if (!csv_path.empty())
        out << "Writing CSV to " << csv_path << std::endl;

    subscription_tick_source source(session.transport(), watch->subjects);

    std::function<bool(const marketdata::messaging::market_tick&)> accept;
    if (watch->series)
        accept = [series = *watch->series](const marketdata::messaging::market_tick& tick) {
            return tick_in_series(tick, series);
        };

    interrupted = 0;
    const auto previous = std::signal(SIGINT, on_interrupt);
    const auto output = run_stream(source, [] { return interrupted != 0; }, csv_path, out, accept);
    std::signal(SIGINT, previous);

    if (output.count == 0)
        out << "No ticks arrived on '" << watch->subjects.front() << "'." << std::endl;
    out << output.count << " tick(s)." << std::endl;
    if (!output.csv_error.empty())
        fail(out) << "Cannot write CSV file: " << output.csv_error << std::endl;
    else if (!csv_path.empty())
        out << "CSV written to " << csv_path << " (" << output.rows.size() - 1 << " tick row(s))."
            << std::endl;
    BOOST_LOG_SEV(lg(), info) << "Streamed " << output.count << " tick(s) for '" << uri << "'.";
}

}
