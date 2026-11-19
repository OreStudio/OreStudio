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
#include "ores.eventing.api/domain/entity_event_traits.hpp"
#include "ores.marketdata.api/datum/oresmd_uri_codec.hpp"
#include "ores.marketdata.api/domain/tick_subjects.hpp"
#include "ores.marketdata.api/eventing/route_event.hpp"
#include "ores.marketdata.api/eventing/subscription_event.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/buffered_subscription.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/subscription.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.shell/app/command_args.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include "ores.shell/app/commands/marketdata/adapter_commands.hpp"
#include "ores.shell/app/commands/marketdata/adapter_status_commands.hpp"
#include "ores.shell/app/commands/marketdata/curve_publish_commands.hpp"
#include "ores.shell/app/commands/marketdata/feed_commands.hpp"
#include "ores.shell/app/commands/marketdata/feed_scope_commands.hpp"
#include "ores.shell/app/commands/marketdata/fx_forward_publish_commands.hpp"
#include "ores.shell/app/commands/marketdata/fx_forward_publish_point_commands.hpp"
#include "ores.shell/app/commands/marketdata/fx_forward_publish_spot_commands.hpp"
#include "ores.shell/app/commands/marketdata/fx_vol_surface_calibration_commands.hpp"
#include "ores.shell/app/commands/marketdata/fx_vol_surface_publish_commands.hpp"
#include "ores.shell/app/commands/marketdata/market_fixing_commands.hpp"
#include "ores.shell/app/commands/marketdata/market_observation_commands.hpp"
#include "ores.shell/app/commands/marketdata/market_series_asset_class_commands.hpp"
#include "ores.shell/app/commands/marketdata/market_series_commands.hpp"
#include "ores.shell/app/commands/marketdata/observation_lineage_commands.hpp"
#include "ores.shell/app/commands/marketdata/oresmd_index_commands.hpp"
#include "ores.shell/app/commands/marketdata/series_classification_rule_commands.hpp"
#include "ores.shell/app/commands/marketdata/series_identity_shape_commands.hpp"
#include "ores.shell/app/commands/marketdata/series_identity_shape_entry_commands.hpp"
#include "ores.shell/app/commands/marketdata/supplier_commands.hpp"
#include "ores.shell/app/request_helpers.hpp"
#include "ores.shell/app/shell_root_menu.hpp"
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <atomic>
#include <chrono>
#include <cli/cli.h>
#include <csignal>
#include <expected>
#include <format>
#include <fstream>
#include <functional>
#include <map>
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
 * @brief The ticks a live watch carries, on the subjects of the feeds the party may watch now.
 *
 * The watch holds one subscription for each subject, and reads only what arrived since the
 * last call, so a tick is printed once. The broker does not check subjects, so a change to a
 * route or a subscription marks the watch, and the next read asks again which feeds the party
 * may watch and changes the subscriptions to match. A feed the party loses stops arriving.
 */
class watched_tick_source final : public tick_source {
public:
    using subjects_now = std::function<std::expected<std::vector<std::string>, std::string>()>;

    watched_tick_source(ores::nats::service::client& transport,
                        const std::vector<std::string>& subjects,
                        subjects_now ask,
                        std::ostream& out)
        : transport_(transport)
        , ask_(std::move(ask))
        , out_(out) {
        watch(subjects);
        using ores::eventing::domain::event_subject_wildcard;
        for (const auto& events :
             {event_subject_wildcard<marketdata::messaging::route_event>(),
              event_subject_wildcard<marketdata::messaging::subscription_event>()})
            events_.push_back(
                transport.subscribe(events, [this](ores::nats::message) { changed_ = true; }));
    }

    std::pair<std::vector<marketdata::messaging::market_tick>, bool> next() override {
        if (changed_.exchange(false)) {
            // A failed question keeps the feeds already held, so a revocation takes effect at
            // the next change that the service answers. A watch that dropped every feed on a
            // failure would go dark each time the service restarts.
            if (const auto wanted = ask_())
                watch(*wanted);
            else
                fail(out_) << "Failed to ask again which feeds to watch: " << wanted.error()
                           << std::endl;
        }
        std::vector<marketdata::messaging::market_tick> ticks;
        for (auto& [subject, held] : subscriptions_) {
            auto& [subscription, read] = held;
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
    void watch(const std::vector<std::string>& wanted) {
        std::vector<std::string> held;
        held.reserve(subscriptions_.size());
        for (const auto& entry : subscriptions_)
            held.push_back(entry.first);
        const auto change = marketdata_commands::change_watch(held, wanted);
        for (const auto& subject : change.dropped) {
            subscriptions_.erase(subject);
            out_ << "Stopped watching '" << subject << "'." << std::endl;
        }
        for (const auto& subject : change.added) {
            subscriptions_.emplace(
                subject,
                std::pair{transport_.subscribe_buffered(subject, stream_buffer_capacity),
                          std::size_t{0}});
            out_ << "Watching '" << subject << "'." << std::endl;
        }
    }

    ores::nats::service::client& transport_;
    subjects_now ask_;
    std::ostream& out_;
    std::map<std::string, std::pair<ores::nats::service::buffered_subscription, std::size_t>>
        subscriptions_;
    std::atomic<bool> changed_{false};
    // Last, so the event handlers stop before the state they write goes.
    std::vector<ores::nats::service::subscription> events_;
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
    curve_publish_commands::register_commands(root_menu, session);
    fx_forward_publish_commands::register_commands(root_menu, session);
    fx_forward_publish_point_commands::register_commands(root_menu, session);
    fx_forward_publish_spot_commands::register_commands(root_menu, session);
    fx_vol_surface_calibration_commands::register_commands(root_menu, session);
    fx_vol_surface_publish_commands::register_commands(root_menu, session);
    feed_commands::register_commands(root_menu, session);
    feed_scope_commands::register_commands(root_menu, session);
    adapter_commands::register_commands(root_menu, session);
    supplier_commands::register_commands(root_menu, session);
    market_fixing_commands::register_commands(root_menu, session);
    market_observation_commands::register_commands(root_menu, session);
    market_series_commands::register_commands(root_menu, session);
    market_series_asset_class_commands::register_commands(root_menu, session);
    observation_lineage_commands::register_commands(root_menu, session);
    series_classification_rule_commands::register_commands(root_menu, session);
    series_identity_shape_commands::register_commands(root_menu, session);
    series_identity_shape_entry_commands::register_commands(root_menu, session);

    // Import has no modelled operation family yet, so it stays hand-written.
    auto marketdata_menu = std::make_unique<cli::Menu>("marketdata");
    adapter_status_commands::register_commands(*marketdata_menu, session);

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
                             "[--fields <name=value,...>] [--party <id>] [--provider <code>] "
                             "[--from <date|instant>] "
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

std::expected<std::vector<std::string>, std::string> marketdata_commands::stream_subjects(
    const std::vector<marketdata::messaging::watchable_feed>& feeds, std::string_view uri) {
    const auto datum = marketdata::datum::oresmd_uri_codec::read(uri);
    if (!datum)
        return std::unexpected(datum.error());
    // A URI that reads but whose series cannot be written is refused here, with no feed asked.
    if (const auto series =
            marketdata::datum::oresmd_uri_codec::write(marketdata::datum::series_of(*datum));
        !series)
        return std::unexpected(series.error());
    std::vector<std::string> subjects;
    subjects.reserve(feeds.size());
    for (const auto& feed : feeds) {
        auto subject =
            marketdata::domain::feed_tick_watch_subject(feed.provider_code, feed.feed_id, *datum);
        if (!subject)
            return std::unexpected(subject.error());
        subjects.push_back(std::move(*subject));
    }
    return subjects;
}

watch_change marketdata_commands::change_watch(const std::vector<std::string>& held,
                                               const std::vector<std::string>& wanted) {
    watch_change change;
    for (const auto& subject : wanted)
        if (std::ranges::find(held, subject) == held.end() &&
            std::ranges::find(change.added, subject) == change.added.end())
            change.added.push_back(subject);
    for (const auto& subject : held)
        if (std::ranges::find(wanted, subject) == wanted.end())
            change.dropped.push_back(subject);
    return change;
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

stream_output marketdata_commands::run_stream(tick_source& source,
                                              const std::function<bool()>& cancelled,
                                              const std::string& csv_path,
                                              std::ostream& out) {
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
                              {.name = "provider", .requires_value = true, .default_value = ""},
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
        fail(out)
            << "Usage: marketdata ticks --asset <class> --scope <scope> "
               "--instrument <type> --quote <type> [--fields <name=value,...>] "
               "[--party <id>] [--provider <code>] [--from <date|instant>] [--to <date|instant>] "
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
    req.identity.provider_code = parsed->flag("provider");
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

    // The codec's own message names what the URI got wrong, which a raw subject error would not,
    // so the URI is read before the service is asked anything.
    if (const auto checked = stream_subjects({}, uri); !checked) {
        fail(out) << checked.error() << std::endl;
        return;
    }

    // The broker does not check subjects, so the shell watches only the feeds the service says
    // the party may show on a screen, and asks again when they may have changed.
    const auto ask =
        [&out, &session, uri]() -> std::expected<std::vector<std::string>, std::string> {
        marketdata::messaging::list_watchable_feeds_request req;
        req.for_display = true;
        const auto result = do_auth_request<marketdata::messaging::list_watchable_feeds_response>(
            out, session, std::string(req.nats_subject), req);
        if (!result)
            return std::unexpected("the market data service did not answer");
        if (result->result.outcome != ores::utility::domain::outcome::ok)
            return std::unexpected(result->result.message + " (" + result->result.code + ")");
        return stream_subjects(result->feeds, uri);
    };
    const auto subjects = ask();
    if (!subjects) {
        fail(out) << "Failed to read which feeds the party may watch: " << subjects.error()
                  << std::endl;
        return;
    }
    out << "Watching " << uri << " on " << subjects->size() << " feed(s); Ctrl-C to stop."
        << std::endl;
    if (!csv_path.empty())
        out << "Writing CSV to " << csv_path << std::endl;

    watched_tick_source source(session.transport(), *subjects, ask, out);

    interrupted = 0;
    const auto previous = std::signal(SIGINT, on_interrupt);
    const auto output = run_stream(source, [] { return interrupted != 0; }, csv_path, out);
    std::signal(SIGINT, previous);

    if (output.count == 0)
        out << "No ticks arrived for " << uri << "." << std::endl;
    out << output.count << " tick(s)." << std::endl;
    if (!output.csv_error.empty())
        fail(out) << "Cannot write CSV file: " << output.csv_error << std::endl;
    else if (!csv_path.empty())
        out << "CSV written to " << csv_path << " (" << output.rows.size() - 1 << " tick row(s))."
            << std::endl;
    BOOST_LOG_SEV(lg(), info) << "Streamed " << output.count << " tick(s) for '" << uri << "'.";
}

}
