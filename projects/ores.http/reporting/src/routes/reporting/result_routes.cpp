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
#include "ores.http/routes/reporting/result_routes.hpp"
#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.http/routes/reporting/result_rollup.hpp"
#include "ores.http/routes/reporting/run_lookup.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/messaging/book_protocol.hpp"
#include "ores.refdata.api/messaging/counterparty_protocol.hpp"
#include "ores.refdata.api/messaging/portfolio_protocol.hpp"
#include "ores.reporting.api/messaging/report_instance_protocol.hpp"
#include "ores.reporting.api/messaging/run_result_protocol.hpp"
#include "ores.trading.api/messaging/trade_booking_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <algorithm>
#include <cmath>
#include <format>
#include <functional>
#include <iterator>
#include <map>
#include <optional>
#include <set>
#include <sstream>
#include <string>
#include <string_view>
#include <vector>

namespace ores::http::routes::reporting {

namespace {

namespace json = ores::utility::codec::json;
namespace rr = ores::http::net::record_routes;
namespace forwarding = ores::http::forwarding;
namespace reporting_messaging = ores::reporting::messaging;
namespace refdata_messaging = ores::refdata::messaging;
namespace trading_messaging = ores::trading::messaging;

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::nats::service::nats_client;

auto& result_lg() {
    static auto instance = ores::logging::make_logger("ores.http.reporting.result_routes");
    return instance;
}

namespace results {

using rollup::aggregate_row;
using rollup::change_set;
using rollup::money;
using rollup::priced_trade;

/// How many rows one run's read asks for: a large run, not a whole table.
constexpr std::uint32_t row_limit = 20000;

/// The largest page any read of this unit asks for.
constexpr std::uint32_t page_limit = 1000;

/// The most pages a read follows before it stops, so a bad total cannot hang it.
constexpr int page_count_limit = 1000;

/// The uuid a row carries when the engine stated no counterparty.
constexpr std::string_view nil_id = "00000000-0000-0000-0000-000000000000";

/// The names and books behind the ids a run's rows carry.
struct trade_records final {
    std::map<std::string, std::string> counterparty_name;
    std::map<std::string, std::string> counterparty_parent;
    std::map<std::string, std::string> book_of;
    std::map<std::string, std::string> book_name;
    std::map<std::string, std::string> book_portfolio;
    std::map<std::string, std::string> portfolio_name;
    std::map<std::string, std::string> portfolio_parent;
    bool complete = false;
};

/// The rows of a run, and what the screen needs to group them.
struct rows_answer final {
    std::vector<priced_trade> rows;
    std::string reason;
    bool records_complete = true;
    trade_records records;
};

/// A figure to two places, as the screen shows it.
std::string two_places(double value) {
    if (!std::isfinite(value))
        return "0.00";
    return std::format("{:.2f}", value);
}

/// A figure as the engine stated it, without the places a total gains.
std::string figure_text(double value) {
    if (!std::isfinite(value))
        return "0";
    return std::format("{}", value);
}

/// The state word one run reads as, or nothing when it cannot be read.
std::optional<std::string>
state_name_of(const http_request& req, nats_client& session, const boost::uuids::uuid& id) {
    try {
        reporting_messaging::get_report_instance_health_request stated;
        stated.instance_ids = std::vector{id};
        const auto reply = forwarding::forward(req, session, stated);
        for (const auto& entry : reply.health)
            if (entry.report_instance_id == id)
                return entry.state_name;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(result_lg(), ores::logging::warn)
            << "The state of run " << boost::uuids::to_string(id)
            << " could not be read: " << e.what();
    }
    return std::nullopt;
}

/// The figures one run produced, read by the run they belong to.
std::vector<ores::reporting::domain::run_result>
read_run_results(const http_request& req, nats_client& session, const boost::uuids::uuid& run_id) {
    std::vector<ores::reporting::domain::run_result> results;
    for (auto page = 0; page < page_count_limit; ++page) {
        reporting_messaging::list_run_results_request msg;
        msg.offset = static_cast<std::uint32_t>(results.size());
        msg.limit = row_limit;
        msg.filter.emplace();
        msg.filter->report_instance_id = run_id;
        const auto reply = forwarding::forward(req, session, msg);
        rr::require_done(reply.result);
        results.insert(results.end(),
                       std::make_move_iterator(reply.results.begin()),
                       std::make_move_iterator(reply.results.end()));
        if (reply.results.empty() || results.size() >= reply.total)
            break;
    }
    return results;
}

/// The names and books behind the ids a run's rows carry.
trade_records read_trade_records(const http_request& req,
                                 nats_client& session,
                                 const std::vector<std::string>& trade_ids) {
    trade_records records;
    try {
        // The bookings are the only read narrowed to this run's trades; the
        // catalogues beside it are read whole.
        std::vector<boost::uuids::uuid> ids;
        for (const auto& id : trade_ids) {
            try {
                ids.push_back(boost::uuids::string_generator()(id));
            } catch (const std::exception&) {
                // An id the engine stated as text and not as a uuid names no
                // booking this estate can look up.
            }
        }

        refdata_messaging::list_counterparties_request counterparties_msg;
        counterparties_msg.limit = page_limit;
        const auto counterparties = forwarding::forward(req, session, counterparties_msg);
        for (const auto& person : counterparties.counterparties) {
            const auto id = boost::uuids::to_string(person.id);
            records.counterparty_name.emplace(id, person.full_name);
            if (person.parent_counterparty_id && !person.parent_counterparty_id->is_nil())
                records.counterparty_parent.emplace(
                    id, boost::uuids::to_string(*person.parent_counterparty_id));
        }

        trading_messaging::list_trade_bookings_request bookings_msg;
        bookings_msg.limit = page_limit;
        bookings_msg.filter.emplace();
        bookings_msg.filter->trade_id_one_of = std::move(ids);
        const auto bookings = forwarding::forward(req, session, bookings_msg);
        for (const auto& booking : bookings.trade_bookings)
            records.book_of.emplace(boost::uuids::to_string(booking.trade_id),
                                    boost::uuids::to_string(booking.book_id));

        refdata_messaging::list_books_request books_msg;
        books_msg.limit = page_limit;
        const auto books = forwarding::forward(req, session, books_msg);
        for (const auto& book : books.books) {
            const auto id = boost::uuids::to_string(book.id);
            records.book_name.emplace(id, book.name);
            if (!book.parent_portfolio_id.is_nil())
                records.book_portfolio.emplace(id,
                                               boost::uuids::to_string(book.parent_portfolio_id));
        }

        // A portfolio read that fails costs the portfolio names, not the
        // counterparties and the books beside it.
        try {
            refdata_messaging::list_portfolios_request portfolios_msg;
            portfolios_msg.limit = page_limit;
            const auto portfolios = forwarding::forward(req, session, portfolios_msg);
            for (const auto& portfolio : portfolios.portfolios) {
                const auto id = boost::uuids::to_string(portfolio.id);
                records.portfolio_name.emplace(id, portfolio.name);
                if (portfolio.parent_portfolio_id && !portfolio.parent_portfolio_id->is_nil())
                    records.portfolio_parent.emplace(
                        id, boost::uuids::to_string(*portfolio.parent_portfolio_id));
            }
            records.complete = true;
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(result_lg(), ores::logging::warn)
                << "The portfolios of the run's books could not be read: " << e.what();
            records.complete = false;
        }
    } catch (const std::exception& e) {
        // A read that fails leaves the names empty rather than failing the
        // figures: a run's numbers are the answer and its names annotate them.
        BOOST_LOG_SEV(result_lg(), ores::logging::warn)
            << "The records behind a run's trades could not be read: " << e.what();
        return trade_records{};
    }
    return records;
}

/**
 * @brief The rows of a run's result, or the reason there are none.
 *
 * The rows are the post-processor's: one per subject, measure and currency, read
 * from the result table by the run they belong to. A run that completed before
 * the post-processor existed has no rows, and answers the reason the screen
 * states rather than reading the archive it left behind.
 */
rows_answer
rows_of_run(const http_request& req, nats_client& session, const boost::uuids::uuid& run_id) {
    rows_answer out;
    const auto stated = read_run_results(req, session, run_id);
    if (stated.empty()) {
        out.reason = "no-rows";
        return out;
    }

    // The engine states one row per measure, so the measures of one trade are
    // gathered before the trade is a line of the answer.
    struct figures final {
        double npv = 0;
        std::string npv_currency;
        double npv_base = 0;
        bool has_npv = false;
        bool has_npv_base = false;
        double notional = 0;
        std::string notional_currency;
        std::string counterparty;
    };
    std::map<std::string, figures> by_trade;
    // The trades keep the order the run stated them in, so the screen reads the
    // same table twice alike.
    std::vector<std::string> order_of_appearance;
    std::set<std::string> seen;
    for (const auto& row : stated) {
        if (row.subject_id.empty())
            continue;
        if (row.measure != "npv" && row.measure != "npv_base" && row.measure != "notional")
            continue;
        auto& found = by_trade[row.subject_id];
        if (seen.insert(row.subject_id).second)
            order_of_appearance.push_back(row.subject_id);
        if (row.measure == "npv") {
            found.npv = row.figure;
            found.npv_currency = row.currency;
            found.has_npv = true;
        } else if (row.measure == "npv_base") {
            found.npv_base = row.figure;
            found.has_npv_base = true;
        } else if (row.measure == "notional") {
            found.notional = row.figure;
            found.notional_currency = row.currency;
        }
        if (row.counterparty_id && !row.counterparty_id->is_nil())
            found.counterparty = boost::uuids::to_string(*row.counterparty_id);
    }

    std::vector<std::string> trade_ids;
    for (const auto& trade_id : order_of_appearance) {
        const auto& found = by_trade[trade_id];
        // A trade the run stated no NPV for is not a line the screen can draw.
        if (!found.has_npv)
            continue;
        priced_trade row;
        row.trade_id = trade_id;
        row.npv = found.npv;
        row.npv_currency = found.npv_currency;
        row.npv_base = found.npv_base;
        row.has_npv_base = found.has_npv_base;
        row.notional = found.notional;
        row.notional_currency = found.notional_currency;
        row.counterparty = found.counterparty;
        out.rows.push_back(std::move(row));
        trade_ids.push_back(trade_id);
    }
    if (out.rows.empty()) {
        out.reason = "no-figures";
        return out;
    }

    out.records = read_trade_records(req, session, trade_ids);
    out.records_complete = out.records.complete;
    for (auto& row : out.rows) {
        const auto book = out.records.book_of.find(row.trade_id);
        row.book_id = book == out.records.book_of.end() ? std::string() : book->second;
        const auto person = out.records.counterparty_name.find(row.counterparty);
        row.counterparty_name =
            person == out.records.counterparty_name.end() ? std::string() : person->second;
        const auto named = out.records.book_name.find(row.book_id);
        row.book_name = named == out.records.book_name.end() ? std::string() : named->second;
    }
    return out;
}

/// The answer at one level.
struct level_answer final {
    std::string level;
    /// The trades themselves, stated when the reader asked for them.
    std::vector<priced_trade> trades;
    std::vector<aggregate_row> rows;
    std::vector<money> totals;
    std::size_t trade_count = 0;
    std::string reason;
    bool records_complete = true;
    std::optional<std::string> baseline_id;
    std::vector<money> baseline_totals;
    std::vector<money> baseline_change;
    std::vector<money> baseline_change_pct;
};

/// The answer for the level the reader asked for.
level_answer results_of_run(const http_request& req,
                            nats_client& session,
                            const boost::uuids::uuid& run_id,
                            const std::string& level) {
    auto read = rows_of_run(req, session, run_id);
    level_answer out;
    out.level = level;
    out.totals = rollup::npv_totals(read.rows);
    out.trade_count = read.rows.size();
    out.reason = read.reason;
    out.records_complete = read.records_complete;

    if (level == "trade") {
        out.trades = std::move(read.rows);
        return out;
    }

    const auto key_of = [&level](const priced_trade& row) {
        return level == "book" ? row.book_id : row.counterparty;
    };
    std::map<std::string, std::string> names;
    for (const auto& row : read.rows)
        names[key_of(row)] = level == "book" ? row.book_name : row.counterparty_name;

    const auto rolled = aggregate_rows(read.rows, key_of, names);
    out.rows =
        level == "counterparty" ?
            with_groups(rolled, read.records.counterparty_parent, read.records.counterparty_name) :
            with_portfolio_tree(rolled,
                                read.records.book_portfolio,
                                read.records.portfolio_name,
                                read.records.portfolio_parent);
    return out;
}

/**
 * @brief The answer for a level, stated against an earlier run of the same report.
 *
 * The two runs are read at the same level and compared row by row.
 */
level_answer compared_of_runs(const http_request& req,
                              nats_client& session,
                              const boost::uuids::uuid& run_id,
                              const boost::uuids::uuid& baseline_id,
                              const std::string& baseline_text,
                              const std::string& level) {
    auto current = results_of_run(req, session, run_id, level);
    const auto earlier = results_of_run(req, session, baseline_id, level);
    const auto changed = change_between(current.totals, earlier.totals);
    current.rows = compared_with(current.rows, earlier.rows);
    current.baseline_id = baseline_text;
    current.baseline_totals = earlier.totals;
    current.baseline_change = changed.change;
    current.baseline_change_pct = changed.pct;
    return current;
}

/// A row's figures as the screen reads them.
std::string money_json(const std::vector<money>& figures) {
    std::vector<std::string> written;
    written.reserve(figures.size());
    for (const auto& one : figures)
        written.push_back("{\"currency\":" + json::write(one.currency) + ",\"total\":" +
                          json::write(one.stated ? two_places(one.total) : std::string()) + "}");
    return rr::array_of(written);
}

/// The change a comparison states, beside the run it is compared with.
std::string baseline_json(const level_answer& answer) {
    if (!answer.baseline_id)
        return "null";
    return "{\"id\":" + json::write(*answer.baseline_id) +
           ",\"totals\":" + money_json(answer.baseline_totals) +
           ",\"change\":" + money_json(answer.baseline_change) +
           ",\"change_pct\":" + money_json(answer.baseline_change_pct) + "}";
}

/// One trade's line of the answer.
std::string trade_json(const priced_trade& row) {
    return "{\"trade_id\":" + json::write(row.trade_id) + ",\"trade_type\":\"\"" +
           ",\"npv\":" + json::write(figure_text(row.npv)) +
           ",\"npv_currency\":" + json::write(row.npv_currency) + ",\"npv_base\":" +
           json::write(row.has_npv_base ? figure_text(row.npv_base) : std::string()) +
           ",\"notional\":" + json::write(figure_text(row.notional)) +
           ",\"notional_currency\":" + json::write(row.notional_currency) +
           ",\"counterparty\":" + json::write(row.counterparty) +
           ",\"counterparty_name\":" + json::write(row.counterparty_name) +
           ",\"book_id\":" + json::write(row.book_id) +
           ",\"book_name\":" + json::write(row.book_name) + "}";
}

/// One book's or counterparty's line of the answer.
std::string aggregate_json(const aggregate_row& row) {
    return "{\"key\":" + json::write(row.key) + ",\"name\":" + json::write(row.name) +
           ",\"trades\":" + std::to_string(row.trades) + ",\"totals\":" + money_json(row.totals) +
           ",\"notional\":" + money_json(row.notional) + ",\"group\":" + json::write(row.group) +
           ",\"is_group\":" + (row.is_group ? "true" : "false") +
           ",\"previous\":" + money_json(row.previous) + ",\"change\":" + money_json(row.change) +
           ",\"change_pct\":" + money_json(row.change_pct) +
           ",\"is_removed\":" + (row.is_removed ? "true" : "false") +
           ",\"depth\":" + std::to_string(row.depth) + "}";
}

/// The answer as the screen reads it.
std::string answer_json(const level_answer& answer) {
    std::vector<std::string> rows;
    if (answer.level == "trade") {
        rows.reserve(answer.trades.size());
        for (const auto& row : answer.trades)
            rows.push_back(trade_json(row));
    } else {
        rows.reserve(answer.rows.size());
        for (const auto& row : answer.rows)
            rows.push_back(aggregate_json(row));
    }

    std::ostringstream body;
    body << "{\"level\":" << json::write(answer.level) << ",\"rows\":" << rr::array_of(rows)
         << ",\"totals\":" << money_json(answer.totals) << ",\"tradeCount\":" << answer.trade_count
         << ",\"reason\":" << json::write(answer.reason)
         << ",\"recordsComplete\":" << (answer.records_complete ? "true" : "false");
    // A trade is read on its own, so only an aggregate states what it is
    // compared with.
    if (answer.level != "trade")
        body << ",\"baseline\":" << baseline_json(answer);
    body << '}';
    return body.str();
}

}

}

using namespace results;

void result_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    using forwarding::guarded;
    const auto add = [&](ores::http::net::route_builder builder) {
        const auto built = builder.tags({"reporting"}).auth_required().build();
        router->add_route(built);
        registry->register_route(built);
    };

    add(router->get("/api/reporting/runs/{id}/results")
            .summary("The figures one report run produced, at the level the reader asks for")
            .description("Trade, book or counterparty: the run's figures rolled up to the level, "
                         "with each row's share of the run and, when an earlier run is named, what "
                         "it was there and how far it moved.")
            .query_param("by", "string", "", false, "trade, book or counterparty")
            .query_param("baseline", "string", "", false, "The earlier run to compare with")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<reporting_messaging::list_run_results_request>([&] {
                    const auto raw = req.get_path_param("id");
                    if (!rr::is_uuid(raw))
                        rr::invalid("The address names an id that is not a uuid.");
                    rr::refuse_unknown_params(req, {"by", "baseline"});

                    auto level = req.get_query_param("by");
                    if (level.empty())
                        level = "trade";
                    if (level != "trade" && level != "book" && level != "counterparty")
                        rr::invalid("The level asked for is not one a result is read at.");

                    const auto baseline = req.get_query_param("baseline");
                    if (!baseline.empty()) {
                        if (!rr::is_uuid(baseline))
                            rr::invalid("The address names an id that is not a uuid.");
                        if (baseline == raw)
                            rr::invalid("A run is not compared with itself.");
                        if (level == "trade")
                            rr::invalid("A trade is read on its own, so it is not compared.");
                    }

                    const auto run = run_of(req, session, raw);
                    if (!run) {
                        // A run the caller cannot read has no figures, and the
                        // screen states that rather than failing.
                        level_answer none;
                        none.level = level;
                        none.reason = "no-run";
                        if (level != "trade")
                            none.baseline_id = std::nullopt;
                        return http_response::json(answer_json(none));
                    }

                    const auto run_id = boost::uuids::string_generator()(raw);
                    if (baseline.empty()) {
                        auto answer = results_of_run(req, session, run_id, level);
                        return http_response::json(answer_json(answer));
                    }

                    const auto earlier = run_of(req, session, baseline);
                    if (!earlier)
                        rr::invalid("The run compared with does not exist.");
                    if (earlier->definition_id != run->definition_id)
                        rr::invalid("The run compared with is a run of another report.");
                    // A run that has not finished has figures that are still
                    // moving, so it is not a baseline.
                    const auto state = state_name_of(req, session, earlier->id);
                    if (!state || *state != "completed")
                        rr::invalid("The run compared with has not completed.");
                    return http_response::json(
                        answer_json(compared_of_runs(req,
                                                     session,
                                                     run_id,
                                                     boost::uuids::string_generator()(baseline),
                                                     baseline,
                                                     level)));
                });
            }));
}

}
