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
#include "ores.http/routes/marketdata/public_rates_views.hpp"
#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.api/net/view_answer.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.logging/logger.hpp"
#include "ores.marketdata.api/messaging/feed_protocol.hpp"
#include "ores.marketdata.api/messaging/feed_scope_protocol.hpp"
#include "ores.marketdata.api/messaging/market_fixing_protocol.hpp"
#include "ores.marketdata.api/messaging/market_series_protocol.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.marketdata.api/messaging/provider_protocol.hpp"
#include "ores.marketdata.api/messaging/session_protocol.hpp"
#include "ores.marketdata.api/messaging/supplier_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include "ores.utility/codec/text_form.hpp"
#include <boost/asio/awaitable.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <chrono>
#include <cstddef>
#include <map>
#include <optional>
#include <set>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace ores::http::routes::marketdata {

namespace {

namespace json = ores::utility::codec::json;
namespace rr = ores::http::net::record_routes;
namespace forwarding = ores::http::forwarding;
namespace marketdata_domain = ores::marketdata::domain;
namespace marketdata_messaging = ores::marketdata::messaging;

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::nats::service::nats_client;

/// The sentence the file states when its query is not the one it takes.
constexpr std::string_view public_file_refused =
    "The fixings file names a publisher, and a range and item if it wants them.";

/// How long the public views stay fresh: a public publisher's fixings arrive daily.
constexpr std::chrono::seconds public_stale_after{5 * 60};

/// How many series one read of fixings names at a time.
constexpr std::size_t series_batch = 12;

auto& public_rates_lg() {
    static auto instance = ores::logging::make_logger("ores.http.marketdata.public_rates_views");
    return instance;
}

/// The pair an FX fixing item names.
struct fixing_pair final {
    std::string base;
    std::string quote;
};

/**
 * The pairs of the series a view names, each read once and kept for the request.
 *
 * The grammar of an oresmd URI belongs to the codec, so the gateway asks the
 * codec's own operation rather than parsing the text. The answer states the
 * kind, the instrument family, the subject and the fields, which is every part
 * the pair is made of: the subject is the base and the ccy field is the quote.
 *
 * A URI the codec refuses, or one whose kind or family is not an FX fixing,
 * names no pair, which is a fact about the item and not a failure of the view.
 */
class pair_reader final {
public:
    pair_reader(const http_request& req, nats_client& session)
        : req_(req)
        , session_(session) {}

    std::optional<fixing_pair> of(const std::string& uri) {
        if (const auto found = cache_.find(uri); found != cache_.end())
            return found->second;

        marketdata_messaging::parse_oresmd_uri_request msg;
        msg.uri = uri;
        const auto parsed = forwarding::forward(req_, session_, msg);
        std::optional<fixing_pair> pair;
        if (!parsed.success) {
            ORES_LOG_SEV(public_rates_lg(), ores::logging::debug)
                << "The codec refused the item " << uri << ": " << parsed.message;
        } else if (parsed.kind == "fixing" && parsed.instrument == "fx" &&
                   !parsed.subject.empty()) {
            for (const auto& field : parsed.fields) {
                if (field.name != "ccy")
                    continue;
                if (!field.text.empty())
                    pair = fixing_pair{parsed.subject, field.text};
                break;
            }
        }
        cache_.emplace(uri, pair);
        return pair;
    }

private:
    const http_request& req_;
    nats_client& session_;
    std::map<std::string, std::optional<fixing_pair>> cache_;
};

/// One fixing of the catalogue, as the screens read it.
struct fixing_row final {
    std::string date;
    std::string value;
};

/// The public publishers, their feeds and series, and the fixings behind them.
struct public_catalogue final {
    std::vector<marketdata_domain::provider> providers;
    std::vector<marketdata_domain::feed> feeds;
    std::vector<marketdata_domain::feed_scope> scopes;
    std::vector<marketdata_domain::session> sessions;
    std::vector<marketdata_domain::supplier> suppliers;
    std::vector<marketdata_domain::market_series> series;
    std::map<std::string, std::vector<fixing_row>> fixings;
};

/**
 * The catalogue the three public views read.
 *
 * A publisher is public when it states a terms page, because only such a
 * publisher is read through the official adapter. The series and the fixings
 * are then read for those publishers alone, so a private publisher's rows never
 * travel to a screen that cannot show them.
 */
public_catalogue read_public_catalogue(const http_request& req, nats_client& session) {
    public_catalogue out;
    const auto publishers =
        forwarding::forward_all(req,
                                session,
                                marketdata_messaging::list_providers_request{},
                                &marketdata_messaging::list_providers_response::providers);
    for (const auto& publisher : publishers)
        if (!publisher.terms_url.empty())
            out.providers.push_back(publisher);
    if (out.providers.empty())
        return out;

    std::vector<std::string> codes;
    codes.reserve(out.providers.size());
    for (const auto& publisher : out.providers)
        codes.push_back(publisher.code);

    marketdata_messaging::list_market_series_request series_read;
    series_read.filter.emplace();
    series_read.filter->provider_code_one_of = codes;
    out.series =
        forwarding::forward_all(req,
                                session,
                                series_read,
                                &marketdata_messaging::list_market_series_response::market_series);

    out.feeds = forwarding::forward_all(req,
                                        session,
                                        marketdata_messaging::list_feeds_request{},
                                        &marketdata_messaging::list_feeds_response::feeds);
    out.scopes = forwarding::forward_all(req,
                                         session,
                                         marketdata_messaging::list_feed_scopes_request{},
                                         &marketdata_messaging::list_feed_scopes_response::scopes);
    out.sessions = forwarding::forward_all(req,
                                           session,
                                           marketdata_messaging::list_sessions_request{},
                                           &marketdata_messaging::list_sessions_response::sessions);
    out.suppliers =
        forwarding::forward_all(req,
                                session,
                                marketdata_messaging::list_suppliers_request{},
                                &marketdata_messaging::list_suppliers_response::suppliers);

    std::vector<boost::uuids::uuid> ids;
    ids.reserve(out.series.size());
    for (const auto& series : out.series)
        ids.push_back(series.id);
    for (std::size_t at = 0; at < ids.size(); at += series_batch) {
        const auto end = std::min(ids.size(), at + series_batch);
        marketdata_messaging::list_market_fixings_request fixings_read;
        fixings_read.filter.emplace();
        fixings_read.filter->series_id_one_of =
            std::vector<boost::uuids::uuid>(ids.begin() + static_cast<std::ptrdiff_t>(at),
                                            ids.begin() + static_cast<std::ptrdiff_t>(end));
        auto rows = forwarding::forward_all(
            req,
            session,
            fixings_read,
            &marketdata_messaging::list_market_fixings_response::market_fixings);
        for (const auto& fixing : rows) {
            out.fixings[boost::uuids::to_string(fixing.series_id)].push_back(
                {ores::utility::codec::text_form<std::chrono::year_month_day>::to_text(
                     fixing.fixing_date),
                 fixing.value});
        }
    }
    for (auto& [id, rows] : out.fixings)
        std::ranges::sort(rows, {}, &fixing_row::date);
    return out;
}

/// The name a person reads for a publisher's file.
std::string
file_name(const std::string& publisher, const std::string& from, const std::string& to) {
    std::string suffix;
    if (!from.empty())
        suffix += "-" + from;
    if (!to.empty())
        suffix += "_" + to;
    return publisher + "-fixings" + suffix + ".csv";
}

/// A CSV cell, quoted when the value holds a comma, a quote or a line end.
std::string cell(const std::string& value) {
    if (value.find_first_of(",\"\n") == std::string::npos)
        return value;
    std::string quoted("\"");
    for (const char c : value) {
        if (c == '"')
            quoted += '"';
        quoted += c;
    }
    quoted += '"';
    return quoted;
}

}

http_response public_rates_views::publishers_answer(const http_request& req, std::string body) {
    return ores::http::net::view_answer(
        req, std::move(body), ores::http::net::view_cache_policy::fresh_for(public_stale_after));
}

http_response public_rates_views::coverage_answer(const http_request& req, std::string body) {
    return ores::http::net::view_answer(
        req, std::move(body), ores::http::net::view_cache_policy::fresh_for(public_stale_after));
}

void public_rates_views::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    using forwarding::guarded;
    const auto add = [&](ores::http::net::route_builder builder) {
        const auto built = builder.tags({"marketdata"}).auth_required().build();
        router->add_route(built);
        registry->register_route(built);
    };

    add(router->get("/api/marketdata/public-rates")
            .summary("Every public publisher, with its feeds and the series it fills")
            .description("A composed view owned by ores.marketdata: the publishers that state a "
                         "terms page, each with the feeds that carry it and the series it fills. "
                         "A feed states the supplier behind its session and the asset classes "
                         "its scopes name, and a series states the pair its item names. A series "
                         "whose item names no pair is left out, because a screen draws a pair. "
                         "The answer carries an ETag and stays fresh for 5 minutes.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<marketdata_messaging::list_providers_request>([&] {
                    rr::refuse_unknown_params(req, {});
                    auto catalogue = read_public_catalogue(req, session);
                    pair_reader pairs(req, session);

                    std::map<std::string, const marketdata_domain::session*> session_of;
                    for (const auto& one : catalogue.sessions)
                        session_of.emplace(one.code, &one);
                    std::map<std::string, const marketdata_domain::supplier*> supplier_of;
                    for (const auto& one : catalogue.suppliers)
                        supplier_of.emplace(one.code, &one);

                    std::ranges::sort(catalogue.providers, {}, &marketdata_domain::provider::name);
                    std::vector<std::string> written;
                    written.reserve(catalogue.providers.size());
                    for (const auto& publisher : catalogue.providers) {
                        std::vector<std::string> feed_rows;
                        for (const auto& feed : catalogue.feeds) {
                            if (feed.publisher_code != publisher.code)
                                continue;
                            const auto found = session_of.find(feed.session_code);
                            const auto* feed_session =
                                found == session_of.end() ? nullptr : found->second;
                            std::string supplier_code;
                            std::string supplier_name;
                            if (feed_session != nullptr) {
                                supplier_code = feed_session->supplier_code;
                                const auto supplier = supplier_of.find(supplier_code);
                                if (supplier != supplier_of.end())
                                    supplier_name = supplier->second->name;
                            }
                            std::set<std::string> classes;
                            for (const auto& scope : catalogue.scopes)
                                if (boost::uuids::to_string(scope.feed_id) ==
                                    boost::uuids::to_string(feed.id))
                                    classes.insert(scope.asset_class);
                            std::vector<std::string> class_json;
                            for (const auto& one : classes)
                                class_json.push_back(json::write(one));

                            feed_rows.push_back(
                                "{\"id\":" + json::write(boost::uuids::to_string(feed.id)) +
                                ",\"name\":" + json::write(feed.name) +
                                ",\"service\":" + json::write(feed.service) +
                                ",\"provider_key\":" + json::write(feed.provider_key) +
                                ",\"is_default\":" + json::write(feed.is_default) +
                                ",\"session_code\":" + json::write(feed.session_code) +
                                ",\"supplier_code\":" + json::write(supplier_code) +
                                ",\"supplier_name\":" + json::write(supplier_name) +
                                ",\"asset_classes\":" + rr::array_of(class_json) + "}");
                        }

                        std::vector<std::pair<std::string, std::string>> series_rows;
                        for (const auto& series : catalogue.series) {
                            if (series.provider_code != publisher.code)
                                continue;
                            const auto pair = pairs.of(series.oresmd_uri);
                            if (!pair)
                                continue;
                            series_rows.emplace_back(
                                pair->quote,
                                "{\"id\":" + json::write(boost::uuids::to_string(series.id)) +
                                    ",\"oresmd_uri\":" + json::write(series.oresmd_uri) +
                                    ",\"base\":" + json::write(pair->base) +
                                    ",\"quote\":" + json::write(pair->quote) + "}");
                        }
                        std::ranges::sort(
                            series_rows, {}, &std::pair<std::string, std::string>::first);
                        std::vector<std::string> series_json;
                        series_json.reserve(series_rows.size());
                        for (const auto& [quote, row] : series_rows)
                            series_json.push_back(row);

                        written.push_back(
                            "{\"code\":" + json::write(publisher.code) +
                            ",\"version\":" + std::to_string(publisher.version) +
                            ",\"name\":" + json::write(publisher.name) +
                            ",\"description\":" + json::write(publisher.description) +
                            ",\"kind\":" + json::write(publisher.kind) +
                            ",\"terms_url\":" + json::write(publisher.terms_url) +
                            ",\"terms_credit\":" + json::write(publisher.terms_credit) +
                            ",\"allows_storage\":" + json::write(publisher.allows_storage) +
                            ",\"terms_accepted\":" +
                            json::write(!publisher.terms_accepted_digest.empty()) +
                            ",\"terms_accepted_by\":" + json::write(publisher.terms_accepted_by) +
                            ",\"terms_accepted_at\":" + json::write(publisher.terms_accepted_at) +
                            ",\"feeds\":" + rr::array_of(feed_rows) +
                            ",\"series\":" + rr::array_of(series_json) + "}");
                    }

                    ORES_LOG_SEV(public_rates_lg(), ores::logging::debug)
                        << "The public catalogue holds " << written.size() << " publishers and "
                        << catalogue.series.size() << " series";
                    return publishers_answer(req, "{\"providers\":" + rr::array_of(written) + "}");
                });
            }));

    add(router->get("/api/marketdata/public-rates/coverage")
            .summary("The coverage of every series a public publisher fills")
            .description("A composed view owned by ores.marketdata: each public series' first "
                         "date, last date and count of values, which the list reads to say "
                         "whether a publisher is arriving and the availability tab to say how "
                         "far back. A series that holds no value is left out, because a series "
                         "with no coverage is not a coverage of zero. The answer carries an ETag "
                         "and stays fresh for 5 minutes.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<marketdata_messaging::list_providers_request>([&] {
                    rr::refuse_unknown_params(req, {});
                    const auto catalogue = read_public_catalogue(req, session);

                    std::vector<std::string> written;
                    for (const auto& [id, rows] : catalogue.fixings) {
                        if (rows.empty())
                            continue;
                        written.push_back(json::write(id) +
                                          ":{\"first\":" + json::write(rows.front().date) +
                                          ",\"last\":" + json::write(rows.back().date) +
                                          ",\"count\":" + std::to_string(rows.size()) + "}");
                    }

                    std::string series("{");
                    for (std::size_t i = 0; i < written.size(); ++i) {
                        if (i != 0)
                            series += ",";
                        series += written[i];
                    }
                    series += "}";
                    return coverage_answer(req, "{\"series\":" + series + "}");
                });
            }));

    add(router->get("/api/marketdata/public-rates/fixings.csv")
            .summary("One public publisher's historical fixings, as a file")
            .description("A composed view owned by ores.marketdata: the publisher's series, "
                         "filtered to the date range and the item when the query names them, "
                         "written as CSV with the value as the store holds it. The file states "
                         "the rows it carries so a screen can say how many they are, and it is "
                         "built on each request, so it is not kept.")
            .query_param("publisher", "string", "", true, "The public publisher's code")
            .query_param("from", "string", "", false, "The first date, as a date")
            .query_param("to", "string", "", false, "The last date, as a date")
            .query_param("item", "string", "", false, "The oresmd URI of one item")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<marketdata_messaging::list_providers_request>([&] {
                    rr::refuse_unknown_params(req, {"publisher", "from", "to", "item"});
                    const auto publisher = req.get_query_param("publisher");
                    const auto from = req.get_query_param("from");
                    const auto to = req.get_query_param("to");
                    const auto item = req.get_query_param("item");
                    if (publisher.empty())
                        rr::invalid(std::string(public_file_refused));

                    const auto catalogue = read_public_catalogue(req, session);
                    pair_reader pairs(req, session);
                    const auto found = std::ranges::find(
                        catalogue.providers, publisher, &marketdata_domain::provider::code);
                    if (found == catalogue.providers.end())
                        rr::invalid("No public publisher is named " + publisher + ".");

                    std::string csv("publisher,date,pair,item,value");
                    std::size_t rows = 0;
                    for (const auto& series : catalogue.series) {
                        if (series.provider_code != found->code)
                            continue;
                        if (!item.empty() && item != series.oresmd_uri)
                            continue;
                        const auto pair = pairs.of(series.oresmd_uri);
                        const auto named = pair ? pair->base + "/" + pair->quote : std::string{};
                        const auto held =
                            catalogue.fixings.find(boost::uuids::to_string(series.id));
                        if (held == catalogue.fixings.end())
                            continue;
                        for (const auto& fixing : held->second) {
                            if (!from.empty() && fixing.date < from)
                                continue;
                            if (!to.empty() && fixing.date > to)
                                continue;
                            csv += "\n" + cell(found->code) + "," + cell(fixing.date) + "," +
                                   cell(named) + "," + cell(series.oresmd_uri) + "," +
                                   cell(fixing.value);
                            ++rows;
                        }
                    }
                    csv += "\n";

                    http_response answer;
                    answer.content_type = "text/csv; charset=utf-8";
                    answer.body = std::move(csv);
                    answer.set_header("content-disposition",
                                      "attachment; filename=\"" + file_name(found->code, from, to) +
                                          "\"");
                    answer.set_header("x-fixing-rows", std::to_string(rows));
                    answer.set_header("cache-control", "private, no-store");
                    return answer;
                });
            }));
}

}
