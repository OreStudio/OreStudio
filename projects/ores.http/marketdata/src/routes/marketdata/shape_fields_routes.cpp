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
#include "ores.http/routes/marketdata/shape_fields_routes.hpp"
#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.api/net/view_answer.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.logging/logger.hpp"
#include "ores.marketdata.api/domain/series_classification_rule.hpp"
#include "ores.marketdata.api/domain/series_identity_shape.hpp"
#include "ores.marketdata.api/messaging/series_classification_rule_protocol.hpp"
#include "ores.marketdata.api/messaging/series_identity_shape_entry_protocol.hpp"
#include "ores.marketdata.api/messaging/series_identity_shape_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <boost/asio/awaitable.hpp>
#include <algorithm>
#include <cstddef>
#include <map>
#include <string>
#include <tuple>
#include <utility>
#include <vector>

namespace ores::http::routes::marketdata {

namespace {

namespace rr = ores::http::net::record_routes;
namespace forwarding = ores::http::forwarding;
namespace marketdata_domain = ores::marketdata::domain;
namespace marketdata_messaging = ores::marketdata::messaging;
namespace json = ores::utility::codec::json;

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::nats::service::nats_client;

auto& shape_fields_lg() {
    static auto instance = ores::logging::make_logger("ores.http.marketdata.shape_fields_routes");
    return instance;
}

/// One rule of the catalogue, as the editor reads it.
std::string shape_fields_rule_row(const marketdata_domain::series_classification_rule& rule) {
    return "{\"instrument\":" + json::write(rule.instrument) +
           ",\"quote\":" + json::write(rule.quote) + ",\"key_kind\":" + json::write(rule.key_kind) +
           ",\"series_subclass_code\":" + json::write(rule.series_subclass_code) + "}";
}

/// One field of one family, the family's own parts beside the field.
struct shape_fields_field final {
    std::string asset_class;
    std::string series_subclass;
    std::string index_family;
    std::string field;
    std::string requirement;
    std::string description;
};

std::string shape_fields_field_row(const shape_fields_field& row) {
    return "{\"asset_class\":" + json::write(row.asset_class) +
           ",\"series_subclass\":" + json::write(row.series_subclass) +
           ",\"index_family\":" + json::write(row.index_family) +
           ",\"field\":" + json::write(row.field) +
           ",\"requirement\":" + json::write(row.requirement) +
           ",\"description\":" + json::write(row.description) + "}";
}

/// The family of each shape, read by the code its entries name.
std::map<std::string, const marketdata_domain::series_identity_shape*>
shape_fields_families(const std::vector<marketdata_domain::series_identity_shape>& shapes) {
    std::map<std::string, const marketdata_domain::series_identity_shape*> families;
    for (const auto& shape : shapes)
        families.emplace(shape.code, &shape);
    return families;
}

}

http_response shape_fields_routes::fields_answer(const http_request& req, std::string body) {
    return ores::http::net::view_answer(
        req, std::move(body), ores::http::net::view_cache_policy::revalidated());
}

http_response shape_fields_routes::rules_answer(const http_request& req, std::string body) {
    return ores::http::net::view_answer(
        req, std::move(body), ores::http::net::view_cache_policy::revalidated());
}

void shape_fields_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    using forwarding::guarded;
    const auto rules =
        router->get("/api/marketdata/series-classification-rules").catalogue("web_get_marketdata_series_classification_rules", "market-data", "series_classification_rules", false)
            .summary("The rules that give a series its subclass")
            .description("A composed view owned by ores.marketdata: every classification rule "
                         "the catalogue holds, ordered by the kind of identity it is keyed by, "
                         "then by its instrument and its quote. One read, and the gateway sorts "
                         "it. A rule states the instrument and the quote of the URI it "
                         "classifies, the kind of identity it is keyed by, and the series "
                         "subclass it gives. The answer carries an ETag and is asked about at "
                         "each read.")
            .tags({"marketdata"})
            .auth_required()
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<
                    marketdata_messaging::list_series_classification_rules_request>([&] {
                    rr::refuse_unknown_params(req, {});

                    auto held = forwarding::forward_all(
                        req,
                        session,
                        marketdata_messaging::list_series_classification_rules_request{},
                        &marketdata_messaging::list_series_classification_rules_response::rules);
                    std::ranges::sort(held, [](const auto& left, const auto& right) {
                        if (left.key_kind != right.key_kind)
                            return left.key_kind < right.key_kind;
                        if (left.instrument != right.instrument)
                            return left.instrument < right.instrument;
                        return left.quote < right.quote;
                    });

                    std::vector<std::string> rows;
                    rows.reserve(held.size());
                    for (const auto& rule : held)
                        rows.push_back(shape_fields_rule_row(rule));

                    ORES_LOG_SEV(shape_fields_lg(), ores::logging::debug)
                        << "The classification catalogue holds " << rows.size() << " rules";
                    return rules_answer(req, "{\"rules\":" + rr::array_of(rows) + "}");
                });
            })
            .build();
    router->add_route(rules);
    registry->register_route(rules);

    const auto fields =
        router->get("/api/marketdata/series-identity-shape-fields").catalogue("web_get_marketdata_series_identity_shape_fields", "market-data", "series_identity_shape_fields", false)
            .summary("Every field a family requires or forbids")
            .description("A composed view owned by ores.marketdata: the identity shapes and "
                         "their entries read together, one row per field with the family's "
                         "asset class, series subclass and index family beside it. Two reads, "
                         "and the gateway joins them on the shape's code and orders the rows by "
                         "family, then by field. An entry whose shape is not in the catalogue "
                         "names no family, so it is left out and the count of those is logged. "
                         "The answer carries an ETag and is asked about at each read, because "
                         "the caller's own writes change it.")
            .tags({"marketdata"})
            .auth_required()
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<marketdata_messaging::list_series_identity_shapes_request>([&] {
                    rr::refuse_unknown_params(req, {});

                    const auto shapes = forwarding::forward_all(
                        req,
                        session,
                        marketdata_messaging::list_series_identity_shapes_request{},
                        &marketdata_messaging::list_series_identity_shapes_response::shapes);
                    const auto entries = forwarding::forward_all(
                        req,
                        session,
                        marketdata_messaging::list_series_identity_shape_entries_request{},
                        &marketdata_messaging::list_series_identity_shape_entries_response::
                            entries);

                    const auto families = shape_fields_families(shapes);
                    std::vector<shape_fields_field> rows;
                    rows.reserve(entries.size());
                    std::size_t orphaned = 0;
                    for (const auto& entry : entries) {
                        const auto found = families.find(entry.shape_code);
                        if (found == families.end()) {
                            ++orphaned;
                            continue;
                        }
                        const auto& family = *found->second;
                        rows.push_back(shape_fields_field{family.asset_class,
                                                          family.series_subclass,
                                                          family.index_family,
                                                          entry.field,
                                                          entry.requirement,
                                                          entry.description});
                    }
                    std::ranges::sort(rows, [](const auto& left, const auto& right) {
                        return std::tie(left.asset_class,
                                        left.series_subclass,
                                        left.index_family,
                                        left.field) < std::tie(right.asset_class,
                                                               right.series_subclass,
                                                               right.index_family,
                                                               right.field);
                    });

                    if (orphaned != 0)
                        ORES_LOG_SEV(shape_fields_lg(), ores::logging::warn)
                            << "The shapes catalogue names no family for " << orphaned
                            << " entries, so they are left out";

                    std::vector<std::string> written;
                    written.reserve(rows.size());
                    for (const auto& row : rows)
                        written.push_back(shape_fields_field_row(row));

                    ORES_LOG_SEV(shape_fields_lg(), ores::logging::debug)
                        << "The shapes catalogue states " << written.size() << " fields";
                    return fields_answer(req, "{\"fields\":" + rr::array_of(written) + "}");
                });
            })
            .build();
    router->add_route(fields);
    registry->register_route(fields);
}

}
