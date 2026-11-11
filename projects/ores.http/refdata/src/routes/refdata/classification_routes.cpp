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
#include "ores.http/routes/refdata/classification_routes.hpp"
#include "ores.dq.api/domain/badge_definition_codec.hpp"
#include "ores.dq.api/domain/badge_mapping_codec.hpp"
#include "ores.dq.api/messaging/badge_definition_protocol.hpp"
#include "ores.dq.api/messaging/badge_mapping_protocol.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.http/routes/refdata/asset_class_code_classification_routes.hpp"
#include "ores.http/routes/refdata/book_purpose_type_classification_routes.hpp"
#include "ores.http/routes/refdata/book_status_classification_routes.hpp"
#include "ores.http/routes/refdata/business_day_convention_type_classification_routes.hpp"
#include "ores.http/routes/refdata/calendar_name_classification_routes.hpp"
#include "ores.http/routes/refdata/calendar_type_classification_routes.hpp"
#include "ores.http/routes/refdata/contact_type_classification_routes.hpp"
#include "ores.http/routes/refdata/currency_market_tier_classification_routes.hpp"
#include "ores.http/routes/refdata/currency_pair_classification_classification_routes.hpp"
#include "ores.http/routes/refdata/curve_role_classification_routes.hpp"
#include "ores.http/routes/refdata/day_count_fraction_type_classification_routes.hpp"
#include "ores.http/routes/refdata/day_counter_classification_routes.hpp"
#include "ores.http/routes/refdata/derivation_kind_classification_routes.hpp"
#include "ores.http/routes/refdata/diary_entry_type_classification_routes.hpp"
#include "ores.http/routes/refdata/floating_index_type_classification_routes.hpp"
#include "ores.http/routes/refdata/ledger_feed_type_classification_routes.hpp"
#include "ores.http/routes/refdata/leg_type_classification_routes.hpp"
#include "ores.http/routes/refdata/monetary_nature_classification_routes.hpp"
#include "ores.http/routes/refdata/party_status_classification_routes.hpp"
#include "ores.http/routes/refdata/party_type_classification_routes.hpp"
#include "ores.http/routes/refdata/purpose_type_classification_routes.hpp"
#include "ores.http/routes/refdata/regulatory_book_type_classification_routes.hpp"
#include "ores.http/routes/refdata/rounding_type_classification_routes.hpp"
#include "ores.http/routes/refdata/series_subclass_code_classification_routes.hpp"
#include "ores.http/routes/refdata/tenor_anchor_classification_routes.hpp"
#include "ores.http/routes/refdata/tenor_kind_classification_routes.hpp"
#include "ores.http/routes/refdata/tenor_resolution_algorithm_classification_routes.hpp"
#include "ores.http/routes/refdata/tenor_unit_classification_routes.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <boost/asio/awaitable.hpp>
#include <algorithm>
#include <cstdint>
#include <exception>
#include <vector>

namespace ores::http::routes::refdata {

using namespace ores::logging;
using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::nats::service::nats_client;
namespace dq = ores::dq::messaging;
namespace rr = ores::http::net::record_routes;
namespace json = ores::utility::codec::json;

namespace {

auto& lg() {
    static auto instance = make_logger("ores.http.routes.refdata.classification_routes");
    return instance;
}

/// The label catalogue holds fewer mappings than this, so one page reads them all.
constexpr std::uint32_t mapping_page = 2000;

/// One list as the catalogue lists it: its entry, and how its rows are counted.
struct catalogue_list final {
    std::string (*entry)(std::optional<std::uint64_t>);
    std::uint64_t (*count)(const http_request&, nats_client&);
};

/// The lists in the order the screen offers them.
const std::vector<catalogue_list>& catalogue_lists() {
    static const std::vector<catalogue_list> lists{
        {&monetary_nature_classification_routes::catalogue_entry,
         &monetary_nature_classification_routes::count},
        {&rounding_type_classification_routes::catalogue_entry,
         &rounding_type_classification_routes::count},
        {&currency_market_tier_classification_routes::catalogue_entry,
         &currency_market_tier_classification_routes::count},
        {&currency_pair_classification_classification_routes::catalogue_entry,
         &currency_pair_classification_classification_routes::count},
        {&calendar_type_classification_routes::catalogue_entry,
         &calendar_type_classification_routes::count},
        {&diary_entry_type_classification_routes::catalogue_entry,
         &diary_entry_type_classification_routes::count},
        {&calendar_name_classification_routes::catalogue_entry,
         &calendar_name_classification_routes::count},
        {&business_day_convention_type_classification_routes::catalogue_entry,
         &business_day_convention_type_classification_routes::count},
        {&party_type_classification_routes::catalogue_entry,
         &party_type_classification_routes::count},
        {&party_status_classification_routes::catalogue_entry,
         &party_status_classification_routes::count},
        {&contact_type_classification_routes::catalogue_entry,
         &contact_type_classification_routes::count},
        {&book_status_classification_routes::catalogue_entry,
         &book_status_classification_routes::count},
        {&book_purpose_type_classification_routes::catalogue_entry,
         &book_purpose_type_classification_routes::count},
        {&ledger_feed_type_classification_routes::catalogue_entry,
         &ledger_feed_type_classification_routes::count},
        {&regulatory_book_type_classification_routes::catalogue_entry,
         &regulatory_book_type_classification_routes::count},
        {&purpose_type_classification_routes::catalogue_entry,
         &purpose_type_classification_routes::count},
        {&asset_class_code_classification_routes::catalogue_entry,
         &asset_class_code_classification_routes::count},
        {&leg_type_classification_routes::catalogue_entry, &leg_type_classification_routes::count},
        {&floating_index_type_classification_routes::catalogue_entry,
         &floating_index_type_classification_routes::count},
        {&day_counter_classification_routes::catalogue_entry,
         &day_counter_classification_routes::count},
        {&day_count_fraction_type_classification_routes::catalogue_entry,
         &day_count_fraction_type_classification_routes::count},
        {&curve_role_classification_routes::catalogue_entry,
         &curve_role_classification_routes::count},
        {&tenor_kind_classification_routes::catalogue_entry,
         &tenor_kind_classification_routes::count},
        {&tenor_unit_classification_routes::catalogue_entry,
         &tenor_unit_classification_routes::count},
        {&tenor_anchor_classification_routes::catalogue_entry,
         &tenor_anchor_classification_routes::count},
        {&tenor_resolution_algorithm_classification_routes::catalogue_entry,
         &tenor_resolution_algorithm_classification_routes::count},
        {&derivation_kind_classification_routes::catalogue_entry,
         &derivation_kind_classification_routes::count},
        {&series_subclass_code_classification_routes::catalogue_entry,
         &series_subclass_code_classification_routes::count},
    };
    return lists;
}

/**
 * @brief The lists the screen offers, each with how many rows it holds.
 *
 * The counts are read one list at a time, as the BFF read them, and a count
 * that cannot be read is null rather than a failed page.
 */
std::string list_catalogue_body(const http_request& req, nats_client& session) {
    std::string out = "{\"lists\":[";
    bool first = true;
    for (const auto& list : catalogue_lists()) {
        std::optional<std::uint64_t> count;
        try {
            count = list.count(req, session);
        } catch (const rr::refusal& refusal) {
            BOOST_LOG_SEV(lg(), warn) << "A list could not be counted: " << refusal.message;
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), warn) << "A list could not be counted: " << e.what();
        }
        if (!first)
            out += ",";
        first = false;
        out += list.entry(count);
    }
    return out + "]}";
}

/// Every badge mapping, of every code domain.
std::vector<ores::dq::domain::badge_mapping> all_mappings(const http_request& req,
                                                          nats_client& session) {
    dq::list_badge_mappings_request msg;
    msg.offset = 0;
    msg.limit = mapping_page;
    msg.order.field = "";
    msg.order.descending = false;
    auto reply = ores::http::forwarding::forward(req, session, msg);
    rr::require_ok(reply.result);
    return std::move(reply.badge_mappings);
}

/**
 * @brief The label catalogue: every label, the labels each code domain uses,
 * and the badge each code of each domain is painted with.
 */
std::string labels_body(const http_request& req, nats_client& session) {
    dq::list_badge_definitions_request definitions_msg;
    definitions_msg.offset = 0;
    definitions_msg.limit = 200;
    definitions_msg.order.field = "";
    definitions_msg.order.descending = false;
    const auto definitions = ores::http::forwarding::forward(req, session, definitions_msg);
    rr::require_ok(definitions.result);
    const auto mappings = all_mappings(req, session);

    std::string out = "{\"labels\":[";
    for (std::size_t i = 0; i < definitions.definitions.size(); ++i) {
        const auto& d = definitions.definitions[i];
        if (i != 0)
            out += ",";
        out += "{\"code\":" + json::write(d.code) + ",\"label\":" + json::write(d.name) +
               ",\"description\":" + json::write(d.description) +
               ",\"backgroundColour\":" + json::write(d.background_colour) +
               ",\"textColour\":" + json::write(d.text_colour) +
               ",\"severity\":" + json::write(d.severity_code) + "}";
    }

    std::vector<std::string> domain_order;
    std::map<std::string, std::vector<std::string>> domains;
    std::map<std::string, std::vector<std::pair<std::string, std::string>>> mapped;
    for (const auto& m : mappings) {
        if (!domains.contains(m.code_domain_code))
            domain_order.push_back(m.code_domain_code);
        auto& codes = domains[m.code_domain_code];
        if (std::find(codes.begin(), codes.end(), m.badge_code) == codes.end())
            codes.push_back(m.badge_code);
        mapped[m.code_domain_code].emplace_back(m.entity_code, m.badge_code);
    }

    out += "],\"domains\":{";
    for (std::size_t i = 0; i < domain_order.size(); ++i) {
        if (i != 0)
            out += ",";
        out += json::write(domain_order[i]) + ":[";
        const auto& codes = domains[domain_order[i]];
        for (std::size_t j = 0; j < codes.size(); ++j)
            out += (j != 0 ? "," : "") + json::write(codes[j]);
        out += "]";
    }
    out += "},\"mapped\":{";
    for (std::size_t i = 0; i < domain_order.size(); ++i) {
        if (i != 0)
            out += ",";
        out += json::write(domain_order[i]) + ":{";
        const auto& pairs = mapped[domain_order[i]];
        for (std::size_t j = 0; j < pairs.size(); ++j)
            out += (j != 0 ? "," : "") + json::write(pairs[j].first) + ":" +
                   json::write(pairs[j].second);
        out += "}";
    }
    return out + "}}";
}

}

std::map<std::string, std::string> classification_routes::labels_of(const http_request& req,
                                                                    nats_client& session,
                                                                    std::string_view code_domain) {
    dq::list_by_code_domain_code_badge_mappings_request msg;
    msg.code_domain_code = std::string(code_domain);
    msg.scope = ores::utility::domain::scope::direct;
    msg.offset = 0;
    msg.limit = mapping_page;
    msg.order.field = "";
    msg.order.descending = false;
    const auto reply = ores::http::forwarding::forward(req, session, msg);
    rr::require_ok(reply.result);
    std::map<std::string, std::string> labels;
    for (const auto& mapping : reply.badge_mappings)
        labels[mapping.entity_code] = mapping.badge_code;
    return labels;
}

void classification_routes::set_label(const http_request& req,
                                      nats_client& session,
                                      std::string_view code_domain,
                                      const std::string& code,
                                      const std::optional<std::string>& badge_code,
                                      const ores::utility::domain::change_intent& intent) {
    if (!badge_code) {
        dq::delete_badge_mapping_request msg;
        msg.removal.key.code_domain_code = std::string(code_domain);
        msg.removal.key.entity_code = code;
        msg.removal.precondition.kind = ores::utility::domain::precondition_kind::any;
        msg.intent = intent;
        const auto reply = ores::http::forwarding::forward(req, session, msg);
        rr::require_ok(reply.result);
        return;
    }
    dq::put_badge_mapping_request msg;
    msg.change.write.code_domain_code = std::string(code_domain);
    msg.change.write.entity_code = code;
    msg.change.write.badge_code = rr::trimmed(*badge_code);
    msg.change.precondition.kind = ores::utility::domain::precondition_kind::any;
    msg.intent = intent;
    const auto reply = ores::http::forwarding::forward(req, session, msg);
    rr::require_ok(reply.result);
}

void classification_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    monetary_nature_classification_routes::register_routes(router, registry, session);
    rounding_type_classification_routes::register_routes(router, registry, session);
    currency_market_tier_classification_routes::register_routes(router, registry, session);
    currency_pair_classification_classification_routes::register_routes(router, registry, session);
    calendar_type_classification_routes::register_routes(router, registry, session);
    diary_entry_type_classification_routes::register_routes(router, registry, session);
    calendar_name_classification_routes::register_routes(router, registry, session);
    business_day_convention_type_classification_routes::register_routes(router, registry, session);
    party_type_classification_routes::register_routes(router, registry, session);
    party_status_classification_routes::register_routes(router, registry, session);
    contact_type_classification_routes::register_routes(router, registry, session);
    book_status_classification_routes::register_routes(router, registry, session);
    book_purpose_type_classification_routes::register_routes(router, registry, session);
    ledger_feed_type_classification_routes::register_routes(router, registry, session);
    regulatory_book_type_classification_routes::register_routes(router, registry, session);
    purpose_type_classification_routes::register_routes(router, registry, session);
    asset_class_code_classification_routes::register_routes(router, registry, session);
    leg_type_classification_routes::register_routes(router, registry, session);
    floating_index_type_classification_routes::register_routes(router, registry, session);
    day_counter_classification_routes::register_routes(router, registry, session);
    day_count_fraction_type_classification_routes::register_routes(router, registry, session);
    curve_role_classification_routes::register_routes(router, registry, session);
    tenor_kind_classification_routes::register_routes(router, registry, session);
    tenor_unit_classification_routes::register_routes(router, registry, session);
    tenor_anchor_classification_routes::register_routes(router, registry, session);
    tenor_resolution_algorithm_classification_routes::register_routes(router, registry, session);
    derivation_kind_classification_routes::register_routes(router, registry, session);
    series_subclass_code_classification_routes::register_routes(router, registry, session);

    auto catalogue_route =
        router->get("/api/classifications")
            .summary("List the classification lists the screen offers")
            .description("Each list's key, topic, shape, whether it is editable, its "
                         "permissions and how many rows it holds.")
            .tags({"refdata"})
            .auth_required()
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                try {
                    co_return http_response::json(list_catalogue_body(req, session));
                } catch (const std::exception& e) {
                    co_return ores::http::forwarding::unanswered("the classification catalogue", e);
                }
            });
    const auto catalogue_built = catalogue_route.build();
    router->add_route(catalogue_built);
    registry->register_route(catalogue_built);

    auto labels_route =
        router->get("/api/labels")
            .summary("Read the label catalogue")
            .description("Every label, the labels each code domain uses, and the badge each "
                         "code of each domain is painted with.")
            .tags({"refdata"})
            .auth_required()
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                try {
                    co_return http_response::json(labels_body(req, session));
                } catch (const rr::refusal& refusal) {
                    co_return rr::answer(refusal);
                } catch (const std::exception& e) {
                    co_return ores::http::forwarding::unanswered("the label catalogue", e);
                }
            });
    const auto labels_built = labels_route.build();
    router->add_route(labels_built);
    registry->register_route(labels_built);
}

}
