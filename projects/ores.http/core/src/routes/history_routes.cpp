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
#include "ores.http.core/routes/history_routes.hpp"
#include "ores.history.api/messaging/history_protocol.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <algorithm>
#include <exception>
#include <span>
#include <string>
#include <string_view>

namespace ores::http::routes {

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::nats::service::nats_client;
namespace rr = ores::http::net::record_routes;
namespace json = ores::utility::codec::json;

namespace {

/**
 * @brief The entity types a screen reads history for, sorted.
 *
 * It is the BFF's set: the classification lists, the versioned reference data
 * records, the synthetic resources, and the records each screen's history tab
 * reads by name.
 */
constexpr std::string_view history_types[]{
    "ores.analytics.pricing_model_config",
    "ores.analytics.sensitivity_template",
    "ores.iam.tenant",
    "ores.marketdata.curve_publish",
    "ores.refdata.asset_class_code",
    "ores.refdata.book",
    "ores.refdata.book_purpose_type",
    "ores.refdata.book_status",
    "ores.refdata.business_centre",
    "ores.refdata.business_day_convention_type",
    "ores.refdata.business_unit",
    "ores.refdata.business_unit_type",
    "ores.refdata.calendar",
    "ores.refdata.calendar_event",
    "ores.refdata.calendar_exception",
    "ores.refdata.calendar_name",
    "ores.refdata.calendar_rule",
    "ores.refdata.calendar_type",
    "ores.refdata.contact_type",
    "ores.refdata.convention",
    "ores.refdata.counterparty",
    "ores.refdata.counterparty_contact_information",
    "ores.refdata.counterparty_identifier",
    "ores.refdata.country",
    "ores.refdata.currency",
    "ores.refdata.currency_group",
    "ores.refdata.currency_market_tier",
    "ores.refdata.currency_pair",
    "ores.refdata.currency_pair_classification",
    "ores.refdata.currency_pair_convention",
    "ores.refdata.curve_configuration",
    "ores.refdata.curve_role",
    "ores.refdata.day_count_fraction_type",
    "ores.refdata.day_counter",
    "ores.refdata.deposit_convention",
    "ores.refdata.derivation_kind",
    "ores.refdata.diary_entry_type",
    "ores.refdata.floating_index_type",
    "ores.refdata.instrument_code",
    "ores.refdata.ir_curve_bootstrap_config",
    "ores.refdata.ir_curve_bootstrap_pillar",
    "ores.refdata.ledger_feed_type",
    "ores.refdata.leg_type",
    "ores.refdata.monetary_nature",
    "ores.refdata.party",
    "ores.refdata.party_contact_information",
    "ores.refdata.party_id_scheme",
    "ores.refdata.party_identifier",
    "ores.refdata.party_status",
    "ores.refdata.party_type",
    "ores.refdata.payment_frequency",
    "ores.refdata.portfolio",
    "ores.refdata.purpose_type",
    "ores.refdata.regulatory_book_type",
    "ores.refdata.rounding_type",
    "ores.refdata.series_subclass_code",
    "ores.refdata.swap_convention",
    "ores.refdata.tenor",
    "ores.refdata.tenor_anchor",
    "ores.refdata.tenor_convention",
    "ores.refdata.tenor_kind",
    "ores.refdata.tenor_resolution_algorithm",
    "ores.refdata.tenor_unit",
    "ores.reporting.cut",
    "ores.reporting.cut_kind",
    "ores.reporting.report_definition",
    "ores.synthetic.folder",
    "ores.synthetic.fx_spot_generation_config",
    "ores.synthetic.gmm_component",
    "ores.synthetic.ir_curve_generation_config",
    "ores.synthetic.ir_curve_generation_config_process_parameter_value",
    "ores.synthetic.ir_curve_template_entry",
    "ores.synthetic.market_data_generation_config",
    "ores.synthetic.market_data_provider",
    "ores.synthetic.variance_calibration",
    "ores.synthetic.variance_calibration_input",
    "ores.synthetic.variance_calibration_input_point",
    "ores.synthetic.yield_curve_process_parameter_definition",
    "ores.synthetic.yield_curve_process_type",
    "ores.trading.structure",
    "ores.trading.structure_member",
    "ores.trading.trade",
    "ores.trading.trade_additional_field",
    "ores.trading.trade_booking",
    "ores.trading.trade_portfolio",
    "ores.trading.trade_state",
};

static_assert(std::ranges::is_sorted(history_types));

/// A name the address states: 1 to 200 characters.
bool fits(const std::string& value) {
    return !value.empty() && value.size() <= 200;
}

std::string version_json(const ores::history::messaging::entity_history_version& v) {
    std::string out = "{\"version\":" + std::to_string(v.version) +
                      ",\"modifiedBy\":" + json::write(v.modified_by) +
                      ",\"recordedAt\":" + json::write(v.recorded_at) + ",\"fields\":[";
    for (std::size_t i = 0; i < v.fields.size(); ++i)
        out += (i != 0 ? "," : "") + std::string("{\"name\":") + json::write(v.fields[i].name) +
               ",\"value\":" + json::write(v.fields[i].value) + "}";
    out += "],\"changes\":[";
    const auto& entries = v.changes.entries;
    for (std::size_t i = 0; i < entries.size(); ++i)
        out += (i != 0 ? "," : "") + std::string("{\"field\":") +
               json::write(entries[i].field_name) +
               ",\"before\":" + json::write(entries[i].old_value) +
               ",\"after\":" + json::write(entries[i].new_value) + "}";
    return out + "]}";
}

}

std::span<const std::string_view> history_routes::served_types() {
    return history_types;
}

bool history_routes::serves(std::string_view entity_type) {
    return std::ranges::binary_search(history_types, entity_type);
}

boost::asio::awaitable<http_response> history_routes::handle_history(const http_request& req,
                                                                     nats_client& session) {
    try {
        const auto entity_type = req.get_query_param("entityType");
        const auto entity_id = req.get_query_param("entityId");
        if (!fits(entity_type) || !fits(entity_id))
            rr::invalid("Name the record by its entity type and its key.");
        if (!serves(entity_type))
            rr::refuse(http_status::not_found,
                       "not-found",
                       "No history is served for " + entity_type + ".");

        ores::history::messaging::get_entity_history_request msg;
        msg.entity_type = entity_type;
        msg.entity_id = entity_id;
        const auto subject = ores::history::messaging::history_subject_for(entity_type);
        auto reply = ores::http::forwarding::forward_on(req, session, subject, msg);
        // The BFF answered a history read the service refused as a conflict of the
        // request, with the service's own words.
        if (!reply.success)
            rr::refuse(http_status::conflict, "invalid-request", reply.message);

        std::ranges::sort(reply.versions,
                          std::ranges::greater{},
                          &ores::history::messaging::entity_history_version::version);
        std::string out = "{\"versions\":[";
        for (std::size_t i = 0; i < reply.versions.size(); ++i)
            out += (i != 0 ? "," : "") + version_json(reply.versions[i]);
        co_return http_response::json(out + "]}");
    } catch (const rr::refusal& refusal) {
        co_return rr::answer(refusal);
    } catch (const std::exception& e) {
        co_return ores::http::forwarding::unanswered("the entity history", e);
    }
}

void history_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    auto history_route =
        router->get("/api/history")
            .summary("Read the versions of one record")
            .description("Every version, newest first, with its fields and what changed in it.")
            .tags({"history"})
            .auth_required()
            .query_param("entityType", "string", "", true, "The record's entity type")
            .query_param("entityId", "string", "", true, "The record's key")
            .handler([&session](const http_request& req) { return handle_history(req, session); });
    const auto history_built = history_route.build();
    router->add_route(history_built);
    registry->register_route(history_built);
}

}
