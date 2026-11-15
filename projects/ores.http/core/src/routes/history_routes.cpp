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
    "ores.synthetic.fx_vol_contributor",
    "ores.synthetic.fx_vol_surface_feed",
    "ores.synthetic.fx_vol_surface_feed_contributor",
    "ores.synthetic.fx_vol_surface_feed_expiry",
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

std::vector<version_row>
versions_of(const ores::history::messaging::get_entity_history_response& r) {
    std::vector<version_row> versions;
    versions.reserve(r.versions.size());
    for (const auto& v : r.versions) {
        version_row row;
        row.version = v.version;
        row.modified_by = v.modified_by;
        row.recorded_at = v.recorded_at;
        for (const auto& field : v.fields)
            row.fields.push_back({field.name, field.value});
        for (const auto& entry : v.changes.entries)
            row.changes.push_back({entry.field_name, entry.old_value, entry.new_value});
        versions.push_back(std::move(row));
    }
    return versions;
}

}

std::span<const std::string_view> history_routes::served_types() {
    return history_types;
}

bool history_routes::serves(std::string_view entity_type) {
    return std::ranges::binary_search(history_types, entity_type);
}

std::vector<version_row> history_routes::read_versions(const http_request& req,
                                                       nats_client& session,
                                                       std::string_view entity_type,
                                                       std::string_view entity_id) {
    ores::history::messaging::get_entity_history_request msg;
    msg.entity_type = std::string(entity_type);
    msg.entity_id = std::string(entity_id);
    const auto subject = ores::history::messaging::history_subject_for(msg.entity_type);
    auto reply = ores::http::forwarding::forward_on(req, session, subject, msg);
    // The BFF answered a history read the service refused as a conflict of the
    // request, with the service's own words.
    if (!reply.success)
        rr::refuse(http_status::conflict, "invalid-request", reply.message);

    auto versions = versions_of(reply);
    std::ranges::sort(versions, std::ranges::greater{}, &version_row::version);
    return versions;
}

std::vector<std::string> history_routes::version_members(const version_row& row) {
    std::vector<std::string> members;
    members.push_back("\"version\":" + std::to_string(row.version));
    members.push_back("\"modifiedBy\":" + json::write(row.modified_by));
    members.push_back("\"recordedAt\":" + json::write(row.recorded_at));
    std::vector<std::string> fields;
    fields.reserve(row.fields.size());
    for (const auto& field : row.fields)
        fields.push_back("{\"name\":" + json::write(field.name) +
                         ",\"value\":" + json::write(field.value) + "}");
    members.push_back("\"fields\":" + rr::array_of(fields));
    std::vector<std::string> changes;
    changes.reserve(row.changes.size());
    for (const auto& change : row.changes)
        changes.push_back("{\"field\":" + json::write(change.field) +
                          ",\"before\":" + json::write(change.before) +
                          ",\"after\":" + json::write(change.after) + "}");
    members.push_back("\"changes\":" + rr::array_of(changes));
    return members;
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

        const auto versions = read_versions(req, session, entity_type, entity_id);
        std::vector<std::string> rows;
        rows.reserve(versions.size());
        for (const auto& version : versions)
            rows.push_back(rr::object_of(version_members(version)));
        co_return http_response::json("{\"versions\":" + rr::array_of(rows) + "}");
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
