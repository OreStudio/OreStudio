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
#include "ores.http/routes/reporting/pricing_engine_shapes.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.api/net/json_shape.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <string>
#include <vector>

namespace ores::http::routes::reporting::pricing_engine_shapes {

namespace {

namespace json = ores::utility::codec::json;
namespace rr = ores::http::net::record_routes;
using ores::http::net::shape::member;
using ores::http::net::shape::object;

/// One product of a document.
std::string product_json_of(const pricing_product_row& row) {
    return object({member("id", json::write(row.id)),
                   member("version", std::to_string(row.version)),
                   member("typeCode", json::write(row.type_code)),
                   member("model", json::write(row.model)),
                   member("engine", json::write(row.engine)),
                   member("position", std::to_string(row.position))});
}

/// One parameter of a document.
std::string parameter_json_of(const pricing_parameter_row& row) {
    return object(
        {member("id", json::write(row.id)),
         member("version", std::to_string(row.version)),
         member("productId", row.product_id ? json::write(*row.product_id) : std::string("null")),
         member("scope", json::write(row.scope)),
         member("name", json::write(row.name)),
         member("value", json::write(row.value)),
         member("position", std::to_string(row.position))});
}

}

std::vector<std::string> party_members(std::string_view party_id,
                                       const std::map<std::string, engine_party_name>& parties) {
    const auto party = parties.find(std::string(party_id));
    return {member("partyId", json::write(std::string(party_id))),
            member("partyName",
                   json::write(party == parties.end() ? std::string() : party->second.name)),
            member("partyCode",
                   json::write(party == parties.end() ? std::string() : party->second.short_code))};
}

std::string summary_json_of(const ores::analytics::domain::pricing_model_config& row,
                            const pricing_engine_rows::document_counts& counts,
                            const std::map<std::string, engine_party_name>& parties) {
    const auto id = boost::uuids::to_string(row.id);
    const auto products = counts.products.find(id);
    const auto parameters = counts.parameters.find(id);
    auto members = party_members(boost::uuids::to_string(row.party_id), parties);
    members.push_back(member("id", json::write(id)));
    members.push_back(member("version", std::to_string(row.version)));
    members.push_back(member("name", json::write(row.name)));
    members.push_back(member("description", json::write(row.description)));
    members.push_back(member("variant", json::write(row.config_variant)));
    members.push_back(
        member("configurationId", json::write(boost::uuids::to_string(row.configuration_id))));
    members.push_back(member(
        "products", std::to_string(products == counts.products.end() ? 0 : products->second)));
    members.push_back(
        member("parameters",
               std::to_string(parameters == counts.parameters.end() ? 0 : parameters->second)));
    members.push_back(member("modifiedBy", json::write(row.modified_by)));
    members.push_back(member("recordedAt", json::write(row.recorded_at)));
    return object(members);
}

std::string document_json_of(const pricing_document_row& document,
                             const std::map<std::string, engine_party_name>& parties,
                             const std::string& reports) {
    auto members = party_members(document.party_id, parties);
    members.push_back(member("id", json::write(document.id)));
    members.push_back(member("version", std::to_string(document.version)));
    members.push_back(member("name", json::write(document.name)));
    members.push_back(member("description", json::write(document.description)));
    members.push_back(member("variant", json::write(document.variant)));
    members.push_back(member("configurationId", json::write(document.configuration_id)));
    members.push_back(member("modifiedBy", json::write(document.modified_by)));
    members.push_back(member("recordedAt", json::write(document.recorded_at)));
    members.push_back(member("changeReason", json::write(document.change_reason)));
    members.push_back(member("changeCommentary", json::write(document.change_commentary)));
    std::vector<std::string> products;
    products.reserve(document.products.size());
    for (const auto& product : document.products)
        products.push_back(product_json_of(product));
    members.push_back(member("products", rr::array_of(products)));
    std::vector<std::string> parameters;
    parameters.reserve(document.parameters.size());
    for (const auto& parameter : document.parameters)
        parameters.push_back(parameter_json_of(parameter));
    members.push_back(member("parameters", rr::array_of(parameters)));
    members.push_back(member("reports", reports));
    return object(members);
}

}
