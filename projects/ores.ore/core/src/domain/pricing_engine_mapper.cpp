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
#include "ores.ore.core/domain/pricing_engine_mapper.hpp"
#include "ores.utility/uuid/uuid_v7_generator.hpp"
#include <algorithm>
#include <boost/uuid/uuid.hpp>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace ores::ore::domain {

namespace {

constexpr std::string_view audit_modified_by = "ores";
constexpr std::string_view audit_reason_code = "system.external_data_import";
constexpr std::string_view audit_commentary = "Imported from ORE XML";

// The three strings the parameter_scope column holds. A parameter is scoped by
// the element that carried it, because that is the only thing that says which
// table its value belongs to.
constexpr std::string_view scope_model = "model";
constexpr std::string_view scope_engine = "engine";
constexpr std::string_view scope_global = "global";

boost::uuids::uuid new_uuid() {
    static thread_local ores::utility::uuid::uuid_v7_generator generator;
    return generator();
}

template <typename T>
void set_audit(T& r) {
    r.modified_by = std::string(audit_modified_by);
    r.performed_by = std::string(audit_modified_by);
    r.change_reason_code = std::string(audit_reason_code);
    r.change_commentary = std::string(audit_commentary);
}

// Every generated text element is a distinct struct derived from xsd::string,
// so the base subobject is the only assignment target a std::string converts
// to.
template <typename T>
void assign_text(T& target, const std::string& value) {
    static_cast<xsd::string&>(target) = value;
}

// The document writes the two parameter lists the same way, and the only thing
// that differs is the scope it records, so one helper covers both.
void append_parameters(std::vector<analytics::domain::pricing_model_product_parameter>& out,
                       const boost::uuids::uuid& config_id,
                       const std::optional<boost::uuids::uuid>& product_id,
                       const std::string_view scope,
                       const xsd::vector<domain::parameter>& parameters) {
    int position = 0;
    for (const auto& parameter : parameters) {
        analytics::domain::pricing_model_product_parameter row;
        row.id = new_uuid();
        row.pricing_model_config_id = config_id;
        row.pricing_model_product_id = product_id;
        row.parameter_scope = std::string(scope);
        row.parameter_name = parameter.name;
        row.parameter_value = static_cast<const xsd::string&>(parameter);
        row.position = position++;
        set_audit(row);
        out.push_back(std::move(row));
    }
}

}

mapped_pricing_engines pricing_engine_mapper::map(const pricingengines& v) {
    mapped_pricing_engines mapped;

    auto& config = mapped.config;
    config.id = new_uuid();
    config.name = "PricingEngines";
    config.description = "Imported from ORE XML";
    config.config_variant = "";
    set_audit(config);

    int product_position = 0;
    for (const auto& source : v.Product) {
        analytics::domain::pricing_model_product product;
        product.id = new_uuid();
        product.pricing_model_config_id = config.id;
        product.pricing_engine_type_code = source.type;
        product.model = source.Model;
        product.engine = source.Engine;
        product.position = product_position++;
        set_audit(product);
        mapped.products.push_back(product);

        append_parameters(mapped.parameters, config.id, product.id, scope_model,
                          source.ModelParameters.Parameter);
        append_parameters(mapped.parameters, config.id, product.id, scope_engine,
                          source.EngineParameters.Parameter);
    }

    if (v.GlobalParameters) {
        append_parameters(mapped.parameters, config.id, std::nullopt, scope_global,
                          v.GlobalParameters->Parameter);
    }

    return mapped;
}

pricingengines pricing_engine_mapper::reverse(const mapped_pricing_engines& v) {
    pricingengines document;

    std::vector<analytics::domain::pricing_model_product> products = v.products;
    std::sort(products.begin(), products.end(),
              [](const auto& lhs, const auto& rhs) { return lhs.position < rhs.position; });

    const auto in_scope = [&v](const boost::uuids::uuid& product_id,
                               const std::string_view scope) {
        std::vector<analytics::domain::pricing_model_product_parameter> rows;
        for (const auto& row : v.parameters) {
            if (row.parameter_scope != scope)
                continue;
            if (row.pricing_model_product_id != product_id)
                continue;
            rows.push_back(row);
        }
        std::sort(rows.begin(), rows.end(),
                  [](const auto& lhs, const auto& rhs) { return lhs.position < rhs.position; });
        return rows;
    };

    for (const auto& product : products) {
        domain::product source;
        source.type = product.pricing_engine_type_code;
        assign_text(source.Model, product.model);
        assign_text(source.Engine, product.engine);

        for (const auto& row : in_scope(product.id, scope_model)) {
            domain::parameter parameter;
            parameter.name = row.parameter_name;
            assign_text(parameter, row.parameter_value);
            source.ModelParameters.Parameter.push_back(parameter);
        }
        for (const auto& row : in_scope(product.id, scope_engine)) {
            domain::parameter parameter;
            parameter.name = row.parameter_name;
            assign_text(parameter, row.parameter_value);
            source.EngineParameters.Parameter.push_back(parameter);
        }

        document.Product.push_back(std::move(source));
    }

    std::vector<analytics::domain::pricing_model_product_parameter> globals;
    for (const auto& row : v.parameters) {
        if (row.parameter_scope != scope_global)
            continue;
        if (row.pricing_model_product_id)
            continue;
        globals.push_back(row);
    }
    std::sort(globals.begin(), globals.end(),
              [](const auto& lhs, const auto& rhs) { return lhs.position < rhs.position; });

    if (!globals.empty()) {
        domain::globalParameters parameters;
        for (const auto& row : globals) {
            domain::parameter parameter;
            parameter.name = row.parameter_name;
            assign_text(parameter, row.parameter_value);
            parameters.Parameter.push_back(parameter);
        }
        document.GlobalParameters = std::move(parameters);
    }

    return document;
}

}
