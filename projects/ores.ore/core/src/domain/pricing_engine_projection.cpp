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
#include "ores.ore.core/domain/pricing_engine_projection.hpp"
#include "ores.ore.core/xml/binding.hpp"
#include "ores.ore.core/xml/emitter.hpp"
#include "ores.ore.core/xml/projection.hpp"
#include "ores.utility/uuid/uuid_v7_generator.hpp"
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <format>
#include <map>
#include <optional>
#include <set>
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace ores::ore::domain {

namespace {

namespace xg = xml::generated::pricing_engines;

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

// The document writes the two parameter lists the same way, and the only thing
// that differs is the scope it records, so one helper covers both.
void append_parameters(std::vector<analytics::domain::pricing_model_product_parameter>& out,
                       const boost::uuids::uuid& config_id,
                       const std::optional<boost::uuids::uuid>& product_id,
                       const std::string_view scope,
                       const std::vector<xml::generated::pricing_engines::parameter>& parameters) {
    int position = 0;
    for (const auto& parameter : parameters) {
        analytics::domain::pricing_model_product_parameter row;
        row.id = new_uuid();
        row.pricing_model_config_id = config_id;
        row.pricing_model_product_id = product_id;
        row.parameter_scope = std::string(scope);
        row.parameter_name = parameter.name;
        row.parameter_value = parameter.value;
        row.position = position++;
        set_audit(row);
        out.push_back(std::move(row));
    }
}

// Rows from the database or the shell can share a position, so the id breaks
// the tie. That does not recover the order rows were created in, because a
// UUIDv7 id is random within a millisecond, but it makes the export stable.
template <typename Row>
bool by_position(const Row* lhs, const Row* rhs) {
    if (lhs->position != rhs->position)
        return lhs->position < rhs->position;
    return lhs->id < rhs->id;
}

using parameter_row = analytics::domain::pricing_model_product_parameter;
using parameter_key = std::pair<std::optional<boost::uuids::uuid>, std::string>;

// Groups the parameter rows by the product and scope that own them, and
// refuses any row the document has no place for: an unknown scope, a global
// row with a product, or a model or engine row whose product is absent.
// Dropping such a row would pass as a round trip while losing data.
std::map<parameter_key, std::vector<const parameter_row*>>
group_parameters(const ores::analytics::messaging::pricing_engines_document& v) {
    std::set<boost::uuids::uuid> product_ids;
    for (const auto& product : v.products)
        product_ids.insert(product.id);

    std::map<parameter_key, std::vector<const parameter_row*>> groups;
    for (const auto& row : v.parameters) {
        const auto describe = [&row] {
            return "pricing_engine_projection: parameter " + boost::uuids::to_string(row.id) +
                   " (" + row.parameter_name + ")";
        };
        if (row.parameter_scope == scope_global) {
            if (row.pricing_model_product_id)
                throw std::runtime_error(describe() + " is global but names a product");
        } else if (row.parameter_scope == scope_model || row.parameter_scope == scope_engine) {
            if (!row.pricing_model_product_id ||
                !product_ids.contains(*row.pricing_model_product_id))
                throw std::runtime_error(describe() + " names no product in the document");
        } else {
            throw std::runtime_error(describe() + " has unknown scope '" + row.parameter_scope +
                                     "'");
        }
        groups[{row.pricing_model_product_id, row.parameter_scope}].push_back(&row);
    }
    for (auto& [key, rows] : groups)
        std::sort(rows.begin(), rows.end(), by_position<parameter_row>);
    return groups;
}

}

ores::analytics::messaging::pricing_engines_document
pricing_engine_projection::map(const document& v) {
    ores::analytics::messaging::pricing_engines_document mapped;

    auto& config = mapped.config;
    config.id = new_uuid();
    config.name = "PricingEngines";
    config.description = "Imported from ORE XML";
    config.config_variant = "";
    set_audit(config);

    int product_position = 0;
    for (const auto& source : v.product) {
        analytics::domain::pricing_model_product product;
        product.id = new_uuid();
        product.pricing_model_config_id = config.id;
        product.pricing_engine_type_code = source.type;
        product.model = source.model;
        product.engine = source.engine;
        product.position = product_position++;
        set_audit(product);
        mapped.products.push_back(product);

        append_parameters(mapped.parameters,
                          config.id,
                          product.id,
                          scope_model,
                          source.model_parameters.parameter);
        append_parameters(mapped.parameters,
                          config.id,
                          product.id,
                          scope_engine,
                          source.engine_parameters.parameter);
    }

    if (xml::is_present(*xg::pricing_engines_document().root, &v, "GlobalParameters")) {
        append_parameters(mapped.parameters,
                          config.id,
                          std::nullopt,
                          scope_global,
                          v.global_parameters.parameter);
    }

    return mapped;
}

pricing_engine_projection::document pricing_engine_projection::read(std::string_view text,
                                                                    std::string_view origin) {
    document record;
    const auto bound = xml::binder::bind(xg::pricing_engines_document(), text, &record);
    if (!bound) {
        const auto where = origin.empty() ? std::string("pricing engines") : std::string(origin);
        throw std::invalid_argument(
            std::format("{}: line {}: {}", where, bound.error().line, bound.error().message));
    }
    return record;
}

std::string pricing_engine_projection::write(const document& v) {
    std::string out;
    const auto emitted = xml::emitter::emit(xg::pricing_engines_document(), &v, out);
    if (!emitted)
        throw std::runtime_error(emitted.error().message);
    return out;
}

pricing_engine_projection::document
pricing_engine_projection::reverse(const ores::analytics::messaging::pricing_engines_document& v) {
    const auto* root = xg::pricing_engines_document().root;
    document record;

    const auto groups = group_parameters(v);
    const auto in_scope =
        [&groups](const std::optional<boost::uuids::uuid>& product_id,
                  const std::string_view scope) -> const std::vector<const parameter_row*>& {
        static const std::vector<const parameter_row*> none;
        const auto it = groups.find({product_id, std::string(scope)});
        return it == groups.end() ? none : it->second;
    };
    const auto& product_type = xml::projection::child(*root, "Product");
    const auto& model_parameters_type = xml::projection::child(product_type, "ModelParameters");
    const auto& engine_parameters_type = xml::projection::child(product_type, "EngineParameters");
    const auto& global_parameters_type = xml::projection::child(*root, "GlobalParameters");
    const auto& parameter_type = xml::projection::child(model_parameters_type, "Parameter");

    const auto to_parameter = [&parameter_type](const parameter_row& row) {
        xg::parameter parameter;
        parameter.name = row.parameter_name;
        parameter.value = row.parameter_value;
        xml::projection::state(parameter_type, &parameter, "name");
        xml::mark_text_present(parameter_type, &parameter);
        return parameter;
    };

    std::vector<const analytics::domain::pricing_model_product*> products;
    for (const auto& product : v.products)
        products.push_back(&product);
    std::sort(
        products.begin(), products.end(), by_position<analytics::domain::pricing_model_product>);

    for (const auto* product : products) {
        xg::product source;
        source.type = product->pricing_engine_type_code;
        source.model = product->model;
        source.engine = product->engine;
        xml::projection::state(product_type, &source, "type");
        xml::projection::state(product_type, &source, "Model");
        xml::projection::state(product_type, &source, "Engine");

        for (const auto* row : in_scope(product->id, scope_model))
            source.model_parameters.parameter.push_back(to_parameter(*row));
        for (const auto* row : in_scope(product->id, scope_engine))
            source.engine_parameters.parameter.push_back(to_parameter(*row));

        // Both lists are required elements, so an empty one is written as an
        // empty element rather than left out.
        xml::projection::state(model_parameters_type, &source.model_parameters, "Parameter");
        xml::projection::state(product_type, &source, "ModelParameters");
        xml::projection::state(engine_parameters_type, &source.engine_parameters, "Parameter");
        xml::projection::state(product_type, &source, "EngineParameters");

        record.product.push_back(std::move(source));
    }
    xml::projection::state(*root, &record, "Product");

    // An empty <GlobalParameters/> and an absent one map to the same rows, so
    // the export writes the element only when a global row exists. No shipped
    // document writes an empty one; keeping the difference would need a column
    // of its own.
    const auto& globals = in_scope(std::nullopt, scope_global);
    if (!globals.empty()) {
        for (const auto* row : globals)
            record.global_parameters.parameter.push_back(to_parameter(*row));
        xml::projection::state(global_parameters_type, &record.global_parameters, "Parameter");
        xml::projection::state(*root, &record, "GlobalParameters");
    }

    return record;
}

}
