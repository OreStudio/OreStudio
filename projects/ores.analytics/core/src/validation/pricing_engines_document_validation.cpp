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
#include "ores.analytics.core/validation/pricing_engines_document_validation.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <cctype>
#include <optional>
#include <set>
#include <string>
#include <string_view>

namespace ores::analytics::validation {

namespace {

using domain::pricing_model_product;
using domain::pricing_model_product_parameter;
using messaging::pricing_engines_document;
using messaging::pricing_engines_document_problem;

constexpr std::string_view blanks = " \t\n\r\f\v";

bool blank(std::string_view text) {
    return text.find_first_not_of(blanks) == std::string_view::npos;
}

std::string id_of(const boost::uuids::uuid& id) {
    return boost::uuids::to_string(id);
}

/** The product a setting belongs to, when the document holds it. */
const pricing_model_product* product_of(const pricing_engines_document& document,
                                        const std::optional<boost::uuids::uuid>& id) {
    if (!id)
        return nullptr;
    for (const auto& product : document.products)
        if (product.id == *id)
            return &product;
    return nullptr;
}

/**
 * The thing a setting belongs to, in the words a message names it by.
 *
 * A setting with no product belongs to the configuration, which is what the
 * tables store as a global parameter.
 */
std::string owner_of(const pricing_model_product* product) {
    if (product == nullptr)
        return "the configuration";
    if (blank(product->pricing_engine_type_code))
        return "a product";
    return "the " + product->pricing_engine_type_code + " product";
}

std::string capitalised(std::string text) {
    if (!text.empty())
        text.front() = static_cast<char>(std::toupper(static_cast<unsigned char>(text.front())));
    return text;
}

/**
 * What identifies a setting within its owner: the tables let a scope name the
 * same parameter twice and the document keeps both rows, so the pair of the
 * scope and the name is the key a repeat is found by.
 */
std::string key_of(const std::string& product_id,
                   const pricing_model_product_parameter& parameter) {
    return product_id + '\x1f' + parameter.parameter_scope + '\x1f' + parameter.parameter_name;
}

}

std::vector<pricing_engines_document_problem>
problems_of(const pricing_engines_document& document) {
    std::vector<pricing_engines_document_problem> found;

    if (blank(document.config.name))
        found.push_back({.code = "name_missing", .message = "The configuration has no name."});

    if (document.products.empty())
        found.push_back({.code = "no_products", .message = "The configuration has no products."});

    for (const auto& product : document.products) {
        const auto product_id = id_of(product.id);
        if (blank(product.model))
            found.push_back({.code = "model_missing",
                             .product_id = product_id,
                             .message = capitalised(owner_of(&product)) + " has no model."});
        if (blank(product.engine))
            found.push_back({.code = "engine_missing",
                             .product_id = product_id,
                             .message = capitalised(owner_of(&product)) + " has no engine."});
    }

    std::set<std::string> stated;
    for (const auto& parameter : document.parameters) {
        const auto product_id =
            parameter.pricing_model_product_id ? id_of(*parameter.pricing_model_product_id) : "";
        const auto setting = "the setting " + parameter.parameter_name + " of " +
                             owner_of(product_of(document, parameter.pricing_model_product_id));

        if (!stated.insert(key_of(product_id, parameter)).second)
            found.push_back({.code = "parameter_repeated",
                             .product_id = product_id,
                             .parameter_scope = parameter.parameter_scope,
                             .parameter_name = parameter.parameter_name,
                             .message = capitalised(setting) + " appears more than once."});

        if (blank(parameter.parameter_value))
            found.push_back({.code = "parameter_value_missing",
                             .product_id = product_id,
                             .parameter_scope = parameter.parameter_scope,
                             .parameter_name = parameter.parameter_name,
                             .message = capitalised(setting) + " has no value."});
    }

    return found;
}

}
