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
#include "ores.ore.core/domain/pricing_engines_import.hpp"
#include "ores.utility/uuid/uuid_v7_generator.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <array>
#include <format>
#include <map>
#include <optional>
#include <pugixml.hpp>
#include <set>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

namespace ores::ore::domain {

namespace {

using pei_parameter_row = analytics::domain::pricing_model_product_parameter;
using pei_product_row = analytics::domain::pricing_model_product;
using pei_document = ores::analytics::messaging::pricing_engines_document;

constexpr std::string_view pei_scope_model = "model";
constexpr std::string_view pei_scope_engine = "engine";
constexpr std::string_view pei_scope_global = "global";

constexpr std::string_view pei_audit_modified_by = "ores";
constexpr std::string_view pei_audit_reason_code = "system.external_data_import";
constexpr std::string_view pei_audit_commentary = "Imported from ORE XML";

/// How much of a setting's value the review states before it stops.
constexpr std::size_t pei_value_width = 60;

boost::uuids::uuid pei_new_uuid() {
    static thread_local ores::utility::uuid::uuid_v7_generator generator;
    return generator();
}

template <typename T>
void pei_set_audit(T& row) {
    row.modified_by = std::string(pei_audit_modified_by);
    row.performed_by = std::string(pei_audit_modified_by);
    row.change_reason_code = std::string(pei_audit_reason_code);
    row.change_commentary = std::string(pei_audit_commentary);
}

/// The text of an element with the whitespace a pretty-printed document leaves
/// around it taken off; the value itself is never reformatted.
std::string pei_text_of(const pugi::xml_node& node) {
    std::string_view text = node.text().get();
    const auto first = text.find_first_not_of(" \t\r\n\f\v");
    if (first == std::string_view::npos)
        return {};
    const auto last = text.find_last_not_of(" \t\r\n\f\v");
    return std::string(text.substr(first, last - first + 1));
}

std::string pei_shortened(const std::string& value) {
    if (value.size() <= pei_value_width)
        return value;
    return value.substr(0, pei_value_width) + "…";
}

/// A product's identity within its configuration: its type and which product of
/// that type it is, counted from zero.
std::string pei_product_key(const std::string& type_code, int occurrence) {
    return std::format("{}#{}", type_code, occurrence);
}

/// A setting's identity within its product, and within the configuration when it
/// belongs to no product.
std::string pei_setting_key(const std::string& owner_key,
                        std::string_view scope,
                        const std::string& name,
                        int occurrence) {
    if (occurrence == 0)
        return std::format("{}/{}/{}", owner_key, scope, name);
    return std::format("{}/{}/{}#{}", owner_key, scope, name, occurrence);
}

pricing_engines_import_issue pei_dropped(const std::string& subject, const std::string& detail) {
    return {.kind = "dropped", .subject = subject, .detail = detail};
}

pricing_engines_import_issue pei_missing(const std::string& subject, const std::string& detail) {
    return {.kind = "missing", .subject = subject, .detail = detail};
}

/// Which target rows a file's rows are compared against, rebuilt on demand: the
/// two walks that need it — the difference and the merge — ask the same
/// question of the same rows.
struct pei_target_rows {
    std::map<std::string, std::vector<const pei_product_row*>> by_type;
    std::map<boost::uuids::uuid, std::map<std::string, std::vector<const pei_parameter_row*>>> settings;
    std::map<std::string, std::vector<const pei_parameter_row*>> globals;

    const std::vector<const pei_parameter_row*>& settings_of(const std::optional<boost::uuids::uuid>& id,
                                                        const std::string& key) const {
        static const std::vector<const pei_parameter_row*> none;
        if (!id)
            return none;
        const auto owner = settings.find(*id);
        if (owner == settings.end())
            return none;
        const auto found = owner->second.find(key);
        return found == owner->second.end() ? none : found->second;
    }
};

const std::vector<const pei_product_row*>& pei_products_of(const pei_target_rows& target,
                                                   const std::string& type_code) {
    static const std::vector<const pei_product_row*> none;
    const auto found = target.by_type.find(type_code);
    return found == target.by_type.end() ? none : found->second;
}

pei_target_rows pei_group_target(const pei_document& target) {
    pei_target_rows grouped;
    std::vector<const pei_product_row*> products;
    for (const auto& product : target.products)
        products.push_back(&product);
    std::ranges::sort(products, [](const pei_product_row* lhs, const pei_product_row* rhs) {
        if (lhs->position != rhs->position)
            return lhs->position < rhs->position;
        return lhs->id < rhs->id;
    });
    for (const auto* product : products)
        grouped.by_type[product->pricing_engine_type_code].push_back(product);

    std::vector<const pei_parameter_row*> parameters;
    for (const auto& parameter : target.parameters)
        parameters.push_back(&parameter);
    std::ranges::sort(parameters, [](const pei_parameter_row* lhs, const pei_parameter_row* rhs) {
        if (lhs->position != rhs->position)
            return lhs->position < rhs->position;
        return lhs->id < rhs->id;
    });
    for (const auto* parameter : parameters) {
        if (parameter->parameter_scope == pei_scope_global)
            grouped.globals[parameter->parameter_name].push_back(parameter);
        else if (parameter->pricing_model_product_id)
            grouped.settings[*parameter->pricing_model_product_id]
                [parameter->parameter_scope + '\x1f' + parameter->parameter_name]
                    .push_back(parameter);
    }
    return grouped;
}

std::string pei_settings_key(std::string_view scope, const std::string& name) {
    return std::string(scope) + '\x1f' + name;
}

/// The target row a file row is compared against, or nothing when the target
/// holds no such row.
std::optional<boost::uuids::uuid> pei_id_of(const pei_product_row* product) {
    if (product == nullptr)
        return std::nullopt;
    return product->id;
}

/// The product occurrence a file product is, counted among the file's products
/// of its own type.
int pei_occurrence_of(const std::map<std::string, int>& counts, const std::string& type_code) {
    const auto found = counts.find(type_code);
    return found == counts.end() ? 0 : found->second;
}

}


pricing_engines_import_file pricing_engines_import::parse(std::string_view text) {
    pricing_engines_import_file out;

    pugi::xml_document document;
    const auto loaded = document.load_buffer(text.data(), text.size());
    if (!loaded) {
        out.refusal = std::format("The file could not be read as XML: {} at byte {}.",
                                  loaded.description(),
                                  loaded.offset);
        return out;
    }
    const auto root = document.document_element();
    if (!root) {
        out.refusal = "The file states no root element.";
        return out;
    }
    out.root_element = root.name();
    if (out.root_element != root_element) {
        out.refusal = std::format("This file is not a pricing engine configuration: its root "
                                  "element is {}. Choose a {} file.",
                                  out.root_element,
                                  root_element);
        return out;
    }

    auto& rows = out.document;
    rows.config.id = pei_new_uuid();
    // The name, the variant and the reporting configuration the document fills
    // are the caller's: the file states none of them.
    rows.config.name = std::string(root_element);
    rows.config.description = std::string(pei_audit_commentary);
    pei_set_audit(rows.config);

    std::set<std::string> stated_settings;
    std::map<std::string, int> type_counts;
    std::set<std::string> stated_globals;
    int product_position = 0;

    const auto read_settings =
        [&](const pugi::xml_node& section,
            const std::optional<boost::uuids::uuid>& product,
            const std::string& owner_key,
            std::string_view scope) {
            int position = 0;
            for (const auto& node : section.children()) {
                if (node.type() != pugi::node_element)
                    continue;
                if (std::string_view(node.name()) != "Parameter") {
                    out.issues.push_back(pei_dropped(
                        std::format("{}/{}", owner_key, node.name()),
                        "The tables hold settings only; this element is left out."));
                    continue;
                }
                for (const auto& attribute : node.attributes())
                    if (std::string_view(attribute.name()) != "name")
                        out.issues.push_back(
                            pei_dropped(std::format("{}/Parameter@{}", owner_key, attribute.name()),
                                    "A setting is a name and a value; this attribute is left "
                                    "out."));
                const auto name = std::string(node.attribute("name").value());
                if (name.empty()) {
                    out.issues.push_back(pei_missing(std::format("{}/Parameter", owner_key),
                                                 "A setting with no name cannot be written; it "
                                                 "is left out."));
                    continue;
                }
                const auto identity = pei_settings_key(scope, name);
                if (!stated_settings.insert(owner_key + '\x1f' + identity).second) {
                    out.issues.push_back(
                        {.kind = "duplicate",
                         .subject = std::format("{}/{}/{}", owner_key, scope, name),
                         .detail = "The file states this setting twice. A configuration cannot "
                                   "hold it twice, so the first value stands; change the file to "
                                   "choose another."});
                    continue;
                }
                pei_parameter_row row;
                row.id = pei_new_uuid();
                row.pricing_model_config_id = rows.config.id;
                row.pricing_model_product_id = product;
                row.parameter_scope = std::string(scope);
                row.parameter_name = name;
                row.parameter_value = pei_text_of(node);
                row.position = position++;
                pei_set_audit(row);
                rows.parameters.push_back(std::move(row));
            }
        };

    for (const auto& child : root.children()) {
        if (child.type() != pugi::node_element)
            continue;
        const std::string_view name(child.name());
        if (name == "GlobalParameters") {
            const auto before = rows.parameters.size();
            read_settings(child, std::nullopt, std::string(), pei_scope_global);
            for (auto at = before; at < rows.parameters.size(); ++at)
                if (stated_globals.insert(rows.parameters[at].parameter_name).second)
                    out.global_parameters.push_back(rows.parameters[at].parameter_name);
            continue;
        }
        if (name != "Product") {
            out.issues.push_back(
                pei_dropped(std::string(name),
                        "The tables hold products and settings only; this element is left out."));
            continue;
        }

        for (const auto& attribute : child.attributes())
            if (std::string_view(attribute.name()) != "type")
                out.issues.push_back(pei_dropped(std::format("Product@{}", attribute.name()),
                                             "A product is a type, a model and an engine; this "
                                             "attribute is left out."));

        const auto type_code = std::string(child.attribute("type").value());
        if (type_code.empty()) {
            out.issues.push_back(pei_missing("Product", "A product with no type cannot be written; it "
                                                    "is left out."));
            continue;
        }
        const auto occurrence = pei_occurrence_of(type_counts, type_code);
        ++type_counts[type_code];
        if (occurrence > 0)
            out.issues.push_back(
                {.kind = "duplicate",
                 .subject = type_code,
                 .detail = std::format("The file names the {} product type {} times. Each stands "
                                       "as its own product, in its own position.",
                                       type_code,
                                       occurrence + 1)});

        pei_product_row product;
        product.id = pei_new_uuid();
        product.pricing_model_config_id = rows.config.id;
        product.pricing_engine_type_code = type_code;
        product.position = product_position++;
        pei_set_audit(product);

        const auto owner_key = pei_product_key(type_code, occurrence);
        const auto settings_before = rows.parameters.size();
        bool model_stated = false;
        bool engine_stated = false;
        for (const auto& node : child.children()) {
            if (node.type() != pugi::node_element)
                continue;
            const std::string_view element(node.name());
            if (element == "Model") {
                product.model = pei_text_of(node);
                model_stated = true;
            } else if (element == "Engine") {
                product.engine = pei_text_of(node);
                engine_stated = true;
            } else if (element == "ModelParameters") {
                read_settings(node, product.id, owner_key, pei_scope_model);
            } else if (element == "EngineParameters") {
                read_settings(node, product.id, owner_key, pei_scope_engine);
            } else {
                out.issues.push_back(
                    pei_dropped(std::format("{}/{}", owner_key, element),
                            "The tables hold a model, its settings, an engine and its settings; "
                            "this element is left out."));
            }
        }
        if (!model_stated)
            out.issues.push_back(
                pei_missing(owner_key,
                        std::format("The {} product states no model, and a document with no model "
                                    "cannot be saved.",
                                    type_code)));
        if (!engine_stated)
            out.issues.push_back(
                pei_missing(owner_key,
                        std::format("The {} product states no engine, and a document with no "
                                    "engine cannot be saved.",
                                    type_code)));

        out.products.push_back({.type_code = type_code,
                                .model = product.model,
                                .engine = product.engine,
                                .parameters = static_cast<std::uint32_t>(rows.parameters.size() -
                                                                         settings_before)});
        rows.products.push_back(std::move(product));
    }

    return out;
}

namespace {

/**
 * The file as the comparison needs it: its products in the order it writes
 * them, the target product each one stands for, and the key each one is named
 * by.
 *
 * A product is matched by its type and by which product of that type it is,
 * because a document may name the same type twice and neither the type nor the
 * model identifies its row.
 */
struct pei_file_view {
    std::vector<const pei_product_row*> products;
    std::map<boost::uuids::uuid, const pei_product_row*> target_of;
    std::map<boost::uuids::uuid, std::string> key_of;
    std::map<std::string, int> stated_by_type;

    [[nodiscard]] const pei_product_row* target(const boost::uuids::uuid& id) const {
        const auto found = target_of.find(id);
        return found == target_of.end() ? nullptr : found->second;
    }
};

pei_file_view pei_view_of(const pei_document& file, const pei_target_rows& target) {
    pei_file_view view;
    for (const auto& product : file.products)
        view.products.push_back(&product);
    std::ranges::sort(view.products, [](const pei_product_row* lhs, const pei_product_row* rhs) {
        if (lhs->position != rhs->position)
            return lhs->position < rhs->position;
        return lhs->id < rhs->id;
    });
    for (const auto* product : view.products) {
        const auto& type_code = product->pricing_engine_type_code;
        const auto occurrence = pei_occurrence_of(view.stated_by_type, type_code);
        ++view.stated_by_type[type_code];
        view.key_of[product->id] = pei_product_key(type_code, occurrence);
        const auto& candidates = pei_products_of(target, type_code);
        if (occurrence < static_cast<int>(candidates.size()))
            view.target_of[product->id] = candidates[occurrence];
    }
    return view;
}

/// The settings the file states under one product in one scope, in their order.
std::vector<const pei_parameter_row*> pei_settings_of(const pei_document& file,
                                             const boost::uuids::uuid& product,
                                             std::string_view scope) {
    std::vector<const pei_parameter_row*> found;
    for (const auto& parameter : file.parameters)
        if (parameter.pricing_model_product_id.has_value() &&
            *parameter.pricing_model_product_id == product && parameter.parameter_scope == scope)
            found.push_back(&parameter);
    return found;
}

/// The settings the file states for the configuration itself, in their order.
std::vector<const pei_parameter_row*> pei_global_settings_of(const pei_document& file) {
    std::vector<const pei_parameter_row*> found;
    for (const auto& parameter : file.parameters)
        if (parameter.parameter_scope == pei_scope_global)
            found.push_back(&parameter);
    return found;
}

/// The scope names the two lists of a product hold, in the order the document
/// writes them.
const std::array<std::string, 2>& pei_scopes() {
    static const std::array<std::string, 2> names{std::string(pei_scope_model),
                                                  std::string(pei_scope_engine)};
    return names;
}

}


pricing_engines_import_difference
pricing_engines_import::difference(const pricing_engines_import_file& file,
                                   const pei_document& target,
                                   const std::set<std::string>& known_types) {
    pricing_engines_import_difference out;
    const auto grouped = pei_group_target(target);
    const auto view = pei_view_of(file.document, grouped);

    for (const auto& product : view.products)
        if (!known_types.contains(product->pricing_engine_type_code))
            out.issues.push_back(
                {.kind = "unknown_type",
                 .subject = product->pricing_engine_type_code,
                 .detail = "The vocabulary does not hold this product type, so the screens cannot "
                           "offer it. The product is written as the file states it."});

    for (const auto* product : view.products) {
        const auto& key = view.key_of.at(product->id);
        const auto* theirs = view.target(product->id);

        if (theirs == nullptr) {
            ++out.added;
            out.rows.push_back({.key = key,
                                .entity = "product",
                                .change = "added",
                                .owner = {},
                                .name = {},
                                .what = std::format("Model {}, engine {}",
                                                    product->model,
                                                    product->engine)});
        } else {
            std::string what;
            const auto note = [&what](std::string_view label,
                                      const std::string& held,
                                      const std::string& stated) {
                if (held == stated)
                    return;
                if (!what.empty())
                    what += "; ";
                what += std::format("{} {} → {}", label, pei_shortened(held), pei_shortened(stated));
            };
            note("Model", theirs->model, product->model);
            note("Engine", theirs->engine, product->engine);
            if (what.empty())
                ++out.held;
            else
                out.rows.push_back({.key = key,
                                    .entity = "product",
                                    .change = "changed",
                                    .owner = {},
                                    .name = {},
                                    .what = what});
        }

        for (const auto& scope : pei_scopes()) {
            std::map<std::string, int> seen;
            for (const auto* parameter : pei_settings_of(file.document, product->id, scope)) {
                const auto at = seen[parameter->parameter_name]++;
                const auto identity = pei_settings_key(scope, parameter->parameter_name);
                const auto row_key =
                    pei_setting_key(key, scope, parameter->parameter_name, at);
                const auto& found = grouped.settings_of(pei_id_of(theirs), identity);
                const pei_parameter_row* held =
                    at < static_cast<int>(found.size()) ? found[at] : nullptr;
                if (held == nullptr) {
                    ++out.added;
                    out.rows.push_back({.key = row_key,
                                        .entity = "parameter",
                                        .change = "added",
                                        .owner = key,
                                        .name = parameter->parameter_name,
                                        .what = std::format("{} = {}",
                                                            parameter->parameter_name,
                                                            pei_shortened(parameter->parameter_value))});
                } else if (held->parameter_value != parameter->parameter_value) {
                    ++out.changed;
                    out.rows.push_back({.key = row_key,
                                        .entity = "parameter",
                                        .change = "changed",
                                        .owner = key,
                                        .name = parameter->parameter_name,
                                        .what = std::format("{} → {}",
                                                            pei_shortened(held->parameter_value),
                                                            pei_shortened(parameter->parameter_value))});
                } else {
                    ++out.held;
                }
            }
        }
    }

    // Every target product the file does not reach is a removal, and its
    // settings go with it: the save removes a product's settings when it omits
    // the product, so they are not stated as removals of their own.
    for (const auto& [type_code, candidates] : grouped.by_type) {
        const auto stated = pei_occurrence_of(view.stated_by_type, type_code);
        for (auto at = stated; at < static_cast<int>(candidates.size()); ++at) {
            const auto& product = *candidates[at];
            ++out.removed;
            const auto settings = std::ranges::count_if(
                target.parameters, [&product](const pei_parameter_row& parameter) {
                    return parameter.pricing_model_product_id.has_value() &&
                           *parameter.pricing_model_product_id == product.id;
                });
            out.rows.push_back(
                {.key = pei_product_key(type_code, at),
                 .entity = "product",
                 .change = "removed",
                 .owner = {},
                 .name = {},
                 .what = std::format("Not in the file. It carries {} setting{}.",
                                     settings,
                                     settings == 1 ? "" : "s")});
        }
    }

    // A setting the file omits from a product it does reach is a removal of its
    // own.
    for (const auto* product : view.products) {
        const auto* theirs = view.target(product->id);
        if (theirs == nullptr)
            continue;
        const auto& key = view.key_of.at(product->id);
        const auto settings = grouped.settings.find(theirs->id);
        if (settings == grouped.settings.end())
            continue;
        for (const auto& scope : pei_scopes()) {
            std::map<std::string, int> stated;
            for (const auto* parameter : pei_settings_of(file.document, product->id, scope))
                ++stated[parameter->parameter_name];
            for (const auto& [identity, rows] : settings->second) {
                if (identity.rfind(scope + '\x1f', 0) != 0)
                    continue;
                const auto& name = rows.front()->parameter_name;
                const auto from = stated.contains(name) ? stated[name] : 0;
                for (auto at = from; at < static_cast<int>(rows.size()); ++at) {
                    ++out.removed;
                    out.rows.push_back({.key = pei_setting_key(key, scope, name, at),
                                        .entity = "parameter",
                                        .change = "removed",
                                        .owner = key,
                                        .name = name,
                                        .what = "Not in the file"});
                }
            }
        }
    }

    // The root-level settings belong to the configuration, so they are matched
    // by name alone.
    for (const auto* parameter : pei_global_settings_of(file.document)) {
        const auto key = pei_setting_key("/", pei_scope_global, parameter->parameter_name, 0);
        const auto found = grouped.globals.find(parameter->parameter_name);
        const auto* held =
            found == grouped.globals.end() || found->second.empty() ? nullptr :
                                                                     found->second.front();
        if (held == nullptr) {
            ++out.added;
            out.rows.push_back({.key = key,
                                .entity = "parameter",
                                .change = "added",
                                .owner = "the configuration",
                                .name = parameter->parameter_name,
                                .what = std::format("{} = {}",
                                                    parameter->parameter_name,
                                                    pei_shortened(parameter->parameter_value))});
        } else if (held->parameter_value != parameter->parameter_value) {
            ++out.changed;
            out.rows.push_back({.key = key,
                                .entity = "parameter",
                                .change = "changed",
                                .owner = "the configuration",
                                .name = parameter->parameter_name,
                                .what = std::format("{} → {}",
                                                    pei_shortened(held->parameter_value),
                                                    pei_shortened(parameter->parameter_value))});
        } else {
            ++out.held;
        }
    }
    for (const auto& [name, rows] : grouped.globals) {
        if (rows.empty())
            continue;
        const auto stated = std::ranges::any_of(
            file.document.parameters, [&name](const pei_parameter_row& parameter) {
                return parameter.parameter_scope == pei_scope_global &&
                       parameter.parameter_name == name;
            });
        if (stated)
            continue;
        ++out.removed;
        out.rows.push_back({.key = pei_setting_key("/", pei_scope_global, name, 0),
                            .entity = "parameter",
                            .change = "removed",
                            .owner = "the configuration",
                            .name = name,
                            .what = "Not in the file"});
    }

    return out;
}

std::vector<std::string>
pricing_engines_import::kept_by_default(const pricing_engines_import_difference& difference) {
    std::vector<std::string> kept;
    for (const auto& row : difference.rows)
        if (row.change == "removed")
            kept.push_back(row.key);
    return kept;
}

pei_document pricing_engines_import::merge(const pricing_engines_import_file& file,
                                       const pei_document& target,
                                       const std::vector<std::string>& kept_removals) {
    const std::set<std::string> kept(kept_removals.begin(), kept_removals.end());
    const auto grouped = pei_group_target(target);
    const auto view = pei_view_of(file.document, grouped);

    pei_document out;
    out.config = target.config;
    int product_position = 0;

    // The file's rows, each keeping the identity and the version of the row the
    // target already holds, so a write corrects that row rather than replacing
    // it with a new one and its history stays one line.
    for (const auto* product : view.products) {
        const auto* theirs = view.target(product->id);
        pei_product_row row = *product;
        if (theirs != nullptr) {
            row.id = theirs->id;
            row.version = theirs->version;
            row.tenant_id = theirs->tenant_id;
            row.party_id = theirs->party_id;
        }
        row.position = product_position++;
        out.products.push_back(row);

        for (const auto& scope : pei_scopes()) {
            std::map<std::string, int> seen;
            for (const auto* parameter : pei_settings_of(file.document, product->id, scope)) {
                const auto at = seen[parameter->parameter_name]++;
                const auto identity = pei_settings_key(scope, parameter->parameter_name);
                const auto& found = grouped.settings_of(pei_id_of(theirs), identity);
                pei_parameter_row setting = *parameter;
                setting.pricing_model_product_id = row.id;
                setting.position = at;
                if (at < static_cast<int>(found.size())) {
                    const auto* held = found[at];
                    setting.id = held->id;
                    setting.version = held->version;
                    setting.tenant_id = held->tenant_id;
                    setting.party_id = held->party_id;
                }
                out.parameters.push_back(setting);
            }
        }
    }

    // What the file omits and the person keeps is carried from the target as it
    // stands: the document says nothing about it, so nothing about it moves.
    const auto carry = [&](const pei_product_row& product) {
        pei_product_row row = product;
        row.position = product_position++;
        out.products.push_back(row);
        std::map<std::string, int> positions;
        for (const auto& parameter : target.parameters) {
            if (!parameter.pricing_model_product_id.has_value() ||
                *parameter.pricing_model_product_id != product.id)
                continue;
            pei_parameter_row setting = parameter;
            setting.position = positions[parameter.parameter_scope]++;
            out.parameters.push_back(setting);
        }
    };
    for (const auto& [type_code, candidates] : grouped.by_type) {
        const auto stated = pei_occurrence_of(view.stated_by_type, type_code);
        for (auto at = stated; at < static_cast<int>(candidates.size()); ++at)
            if (kept.contains(pei_product_key(type_code, at)))
                carry(*candidates[at]);
    }

    // A setting of a product that stays is a removal of its own, unless the
    // person kept it.
    for (const auto* product : view.products) {
        const auto* theirs = view.target(product->id);
        if (theirs == nullptr)
            continue;
        const auto settings = grouped.settings.find(theirs->id);
        if (settings == grouped.settings.end())
            continue;
        const auto& key = view.key_of.at(product->id);
        for (const auto& scope : pei_scopes()) {
            std::map<std::string, int> stated;
            for (const auto* parameter : pei_settings_of(file.document, product->id, scope))
                ++stated[parameter->parameter_name];
            for (const auto& [identity, rows] : settings->second) {
                if (identity.rfind(scope + '\x1f', 0) != 0)
                    continue;
                const auto& name = rows.front()->parameter_name;
                const auto from = stated.contains(name) ? stated[name] : 0;
                for (auto at = from; at < static_cast<int>(rows.size()); ++at) {
                    if (!kept.contains(pei_setting_key(key, scope, name, at)))
                        continue;
                    pei_parameter_row setting = *rows[at];
                    setting.position = static_cast<int>(out.parameters.size());
                    out.parameters.push_back(setting);
                }
            }
        }
    }

    for (const auto* parameter : pei_global_settings_of(file.document)) {
        pei_parameter_row setting = *parameter;
        const auto found = grouped.globals.find(parameter->parameter_name);
        if (found != grouped.globals.end() && !found->second.empty()) {
            const auto* held = found->second.front();
            setting.id = held->id;
            setting.version = held->version;
            setting.tenant_id = held->tenant_id;
            setting.party_id = held->party_id;
        }
        out.parameters.push_back(setting);
    }
    for (const auto& [name, rows] : grouped.globals) {
        if (rows.empty())
            continue;
        const auto stated = std::ranges::any_of(
            file.document.parameters, [&name](const pei_parameter_row& parameter) {
                return parameter.parameter_scope == pei_scope_global &&
                       parameter.parameter_name == name;
            });
        if (stated)
            continue;
        if (kept.contains(pei_setting_key("/", pei_scope_global, name, 0)))
            out.parameters.push_back(*rows.front());
    }

    return out;
}

}
