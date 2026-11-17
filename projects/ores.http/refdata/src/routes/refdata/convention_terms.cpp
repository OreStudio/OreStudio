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
#include "ores.http/routes/refdata/convention_terms.hpp"
#include "ores.http.api/net/json_shape.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.core/routes/ore_value_set_tables.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.http/routes/refdata/catalogue_choices.hpp"
#include "ores.refdata.api/messaging/convention_kind_term_protocol.hpp"
#include "ores.reporting.api/messaging/ore_parameter_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <algorithm>
#include <cstdlib>
#include <map>
#include <optional>
#include <string>
#include <utility>
#include <vector>
#include <yyjson.h>

namespace ores::http::routes::refdata::terms {

namespace {

namespace forwarding = ores::http::forwarding;
namespace json = ores::utility::codec::json;
namespace refdata_messaging = ores::refdata::messaging;
namespace reporting_messaging = ores::reporting::messaging;
using ores::http::domain::http_request;
using ores::http::net::shape::member;
using ores::http::net::shape::object;
using ores::nats::service::nats_client;
using ores::reporting::domain::ore_parameter;

/// The contract entry of one kind: the leaves of one schema, by the path it keys them at.
using contract = std::map<std::string, const ore_parameter*, std::less<>>;

/// The key a contract entry is found by: the schema's name and the leaf's path.
std::string contract_key(std::string_view schema, std::string_view xpath) {
    return std::string(schema) + "|" + std::string(xpath);
}

/// One path's step, or empty for a step the path does not have.
std::string step(std::string_view xpath, std::size_t index, bool from_end) {
    std::vector<std::string> steps;
    std::string current;
    for (const auto character : xpath) {
        if (character == '/') {
            if (!current.empty())
                steps.push_back(std::move(current));
            current.clear();
            continue;
        }
        current += character;
    }
    if (!current.empty())
        steps.push_back(std::move(current));
    if (steps.empty())
        return std::string();
    if (from_end)
        return steps.back();
    return index < steps.size() ? steps[index] : std::string();
}

/// The cells one JSON row states, by the column the row names.
document::row cells_of(std::string_view row_json) {
    document::row cells;
    const std::unique_ptr<yyjson_doc, decltype(&yyjson_doc_free)> doc(
        yyjson_read(row_json.data(), row_json.size(), 0), &yyjson_doc_free);
    auto* root = doc ? yyjson_doc_get_root(doc.get()) : nullptr;
    if (!yyjson_is_obj(root))
        return cells;
    std::size_t index = 0;
    std::size_t count = 0;
    yyjson_val* key = nullptr;
    yyjson_val* value = nullptr;
    yyjson_obj_foreach(root, index, count, key, value) {
        const std::string name(yyjson_get_str(key), yyjson_get_len(key));
        if (yyjson_is_null(value)) {
            cells.emplace(name, document::cell());
            continue;
        }
        if (yyjson_is_str(value)) {
            cells.emplace(
                name, document::cell(std::string(yyjson_get_str(value), yyjson_get_len(value))));
            continue;
        }
        std::size_t length = 0;
        char* written = yyjson_val_write(value, 0, &length);
        cells.emplace(name, document::cell(std::string(written, length)));
        std::free(written);
    }
    return cells;
}

}

std::vector<term_view> read(const http_request& req,
                            nats_client& session,
                            std::string_view kind_code,
                            std::string_view row_json,
                            const std::set<std::string, std::less<>>& held) {
    const auto stated =
        forwarding::forward_all(req,
                                session,
                                refdata_messaging::list_convention_kind_terms_request{},
                                &refdata_messaging::list_convention_kind_terms_response::terms);
    const auto parameters =
        forwarding::forward_all(req,
                                session,
                                reporting_messaging::list_ore_parameters_request{},
                                &reporting_messaging::list_ore_parameters_response::parameters);
    contract entries;
    for (const auto& parameter : parameters)
        entries.emplace(contract_key(parameter.schema_name, parameter.xpath), &parameter);
    const auto values = ores::http::routes::value_set_reader(
        ores::http::routes::read_value_set_tables(req, session),
        [&req, &session](std::string_view relation) {
            return catalogue::choices_of(req, session, relation);
        });
    const auto cells = cells_of(row_json);

    std::vector<const ores::refdata::domain::convention_kind_term*> mine;
    for (const auto& term : stated)
        if (term.kind_code == kind_code && (held.empty() || held.contains(term.entity_name)))
            mine.push_back(&term);
    std::ranges::stable_sort(mine, {}, [](const auto* term) { return term->position; });

    std::vector<term_view> out;
    out.reserve(mine.size());
    for (const auto* term : mine) {
        const auto found = entries.find(contract_key(term->schema_name, term->xpath));
        const auto* entry = found == entries.end() ? nullptr : found->second;
        const auto value = [&cells](const std::string& column) {
            const auto stated_value = cells.find(column);
            return stated_value == cells.end() ? document::cell() : stated_value->second;
        }(term->column_name);
        term_view drawn;
        drawn.entity = term->entity_name;
        drawn.column = term->column_name;
        drawn.xpath = term->xpath;
        drawn.value = value;
        drawn.documented = entry != nullptr;
        drawn.required = entry != nullptr && entry->is_required;
        if (entry == nullptr) {
            drawn.scope = step(term->xpath, 1, false);
            drawn.ore = step(term->xpath, 0, true);
            out.push_back(std::move(drawn));
            continue;
        }
        drawn.scope = entry->scope;
        drawn.ore = entry->name;
        drawn.label = entry->label;
        drawn.help = entry->help;
        drawn.description = entry->description;
        drawn.default_text = entry->default_text;
        drawn.format = entry->format;
        drawn.introduced_in = entry->introduced_in;
        drawn.removed_in = entry->removed_in;
        drawn.fields = values.fields_of(entry->scope, entry->name);
        out.push_back(std::move(drawn));
    }
    return out;
}

std::string json_of(const term_view& term) {
    auto members =
        std::vector<std::string>{member("entity", json::write(term.entity)),
                                 member("column", json::write(term.column)),
                                 member("scope", json::write(term.scope)),
                                 member("ore", json::write(term.ore)),
                                 member("xpath", json::write(term.xpath)),
                                 member("value", json::write(term.value)),
                                 member("documented", term.documented ? "true" : "false"),
                                 member("required", term.required ? "true" : "false"),
                                 member("label", json::write(term.label)),
                                 member("help", json::write(term.help)),
                                 member("description", json::write(term.description)),
                                 member("defaultText", json::write(term.default_text)),
                                 member("format", json::write(term.format)),
                                 member("introducedIn", json::write(term.introduced_in)),
                                 member("removedIn", json::write(term.removed_in))};
    const auto values = ores::http::routes::value_fields_members(term.fields);
    members.insert(members.end(), values.begin(), values.end());
    return object(members);
}

std::string json_of(const std::vector<term_view>& terms) {
    std::vector<std::string> written;
    written.reserve(terms.size());
    for (const auto& term : terms)
        written.push_back(json_of(term));
    return ores::http::net::record_routes::array_of(written);
}

void name_term(ores::utility::domain::result& result, const std::vector<term_view>& terms) {
    if (result.outcome == ores::utility::domain::outcome::ok || !result.fields.empty())
        return;
    for (const auto& term : terms) {
        const auto named = " for " + term.scope + "." + term.ore + ": ";
        const auto at = result.message.find(named);
        if (at == std::string::npos)
            continue;
        // The store wraps the validation function's sentence in its own words and
        // a context line; the term shows the sentence alone.
        const auto from = result.message.rfind("Invalid value ", at);
        const auto to = result.message.find('\n', at);
        const auto sentence = result.message.substr(
            from == std::string::npos ? 0 : from,
            to == std::string::npos ? std::string::npos :
                                      to - (from == std::string::npos ? 0 : from));
        ores::utility::domain::field_failure failure;
        failure.field = term.column;
        failure.code = result.code;
        failure.message = sentence;
        while (!failure.message.empty() && failure.message.back() == ' ')
            failure.message.pop_back();
        result.fields.push_back(std::move(failure));
        return;
    }
}

}
