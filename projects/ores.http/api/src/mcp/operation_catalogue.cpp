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
#include "ores.http.api/mcp/operation_catalogue.hpp"
#include "ores.utility/codec/json_document.hpp"
#include <algorithm>
#include <cctype>
#include <cmath>

namespace ores::http::mcp {

using ores::utility::codec::json_document;
using ores::utility::codec::json_value;

namespace {

/// BM25's term-frequency saturation and length normalisation, at the values
/// Robertson and Zaragoza recommend.
constexpr double k1 = 1.2;
constexpr double b = 0.75;

/// The weight of the share of a tool's own name the words cover: the name is
/// the operation's shortest description, so a name the words say in full
/// outranks a longer one that repeats a word of them.
constexpr double name_weight = 2.0;

std::string fold(std::string word) {
    if (word.size() > 4 && word.ends_with("ies"))
        return word.substr(0, word.size() - 3) + "y";
    if (word.size() > 3 && word.ends_with('s') && !word.ends_with("ss"))
        word.pop_back();
    return word;
}

/// A value for a JSON Schema node, nested no deeper than a model needs to see.
void place_example(json_value target,
                   std::string_view name,
                   const json_value& schema,
                   const json_value& defs,
                   int depth) {
    if (const auto ref = schema.member("$ref").text_or(); ref.starts_with("#/$defs/")) {
        const auto found = defs.member(ref.substr(8));
        if (found.exists() && depth < 6)
            place_example(target, name, found, defs, depth + 1);
        else
            target.add_object(name);
        return;
    }
    const auto type = schema.member("type").text_or();
    const auto format = schema.member("format").text_or();
    if (const auto choices = schema.member("enum"); choices.is_array() && choices.size() > 0) {
        target.add(name, choices.element(0));
        return;
    }
    if (type == "object" && depth < 6) {
        auto object = target.add_object(name);
        for (const auto& [member, member_schema] : schema.member("properties").members())
            place_example(object, member, member_schema, defs, depth + 1);
        return;
    }
    if (type == "array" && depth < 6) {
        auto list = target.add_array(name);
        auto holder = json_document::make_object();
        place_example(holder.root(), "item", schema.member("items"), defs, depth + 1);
        list.push(holder.root().member("item"));
        return;
    }
    if (type == "integer" || type == "number") {
        target.set(name, std::int64_t{0});
        return;
    }
    if (type == "boolean") {
        target.set(name, false);
        return;
    }
    if (format == "uuid") {
        target.set(name, "00000000-0000-0000-0000-000000000000");
        return;
    }
    if (format == "date") {
        target.set(name, "2026-01-31");
        return;
    }
    if (format == "date-time") {
        target.set(name, "2026-01-31T00:00:00Z");
        return;
    }
    target.set(name, "text");
}

}

std::vector<std::string> search_terms(std::string_view text) {
    std::vector<std::string> out;
    std::string word;
    const auto flush = [&] {
        if (!word.empty())
            out.push_back(fold(std::move(word)));
        word.clear();
    };
    for (const auto c : text) {
        if (std::isalnum(static_cast<unsigned char>(c)))
            word.push_back(static_cast<char>(std::tolower(static_cast<unsigned char>(c))));
        else
            flush();
    }
    flush();
    return out;
}

operation_catalogue::operation_catalogue(const std::vector<domain::route>& routes) {
    for (const auto& route : routes) {
        if (route.catalogue.transport || route.sink || !route.catalogue.declared())
            continue;
        operation op;
        op.tool = route.catalogue.tool;
        op.area = route.catalogue.area;
        op.entity = route.catalogue.entity;
        op.summary = route.catalogue.summary;
        op.description = route.catalogue.description;
        op.writes = route.catalogue.writes;
        op.method = route.method;
        op.pattern = route.pattern;
        op.path_params = route.param_names;
        op.query_params = route.query_params;
        op.body = route.body_schema;
        op.required_roles = route.required_roles;
        op.handler = route.handler;
        by_tool_.emplace(op.tool, operations_.size());
        operations_.push_back(std::move(op));
    }

    std::size_t total = 0;
    for (const auto& op : operations_) {
        std::unordered_map<std::string, std::size_t> counts;
        std::size_t length = 0;
        for (const auto* field : {&op.tool, &op.area, &op.entity, &op.summary, &op.description})
            for (auto& term : search_terms(*field)) {
                ++counts[term];
                ++length;
            }
        for (const auto& [term, count] : counts)
            ++document_frequency_[term];
        term_counts_.push_back(std::move(counts));
        lengths_.push_back(length);
        total += length;
    }
    if (!operations_.empty())
        average_length_ = static_cast<double>(total) / static_cast<double>(operations_.size());
}

const operation* operation_catalogue::find(std::string_view tool) const {
    const auto found = by_tool_.find(std::string(tool));
    return found == by_tool_.end() ? nullptr : &operations_[found->second];
}

std::vector<const operation*>
operation_catalogue::search(std::string_view words,
                            std::size_t limit,
                            const std::function<bool(const operation&)>& admits) const {
    const auto terms = search_terms(words);
    const auto documents = static_cast<double>(operations_.size());
    std::vector<std::pair<double, std::size_t>> scored;
    for (std::size_t i = 0; i < operations_.size(); ++i) {
        if (!admits(operations_[i]))
            continue;
        double score = 0;
        for (const auto& term : terms) {
            const auto count = term_counts_[i].find(term);
            if (count == term_counts_[i].end())
                continue;
            const auto df = static_cast<double>(document_frequency_.at(term));
            const auto idf = std::log(1.0 + (documents - df + 0.5) / (df + 0.5));
            const auto tf = static_cast<double>(count->second);
            const auto norm = 1.0 - b + b * static_cast<double>(lengths_[i]) / average_length_;
            score += idf * tf * (k1 + 1.0) / (tf + k1 * norm);
        }
        if (score > 0) {
            const auto name = search_terms(operations_[i].tool);
            const auto covered = std::ranges::count_if(name, [&](const std::string& t) {
                return std::ranges::find(terms, t) != terms.end();
            });
            score += name_weight * static_cast<double>(covered) /
                     static_cast<double>(std::max<std::size_t>(1, name.size()));
            scored.emplace_back(score, i);
        }
    }
    std::ranges::stable_sort(scored,
                             [](const auto& l, const auto& r) { return l.first > r.first; });
    std::vector<const operation*> out;
    for (std::size_t i = 0; i < scored.size() && i < limit; ++i)
        out.push_back(&operations_[scored[i].second]);
    return out;
}

std::string input_schema_of(const operation& op) {
    auto doc = json_document::make_object();
    auto root = doc.root();
    root.set("type", "object");
    auto properties = root.add_object("properties");
    auto required = root.add_array("required");
    for (const auto& name : op.path_params) {
        properties.add_object(name).set("type", "string");
        required.push(name);
    }
    for (const auto& param : op.query_params) {
        auto property = properties.add_object(param.name);
        property.set("type", param.type);
        if (!param.format.empty())
            property.set("format", param.format);
        if (!param.description.empty())
            property.set("description", param.description);
        if (param.required)
            required.push(param.name);
    }
    if (op.body) {
        // The body's definitions move to the root, where its references
        // point, so the schema an MCP client reads resolves.
        const auto schema = json_document::parse(op.body->json_schema);
        if (schema && schema->root().is_object()) {
            auto body = properties.add_object("body");
            for (const auto& [member, value] : schema->root().members()) {
                if (member != "$defs") {
                    body.add(member, value);
                    continue;
                }
                auto defs = root.add_object("$defs");
                for (const auto& [def, definition] : value.members())
                    defs.add(def, definition);
            }
        } else {
            properties.add_object("body").set("type", "object");
        }
        if (op.body->required)
            required.push("body");
    }
    root.set("additionalProperties", false);
    return doc.write();
}

std::string example_of(const operation& op) {
    const auto schema = json_document::parse(input_schema_of(op));
    auto doc = json_document::make_object();
    auto root = doc.root();
    const auto defs = schema->root().member("$defs");
    for (const auto& [name, property] : schema->root().member("properties").members())
        place_example(root, name, property, defs, 0);
    return doc.write();
}

}
