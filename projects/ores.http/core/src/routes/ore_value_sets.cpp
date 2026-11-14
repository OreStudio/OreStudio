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
#include "ores.http.core/routes/ore_value_sets.hpp"
#include <algorithm>
#include <array>
#include <utility>

namespace ores::http::routes {

namespace {

constexpr std::array<std::string_view, 8> kinds{
    "text", "integer", "number", "range", "enumerated", "catalogue", "regex", "boolean"};

constexpr std::string_view boolean_kind = "boolean";
constexpr std::string_view catalogue_kind = "catalogue";
constexpr std::string_view enumerated_kind = "enumerated";

/// The built-in values of a boolean set.
const std::vector<value_choice>& boolean_choices() {
    static const std::vector<value_choice> choices{{"true", "true"}, {"false", "false"}};
    return choices;
}

/**
 * @brief The scope a setting's link row names, in the words the tables use.
 *
 * Three scopes travel under a longer word on the document side, so a setting the
 * data links as =model= is the one the document writes as =ModelParameters=.
 */
std::string link_scope_of(std::string_view scope) {
    if (scope == "model")
        return "ModelParameters";
    if (scope == "engine")
        return "EngineParameters";
    return std::string(scope);
}

/// The key a setting travels under in the link table.
std::string setting_key(std::string_view scope, std::string_view name) {
    return link_scope_of(scope) + "|" + std::string(name);
}

bool known_kind(std::string_view kind) {
    return std::ranges::find(kinds, kind) != kinds.end();
}

}

value_set_error::value_set_error(const std::string& sentence)
    : std::runtime_error(sentence) {}

std::span<const std::string_view> source_kinds() {
    return kinds;
}

value_set_reader::value_set_reader(value_set_tables tables, relation_reader catalogue_of)
    : catalogue_of_(std::move(catalogue_of)) {
    for (auto& link : tables.links)
        set_for_setting_.emplace(std::move(link.scope) + "|" + std::move(link.name),
                                 std::move(link.value_set_code));

    for (auto& set : tables.sets)
        set_of_.emplace(set.code, std::move(set));

    for (auto& catalogue : tables.catalogues)
        relation_of_.emplace(std::move(catalogue.code), std::move(catalogue.relation));

    std::map<std::string, std::vector<std::pair<int, value_choice>>> ordered;
    for (auto& row : tables.values) {
        // A row that names a canonical form is another rendering of one, not a
        // choice: a configuration holds the form, and a control offers it once.
        if (!row.canonical_value.empty())
            continue;
        ordered[row.value_set_code].emplace_back(
            row.display_order, value_choice{std::move(row.value), std::move(row.label)});
    }
    for (auto& [code, entries] : ordered) {
        std::ranges::stable_sort(entries, {}, &std::pair<int, value_choice>::first);
        auto& choices = enumerated_[code];
        choices.reserve(entries.size());
        for (auto& entry : entries)
            choices.push_back(std::move(entry.second));
    }
}

const std::vector<value_choice>&
value_set_reader::relation_choices(std::string_view code, std::string_view relation) const {
    const auto known = by_relation_.find(std::string(relation));
    if (known != by_relation_.end())
        return known->second;
    if (!catalogue_of_)
        throw value_set_error("The value set \"" + std::string(code) +
                              "\" names the catalogue "
                              "relation \"" +
                              std::string(relation) + "\", which no reader knows.");
    auto choices = catalogue_of_(relation);
    std::erase_if(choices, [](const value_choice& choice) { return choice.value.empty(); });
    return by_relation_.emplace(std::string(relation), std::move(choices)).first->second;
}

std::vector<value_choice> value_set_reader::choices_of(std::string_view code,
                                                       const std::string& kind) const {
    if (kind == boolean_kind)
        return boolean_choices();
    if (kind == enumerated_kind) {
        const auto found = enumerated_.find(std::string(code));
        return found == enumerated_.end() ? std::vector<value_choice>{} : found->second;
    }
    if (kind == catalogue_kind) {
        const auto set = set_of_.find(std::string(code));
        const auto relation = set == set_of_.end() ? relation_of_.end() :
                                                     relation_of_.find(set->second.catalogue_code);
        if (relation == relation_of_.end())
            throw value_set_error("The value set \"" + std::string(code) +
                                  "\" names a catalogue that no catalogue row holds.");
        return relation_choices(code, relation->second);
    }
    return {};
}

value_fields value_set_reader::fields_of(std::string_view scope, std::string_view name) const {
    const auto key = setting_key(scope, name);
    const auto answered = answered_.find(key);
    if (answered != answered_.end())
        return answered->second;

    const auto link = set_for_setting_.find(key);
    const auto set = link == set_for_setting_.end() ? set_of_.end() : set_of_.find(link->second);
    if (set == set_of_.end())
        return answered_.emplace(key, value_fields{}).first->second;

    const auto& kind = set->second.source_kind;
    if (!known_kind(kind))
        throw value_set_error("The value set \"" + set->first + "\" states the source kind \"" +
                              kind + "\", which this build does not know.");
    value_fields found;
    found.value_set = set->first;
    found.source_kind = kind;
    found.catalogue = set->second.catalogue_code;
    found.choices = choices_of(set->first, kind);
    found.minimum = set->second.minimum;
    found.maximum = set->second.maximum;
    found.pattern = set->second.pattern;
    return answered_.emplace(key, std::move(found)).first->second;
}

}
