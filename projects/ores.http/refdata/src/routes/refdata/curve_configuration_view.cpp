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
#include "ores.http/routes/refdata/curve_configuration_view.hpp"
#include "ores.http.api/net/json_shape.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.core/routes/ore_value_set_tables.hpp"
#include "ores.http/routes/refdata/curve_section_wiring.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <algorithm>
#include <cctype>
#include <optional>
#include <string>
#include <utility>
#include <vector>

namespace ores::http::routes::refdata::curve_view {

namespace {

namespace json = ores::utility::codec::json;
namespace rr = ores::http::net::record_routes;
namespace wiring = ores::http::routes::refdata::wiring;
using ores::http::net::shape::member;
using ores::http::net::shape::object;
using ores::reporting::domain::ore_parameter;

/// The document element every curve leaf's path starts at.
constexpr std::string_view root_element = "CurveConfiguration";

/// The table every entry's identifier and description live in.
constexpr std::string_view definition_entity = "curve_definition";

/// The rows of an entry's own tables, by the table's entity, in the wiring's order.
using entry_rows = std::vector<std::pair<std::string_view, const document::row*>>;

/// How many leaves of the document the screen draws, and how many are documented.
struct leaf_count final {
    std::size_t documented = 0;
    std::size_t total = 0;
};

/// One leaf as the screen draws it, and whether the contract documents it.
struct drawn_leaf final {
    std::string json;
    bool documented = false;
};

/**
 * @brief The value one leaf holds, as the screen reads it, or nothing.
 */
document::cell value_of(const document::row* source, std::string_view column) {
    return source == nullptr ? document::cell() : document::cell_at(*source, column);
}

/// The rows of the section's tables that belong to the entry.
entry_rows rows_of_entry(const document::cells& document,
                         const wiring::curve_section& section,
                         std::string_view entry_id) {
    entry_rows found;
    for (const auto& entity : section.entities) {
        const auto* rows = document.find(entity.vector);
        if (rows == nullptr)
            continue;
        const auto row = std::ranges::find_if(*rows, [&](const document::row& candidate) {
            return document::text_of(candidate, section.definition_key) == entry_id;
        });
        if (row != rows->end())
            found.emplace_back(entity.entity, &*row);
    }
    return found;
}

const document::row* row_named(const entry_rows& rows, std::string_view entity) {
    const auto found =
        std::ranges::find_if(rows, [&](const auto& pair) { return pair.first == entity; });
    return found == rows.end() ? nullptr : found->second;
}

/// Whether a leaf states a flag as set, in the spellings ORE writes one.
bool is_set(std::string_view value) {
    std::string lowered;
    lowered.reserve(value.size());
    for (const auto character : value)
        lowered.push_back(static_cast<char>(std::tolower(static_cast<unsigned char>(character))));
    const auto first = lowered.find_first_not_of(" \t\r\n");
    if (first == std::string::npos)
        return false;
    const auto last = lowered.find_last_not_of(" \t\r\n");
    const auto word = lowered.substr(first, last - first + 1);
    return word == "true" || word == "t" || word == "y" || word == "yes" || word == "1";
}

/// One field of the contract entry, empty for a leaf no contract entry holds.
std::string parameter_field(const ore_parameter* entry, std::string ore_parameter::* field) {
    return entry == nullptr ? std::string() : entry->*field;
}

/**
 * @brief One leaf of one entry, with the words the contract gives it and the
 * values the value sets accept.
 */
drawn_leaf leaf_json(std::string_view entity,
                     std::string_view column,
                     std::string_view ore,
                     const std::string& xpath,
                     const document::cell& value,
                     const vocabulary& words) {
    const auto found = words.parameters.find(xpath);
    const auto* entry = found == words.parameters.end() ? nullptr : found->second;
    auto fields = ores::http::routes::value_fields{};
    if (entry != nullptr)
        fields = words.values.fields_of(entry->scope, entry->name);
    auto members = std::vector<std::string>{
        member("entity", json::write(std::string(entity))),
        member("column", json::write(std::string(column))),
        member("ore", json::write(std::string(ore))),
        member("xpath", json::write(xpath)),
        member("value", json::write(value)),
        member("documented", entry == nullptr ? "false" : "true"),
        member("required", entry != nullptr && entry->is_required ? "true" : "false"),
        member("label", json::write(parameter_field(entry, &ore_parameter::label))),
        member("help", json::write(parameter_field(entry, &ore_parameter::help))),
        member("description", json::write(parameter_field(entry, &ore_parameter::description))),
        member("defaultText", json::write(parameter_field(entry, &ore_parameter::default_text))),
        member("format", json::write(parameter_field(entry, &ore_parameter::format))),
        member("introducedIn", json::write(parameter_field(entry, &ore_parameter::introduced_in))),
        member("removedIn", json::write(parameter_field(entry, &ore_parameter::removed_in)))};
    const auto values = ores::http::routes::value_fields_members(fields);
    members.insert(members.end(), values.begin(), values.end());
    return {object(members), entry != nullptr};
}

/**
 * @brief Whether the entry states one child element, or nothing for an element
 * the entry records in no column.
 */
std::optional<bool> block_presence(const entry_rows& rows, const wiring::curve_block& block) {
    if (!block.presence_entity.empty())
        return row_named(rows, block.presence_entity) != nullptr;
    if (block.presence.empty())
        return std::nullopt;
    for (const auto& pair : rows) {
        const auto* row = pair.second;
        if (row == nullptr)
            continue;
        const auto stated = row->find(block.presence);
        if (stated == row->end())
            continue;
        if (!stated->second)
            return std::nullopt;
        const auto& value = *stated->second;
        return value == "true" || value == "Y" || value == "yes";
    }
    return std::nullopt;
}

/// The quotes one segment holds, in the order the document wrote them.
std::string quotes_json(const document::cells& document,
                        std::string_view parent_key,
                        std::string_view segment_id) {
    std::vector<const document::row*> found;
    for (const auto& row : document.at("quotes"))
        if (document::text_of(row, parent_key) == segment_id)
            found.push_back(&row);
    std::ranges::stable_sort(
        found, {}, [](const document::row* row) { return document::number_of(*row, "position"); });
    std::vector<std::string> quotes;
    quotes.reserve(found.size());
    for (const auto* row : found)
        quotes.push_back(
            object({member("id", json::write(document::text_of(*row, "id"))),
                    member("list", json::write(document::text_of(*row, "quote_list"))),
                    member("text", json::write(document::text_of(*row, "quote_text"))),
                    member("optional",
                           is_set(document::text_of(*row, "optional_flag")) ? "true" : "false"),
                    member("rateQuote", json::write(document::text_of(*row, "rate_quote"))),
                    member("spreadQuote", json::write(document::text_of(*row, "spread_quote")))}));
    return rr::array_of(quotes);
}

/**
 * @brief The leaves of one row of a collection, which are the ones its kind
 * reads.
 *
 * A collection's table is one flat table for every kind, so a row holds a column
 * for every leaf of every kind. What a kind reads is the projection's dispatch,
 * and the wiring states it: a Simple segment reads its conventions and its
 * projection curve, a Direct one reads its conventions alone.
 */
std::vector<drawn_leaf> collection_leaves(const document::cells& document,
                                          std::string_view section,
                                          std::string_view entry_element,
                                          const wiring::curve_collection& rule,
                                          std::string_view kind,
                                          std::string_view row_id,
                                          const vocabulary& words) {
    std::vector<drawn_leaf> leaves;
    if (kind.empty())
        return leaves;
    const auto declared = std::ranges::find_if(
        rule.kinds, [&](const auto& candidate) { return candidate.kind == kind; });
    if (declared == rule.kinds.end())
        return leaves;
    const auto path = "/" + std::string(root_element) + "/" + std::string(section) + "/" +
                      std::string(entry_element) + "/" + std::string(rule.element) + "/" +
                      std::string(kind) + "/";
    for (const auto& leaf : declared->leaves) {
        const document::row* part = nullptr;
        if (const auto* rows = document.find(leaf.vector)) {
            const auto found = std::ranges::find_if(*rows, [&](const document::row& candidate) {
                return document::text_of(candidate, rule.part_parent) == row_id;
            });
            if (found != rows->end())
                part = &*found;
        }
        leaves.push_back(leaf_json(leaf.entity,
                                   leaf.column,
                                   leaf.ore,
                                   path + std::string(leaf.ore),
                                   value_of(part, leaf.column),
                                   words));
    }
    return leaves;
}

/// The child collections an entry holds rows of, in the order the wiring states.
std::vector<std::string> collections_json(const document::cells& document,
                                          const wiring::curve_section& section,
                                          std::string_view entry_id,
                                          const vocabulary& words,
                                          leaf_count& counts) {
    std::vector<std::string> collections;
    for (const auto& rule : section.collections) {
        const auto* rows = document.find(rule.vector);
        const auto declared = std::ranges::find_if(
            section.blocks, [&](const auto& block) { return block.element == rule.element; });
        if (declared == section.blocks.end() && (rows == nullptr || rows->empty()))
            continue;
        std::vector<std::string> written;
        if (rows != nullptr) {
            for (const auto& row : *rows) {
                if (document::text_of(row, rule.parent_key) != entry_id)
                    continue;
                const auto kind = [&] {
                    const auto found =
                        words.segment_kinds.find(document::text_of(row, rule.kind_column));
                    return found == words.segment_kinds.end() ? std::string() : found->second;
                }();
                const auto row_id = document::text_of(row, "id");
                std::vector<std::string> leaves;
                for (auto& leaf : collection_leaves(document,
                                                    section.section,
                                                    section.entry_element,
                                                    rule,
                                                    kind,
                                                    row_id,
                                                    words)) {
                    ++counts.total;
                    counts.documented += leaf.documented ? 1 : 0;
                    leaves.push_back(std::move(leaf.json));
                }
                written.push_back(
                    object({member("id", json::write(row_id)),
                            member("kind", json::write(kind)),
                            member("leaves", rr::array_of(leaves)),
                            member("quotes",
                                   rule.quote_parent.empty() ?
                                       "[]" :
                                       quotes_json(document, rule.quote_parent, row_id))}));
            }
        }
        collections.push_back(object({member("element", json::write(std::string(rule.element))),
                                      member("rows", rr::array_of(written))}));
    }
    return collections;
}

/// One entry of a section: its definition row and the rows of its own tables.
std::string entry_json(const document::row& definition,
                       const document::cells& document,
                       const wiring::curve_section* section,
                       const vocabulary& words,
                       leaf_count& counts) {
    const auto id = document::text_of(definition, "id");
    const auto rows = section == nullptr ? entry_rows{} : rows_of_entry(document, *section, id);
    std::vector<std::string> parameters;
    std::vector<std::string> blocks;
    std::vector<std::string> collections;
    if (section != nullptr) {
        for (const auto& parameter : section->parameters) {
            const auto* source = parameter.entity == definition_entity ?
                                     &definition :
                                     row_named(rows, parameter.entity);
            auto leaf = leaf_json(parameter.entity,
                                  parameter.column,
                                  parameter.ore,
                                  "/" + std::string(parameter.xpath),
                                  value_of(source, parameter.column),
                                  words);
            ++counts.total;
            counts.documented += leaf.documented ? 1 : 0;
            parameters.push_back(std::move(leaf.json));
        }
        for (const auto& block : section->blocks)
            blocks.push_back(object({member("element", json::write(std::string(block.element))),
                                     member("present", json::write(block_presence(rows, block)))}));
        collections = collections_json(document, *section, id, words, counts);
    }
    return object({member("id", json::write(id)),
                   member("curveId", json::write(document::text_of(definition, "curve_id"))),
                   member("description", json::write(document::text_of(definition, "description"))),
                   member("version", std::to_string(document::number_of(definition, "version"))),
                   member("position", std::to_string(document::number_of(definition, "position"))),
                   member("parameters", rr::array_of(parameters)),
                   member("blocks", rr::array_of(blocks)),
                   member("collections", rr::array_of(collections))});
}

/**
 * @brief The sections in the order the schema declares them.
 *
 * A section the wiring does not name is listed after the ones it does, and says
 * that its columns are unknown, because a section the data holds and the
 * projection does not map yet is a gap to show and not a section to hide.
 */
std::vector<const ores::refdata::domain::curve_section*>
ordered_sections(const std::vector<ores::refdata::domain::curve_section>& rows) {
    const auto& table = wiring::sections();
    const auto order_of = [&](std::string_view code) -> std::size_t {
        const auto found = std::ranges::find_if(
            table, [&](const wiring::curve_section& section) { return section.section == code; });
        return found == table.end() ? table.size() :
                                      static_cast<std::size_t>(found - table.begin());
    };
    std::vector<const ores::refdata::domain::curve_section*> ordered;
    ordered.reserve(rows.size());
    for (const auto& row : rows)
        ordered.push_back(&row);
    std::ranges::stable_sort(
        ordered, {}, [&](const auto* row) { return std::pair(order_of(row->code), row->code); });
    return ordered;
}

}

std::string view_json_of(const document::cells& document,
                         std::string_view configuration_id,
                         const std::vector<ores::refdata::domain::curve_section>& sections,
                         const vocabulary& words) {
    leaf_count counts;
    const auto& definitions = document.at("definitions");
    std::vector<std::string> drawn_sections;
    for (const auto* section_row : ordered_sections(sections)) {
        const auto* section = wiring::section_of(section_row->code);
        std::vector<std::string> entries;
        std::vector<const document::row*> found;
        for (const auto& definition : definitions)
            if (document::text_of(definition, "section_code") == section_row->code)
                found.push_back(&definition);
        std::ranges::stable_sort(found, {}, [](const document::row* row) {
            return document::number_of(*row, "position");
        });
        entries.reserve(found.size());
        for (const auto* definition : found)
            entries.push_back(entry_json(*definition, document, section, words, counts));
        const auto entry_element =
            !section_row->entry_element.empty() ?
                section_row->entry_element :
                (section == nullptr ? std::string() : std::string(section->entry_element));
        drawn_sections.push_back(
            object({member("code", json::write(section_row->code)),
                    member("entryElement", json::write(entry_element)),
                    member("description", json::write(section_row->description)),
                    member("mapped", section == nullptr ? "false" : "true"),
                    member("entries", rr::array_of(entries))}));
    }

    const auto& header = document.header();
    return object({member("configurationId", json::write(std::string(configuration_id))),
                   member("id", json::write(document::text_of(header, "id"))),
                   member("name", json::write(document::text_of(header, "name"))),
                   member("description", json::write(document::text_of(header, "description"))),
                   member("version", std::to_string(document::number_of(header, "version"))),
                   member("partyId", json::write(document::text_of(header, "party_id"))),
                   member("modifiedBy", json::write(document::text_of(header, "modified_by"))),
                   member("recordedAt", json::write(document::text_of(header, "recorded_at"))),
                   member("documented", std::to_string(counts.documented)),
                   member("total", std::to_string(counts.total)),
                   member("sections", rr::array_of(drawn_sections))});
}

}
