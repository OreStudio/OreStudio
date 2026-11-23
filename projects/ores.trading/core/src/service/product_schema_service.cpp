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
#include "ores.trading.core/service/product_schema_service.hpp"
#include <algorithm>
#include <array>
#include <deque>
#include <set>

namespace ores::trading::service {

namespace {

constexpr std::string_view trade_type_name = "trade";

/**
 * The schema type a field's value is, from record:TYPE or list:TYPE; empty for
 * a leaf, including a repeated one such as list:string.
 */
std::string schema_type_of(const std::string& value_domain) {
    constexpr std::array<std::string_view, 5> leaf_kinds = {"string", "boolean", "integer",
                                                            "double", "date"};
    for (const std::string_view prefix : {"record:", "list:"}) {
        if (!value_domain.starts_with(prefix))
            continue;
        auto type = value_domain.substr(prefix.size());
        return std::ranges::find(leaf_kinds, type) == leaf_kinds.end() ? type : std::string{};
    }
    return {};
}

messaging::product_field to_field(const ores::ore::domain::ore_parameter& p) {
    return {.type_name = p.subtype,
            .name = p.name,
            .position = p.position,
            .label = p.label,
            .help = p.help,
            .description = p.description,
            .value_domain = p.ore_parameter_value_domain_code,
            .is_required = p.is_required,
            .default_text = p.default_text,
            .format = p.format,
            .xpath = p.xpath};
}

std::vector<ores::ore::domain::ore_parameter> in_order(
    const instrument_fields& fields, const std::string& type) {
    const auto it = fields.find(type);
    if (it == fields.end())
        return {};
    auto out = it->second;
    std::ranges::sort(out, {}, &ores::ore::domain::ore_parameter::position);
    return out;
}

/**
 * The element row of a trade type, whose XPath names the trade type, and then
 * the fields of every type reachable from it, each type once, breadth first. A
 * type a field names that the fields hold nothing for is added to missing.
 */
std::vector<messaging::product_field> fields_of(const std::string& trade_type,
                                                const instrument_fields& fields,
                                                std::vector<std::string>& missing) {
    const auto prefix = "/Trade[TradeType='" + trade_type + "']/";
    std::vector<messaging::product_field> out;
    std::deque<std::string> pending;
    // The seed writes one row of the trade per trade type, its element, so the
    // first row under the trade type's prefix is the only one.
    for (const auto& row : in_order(fields, std::string(trade_type_name))) {
        if (row.xpath.starts_with(prefix)) {
            out.push_back(to_field(row));
            pending.push_back(schema_type_of(row.ore_parameter_value_domain_code));
            break;
        }
    }
    std::set<std::string> seen;
    while (!pending.empty()) {
        const auto type = pending.front();
        pending.pop_front();
        if (type.empty() || !seen.insert(type).second)
            continue;
        if (!fields.contains(type)) {
            missing.push_back(type);
            continue;
        }
        for (const auto& row : in_order(fields, type)) {
            out.push_back(to_field(row));
            pending.push_back(schema_type_of(row.ore_parameter_value_domain_code));
        }
    }
    return out;
}

/**
 * The script a name resolves to: the tenant's own where it has one, which its
 * bookings resolve before the world library's, whatever the order of the rows.
 */
const domain::script* script_named(const product_catalogue& catalogue, const std::string& name) {
    const domain::script* found = nullptr;
    for (const auto& s : catalogue.scripts) {
        if (s.name != name)
            continue;
        if (found == nullptr || found->tenant_id.is_system())
            found = &s;
    }
    return found;
}

void add_scripted_form(messaging::describe_product_response& out,
                       const product_catalogue& catalogue, const domain::script& script) {
    out.forms.push_back({.kind = "scripted",
                         .script_name = script.name,
                         .product_tag = script.product_tag.value_or("")});
    for (const auto& i : catalogue.inputs)
        if (i.script_name == script.name && i.tenant_id == script.tenant_id &&
            i.script_version == script.version)
            out.inputs.push_back(i);
    for (const auto& c : catalogue.codes)
        if (c.script_name == script.name && c.tenant_id == script.tenant_id &&
            c.script_version == script.version)
            out.codes.push_back(c);
    std::ranges::sort(out.inputs, {}, &domain::script_input::sequence_number);
    std::ranges::sort(out.codes, {}, &domain::script_input_code::sequence_number);
}

}

messaging::describe_product_response describe_product(const std::string& code,
                                                      const product_catalogue& catalogue,
                                                      const instrument_fields& fields) {
    messaging::describe_product_response out;
    const auto pt = std::ranges::find(catalogue.product_types, code,
                                      &domain::product_type::code);
    if (pt == catalogue.product_types.end()) {
        const auto* script = script_named(catalogue, code);
        if (script == nullptr) {
            out.message = "No product type or library script is named " + code + ".";
            return out;
        }
        out.product = {.code = code, .name = code, .construction = "script"};
        add_scripted_form(out, catalogue, *script);
        out.success = true;
        return out;
    }

    out.product = {.code = pt->code, .name = pt->name, .construction = pt->construction};
    std::vector<std::string> missing;
    out.fields = fields_of(code, fields, missing);
    if (!missing.empty()) {
        out.fields.clear();
        out.message = "The fields of the product type " + code + " name the schema type " +
                      missing.front() + ", which the trading service's cache of ores.ore's "
                      "parameters holds no fields for.";
        return out;
    }
    if (!out.fields.empty()) {
        out.element = out.fields.front().name;
        out.forms.push_back({.kind = "native"});
    }
    std::set<std::string> twinned;
    for (const auto& t : catalogue.twins)
        if (t.trade_type_code == code)
            twinned.insert(t.script_name);
    for (const auto& name : twinned)
        if (const auto* script = script_named(catalogue, name))
            add_scripted_form(out, catalogue, *script);

    if (out.forms.empty()) {
        out.message = pt->instrument_table.empty()
                          ? "ORE has no trade builder for the product type " + code +
                                ": the catalogue routes it to no instrument table, so it is "
                                "booked only as a ScriptedTrade over a library script."
                          : "The product type " + code + " has no element in ORE's schema and "
                                "no scripted form, so it has no fields to describe.";
        return out;
    }
    out.success = true;
    return out;
}

messaging::list_products_response list_products(const product_catalogue& catalogue) {
    messaging::list_products_response out;
    auto types = catalogue.product_types;
    std::ranges::sort(types, {}, &domain::product_type::display_order);
    std::set<std::string> named;
    for (const auto& pt : types) {
        out.products.push_back(
            {.code = pt.code, .name = pt.name, .construction = pt.construction});
        named.insert(pt.code);
    }
    for (const auto& t : catalogue.twins)
        named.insert(t.script_name);
    std::set<std::string> scripts;
    for (const auto& s : catalogue.scripts)
        if (!named.contains(s.name))
            scripts.insert(s.name);
    for (const auto& name : scripts)
        out.products.push_back({.code = name, .name = name, .construction = "script"});
    out.success = true;
    return out;
}

}
