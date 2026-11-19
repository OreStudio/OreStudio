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
#include "ores.synthetic.core/service/scripted_twins.hpp"
#include "ores.synthetic.core/service/trade_population_planner.hpp"
#include "ores.trading.api/builders/scripted_builders.hpp"
#include "ores.trading.api/domain/instrument_batch_mapper.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "script_twin_recipes.hpp"
#include <chrono>
#include <format>
#include <type_traits>
#include <utility>

namespace ores::synthetic::service {

namespace {

namespace tb = ores::trading::builders;
using ores::trading::domain::instrument_batch;

template <typename T>
struct is_optional : std::false_type {};
template <typename T>
struct is_optional<std::optional<T>> : std::true_type {};

/// A column's value as the script reads it, or nothing for a column with no text form.
template <typename T>
std::optional<std::string> text_of(const T& value) {
    if constexpr (is_optional<T>::value) {
        if (!value)
            return std::nullopt;
        return text_of(*value);
    } else if constexpr (std::is_same_v<T, std::string>) {
        return value;
    } else if constexpr (std::is_same_v<T, bool>) {
        return std::string(value ? "true" : "false");
    } else if constexpr (std::is_same_v<T, double>) {
        return std::format("{}", value);
    } else if constexpr (std::is_integral_v<T>) {
        return std::to_string(value);
    } else if constexpr (std::is_same_v<T, std::chrono::year_month_day>) {
        return std::format("{:%Y-%m-%d}", value);
    } else if constexpr (requires { value.to_string(); }) {
        return value.to_string();
    } else {
        return std::nullopt;
    }
}

/// The named column of the row, as text; nothing when the row has no such column.
template <typename Row>
std::optional<std::string> column_of(const Row& row, std::string_view column) {
    std::optional<std::string> out;
    ores::utility::codec::apply_fields(
        [&](const auto&... field) {
            ((field.name == column ? (void)(out = text_of(row.*(field.member))) : (void)0), ...);
        },
        wire_fields(std::type_identity<Row>{}));
    return out;
}

/// Whether the row carries the role, as its column whose name ends in _role states it.
template <typename Row>
bool holds_role(const Row& row, std::string_view role) {
    bool holds = false;
    ores::utility::codec::apply_fields(
        [&](const auto&... field) {
            (
                [&] {
                    if (!field.name.ends_with("_role"))
                        return;
                    if (const auto value = text_of(row.*(field.member)))
                        holds = holds || *value == role;
                }(),
                ...);
        },
        wire_fields(std::type_identity<Row>{}));
    return holds;
}

/// The column of every row of the named array, under the role when one is given.
std::vector<std::string> rows_column(const instrument_batch& batch,
                                     std::string_view array,
                                     std::optional<std::string_view> role,
                                     std::string_view column) {
    std::vector<std::string> out;
    ores::utility::codec::apply_fields(
        [&](const auto&... field) {
            (
                [&] {
                    if (field.name != array)
                        return;
                    for (const auto& row : batch.*(field.member)) {
                        if (role && !holds_role(row, *role))
                            continue;
                        if (auto value = column_of(row, column))
                            out.push_back(std::move(*value));
                    }
                }(),
                ...);
        },
        wire_fields(std::type_identity<instrument_batch>{}));
    return out;
}

/// The column of the native product row: the row whose trade type is the native one.
std::vector<std::string> product_column(const instrument_batch& batch,
                                        std::string_view trade_type_code,
                                        std::string_view column) {
    std::vector<std::string> out;
    ores::utility::codec::apply_fields(
        [&](const auto&... field) {
            (
                [&] {
                    if (!out.empty())
                        return;
                    for (const auto& row : batch.*(field.member)) {
                        using Row = std::decay_t<decltype(row)>;
                        if constexpr (requires { row.identity.trade_type_code; }) {
                            if (row.identity.trade_type_code != trade_type_code)
                                continue;
                            if (auto value = column_of(row, column))
                                out.push_back(std::move(*value));
                            return;
                        } else {
                            (void)sizeof(Row);
                        }
                    }
                }(),
                ...);
        },
        wire_fields(std::type_identity<instrument_batch>{}));
    return out;
}

std::vector<std::string> one_source(const instrument_batch& batch,
                                    std::string_view trade_type_code,
                                    std::string_view source) {
    if (source.size() >= 2 && source.front() == '\'' && source.back() == '\'')
        return {std::string(source.substr(1, source.size() - 2))};
    const auto dot = source.rfind('.');
    if (dot == std::string_view::npos)
        throw trade_population_error("The twin recipe source '" + std::string(source) +
                                     "' names no column.");
    const auto rows = source.substr(0, dot);
    const auto column = source.substr(dot + 1);
    if (rows == "product")
        return product_column(batch, trade_type_code, column);
    const auto bracket = rows.find('[');
    if (bracket == std::string_view::npos)
        return rows_column(batch, rows, std::nullopt, column);
    const auto role = rows.substr(bracket + 1, rows.size() - bracket - 2);
    return rows_column(batch, rows.substr(0, bracket), role, column);
}

std::string transformed(std::string value, std::string_view transform) {
    if (transform == "sign") {
        if (value == "Long" || value == "Call" || value == "true")
            return "1";
        if (value == "Short" || value == "Put" || value == "false")
            return "-1";
    } else if (transform == "flag") {
        if (value == "true")
            return "1";
        if (value == "false")
            return "0";
    }
    return value;
}

boost::uuids::uuid twin_id_for(const boost::uuids::uuid& native) {
    auto id = native;
    id.data[14] = static_cast<unsigned char>(id.data[14] ^ 0xA5);
    return id;
}

}

std::vector<std::string> twin_source_values(const instrument_batch& batch,
                                            std::string_view trade_type_code,
                                            std::string_view source) {
    std::string_view rest = source;
    while (true) {
        const auto split = rest.find(" or ");
        const auto first = rest.substr(0, split);
        auto values = one_source(batch, trade_type_code, first);
        if (!values.empty() || split == std::string_view::npos)
            return values;
        rest = rest.substr(split + 4);
    }
}

std::optional<domain::trade_population_slot>
scripted_twin_of(const domain::trade_population_slot& native) {
    std::vector<const script_twin_recipe*> recipe;
    for (const auto& row : script_twin_recipes)
        if (row.trade_type_code == native.trade_type_code)
            recipe.push_back(&row);
    if (recipe.empty())
        return std::nullopt;

    std::vector<ores::trading::domain::script_input> inputs;
    std::vector<tb::scripted_variable> variables;
    int sequence = 0;
    for (const auto* row : recipe) {
        ores::trading::domain::script_input input;
        input.script_name = std::string(row->script_name);
        input.sequence_number = ++sequence;
        input.input_name = std::string(row->input_name);
        input.data_type = std::string(row->data_type);
        input.is_array = row->is_array;
        inputs.push_back(std::move(input));

        auto values = twin_source_values(native.instruments, native.trade_type_code, row->source);
        if (values.empty())
            throw trade_population_error(
                "The " + native.trade_type_code + " twin finds no value for " +
                std::string(row->input_name) + " at " + std::string(row->source) + ".");
        for (auto& value : values)
            value = transformed(std::move(value), row->transform);
        tb::scripted_variable variable{.name = std::string(row->input_name),
                                       .type = std::string(row->data_type),
                                       .value = std::string()};
        if (row->is_array)
            variable.value = std::move(values);
        else
            variable.value = std::move(values.front());
        variables.push_back(std::move(variable));
    }
    auto built =
        tb::scripted_trade_builder({.script_name = std::string(recipe.front()->script_name),
                                    .inputs = std::move(inputs),
                                    .variables = std::move(variables)})
            .build();
    if (!built)
        throw trade_population_error("The " + native.trade_type_code + " twin is refused at " +
                                     built.error().term + ": " + built.error().message);

    domain::trade_population_slot twin = native;
    twin.trade_id = twin_id_for(native.trade_id);
    twin.trade_type_code = "ScriptedTrade";
    twin.internal_identifier = native.internal_identifier + "-scripted";
    twin.instruments = instrument_batch{};
    ores::trading::domain::append_instrument(
        twin.instruments, ores::trading::domain::trade_instrument{std::move(*built)});
    twin.bond_issue.reset();
    twin.structure_id.reset();
    twin.structure_role.clear();
    twin.structure_sequence = 0;
    return twin;
}

void add_scripted_twins(domain::trade_population_plan& plan) {
    const auto natives = plan.slots.size();
    for (std::size_t i = 0; i < natives; ++i)
        if (auto twin = scripted_twin_of(plan.slots[i]))
            plan.slots.push_back(std::move(*twin));
}

}
