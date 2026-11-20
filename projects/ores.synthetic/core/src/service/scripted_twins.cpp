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
#include <boost/uuid/name_generator_sha1.hpp>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <charconv>
#include <chrono>
#include <format>
#include <iterator>
#include <type_traits>
#include <utility>
#include <variant>

namespace ores::synthetic::service {

namespace {

namespace tb = ores::trading::builders;

/// The namespace the twins' name-based ids are drawn in.
boost::uuids::uuid native_namespace() {
    return boost::uuids::string_generator()("6f1e2c0a-4b8d-4e2a-9a31-5c7d8e9f0a1b");
}
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
        // An empty text states no value, so a fallback source applies.
        if (value.empty())
            return std::nullopt;
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

/**
 * @brief The column of the native product row: the first row, across the
 * batch's arrays, whose trade type is the native one. A native batch holds one
 * product row; the search stops at the first match.
 */
std::vector<std::string> product_column(const instrument_batch& batch,
                                        std::string_view trade_type_code,
                                        std::string_view column) {
    std::vector<std::string> out;
    bool found = false;
    ores::utility::codec::apply_fields(
        [&](const auto&... field) {
            (
                [&] {
                    using Row = typename std::decay_t<decltype(batch.*(field.member))>::value_type;
                    // Only a product row states a trade type; the shared rows do not.
                    if constexpr (requires(const Row& r) { r.identity.trade_type_code; }) {
                        for (const auto& row : batch.*(field.member)) {
                            if (found || row.identity.trade_type_code != trade_type_code)
                                continue;
                            found = true;
                            if (auto value = column_of(row, column))
                                out.push_back(std::move(*value));
                        }
                    }
                }(),
                ...);
        },
        wire_fields(std::type_identity<instrument_batch>{}));
    return out;
}

/// The parts of a source between each separator, a quoted literal being read whole.
std::vector<std::string_view> parts_of(std::string_view source, std::string_view separator) {
    std::vector<std::string_view> out;
    bool quoted = false;
    std::size_t start = 0;
    for (std::size_t i = 0; i < source.size(); ++i) {
        if (source[i] == '\'')
            quoted = !quoted;
        else if (!quoted && source.substr(i).starts_with(separator)) {
            out.push_back(source.substr(start, i - start));
            start = i + separator.size();
            i = start - 1;
        }
    }
    out.push_back(source.substr(start));
    return out;
}

/**
 * @brief A whole schedule a source names, as instrument_schedules[<role>], and
 * the input a derived schedule is carried over, as ... based on <Input>.
 */
struct schedule_source final {
    std::string_view role;
    std::optional<std::string_view> base;
};

std::optional<schedule_source> schedule_source_of(std::string_view source) {
    constexpr std::string_view prefix = "instrument_schedules[";
    constexpr std::string_view based = " based on ";
    if (!source.starts_with(prefix))
        return std::nullopt;
    std::optional<std::string_view> base;
    if (const auto at = source.find(based); at != std::string_view::npos) {
        base = source.substr(at + based.size());
        source = source.substr(0, at);
    }
    if (!source.ends_with(']') || (base && base->find(' ') != std::string_view::npos))
        return std::nullopt;
    return schedule_source{.role = source.substr(prefix.size(), source.size() - prefix.size() - 1),
                           .base = base};
}

using twin_event = std::variant<tb::scripted_schedule, tb::scripted_dates, tb::scripted_derived>;

/**
 * @brief The schedule under the role, as its rules, its listed dates or, when
 * the source names the input it is carried over, its derivation; nothing when
 * no row states it. The twin carries one schedule block, so a role stated by
 * several rows is refused, as is a block with no date to carry. Each term is
 * carried as the native row states it, absent terms included.
 */
std::optional<twin_event> whole_schedule(const instrument_batch& batch,
                                         const schedule_source& source) {
    const auto role = source.role;
    const auto refuse = [&](const std::string& why) {
        return trade_population_error("The " + std::string(role) + " schedule " + why + ".");
    };
    const auto rows =
        std::ranges::count(batch.instrument_schedules,
                           role,
                           &ores::trading::domain::instrument_schedule::schedule_role);
    if (rows == 0)
        return std::nullopt;
    if (rows > 1)
        throw refuse("is stated by " + std::to_string(rows) + " blocks, and a twin carries one");
    const auto& row =
        *std::ranges::find(batch.instrument_schedules,
                           role,
                           &ores::trading::domain::instrument_schedule::schedule_role);
    if (row.schedule_kind == "dates") {
        tb::scripted_dates out{.calendar = row.calendar, .convention = row.convention};
        for (const auto& date : batch.instrument_schedule_dates)
            if (date.schedule_role == role && date.schedule_sequence_number == row.sequence_number)
                out.dates.push_back(date.schedule_date);
        if (out.dates.empty())
            throw refuse("lists no date");
        return out;
    }
    // The native row names its base by the native element, which the twin
    // states under its own input, so the recipe names that input.
    if (row.schedule_kind == "derived" && source.base) {
        if (!row.shift)
            throw refuse("states no shift");
        return tb::scripted_derived{.base = std::string(*source.base),
                                    .shift = *row.shift,
                                    .calendar = row.calendar,
                                    .convention = row.convention};
    }
    if (row.schedule_kind != "rules")
        throw refuse("is " + row.schedule_kind + ", which a twin cannot read");
    if (!row.start_date || !row.end_date || !row.tenor)
        throw refuse("states no start, end or tenor");
    return tb::scripted_schedule{.start = *row.start_date,
                                 .end = *row.end_date,
                                 .tenor = *row.tenor,
                                 .calendar = row.calendar,
                                 .convention = row.convention,
                                 .term_convention = row.term_convention,
                                 .rule = row.rule};
}

/**
 * @brief The event a source derives from another input of the twin, as
 * <Input> shifted <period> on <calendar> <convention>.
 */
std::optional<tb::scripted_derived> derived_of(std::string_view source) {
    const auto words = parts_of(source, " ");
    if (words.size() != 6 || words[1] != "shifted" || words[3] != "on")
        return std::nullopt;
    return tb::scripted_derived{.base = std::string(words[0]),
                                .shift = std::string(words[2]),
                                .calendar = std::string(words[4]),
                                .convention = std::string(words[5])};
}

/// Whether the source states an event: a whole schedule or a derived one.
bool is_event_source(std::string_view source) {
    return std::ranges::any_of(parts_of(source, " or "), [](std::string_view part) {
        return schedule_source_of(part) || derived_of(part);
    });
}

/**
 * @brief The first alternative of an event source that states an event. Every
 * alternative must state an event. A derived event always states one, so an
 * alternative after it is never read.
 */
std::optional<twin_event> event_of(const instrument_batch& batch, std::string_view source) {
    const auto parts = parts_of(source, " or ");
    for (const auto part : parts)
        if (!schedule_source_of(part) && !derived_of(part))
            throw trade_population_error("The twin recipe source '" + std::string(source) +
                                         "' mixes an event with the value '" + std::string(part) +
                                         "'.");
    for (const auto part : parts) {
        if (auto derived = derived_of(part))
            return std::move(*derived);
        if (auto schedule = whole_schedule(batch, *schedule_source_of(part)))
            return schedule;
    }
    return std::nullopt;
}

std::vector<std::string> one_source(const instrument_batch& batch,
                                    std::string_view trade_type_code,
                                    std::string_view source) {
    if (source.size() >= 2 && source.front() == '\'' && source.back() == '\'')
        return {std::string(source.substr(1, source.size() - 2))};
    if (schedule_source_of(source) || derived_of(source))
        throw trade_population_error("The twin recipe source '" + std::string(source) +
                                     "' states an event, which a value cannot fall back to.");
    const auto dot = source.rfind('.');
    if (dot == std::string_view::npos)
        throw trade_population_error("The twin recipe source '" + std::string(source) +
                                     "' names no column.");
    // A column may pick one of its values by position, as <column>[<n>].
    if (const auto open = source.rfind('['); open != std::string_view::npos && open > dot) {
        std::size_t n = 0;
        const auto digits = source.substr(open + 1, source.size() - open - 2);
        const auto [end, error] = std::from_chars(digits.data(), digits.data() + digits.size(), n);
        if (!source.ends_with(']') || error != std::errc{} ||
            end != digits.data() + digits.size() || n == 0)
            throw trade_population_error("The twin recipe source '" + std::string(source) +
                                         "' picks no value by position.");
        auto values = one_source(batch, trade_type_code, source.substr(0, open));
        if (values.size() < n)
            return {};
        return {std::move(values[n - 1])};
    }
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

/// A date as a column states it, YYYY-MM-DD.
std::chrono::year_month_day date_of(const std::string& text) {
    int y = 0;
    unsigned m = 0, d = 0;
    const auto read = [&](std::size_t at, std::size_t size, auto& out) {
        const auto* first = text.data() + at;
        return std::from_chars(first, first + size, out).ptr == first + size;
    };
    if (text.size() != 10 || text[4] != '-' || text[7] != '-' || !read(0, 4, y) || !read(5, 2, m) ||
        !read(8, 2, d))
        throw trade_population_error("The twin reads '" + text + "' as no date.");
    const std::chrono::year_month_day date{
        std::chrono::year{y}, std::chrono::month{m}, std::chrono::day{d}};
    if (!date.ok())
        throw trade_population_error("The twin reads '" + text + "' as no date.");
    return date;
}

/**
 * @brief An event listed from its parts, as a and b: the dates of each part in
 * turn, a part being a dates schedule or a source of date values. The listed
 * schedules must agree on their calendar and convention, which the event takes.
 */
tb::scripted_dates dated_list(const instrument_batch& batch,
                              std::string_view trade_type_code,
                              std::string_view source) {
    // A list of date values alone states no calendar, so ORE takes its dates as listed.
    tb::scripted_dates out;
    bool adjusted = false;
    for (const auto part : parts_of(source, " and ")) {
        if (!is_event_source(part)) {
            const auto values = twin_source_values(batch, trade_type_code, part);
            if (values.empty())
                throw trade_population_error("The twin recipe source '" + std::string(part) +
                                             "' finds no date.");
            for (const auto& value : values)
                out.dates.push_back(date_of(value));
            continue;
        }
        auto event = event_of(batch, part);
        auto* listed = event ? std::get_if<tb::scripted_dates>(&*event) : nullptr;
        if (!listed)
            throw trade_population_error("The twin recipe source '" + std::string(part) +
                                         "' states no listed dates, which an event list needs.");
        if (adjusted && (listed->calendar != out.calendar || listed->convention != out.convention))
            throw trade_population_error("The twin recipe source '" + std::string(source) +
                                         "' lists schedules adjusted apart.");
        adjusted = true;
        out.calendar = listed->calendar;
        out.convention = listed->convention;
        std::ranges::move(listed->dates, std::back_inserter(out.dates));
    }
    // ORE reads a listed schedule's dates once each, so a repeated date would
    // shorten the event the script counts.
    auto sorted = out.dates;
    std::ranges::sort(sorted);
    if (const auto repeated = std::ranges::adjacent_find(sorted); repeated != sorted.end())
        throw trade_population_error("The twin recipe source '" + std::string(source) + "' lists " +
                                     std::format("{:%Y-%m-%d}", *repeated) +
                                     " twice, which ORE reads once.");
    return out;
}

std::string transformed(std::string value, std::string_view transform) {
    if (transform.empty())
        return value;
    bool known = false;
    for (const auto& row : script_twin_transforms) {
        if (row.transform != transform)
            continue;
        known = true;
        if (row.value == value)
            return std::string(row.code);
    }
    if (!known)
        throw trade_population_error("The twin recipe names the unknown transform '" +
                                     std::string(transform) + "'.");
    throw trade_population_error("The " + std::string(transform) + " transform reads no value '" +
                                 value + "'.");
}

/**
 * @brief The twin's id: a name-based id over the native trade's id and type, so
 * no other trade draws it. The type keeps apart the twins of natives that share
 * an id across separately drawn plans, as the recorded trades of each asset
 * class do.
 */
boost::uuids::uuid twin_id_for(const boost::uuids::uuid& native, std::string_view trade_type) {
    static const boost::uuids::name_generator_sha1 generate(native_namespace());
    return generate(boost::uuids::to_string(native) + "/" + std::string(trade_type) + "/scripted");
}

}

std::vector<std::string> twin_source_values(const instrument_batch& batch,
                                            std::string_view trade_type_code,
                                            std::string_view source) {
    std::vector<std::string> out;
    for (const auto listed : parts_of(source, " and ")) {
        for (const auto part : parts_of(listed, " or ")) {
            auto values = one_source(batch, trade_type_code, part);
            if (values.empty())
                continue;
            std::ranges::move(values, std::back_inserter(out));
            break;
        }
    }
    return out;
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

        tb::scripted_variable variable{.name = std::string(row->input_name),
                                       .type = std::string(row->data_type),
                                       .value = std::string()};
        if (std::string_view(row->data_type) == "Event" &&
            parts_of(row->source, " and ").size() > 1) {
            variable.value = dated_list(native.instruments, native.trade_type_code, row->source);
            variables.push_back(std::move(variable));
            continue;
        }
        // An event is carried as the native trade states it, by rules or by
        // dates, or derived from another input as ORE derives it.
        if (is_event_source(row->source)) {
            auto event = event_of(native.instruments, row->source);
            // A derived event names its base by input name, which ORE resolves
            // only among the trade's other events.
            if (const auto* derived = event ? std::get_if<tb::scripted_derived>(&*event) : nullptr;
                derived && std::ranges::none_of(recipe, [&](const auto* other) {
                    return other != row && other->input_name == derived->base &&
                           other->data_type == "Event";
                }))
                throw trade_population_error("The " + native.trade_type_code + " twin derives " +
                                             std::string(row->input_name) + " from " +
                                             derived->base + ", which is no other Event input.");
            if (!event)
                throw trade_population_error(
                    "The " + native.trade_type_code + " twin finds no schedule for " +
                    std::string(row->input_name) + " at " + std::string(row->source) + ".");
            std::visit([&](auto& e) { variable.value = std::move(e); }, *event);
            variables.push_back(std::move(variable));
            continue;
        }
        auto values = twin_source_values(native.instruments, native.trade_type_code, row->source);
        if (values.empty())
            throw trade_population_error(
                "The " + native.trade_type_code + " twin finds no value for " +
                std::string(row->input_name) + " at " + std::string(row->source) + ".");
        for (auto& value : values)
            value = transformed(std::move(value), row->transform);
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
    twin.trade_id = twin_id_for(native.trade_id, native.trade_type_code);
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
