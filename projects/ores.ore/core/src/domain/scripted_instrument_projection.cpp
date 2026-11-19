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
#include "ores.ore.core/domain/barrier_block_projection.hpp"
#include "ores.ore.core/domain/index_reference.hpp"
#include "ores.ore.core/domain/option_block_projection.hpp"
#include "ores.ore.core/domain/portfolio_instrument_projection.hpp"
#include "ores.ore.core/domain/underlying_block_projection.hpp"
#include "ores.ore.core/xml/binding.hpp"
#include <algorithm>
#include <cctype>
#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace ores::ore::domain {

namespace {

using trading::domain::scripted_instrument;
using trading::domain::scripted_instrument_data;
using trading::domain::scripted_parameter;
using trading::domain::scripted_parameter_value;

/**
 * @brief A scripted instrument with the audit columns an import states.
 *
 * The trading domain keeps the audit beside the product, and every family's
 * reading sets the same four columns.
 */
auto scripted_instrument_projection_make_base(std::string_view trade_type_code)
    -> scripted_instrument {
    scripted_instrument r{};
    r.identity.trade_type_code = std::string(trade_type_code);
    r.audit.modified_by = "ores";
    r.audit.performed_by = "ores";
    r.audit.change_reason_code = "system.external_data_import";
    r.audit.change_commentary = "Imported from ORE XML";
    return r;
}


// ---------------------------------------------------------------------------
// The parameters: ORE reads every scripted trade as named parameters of five
// kinds (scriptedtrade.cpp). A generic trade states them under
// ScriptedTradeData/Data, one list per kind; a named product states each as an
// element named after the variable and typed by its type attribute.
// ---------------------------------------------------------------------------

constexpr std::string_view scripted_owner = "scripted";
constexpr std::string_view event_role = "event";

/// The generic form's parameter lists, in the order the schema states them.
constexpr std::string_view generic_kinds[] = {"Number", "Currency", "Index", "Event", "Daycounter"};

template <typename Record>
const xml::field* field_named(const Record& r, std::string_view name) {
    return xml::find_field(r.type(), name, false);
}

/// A scalar leaf read as the document spells it, whatever kind it is declared.
auto scalar_text(const xml::record_reader& r, std::string_view name) -> std::optional<std::string> {
    const auto* f = field_named(r, name);
    if (f == nullptr || !r.has(name))
        return std::nullopt;
    switch (f->kind) {
        case xml::field_kind::enumeration:
            return r.spelling(name);
        case xml::field_kind::decimal:
            if (const auto value = r.decimal(name))
                return value->to_string();
            return std::nullopt;
        case xml::field_kind::integer:
            if (const auto value = r.integer(name))
                return std::to_string(*value);
            return std::nullopt;
        default:
            return r.text(name);
    }
}

void set_scalar_text(xml::record_writer& w, std::string_view name, const std::string& value) {
    const auto* f = field_named(w, name);
    if (f == nullptr)
        return;
    switch (f->kind) {
        case xml::field_kind::enumeration:
            w.set_spelling(name, value, name);
            break;
        case xml::field_kind::decimal:
            if (const auto parsed = ores::utility::decimal::decimal::from_string(value))
                w.set_decimal(name, *parsed);
            break;
        case xml::field_kind::integer:
            w.set_integer(name, std::stoll(value));
            break;
        default:
            w.set_text(name, value);
    }
}

/// A repeated leaf read as the document spells it, whatever kind it holds.
auto leaf_list(const xml::record_reader& r, std::string_view name) -> std::vector<std::string> {
    const auto* f = field_named(r, name);
    if (f == nullptr)
        return {};
    switch (f->scalar) {
        case xml::scalar_kind::decimal: {
            std::vector<std::string> out;
            for (const auto& value : r.decimals(name))
                out.push_back(value.to_string());
            return out;
        }
        case xml::scalar_kind::enumeration:
            return r.spellings(name);
        default:
            return r.values(name);
    }
}

void append_leaf(xml::record_writer& w, std::string_view name, const std::string& value) {
    const auto* f = field_named(w, name);
    if (f == nullptr)
        return;
    switch (f->scalar) {
        case xml::scalar_kind::decimal:
            if (const auto parsed = ores::utility::decimal::decimal::from_string(value))
                w.append_decimal(name, *parsed);
            break;
        case xml::scalar_kind::enumeration:
            w.append_spelling(name, value, name);
            break;
        default:
            w.append_value(name, value);
    }
}

/// The terms of a derived schedule, each kept on its own column.
struct derived_term {
    std::string_view element;
    std::optional<std::string> scripted_parameter::* column;
};

constexpr derived_term derived_terms[] = {
    {"BaseSchedule", &scripted_parameter::base_schedule},
    {"Shift", &scripted_parameter::shift},
    {"Calendar", &scripted_parameter::calendar},
    {"Convention", &scripted_parameter::convention},
    {"RemoveFirstDate", &scripted_parameter::remove_first_date},
    {"RemoveLastDate", &scripted_parameter::remove_last_date},
    {"ShiftUnit", &scripted_parameter::shift_unit},
    {"ShiftAnchor", &scripted_parameter::shift_anchor},
};

/// The codec reads any name it cannot place as inflation, and a script may name a bare security.
auto script_index_address_of(const std::string& name) -> std::string {
    auto address = address_of(name);
    return address.starts_with("oresmd://inflation/") ? name : address;
}

/**
 * @brief Whether a parameter's declared type is the kind that names an index.
 *
 * The two forms of the document spell that kind differently: the generic form
 * capitalises its kind (=Index=), and a named product states the type attribute
 * its schema gives the variable (=index=). They are one kind, so the comparison
 * is on the word and not on the spelling.
 */
auto names_an_index(std::string_view declared_type) -> bool {
    constexpr std::string_view index = "Index";
    if (declared_type.size() != index.size())
        return false;
    for (std::size_t i = 0; i < index.size(); ++i)
        if (std::tolower(static_cast<unsigned char>(declared_type[i])) !=
            std::tolower(static_cast<unsigned char>(index[i])))
            return false;
    return true;
}

/// An Index parameter names an index. Any other declared type stays text, even if it
/// happens to spell an index.
auto stored_value_of(const scripted_parameter& parameter, const std::string& value) -> std::string {
    return names_an_index(parameter.declared_type) ? script_index_address_of(value) : value;
}

auto stated_value_of(const scripted_parameter& parameter, const std::string& value) -> std::string {
    return names_an_index(parameter.declared_type) ? ore_name_of(value) : value;
}

auto make_parameter(scripted_instrument_data& data, std::string name, std::string type)
    -> scripted_parameter& {
    scripted_parameter row;
    row.sequence_number = static_cast<int>(data.parameters.size()) + 1;
    row.name = std::move(name);
    row.declared_type = std::move(type);
    row.shape = "scalar";
    option_block::stamp_row(row);
    data.parameters.push_back(std::move(row));
    return data.parameters.back();
}

void add_values(scripted_instrument_data& data,
                const scripted_parameter& parameter,
                const std::vector<std::string>& values) {
    int sequence_number = 0;
    for (const auto& value : values) {
        scripted_parameter_value row;
        row.parameter_sequence_number = parameter.sequence_number;
        row.sequence_number = ++sequence_number;
        row.value = stored_value_of(parameter, value);
        option_block::stamp_row(row);
        data.parameter_values.push_back(std::move(row));
    }
}

/// Reads an event's schedule or derived schedule into the parameter.
void read_event(scripted_instrument_data& data,
                scripted_parameter& parameter,
                const xml::record_reader& holder) {
    if (const auto schedule = holder.child("ScheduleData")) {
        parameter.shape = "schedule";
        const auto sequence_number = parameter.sequence_number;
        option_block::read_schedule(
            data, *schedule, std::string(scripted_owner), sequence_number, std::string(event_role));
    } else if (const auto derived = holder.child("DerivedSchedule")) {
        auto& row = data.parameters.back();
        row.shape = "derived_schedule";
        for (const auto& term : derived_terms)
            row.*(term.column) = scalar_text(*derived, term.element);
    }
}

void read_generic_parameters(scripted_instrument_data& data, const xml::record_reader& values) {
    for (const auto kind : generic_kinds) {
        for (const auto& entry : values.children(kind)) {
            auto& parameter =
                make_parameter(data, entry.text("Name").value_or(""), std::string(kind));
            if (const auto value = scalar_text(entry, "Value"))
                parameter.value = stored_value_of(parameter, *value);
            if (kind == "Event") {
                if (const auto coarsening = scalar_text(entry, "ApplyCoarsening"))
                    parameter.apply_coarsening = *coarsening;
                read_event(data, parameter, entry);
            } else if (const auto list = entry.child("Values")) {
                parameter.shape = "vector";
                add_values(data, parameter, leaf_list(*list, "Value"));
            }
        }
    }
}

/// Whether a record type is a named scripted product: every field is one of
/// the free style parameter types the scripted schema declares.
auto is_named_product(const xml::record_type& type) -> bool {
    if (type.field_count == 0)
        return false;
    for (std::uint8_t i = 0; i < type.field_count; ++i) {
        const auto& f = type.fields[i];
        if (f.kind != xml::field_kind::nested || f.type == nullptr ||
            !f.type->name.starts_with("stFreeStyle"))
            return false;
    }
    return true;
}

/// The element of the trade that states a named scripted product, if any.
auto named_product_of(const xml::record_reader& trade) -> std::optional<std::string> {
    const auto& type = trade.type();
    for (std::uint8_t i = 0; i < type.field_count; ++i) {
        const auto& f = type.fields[i];
        // A trade states its product as one of many alternatives, so the
        // product's member is a column alternative rather than a plain child.
        const bool child =
            f.kind == xml::field_kind::nested || f.kind == xml::field_kind::column_nested;
        if (!child || f.type == nullptr || !is_named_product(*f.type))
            continue;
        if (trade.child(f.name))
            return std::string(f.name);
    }
    return std::nullopt;
}

void read_named_parameters(scripted_instrument_data& data, const xml::record_reader& product) {
    const auto& type = product.type();
    for (std::uint8_t i = 0; i < type.field_count; ++i) {
        const auto& f = type.fields[i];
        const auto element = product.child(f.name);
        if (!element)
            continue;
        auto& parameter =
            make_parameter(data, std::string(f.name), element->text("type").value_or(""));
        if (field_named(*element, "ScheduleData") != nullptr) {
            read_event(data, parameter, *element);
        } else if (field_named(*element, "Value") != nullptr) {
            parameter.shape = "vector";
            add_values(data, parameter, leaf_list(*element, "Value"));
        } else if (const auto value = element->own_text()) {
            parameter.value = stored_value_of(parameter, *value);
        }
    }
}

auto values_of(const scripted_instrument_data& data, int parameter_sequence_number)
    -> std::vector<std::string> {
    std::vector<const scripted_parameter_value*> rows;
    for (const auto& row : data.parameter_values)
        if (row.parameter_sequence_number == parameter_sequence_number)
            rows.push_back(&row);
    std::sort(rows.begin(), rows.end(), [](const auto* a, const auto* b) {
        return a->sequence_number < b->sequence_number;
    });
    std::vector<std::string> out;
    for (const auto* row : rows)
        out.push_back(row->value);
    return out;
}

void write_event(xml::record_writer& holder,
                 const scripted_instrument_data& data,
                 const scripted_parameter& parameter) {
    if (parameter.shape == "schedule") {
        option_block::write_stored_schedule(holder.child("ScheduleData"),
                                            data,
                                            std::string(scripted_owner),
                                            parameter.sequence_number,
                                            std::string(event_role));
    } else if (parameter.shape == "derived_schedule") {
        auto derived = holder.child("DerivedSchedule");
        for (const auto& term : derived_terms)
            if (const auto& value = parameter.*(term.column))
                set_scalar_text(derived, term.element, *value);
    }
}

auto ordered_parameters(const scripted_instrument_data& data)
    -> std::vector<const scripted_parameter*> {
    std::vector<const scripted_parameter*> rows;
    for (const auto& row : data.parameters)
        rows.push_back(&row);
    std::sort(rows.begin(), rows.end(), [](const auto* a, const auto* b) {
        return a->sequence_number < b->sequence_number;
    });
    return rows;
}

void write_generic_parameters(xml::record_writer& values, const scripted_instrument_data& data) {
    for (const auto* parameter : ordered_parameters(data)) {
        auto entry = values.append(parameter->declared_type);
        entry.set_text("Name", parameter->name);
        if (parameter->value)
            set_scalar_text(entry, "Value", stated_value_of(*parameter, *parameter->value));
        if (parameter->apply_coarsening)
            set_scalar_text(entry, "ApplyCoarsening", *parameter->apply_coarsening);
        if (parameter->shape == "vector") {
            auto list = entry.child("Values");
            for (const auto& value : values_of(data, parameter->sequence_number))
                append_leaf(list, "Value", stated_value_of(*parameter, value));
        } else {
            write_event(entry, data, *parameter);
        }
    }
}

void write_named_parameters(xml::record_writer& product, const scripted_instrument_data& data) {
    for (const auto* parameter : ordered_parameters(data)) {
        auto element = product.child(parameter->name);
        element.set_text("type", parameter->declared_type);
        if (parameter->shape == "vector") {
            for (const auto& value : values_of(data, parameter->sequence_number))
                append_leaf(element, "Value", stated_value_of(*parameter, value));
        } else if (parameter->shape == "scalar") {
            if (parameter->value)
                element.set_own_text(stated_value_of(*parameter, *parameter->value));
        } else {
            write_event(element, data, *parameter);
        }
    }
}


}
namespace {

auto read_scripted_row(xml::record_reader trade, std::string_view trade_type)
    -> std::optional<scripted_instrument> {
    if (trade_type != "ScriptedTrade")
        return std::nullopt;

    auto r = scripted_instrument_projection_make_base(trade_type);
    if (const auto data = trade.child("ScriptedTradeData")) {
        r.data_element = "ScriptedTradeData";
        if (const auto name = data->text("ScriptName"))
            r.script_name = *name;
        if (const auto script = data->child("Script")) {
            if (const auto code = script->text("Code"))
                r.script_body = *code;
            if (const auto npv = script->text("NPV"))
                r.script_npv = *npv;
        }
        return r;
    }

    // A named product states its variables on the element of the product, so the
    // element's name is the script the trade runs.
    if (const auto element = named_product_of(trade)) {
        r.data_element = *element;
        r.script_name = element->substr(0, element->size() - 4);
    }
    return r;
}

}

auto read_scripted_instrument(xml::record_reader trade, std::string_view trade_type)
    -> std::optional<trading::domain::trade_instrument> {
    auto row = read_scripted_row(trade, trade_type);
    if (!row)
        return std::nullopt;

    scripted_instrument_data data{std::move(*row)};
    if (trade.child("ScriptedTradeData")) {
        if (const auto values = trade.child("ScriptedTradeData")->child("Data"))
            read_generic_parameters(data, *values);
    } else if (const auto element = named_product_of(trade)) {
        read_named_parameters(data, *trade.child(*element));
    }
    return trading::domain::trade_instrument{std::move(data)};
}

auto write_scripted_instrument(xml::record_writer& trade,
                               std::string_view trade_type,
                               const trading::domain::trade_instrument& instrument,
                               const ores::marketdata::projection::ore_projection&) -> bool {
    const auto* carrier = std::get_if<scripted_instrument_data>(&instrument);
    if (carrier == nullptr)
        return false;
    const auto* r = &carrier->instrument;

    if (trade_type != "ScriptedTrade")
        return false;

    // A trade states its variables in the form it was read in: the generic
    // ScriptedTradeData, or the named product's own element.
    if (r->data_element && *r->data_element != "ScriptedTradeData") {
        auto product = trade.child(*r->data_element);
        write_named_parameters(product, *carrier);
        return true;
    }

    auto data = trade.child("ScriptedTradeData");
    if (!r->script_name.empty())
        data.set_text("ScriptName", r->script_name);
    // A row states the body and the variable its result lands in, and each is
    // written when it is held: a body the document gave without a result name is
    // still the body the row read, and dropping it would lose what the import kept.
    if (!r->script_body.empty() || !r->script_npv.empty()) {
        auto script = data.child("Script");
        if (!r->script_body.empty())
            script.set_text("Code", r->script_body);
        if (!r->script_npv.empty())
            script.set_text("NPV", r->script_npv);
    }
    // The schema requires the data element even when the trade states no
    // variable, so it is stated before anything fills it.
    auto values = data.child("Data");
    write_generic_parameters(values, *carrier);
    return true;
}

}
