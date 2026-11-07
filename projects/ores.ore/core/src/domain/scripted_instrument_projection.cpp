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
#include "ores.ore.core/domain/option_block_projection.hpp"
#include "ores.ore.core/domain/portfolio_instrument_projection.hpp"
#include "ores.ore.core/domain/scalar_terms_projection.hpp"
#include "ores.ore.core/xml/binding.hpp"
#include <algorithm>
#include <cctype>
#include <optional>
#include <string>
#include <string_view>
#include <utility>

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
    scripted_instrument r;
    r.identity.trade_type_code = std::string(trade_type_code);
    r.audit.modified_by = "ores";
    r.audit.performed_by = "ores";
    r.audit.change_reason_code = "system.external_data_import";
    r.audit.change_commentary = "Imported from ORE XML";
    return r;
}

auto scripted_instrument_projection_json_escape(std::string_view text) -> std::string {
    std::string out;
    for (const char c : text) {
        if (c == '"')
            out += "\\\"";
        else if (c == '\\')
            out += "\\\\";
        else
            out += c;
    }
    return out;
}

auto json_array(const std::vector<std::string>& values) -> std::string {
    std::string out = "[";
    for (std::size_t i = 0; i < values.size(); ++i) {
        if (i != 0)
            out.push_back(',');
        out += "\"" + scripted_instrument_projection_json_escape(values[i]) + "\"";
    }
    out += "]";
    return out;
}

auto number_text(std::string_view value) -> double {
    try {
        return static_cast<double>(std::stof(std::string(value)));
    } catch (const std::exception&) {
        return 0;
    }
}

auto make_params_json(std::string_view put_call, double notional, double strike) -> std::string {
    return "{\"PutCall\":\"" + std::string(put_call) +
           "\",\"Notional\":" + std::to_string(notional) + ",\"Strike\":" + std::to_string(strike) +
           "}";
}

auto parse_json_string_array(std::string_view json) -> std::vector<std::string> {
    std::vector<std::string> result;
    std::size_t i = 0;
    while (i < json.size() && json[i] != '[')
        ++i;
    if (i >= json.size())
        return result;
    ++i;
    while (i < json.size()) {
        while (i < json.size() && (json[i] == ' ' || json[i] == ',' || json[i] == '\t'))
            ++i;
        if (i >= json.size() || json[i] == ']')
            break;
        if (json[i] != '"') {
            ++i;
            continue;
        }
        ++i;
        std::string value;
        while (i < json.size() && json[i] != '"') {
            if (json[i] == '\\' && i + 1 < json.size()) {
                ++i;
                if (json[i] == '"')
                    value += '"';
                else if (json[i] == '\\')
                    value += '\\';
                else
                    value += json[i];
            } else {
                value += json[i];
            }
            ++i;
        }
        ++i;
        result.push_back(std::move(value));
    }
    return result;
}

auto parse_json_object(std::string_view json) -> std::vector<std::pair<std::string, std::string>> {
    std::vector<std::pair<std::string, std::string>> result;
    std::size_t i = 0;
    while (i < json.size() && json[i] != '{')
        ++i;
    if (i >= json.size())
        return result;
    ++i;
    while (i < json.size()) {
        while (i < json.size() && (json[i] == ' ' || json[i] == ',' || json[i] == '\t'))
            ++i;
        if (i >= json.size() || json[i] == '}')
            break;
        if (json[i] != '"') {
            ++i;
            continue;
        }
        ++i;
        std::string key;
        while (i < json.size() && json[i] != '"')
            key += json[i++];
        ++i;
        while (i < json.size() && json[i] != ':')
            ++i;
        ++i;
        while (i < json.size() && (json[i] == ' ' || json[i] == '\t'))
            ++i;
        std::string value;
        if (i < json.size() && json[i] == '"') {
            ++i;
            while (i < json.size() && json[i] != '"') {
                if (json[i] == '\\' && i + 1 < json.size()) {
                    ++i;
                    value += json[i];
                } else {
                    value += json[i];
                }
                ++i;
            }
            ++i;
        } else {
            while (i < json.size() && json[i] != ',' && json[i] != '}')
                value += json[i++];
        }
        result.emplace_back(std::move(key), std::move(value));
    }
    return result;
}

auto own_text(const xml::record_reader& record, std::string_view name) -> std::string {
    const auto child = record.child(name);
    if (!child)
        return {};
    return child->own_text().value_or("");
}

auto vector_values(const xml::record_reader& record,
                   std::string_view name) -> std::vector<std::string> {
    const auto child = record.child(name);
    if (!child)
        return {};
    return child->values("Value");
}

/**
 * @brief The parameter key that carries one underlying's type.
 *
 * The schema requires an underlying to state a type and the flat table has no
 * column for it, so the type travels in the parameters beside the names the
 * underlying list carries.
 */
auto underlying_type_key(std::size_t index) -> std::string {
    return "Underlying" + std::to_string(index + 1) + "Type";
}

/// The underlying a parameter key names the type of, when the key is one.
auto underlying_of_type_key(std::string_view key) -> std::optional<std::string> {
    constexpr std::string_view prefix = "Underlying";
    constexpr std::string_view suffix = "Type";
    if (!key.starts_with(prefix) || !key.ends_with(suffix) ||
        key.size() <= prefix.size() + suffix.size())
        return std::nullopt;
    return std::string(key.substr(0, key.size() - suffix.size()));
}

/// The type entries a parameter list states, in the order it states them.
auto underlying_types_of(const std::vector<std::pair<std::string, std::string>>& params)
    -> std::vector<std::string> {
    std::vector<std::string> types;
    for (const auto& [key, value] : params)
        if (underlying_of_type_key(key))
            types.push_back(value);
    return types;
}

/// States the type each appended underlying requires, empty when none is known.
void set_underlying_type(xml::record_writer& underlying,
                         const std::vector<std::string>& types,
                         std::size_t index) {
    underlying.set_text("Type", index < types.size() ? types[index] : std::string{});
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
    std::optional<std::string> scripted_parameter::*column;
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

auto make_parameter(scripted_instrument_data& data,
                    std::string name,
                    std::string type) -> scripted_parameter& {
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
        row.value = value;
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
                parameter.value = *value;
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
            parameter.value = *value;
        }
    }
}

auto values_of(const scripted_instrument_data& data,
               int parameter_sequence_number) -> std::vector<std::string> {
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
            set_scalar_text(entry, "Value", *parameter->value);
        if (parameter->apply_coarsening)
            set_scalar_text(entry, "ApplyCoarsening", *parameter->apply_coarsening);
        if (parameter->shape == "vector") {
            auto list = entry.child("Values");
            for (const auto& value : values_of(data, parameter->sequence_number))
                append_leaf(list, "Value", value);
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
                append_leaf(element, "Value", value);
        } else if (parameter->shape == "scalar") {
            if (parameter->value)
                element.set_own_text(*parameter->value);
        } else {
            write_event(element, data, *parameter);
        }
    }
}

}

namespace {

auto read_scripted_row(xml::record_reader trade,
                       std::string_view trade_type) -> std::optional<scripted_instrument> {
    if (trade_type == "ScriptedTrade") {
        auto r = scripted_instrument_projection_make_base(trade_type);
        if (const auto named = trade.child("AsianBasketOptionData")) {
            r.script_name = "AsianBasketOption";
            r.underlyings_json = json_array(vector_values(*named, "Underlyings"));
            r.parameters_json = make_params_json(own_text(*named, "PutCall"),
                                                 number_text(own_text(*named, "Notional")),
                                                 number_text(own_text(*named, "Strike")));
            return r;
        }
        if (const auto named = trade.child("AverageStrikeBasketOptionData")) {
            r.script_name = "AverageStrikeBasketOption";
            r.underlyings_json = json_array(vector_values(*named, "Underlyings"));
            r.parameters_json = make_params_json(
                own_text(*named, "PutCall"), number_text(own_text(*named, "Notional")), 0.0);
            return r;
        }
        if (const auto data = trade.child("ScriptedTradeData")) {
            if (const auto name = data->text("ScriptName"))
                r.script_name = *name;
            std::string npv;
            if (const auto script = data->child("Script")) {
                if (const auto code = script->text("Code"))
                    r.script_body = *code;
                if (const auto value = script->text("NPV"))
                    npv = *value;
            }
            if (const auto values = data->child("Data")) {
                std::string params = "{";
                bool first = true;
                // The script's NPV is a name the schema requires, and the
                // instrument's flat table has no column for it, so it rides in
                // the parameters the table does carry.
                if (!npv.empty()) {
                    params += "\"NPV\":\"" + npv + "\"";
                    first = false;
                }
                for (const auto& number : values->children("Number")) {
                    if (!first)
                        params += ",";
                    params += "\"" + number.text("Name").value_or("") + "\":\"" +
                              number.text("Value").value_or("") + "\"";
                    first = false;
                }
                if (!first)
                    r.parameters_json = params + "}";

                const auto indices = values->children("Index");
                if (!indices.empty()) {
                    std::vector<std::string> names;
                    for (const auto& index : indices)
                        names.push_back(index.text("Value").value_or(""));
                    r.underlyings_json = json_array(names);
                }
            }
        }
        return r;
    }

    if (trade_type == "DoubleDigitalOption") {
        auto r = scripted_instrument_projection_make_base(trade_type);
        r.script_name = "DoubleDigitalOption";
        if (const auto d = trade.child("DoubleDigitalOptionData")) {
            std::vector<std::string> names;
            std::vector<std::string> types;
            for (const auto* name : {"Underlying1", "Underlying2", "Underlying3", "Underlying4"}) {
                if (const auto u = d->child(name)) {
                    if (const auto n = u->text("Name")) {
                        names.push_back(*n);
                        types.push_back(u->text("Type").value_or(""));
                    }
                }
            }
            r.underlyings_json = json_array(names);
            const auto number = [&](std::string_view name) {
                return std::to_string(decimal_as_double(*d, name).value_or(0));
            };
            std::string params = "{\"BinaryPayout\":" + number("BinaryPayout") +
                                 ",\"BinaryLevel1\":" + number("BinaryLevel1") +
                                 ",\"BinaryLevel2\":" + number("BinaryLevel2") + ",\"Type1\":\"" +
                                 d->text("Type1").value_or("") + "\"" + ",\"Type2\":\"" +
                                 d->text("Type2").value_or("") + "\"" + ",\"Position\":\"" +
                                 d->spelling("Position").value_or("") + "\"" + ",\"Expiry\":\"" +
                                 d->text("Expiry").value_or("") + "\"" + ",\"Settlement\":\"" +
                                 d->text("Settlement").value_or("") + "\"" + ",\"PayCcy\":\"" +
                                 d->spelling("PayCcy").value_or("") + "\"";
            for (std::size_t i = 0; i < types.size(); ++i)
                if (!types[i].empty())
                    params += ",\"" + underlying_type_key(i) + "\":\"" + types[i] + "\"";
            r.parameters_json = params + "}";
        }
        return r;
    }

    if (trade_type == "PerformanceOption_01") {
        auto r = scripted_instrument_projection_make_base(trade_type);
        r.script_name = "PerformanceOption_01";
        if (const auto d = trade.child("PerformanceOption01Data")) {
            std::vector<std::string> names;
            std::vector<std::string> types;
            std::string underlying_terms;
            if (const auto underlyings = d->child("Underlyings"))
                for (const auto& u : underlyings->children("Underlying")) {
                    const auto index = std::to_string(names.size() + 1);
                    names.push_back(u.text("Name").value_or(""));
                    types.push_back(u.text("Type").value_or(""));
                    if (const auto weight = u.decimal("Weight"))
                        underlying_terms +=
                            ",\"UnderlyingWeight" + index + "\":\"" + weight->to_string() + "\"";
                    if (const auto price_type = u.text("PriceType"))
                        underlying_terms +=
                            ",\"UnderlyingPriceType" + index + "\":\"" + *price_type + "\"";
                }
            r.underlyings_json = json_array(names);

            const auto number = [&](std::string_view name) {
                return std::to_string(decimal_as_double(*d, name).value_or(0));
            };
            std::string params = "{\"NotionalAmount\":" + number("NotionalAmount") +
                                 ",\"ParticipationRate\":" + number("ParticipationRate") +
                                 ",\"Strike\":" + number("Strike") + ",\"ValuationDate\":\"" +
                                 d->text("ValuationDate").value_or("") + "\"" +
                                 ",\"SettlementDate\":\"" + d->text("SettlementDate").value_or("") +
                                 "\"" + ",\"Position\":\"" + d->spelling("Position").value_or("") +
                                 "\"" + ",\"PayCcy\":\"" + d->spelling("PayCcy").value_or("") +
                                 "\"";
            std::string joined;
            if (const auto prices = d->child("StrikePrices")) {
                for (const auto& strike : prices->decimals("StrikePrice")) {
                    if (!joined.empty())
                        joined += ",";
                    joined += strike.to_string();
                }
            }
            params += ",\"StrikePrices\":\"" + joined + "\"";
            if (const auto included = scalar_terms::read_text(*d, "StrikeIncluded");
                included && d->has("StrikeIncluded"))
                params += ",\"StrikeIncluded\":\"" + *included + "\"";
            for (std::size_t i = 0; i < types.size(); ++i)
                if (!types[i].empty())
                    params += ",\"" + underlying_type_key(i) + "\":\"" + types[i] + "\"";
            r.parameters_json = params + underlying_terms + "}";
        }
        return r;
    }

    return std::nullopt;
}

}

auto read_scripted_instrument(xml::record_reader trade, std::string_view trade_type)
    -> std::optional<trading::domain::trade_instrument> {
    auto row = read_scripted_row(trade, trade_type);
    if (!row)
        return std::nullopt;

    scripted_instrument_data data{std::move(*row)};
    if (trade_type == "ScriptedTrade") {
        if (const auto generic = trade.child("ScriptedTradeData")) {
            data.instrument.data_element = "ScriptedTradeData";
            if (const auto values = generic->child("Data"))
                read_generic_parameters(data, *values);
        } else if (const auto element = named_product_of(trade)) {
            data.instrument.data_element = *element;
            if (data.instrument.script_name.empty())
                data.instrument.script_name = element->substr(0, element->size() - 4);
            read_named_parameters(data, *trade.child(*element));
        }
    }
    return trading::domain::trade_instrument{std::move(data)};
}

auto write_scripted_instrument(xml::record_writer& trade,
                               std::string_view trade_type,
                               const trading::domain::trade_instrument& instrument,
                               const portfolio_reference_curves&) -> bool {
    const auto* carrier = std::get_if<scripted_instrument_data>(&instrument);
    if (carrier == nullptr)
        return false;
    const auto* r = &carrier->instrument;

    // A trade whose parameters are rows states them in the form it was read
    // in: the generic ScriptedTradeData, or the named product's own element.
    if (trade_type == "ScriptedTrade" && !carrier->parameters.empty() && r->data_element &&
        *r->data_element != "ScriptedTradeData") {
        auto product = trade.child(*r->data_element);
        write_named_parameters(product, *carrier);
        return true;
    }

    if (trade_type == "ScriptedTrade") {
        auto data = trade.child("ScriptedTradeData");
        if (!r->script_name.empty())
            data.set_text("ScriptName", r->script_name);
        if (!r->script_body.empty())
            data.child("Script").set_text("Code", r->script_body);

        // The schema requires the data element even when the row states no
        // parameter, so it is stated before anything fills it.
        auto values = data.child("Data");
        const auto numbers = parse_json_object(r->parameters_json);
        if (!carrier->parameters.empty()) {
            for (const auto& [name, value] : numbers)
                if (name == "NPV")
                    data.child("Script").set_text("NPV", value);
            write_generic_parameters(values, *carrier);
            return true;
        }
        for (const auto& [name, value] : numbers) {
            if (name == "NPV") {
                data.child("Script").set_text("NPV", value);
                continue;
            }
            auto number = values.append("Number");
            number.set_text("Name", name);
            number.set_text("Value", value);
        }
        const auto indices = parse_json_string_array(r->underlyings_json);
        for (const auto& value : indices) {
            auto index = values.append("Index");
            index.set_text("Name", "Underlying");
            index.set_text("Value", value);
        }
        return true;
    }

    if (trade_type == "DoubleDigitalOption") {
        auto d = trade.child("DoubleDigitalOptionData");
        const auto params = parse_json_object(r->parameters_json);
        for (const auto& [key, value] : params) {
            if (key == "Type1" || key == "Type2" || key == "Expiry" || key == "Settlement")
                d.set_text(key, value);
            else if (key == "Position")
                d.set_spelling("Position", value, "long short");
            else if (key == "PayCcy")
                d.set_spelling("PayCcy", value, "currency");
            else if (underlying_of_type_key(key))
                continue;
            else
                set_decimal_from_double(d, key, std::stod(value));
        }
        const auto names = parse_json_string_array(r->underlyings_json);
        const auto types = underlying_types_of(params);
        for (std::size_t i = 0; i < names.size() && i < 4; ++i) {
            auto underlying = d.child("Underlying" + std::to_string(i + 1));
            underlying.set_text("Name", names[i]);
            set_underlying_type(underlying, types, i);
        }
        return true;
    }

    if (trade_type == "PerformanceOption_01") {
        auto d = trade.child("PerformanceOption01Data");
        std::vector<std::string> strike_prices;
        const auto params = parse_json_object(r->parameters_json);
        for (const auto& [key, value] : params) {
            if (key == "Position")
                d.set_spelling("Position", value, "long short");
            else if (key == "PayCcy")
                d.set_spelling("PayCcy", value, "currency");
            else if (key == "ValuationDate" || key == "SettlementDate")
                d.set_text(key, value);
            else if (key == "StrikeIncluded")
                scalar_terms::write_text(d, key, value);
            else if (key.starts_with("UnderlyingWeight") || key.starts_with("UnderlyingPriceType"))
                continue;
            else if (key == "StrikePrices") {
                std::size_t start = 0;
                while (start < value.size()) {
                    const auto comma = value.find(',', start);
                    const auto length =
                        comma == std::string::npos ? value.size() - start : comma - start;
                    const auto token = value.substr(start, length);
                    if (!token.empty())
                        strike_prices.push_back(token);
                    if (comma == std::string::npos)
                        break;
                    start = comma + 1;
                }
            } else if (underlying_of_type_key(key))
                continue;
            else
                set_decimal_from_double(d, key, std::stod(value));
        }
        if (!strike_prices.empty()) {
            auto prices = d.child("StrikePrices");
            for (const auto& price : strike_prices) {
                if (const auto parsed = ores::utility::decimal::decimal::from_string(price))
                    prices.append_decimal("StrikePrice", *parsed);
            }
        }
        const auto names = parse_json_string_array(r->underlyings_json);
        if (!names.empty()) {
            const auto types = underlying_types_of(params);
            auto underlyings = d.child("Underlyings");
            for (std::size_t i = 0; i < names.size(); ++i) {
                auto underlying = underlyings.append("Underlying");
                underlying.set_text("Name", names[i]);
                set_underlying_type(underlying, types, i);
                const auto index = std::to_string(i + 1);
                for (const auto& [key, value] : params) {
                    if (key == "UnderlyingWeight" + index) {
                        if (const auto parsed = ores::utility::decimal::decimal::from_string(value))
                            underlying.set_decimal("Weight", *parsed);
                    } else if (key == "UnderlyingPriceType" + index) {
                        scalar_terms::write_text(underlying, "PriceType", value);
                    }
                }
            }
        }
        return true;
    }

    return false;
}

}
