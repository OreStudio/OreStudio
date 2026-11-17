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

/**
 * @brief The products ORE reads through a class of its own, and the element
 * each states its terms in.
 *
 * A scripted trade states its variables with a type on each: under
 * ScriptedTradeData for the generic form, or on the element of the named product
 * whose script it runs. These two state a fixed element instead, whose members
 * carry no type attribute, and ORE reads it with a class that turns the members
 * into the variables the script needs. So the members are the trade's terms and
 * each is a parameter row.
 */
struct class_read_product {
    std::string_view trade_type;
    std::string_view element;
};

constexpr class_read_product class_read_products[] = {
    {"DoubleDigitalOption", "DoubleDigitalOptionData"},
    {"PerformanceOption_01", "PerformanceOption01Data"},
    {"Autocallable_01", "Autocallable01Data"},
    {"EquityBasketOption", "EquityBasketOptionData"},
    {"FxBasketOption", "FxBasketOptionData"},
    {"EquityRainbowOption", "EquityRainbowOptionData"},
    {"FxRainbowOption", "FxRainbowOptionData"},
    {"EquityBestEntryOption", "EquityBestEntryOptionData"},
    {"FxBestEntryOption", "FxBestEntryOptionData"},
    {"EquityStrikeResettableOption", "EquityStrikeResettableOptionData"},
    {"FxStrikeResettableOption", "FxStrikeResettableOptionData"},
    {"EquityWindowBarrierOption", "EquityWindowBarrierOptionData"},
    {"FxWindowBarrierOption", "FxWindowBarrierOptionData"},
    {"EquityGenericBarrierOption", "EquityGenericBarrierOptionData"},
    {"EquityBasketVarianceSwap", "EquityBasketVarianceSwapData"},
    {"FxBasketVarianceSwap", "FxBasketVarianceSwapData"},
    {"EquityForwardVolatilityAgreement", "EquityForwardVolatilityAgreementData"},
    {"FxForwardVolatilityAgreement", "FxForwardVolatilityAgreementData"},
    {"EuropeanOptionBarrier", "EuropeanOptionBarrierData"},
    {"FxWorstOfBasketSwap", "FxWorstOfBasketSwapData"},
};

auto class_read_product_of(std::string_view trade_type) -> const class_read_product* {
    for (const auto& product : class_read_products)
        if (product.trade_type == trade_type)
            return &product;
    return nullptr;
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

// ---------------------------------------------------------------------------
// A product ORE reads through its own class: its members are the trade's terms,
// the element states no type on any of them, and the schema is what says how
// each is written. An Underlying block is an entity of its own and never a
// parameter.
// ---------------------------------------------------------------------------

/// The kind of leaf a field states, by the schema's name for it.
auto leaf_kind_name(xml::scalar_kind kind) -> std::string_view {
    switch (kind) {
        case xml::scalar_kind::text:
            return "text";
        case xml::scalar_kind::integer:
            return "integer";
        case xml::scalar_kind::decimal:
            return "decimal";
        case xml::scalar_kind::real:
            return "real";
        case xml::scalar_kind::enumeration:
            return "enumeration";
    }
    return "text";
}

/// The kind of leaf a scalar member states.
auto member_kind_name(xml::field_kind kind) -> std::string_view {
    switch (kind) {
        case xml::field_kind::integer:
            return "integer";
        case xml::field_kind::decimal:
            return "decimal";
        case xml::field_kind::real:
            return "real";
        case xml::field_kind::enumeration:
            return "enumeration";
        default:
            return "text";
    }
}

/**
 * @brief The leaf a member states, for a member whose values are a list.
 *
 * A list is a container element whose only child is the value, such as
 * StrikePrices holding a StrikePrice per value.
 */
auto container_leaf_of(const xml::field& member) -> const xml::field* {
    if (member.kind != xml::field_kind::nested || member.type == nullptr)
        return nullptr;
    for (std::uint8_t i = 0; i < member.type->field_count; ++i) {
        const auto& f = member.type->fields[i];
        if (f.kind == xml::field_kind::repeated_leaf)
            return &f;
    }
    return nullptr;
}

auto container_leaf_of(const xml::record_type& type, std::string_view member) -> const xml::field* {
    const auto* f = xml::find_field(type, member, false);
    return f == nullptr ? nullptr : container_leaf_of(*f);
}

/// The type a member of such a product's element states, which the schema gives it.
auto declared_type_of(const xml::field& member) -> std::string {
    // A list of indices is an Index list, which holds each as an address.
    if (member.type != nullptr && member.type->name == "stFreeStyleIndexVector")
        return "Index";
    if (const auto* leaf = container_leaf_of(member))
        return std::string(leaf_kind_name(leaf->scalar));
    return std::string(member_kind_name(member.kind));
}

/// Whether the member states one Underlying block.
auto states_an_underlying(const xml::field& member) -> bool {
    return member.kind == xml::field_kind::nested && member.type != nullptr &&
           member.type->name == "underlying";
}

/// Whether the member is an OptionData block, which the option rows hold.
auto states_option_data(const xml::field& member) -> bool {
    return member.kind == xml::field_kind::nested && member.type != nullptr &&
           member.type->name == "optionData";
}

/// Whether the member is itself a schedule, which the schedule rows hold.
auto states_a_schedule(const xml::field& member) -> bool {
    return member.kind == xml::field_kind::nested && member.type != nullptr &&
           member.type->name == "scheduleData";
}

/**
 * @brief Whether the member is a free style scalar: an element whose value is
 * its own text, beside an optional type attribute.
 */
auto states_a_free_style_scalar(const xml::field& member) -> bool {
    return member.kind == xml::field_kind::nested && member.type != nullptr &&
           member.type->name.starts_with("stFreeStyle") &&
           xml::find_field(*member.type, "Value", false) == nullptr &&
           xml::find_field(*member.type, "ScheduleData", false) == nullptr;
}

/// The type a free style scalar declares: its type attribute, or the name its schema type gives.
auto free_style_type_of(const xml::field& member, const xml::record_reader& element)
    -> std::string {
    if (const auto stated = element.text("type"); stated && !stated->empty())
        return *stated;
    return std::string(member.type->name.substr(std::string_view("stFreeStyle").size()));
}

/// Whether the member is one BarrierData block, which the barrier rows hold.
auto states_a_barrier(const xml::field& member) -> bool {
    return member.kind == xml::field_kind::nested && member.type != nullptr &&
           member.type->name == "barrierData" && member.name == "BarrierData";
}

/**
 * @brief Whether the member holds BarrierData blocks with the terms that
 * monitor them, as a generic barrier option's Barriers does.
 */
auto holds_barriers(const xml::field& member) -> bool {
    return member.kind == xml::field_kind::nested && member.type != nullptr &&
           xml::find_field(*member.type, "BarrierData", false) != nullptr;
}

/// Whether the member states its value as one leaf, which is what a parameter holds.
auto states_a_leaf(const xml::field& member) -> bool {
    switch (member.kind) {
        case xml::field_kind::text:
        case xml::field_kind::integer:
        case xml::field_kind::decimal:
        case xml::field_kind::real:
        case xml::field_kind::enumeration:
            return true;
        default:
            return false;
    }
}

/// Whether the member states an event as a schedule, which the class reads as dates.
auto holds_a_schedule(const xml::field& member) -> bool {
    if (member.kind != xml::field_kind::nested || member.type == nullptr)
        return false;
    for (std::uint8_t i = 0; i < member.type->field_count; ++i)
        if (member.type->fields[i].name == "ScheduleData")
            return true;
    return false;
}

/// The repeated Underlying block a member states, for a member that states a list.
auto repeated_underlying_of(const xml::field& member) -> const xml::field* {
    if (member.kind != xml::field_kind::nested || member.type == nullptr)
        return nullptr;
    for (std::uint8_t i = 0; i < member.type->field_count; ++i) {
        const auto& f = member.type->fields[i];
        if (f.kind == xml::field_kind::repeated_nested && f.type != nullptr &&
            f.type->name == "underlying")
            return &f;
    }
    return nullptr;
}

/**
 * @brief The role the underlying or schedule rows of a member are kept under.
 *
 * The role names the element the document states the rows in, so a product
 * that states Underlying1 beside Underlying2 keeps the two apart.
 */
auto role_of_member(std::string_view element) -> std::string {
    std::string role;
    for (const char c : element)
        role += static_cast<char>(std::tolower(static_cast<unsigned char>(c)));
    return role;
}

/**
 * @brief The role a member's barrier rows are kept under, in the vocabulary
 * every family's barriers share: a product's own barriers and a generic
 * barrier option's Barriers are the barrier, its TransatlanticBarrier the
 * transatlantic one.
 */
auto barrier_role_of(std::string_view member) -> std::string {
    return member == "TransatlanticBarrier" ? "transatlantic" : "barrier";
}

/**
 * @brief Whether the member is a block of leaves only, such as a CBO's
 * CBOInvestment, whose leaves are parameters named for the block and the leaf.
 * A block's attributes are not held; no block a product states carries one.
 */
auto states_a_leaf_block(const xml::field& member) -> bool {
    if (member.kind != xml::field_kind::nested || member.type == nullptr ||
        member.type->name.starts_with("stFreeStyle") || member.type->field_count == 0)
        return false;
    for (std::uint8_t i = 0; i < member.type->field_count; ++i)
        if (!member.type->fields[i].is_attribute && !states_a_leaf(member.type->fields[i]))
            return false;
    return true;
}

/// The name a leaf of a barrier holder is kept under: the holder, a dot, the leaf.
auto holder_leaf_name(std::string_view holder, std::string_view leaf) -> std::string {
    return std::string(holder) + "." + std::string(leaf);
}

/// The role the schedule a barrier holder monitors its barriers on is kept under.
auto holder_schedule_role(std::string_view holder) -> std::string {
    return barrier_role_of(holder) + "_schedule";
}

/**
 * @brief Reads a barrier holder: its barriers under the holder's role, each
 * schedule it states under a role of its own, and each of its leaves as a
 * parameter named for the holder and the leaf.
 */
void read_barrier_holder(scripted_instrument_data& data,
                         const xml::record_reader& holder,
                         const xml::field& member) {
    barrier_block::read_barriers(data, holder, barrier_role_of(member.name));
    const auto& type = *member.type;
    for (std::uint8_t i = 0; i < type.field_count; ++i) {
        const auto& f = type.fields[i];
        if (f.name == "BarrierData" || !holder.has(f.name))
            continue;
        if (states_a_schedule(f)) {
            option_block::read_schedule(
                data, *holder.child(f.name), "option", 1, holder_schedule_role(member.name));
        } else if (states_a_leaf(f)) {
            if (const auto value = scalar_text(holder, f.name)) {
                auto& parameter = make_parameter(
                    data, holder_leaf_name(member.name, f.name), declared_type_of(f));
                parameter.value = *value;
            }
        }
    }
}

void write_leaf_block(xml::record_writer& product,
                      const scripted_instrument_data& data,
                      const xml::field& member) {
    const auto& type = *member.type;
    std::optional<xml::record_writer> block;
    for (std::uint8_t i = 0; i < type.field_count; ++i) {
        const auto& leaf = type.fields[i];
        const auto name = holder_leaf_name(member.name, leaf.name);
        for (const auto& p : data.parameters)
            if (p.name == name && p.value) {
                if (!block)
                    block = product.child(member.name);
                set_scalar_text(*block, leaf.name, *p.value);
            }
    }
}

void write_barrier_holder(xml::record_writer& product,
                          const scripted_instrument_data& data,
                          const xml::field& member) {
    const auto role = barrier_role_of(member.name);
    const auto& type = *member.type;
    const auto leaf_value = [&](std::string_view leaf) -> const std::string* {
        const auto name = holder_leaf_name(member.name, leaf);
        for (const auto& p : data.parameters)
            if (p.name == name && p.value)
                return &*p.value;
        return nullptr;
    };
    bool holds_any = barrier_block::has_barriers(data, role);
    for (std::uint8_t i = 0; i < type.field_count && !holds_any; ++i) {
        const auto& f = type.fields[i];
        holds_any =
            states_a_schedule(f) ?
                option_block::has_product_schedule(data, holder_schedule_role(member.name)) :
                leaf_value(f.name) != nullptr;
    }
    if (!holds_any)
        return;
    auto holder = product.child(member.name);
    for (std::uint8_t i = 0; i < type.field_count; ++i) {
        const auto& f = type.fields[i];
        if (f.name == "BarrierData")
            barrier_block::write_barriers(holder, data, role);
        else if (states_a_schedule(f))
            option_block::write_product_schedule(
                holder, data, f.name, holder_schedule_role(member.name));
        else if (const auto* value = leaf_value(f.name))
            set_scalar_text(holder, f.name, *value);
    }
}

void read_class_members(scripted_instrument_data& data, const xml::record_reader& product) {
    const auto& type = product.type();
    for (std::uint8_t i = 0; i < type.field_count; ++i) {
        const auto& f = type.fields[i];
        if (f.is_attribute || states_an_underlying(f) || repeated_underlying_of(f) != nullptr)
            continue;
        if (states_option_data(f)) {
            if (product.has(f.name))
                option_block::read_option_block(data, *product.child(f.name));
            continue;
        }
        if (states_a_schedule(f)) {
            if (product.has(f.name))
                option_block::read_schedule(
                    data, *product.child(f.name), "option", 1, role_of_member(f.name));
            continue;
        }
        if (states_a_barrier(f)) {
            barrier_block::read_barriers(data, product, barrier_role_of(f.name));
            continue;
        }
        if (states_a_leaf_block(f)) {
            if (const auto block = product.child(f.name))
                for (std::uint8_t j = 0; j < f.type->field_count; ++j) {
                    const auto& leaf = f.type->fields[j];
                    if (leaf.is_attribute)
                        continue;
                    if (const auto value = scalar_text(*block, leaf.name)) {
                        auto& parameter = make_parameter(
                            data, holder_leaf_name(f.name, leaf.name), declared_type_of(leaf));
                        parameter.value = *value;
                    }
                }
            continue;
        }
        // A barrier holder may state a ScheduleData, so it is checked before an event.
        if (holds_barriers(f)) {
            if (product.has(f.name))
                read_barrier_holder(data, *product.child(f.name), f);
            continue;
        }
        if (holds_a_schedule(f)) {
            if (product.has(f.name)) {
                auto& parameter = make_parameter(data, std::string(f.name), "Event");
                read_event(data, parameter, *product.child(f.name));
            }
            continue;
        }
        // A member is a parameter only when it states a leaf, or a list of leaves.
        // Anything else is content the projection holds no row for, and reading it
        // as a leaf would refuse the document.
        if (states_a_free_style_scalar(f)) {
            if (const auto element = product.child(f.name)) {
                auto& parameter =
                    make_parameter(data, std::string(f.name), free_style_type_of(f, *element));
                if (const auto value = element->own_text())
                    parameter.value = stored_value_of(parameter, *value);
            }
            continue;
        }
        const auto* leaf = container_leaf_of(f);
        if (leaf == nullptr && !states_a_leaf(f))
            continue;
        if (!product.has(f.name))
            continue;
        auto& parameter = make_parameter(data, std::string(f.name), declared_type_of(f));
        if (leaf != nullptr) {
            parameter.shape = "vector";
            add_values(data, parameter, leaf_list(*product.child(f.name), leaf->name));
        } else if (const auto value = scalar_text(product, f.name)) {
            parameter.value = *value;
        }
    }
}

void read_class_underlyings(scripted_instrument_data& data, const xml::record_reader& product) {
    const auto& type = product.type();
    for (std::uint8_t i = 0; i < type.field_count; ++i) {
        const auto& f = type.fields[i];
        if (f.is_attribute || !product.has(f.name))
            continue;
        if (states_an_underlying(f)) {
            underlying_block::read_underlying(
                data, *product.child(f.name), role_of_member(f.name), 1);
        } else if (const auto* block = repeated_underlying_of(f)) {
            underlying_block::read_underlyings(
                data, *product.child(f.name), block->name, role_of_member(block->name));
        }
    }
}

void write_class_members(xml::record_writer& product, const scripted_instrument_data& data) {
    for (const auto* parameter : ordered_parameters(data)) {
        if (!product.declares(parameter->name))
            continue;
        if (parameter->shape == "schedule" || parameter->shape == "derived_schedule") {
            auto holder = product.child(parameter->name);
            write_event(holder, data, *parameter);
        } else if (parameter->shape == "vector") {
            const auto* leaf = container_leaf_of(product.type(), parameter->name);
            if (leaf == nullptr)
                continue;
            auto container = product.child(parameter->name);
            for (const auto& value : values_of(data, parameter->sequence_number))
                append_leaf(container, leaf->name, stated_value_of(*parameter, value));
        } else if (parameter->value) {
            const auto* f = field_named(product, parameter->name);
            if (f != nullptr && states_a_free_style_scalar(*f))
                product.child(parameter->name)
                    .set_own_text(stated_value_of(*parameter, *parameter->value));
            else
                set_scalar_text(product, parameter->name, *parameter->value);
        }
    }
}

void write_class_blocks(xml::record_writer& product, const scripted_instrument_data& data) {
    const auto& type = product.type();
    for (std::uint8_t i = 0; i < type.field_count; ++i) {
        const auto& f = type.fields[i];
        if (states_option_data(f) && !data.options.empty())
            option_block::write_option_data(product.child(f.name), data);
        else if (states_a_schedule(f))
            option_block::write_product_schedule(product, data, f.name, role_of_member(f.name));
        else if (states_a_barrier(f))
            barrier_block::write_barriers(product, data, barrier_role_of(f.name));
        else if (holds_barriers(f))
            write_barrier_holder(product, data, f);
        else if (states_a_leaf_block(f))
            write_leaf_block(product, data, f);
    }
}

/**
 * @brief Whether the element states both one Underlying and a list of them
 * under the role, as a generic barrier option does; the rows then say which.
 *
 * One row is written as the single form, so a list of one comes back as one
 * Underlying, which ORE reads the same.
 */
auto states_one_or_a_list(const xml::record_type& type, const std::string& role) -> bool {
    bool one = false;
    bool list = false;
    for (std::uint8_t i = 0; i < type.field_count; ++i) {
        const auto& f = type.fields[i];
        if (states_an_underlying(f) && role_of_member(f.name) == role)
            one = true;
        else if (const auto* block = repeated_underlying_of(f);
                 block != nullptr && role_of_member(block->name) == role)
            list = true;
    }
    return one && list;
}

void write_class_underlyings(xml::record_writer& product, const scripted_instrument_data& data) {
    const auto& type = product.type();
    for (std::uint8_t i = 0; i < type.field_count; ++i) {
        const auto& f = type.fields[i];
        if (states_an_underlying(f)) {
            const auto role = role_of_member(f.name);
            if (states_one_or_a_list(type, role) &&
                underlying_block::underlyings_of(data, role).size() > 1)
                continue;
            // The element states one block, which is the first row under its role.
            underlying_block::write_underlyings(product, f.name, data, role);
        } else if (const auto* block = repeated_underlying_of(f)) {
            const auto role = role_of_member(block->name);
            const auto rows = underlying_block::underlyings_of(data, role);
            if (rows.empty() || (rows.size() == 1 && states_one_or_a_list(type, role)))
                continue;
            auto container = product.child(f.name);
            for (const auto* row : rows)
                underlying_block::write_underlying(container.append(block->name), *row);
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
    const auto* class_read = class_read_product_of(trade_type);
    if (class_read != nullptr) {
        auto row = scripted_instrument_projection_make_base(trade_type);
        row.script_name = std::string(trade_type);
        scripted_instrument_data data{std::move(row)};
        if (const auto product = trade.child(class_read->element)) {
            read_class_members(data, *product);
            read_class_underlyings(data, *product);
        }
        return trading::domain::trade_instrument{std::move(data)};
    }

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

    if (const auto* class_read = class_read_product_of(trade_type)) {
        auto product = trade.child(class_read->element);
        write_class_members(product, *carrier);
        write_class_blocks(product, *carrier);
        write_class_underlyings(product, *carrier);
        return true;
    }

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
