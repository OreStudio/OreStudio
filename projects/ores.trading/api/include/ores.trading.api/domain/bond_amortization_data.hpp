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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_field_group.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_TRADING_API_DOMAIN_BOND_AMORTIZATION_DATA_HPP
#define ORES_TRADING_API_DOMAIN_BOND_AMORTIZATION_DATA_HPP

#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include "ores.utility/digest/canonical_form.hpp"
#include <optional>
#include <string>
#include <tuple>

namespace ores::trading::domain {

/**
 * @brief One row of an amortization block.
 *
 */
struct bond_amortization_data {
    /**
     * @brief The kind of amortization step the row states, such as Constant or Floating.
     */
    std::string type;

    /**
     * @brief The amount the step moves the notional by, when the document states one.
     */
    std::optional<ores::utility::decimal::decimal> value;

    /**
     * @brief The date the step starts on, when the document states one.
     */
    std::optional<std::string> start_date;

    /**
     * @brief The date the step ends on, when the document states one.
     */
    std::optional<std::string> end_date;

    /**
     * @brief How often the step repeats over its window, in ORE's tenor spelling.
     */
    std::optional<std::string> frequency;

    /**
     * @brief True when the document allows the notional to fall below zero.
     */
    std::optional<bool> underflow;

    /**
     * @brief Value equality.
     *
     * A field group is a value like the entity that holds it: the entity's
     * comparison is defaulted and reads every member, so a group without a
     * comparison deletes the entity's and fails a build that treats that as
     * an error.
     */
    friend bool operator==(const bond_amortization_data&, const bond_amortization_data&) = default;
};
/**
 * @brief The members of bond_amortization_data, named as the wire names
 * them and in the order the type declares them.
 *
 * A group is a member of the entity that holds it, so the carriers reach this
 * shape through it: the parent's field names the member, and this one names the
 * members inside it.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<bond_amortization_data>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"type", &bond_amortization_data::type},
        utility::codec::field{"value", &bond_amortization_data::value},
        utility::codec::field{"start_date", &bond_amortization_data::start_date},
        utility::codec::field{"end_date", &bond_amortization_data::end_date},
        utility::codec::field{"frequency", &bond_amortization_data::frequency},
        utility::codec::field{"underflow", &bond_amortization_data::underflow},
    };
}

/**
 * @brief The canonical text of the members of bond_amortization_data the customer agreed to.
 *
 * Renders, in member order, the fields the model declares economic. The
 * economic digest hashes this text, so a field the customer did not agree to
 * never moves it.
 */
[[nodiscard]] inline std::string canonical_form(const bond_amortization_data& v) {
    std::string out = "{";
    ores::utility::digest::append_member(out, "type", v.type);
    ores::utility::digest::append_member(out, "value", v.value);
    ores::utility::digest::append_member(out, "start_date", v.start_date);
    ores::utility::digest::append_member(out, "end_date", v.end_date);
    ores::utility::digest::append_member(out, "frequency", v.frequency);
    ores::utility::digest::append_member(out, "underflow", v.underflow);
    out.push_back('}');
    return out;
}

}

#endif
