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
 * Template: cpp_protocol.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_REFDATA_API_MESSAGING_CROSS_CURRENCY_FIX_FLOAT_CONVENTION_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_CROSS_CURRENCY_FIX_FLOAT_CONVENTION_PROTOCOL_HPP

#include "ores.refdata.api/domain/convention.hpp"
#include "ores.refdata.api/domain/cross_currency_fix_float_fixed_leg.hpp"
#include "ores.refdata.api/domain/cross_currency_fix_float_float_leg.hpp"
#include "ores.refdata.api/messaging/convention_protocol.hpp"
#include "ores.refdata.api/messaging/cross_currency_fix_float_fixed_leg_protocol.hpp"
#include "ores.refdata.api/messaging/cross_currency_fix_float_float_leg_protocol.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::messaging {

struct cross_currency_fix_float_convention_key {
    boost::uuids::uuid id;
};

struct cross_currency_fix_float_convention_item {
    cross_currency_fix_float_convention_key key;
    std::optional<ores::refdata::domain::convention> convention;
    std::vector<ores::refdata::domain::cross_currency_fix_float_fixed_leg> fixed_leg;
    std::vector<ores::refdata::domain::cross_currency_fix_float_float_leg> float_leg;
};

struct cross_currency_fix_float_convention_write {
    convention_change change;
    std::vector<cross_currency_fix_float_fixed_leg_change> fixed_leg;
    std::vector<cross_currency_fix_float_float_leg_change> float_leg;
};

struct get_cross_currency_fix_float_convention_request {
    using response_type = struct get_cross_currency_fix_float_convention_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.cross_currency_fix_float_conventions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    cross_currency_fix_float_convention_key key;
};

struct get_cross_currency_fix_float_convention_response {
    ores::utility::domain::result result;
    std::optional<cross_currency_fix_float_convention_item> item;
};

struct get_many_cross_currency_fix_float_conventions_request {
    using response_type = struct get_many_cross_currency_fix_float_conventions_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.cross_currency_fix_float_conventions.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<cross_currency_fix_float_convention_key> keys;
};

struct get_many_cross_currency_fix_float_conventions_response {
    ores::utility::domain::result result;
    std::vector<cross_currency_fix_float_convention_item> items;
};

struct put_cross_currency_fix_float_convention_request {
    using response_type = struct put_cross_currency_fix_float_convention_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.cross_currency_fix_float_conventions.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    cross_currency_fix_float_convention_write item;
    ores::utility::domain::change_intent intent;
};

struct put_cross_currency_fix_float_convention_response {
    ores::utility::domain::result result;
    std::optional<cross_currency_fix_float_convention_item> item;
};

struct put_many_cross_currency_fix_float_conventions_request {
    using response_type = struct put_many_cross_currency_fix_float_conventions_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.cross_currency_fix_float_conventions.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<cross_currency_fix_float_convention_write> items;
    ores::utility::domain::change_intent intent;
};

struct put_many_cross_currency_fix_float_conventions_response {
    ores::utility::domain::result result;
    std::vector<cross_currency_fix_float_convention_item> items;
};

}

#endif
