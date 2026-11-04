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
#ifndef ORES_REFDATA_API_MESSAGING_TENOR_BASIS_SWAP_CONVENTION_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_TENOR_BASIS_SWAP_CONVENTION_PROTOCOL_HPP

#include "ores.refdata.api/domain/convention.hpp"
#include "ores.refdata.api/domain/tenor_basis_swap_long_short.hpp"
#include "ores.refdata.api/domain/tenor_basis_swap_pay_receive.hpp"
#include "ores.refdata.api/messaging/convention_protocol.hpp"
#include "ores.refdata.api/messaging/tenor_basis_swap_long_short_protocol.hpp"
#include "ores.refdata.api/messaging/tenor_basis_swap_pay_receive_protocol.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::messaging {

struct tenor_basis_swap_convention_key {
    boost::uuids::uuid id;

    friend bool operator==(const tenor_basis_swap_convention_key&,
                           const tenor_basis_swap_convention_key&) = default;
};

struct tenor_basis_swap_convention_item {
    tenor_basis_swap_convention_key key;
    std::optional<ores::refdata::domain::convention> convention;
    std::vector<ores::refdata::domain::tenor_basis_swap_pay_receive> pay_receive;
    std::vector<ores::refdata::domain::tenor_basis_swap_long_short> long_short;

    friend bool operator==(const tenor_basis_swap_convention_item&,
                           const tenor_basis_swap_convention_item&) = default;
};

struct tenor_basis_swap_convention_write {
    convention_change change;
    std::vector<tenor_basis_swap_pay_receive_change> pay_receive;
    std::vector<tenor_basis_swap_long_short_change> long_short;

    friend bool operator==(const tenor_basis_swap_convention_write&,
                           const tenor_basis_swap_convention_write&) = default;
};

struct get_tenor_basis_swap_convention_request {
    using response_type = struct get_tenor_basis_swap_convention_response;
    static constexpr std::string_view nats_subject = "refdata.v1.tenor_basis_swap_conventions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    tenor_basis_swap_convention_key key;

    friend bool operator==(const get_tenor_basis_swap_convention_request&,
                           const get_tenor_basis_swap_convention_request&) = default;
};

struct get_tenor_basis_swap_convention_response {
    ores::utility::domain::result result;
    std::optional<tenor_basis_swap_convention_item> item;

    friend bool operator==(const get_tenor_basis_swap_convention_response&,
                           const get_tenor_basis_swap_convention_response&) = default;
};

struct get_many_tenor_basis_swap_conventions_request {
    using response_type = struct get_many_tenor_basis_swap_conventions_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.tenor_basis_swap_conventions.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<tenor_basis_swap_convention_key> keys;

    friend bool operator==(const get_many_tenor_basis_swap_conventions_request&,
                           const get_many_tenor_basis_swap_conventions_request&) = default;
};

struct get_many_tenor_basis_swap_conventions_response {
    ores::utility::domain::result result;
    std::vector<tenor_basis_swap_convention_item> items;

    friend bool operator==(const get_many_tenor_basis_swap_conventions_response&,
                           const get_many_tenor_basis_swap_conventions_response&) = default;
};

struct put_tenor_basis_swap_convention_request {
    using response_type = struct put_tenor_basis_swap_convention_response;
    static constexpr std::string_view nats_subject = "refdata.v1.tenor_basis_swap_conventions.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    tenor_basis_swap_convention_write item;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_tenor_basis_swap_convention_request&,
                           const put_tenor_basis_swap_convention_request&) = default;
};

struct put_tenor_basis_swap_convention_response {
    ores::utility::domain::result result;
    std::optional<tenor_basis_swap_convention_item> item;

    friend bool operator==(const put_tenor_basis_swap_convention_response&,
                           const put_tenor_basis_swap_convention_response&) = default;
};

struct put_many_tenor_basis_swap_conventions_request {
    using response_type = struct put_many_tenor_basis_swap_conventions_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.tenor_basis_swap_conventions.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<tenor_basis_swap_convention_write> items;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_tenor_basis_swap_conventions_request&,
                           const put_many_tenor_basis_swap_conventions_request&) = default;
};

struct put_many_tenor_basis_swap_conventions_response {
    ores::utility::domain::result result;
    std::vector<tenor_basis_swap_convention_item> items;

    friend bool operator==(const put_many_tenor_basis_swap_conventions_response&,
                           const put_many_tenor_basis_swap_conventions_response&) = default;
};

}

#endif
