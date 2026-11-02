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
#ifndef ORES_REFDATA_API_MESSAGING_COMMODITY_FUTURE_CONVENTION_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_COMMODITY_FUTURE_CONVENTION_PROTOCOL_HPP

#include "ores.refdata.api/domain/commodity_future_averaging_terms.hpp"
#include "ores.refdata.api/domain/commodity_future_contract_terms.hpp"
#include "ores.refdata.api/domain/commodity_future_expiry_anchor.hpp"
#include "ores.refdata.api/domain/commodity_future_off_peak_index.hpp"
#include "ores.refdata.api/domain/commodity_future_option_expiry.hpp"
#include "ores.refdata.api/domain/convention.hpp"
#include "ores.refdata.api/messaging/commodity_future_averaging_terms_protocol.hpp"
#include "ores.refdata.api/messaging/commodity_future_contract_terms_protocol.hpp"
#include "ores.refdata.api/messaging/commodity_future_expiry_anchor_protocol.hpp"
#include "ores.refdata.api/messaging/commodity_future_off_peak_index_protocol.hpp"
#include "ores.refdata.api/messaging/commodity_future_option_expiry_protocol.hpp"
#include "ores.refdata.api/messaging/convention_protocol.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::messaging {

struct commodity_future_convention_key {
    boost::uuids::uuid id;
};

struct commodity_future_convention_item {
    commodity_future_convention_key key;
    std::optional<ores::refdata::domain::convention> convention;
    std::vector<ores::refdata::domain::commodity_future_averaging_terms> averaging_terms;
    std::vector<ores::refdata::domain::commodity_future_contract_terms> contract_terms;
    std::vector<ores::refdata::domain::commodity_future_expiry_anchor> expiry_anchor;
    std::vector<ores::refdata::domain::commodity_future_off_peak_index> off_peak_index;
    std::vector<ores::refdata::domain::commodity_future_option_expiry> option_expiry;
};

struct commodity_future_convention_write {
    convention_change change;
    std::vector<commodity_future_averaging_terms_change> averaging_terms;
    std::vector<commodity_future_contract_terms_change> contract_terms;
    std::vector<commodity_future_expiry_anchor_change> expiry_anchor;
    std::vector<commodity_future_off_peak_index_change> off_peak_index;
    std::vector<commodity_future_option_expiry_change> option_expiry;
};

struct get_commodity_future_convention_request {
    using response_type = struct get_commodity_future_convention_response;
    static constexpr std::string_view nats_subject = "refdata.v1.commodity_future_conventions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    commodity_future_convention_key key;
};

struct get_commodity_future_convention_response {
    ores::utility::domain::result result;
    std::optional<commodity_future_convention_item> item;
};

struct get_many_commodity_future_conventions_request {
    using response_type = struct get_many_commodity_future_conventions_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.commodity_future_conventions.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<commodity_future_convention_key> keys;
};

struct get_many_commodity_future_conventions_response {
    ores::utility::domain::result result;
    std::vector<commodity_future_convention_item> items;
};

struct put_commodity_future_convention_request {
    using response_type = struct put_commodity_future_convention_response;
    static constexpr std::string_view nats_subject = "refdata.v1.commodity_future_conventions.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    commodity_future_convention_write item;
    ores::utility::domain::change_intent intent;
};

struct put_commodity_future_convention_response {
    ores::utility::domain::result result;
    std::optional<commodity_future_convention_item> item;
};

struct put_many_commodity_future_conventions_request {
    using response_type = struct put_many_commodity_future_conventions_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.commodity_future_conventions.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<commodity_future_convention_write> items;
    ores::utility::domain::change_intent intent;
};

struct put_many_commodity_future_conventions_response {
    ores::utility::domain::result result;
    std::vector<commodity_future_convention_item> items;
};

}

#endif
