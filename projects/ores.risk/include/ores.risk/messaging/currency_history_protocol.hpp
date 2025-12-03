/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_RISK_MESSAGING_CURRENCY_HISTORY_PROTOCOL_HPP
#define ORES_RISK_MESSAGING_CURRENCY_HISTORY_PROTOCOL_HPP

#include <span>
#include <iosfwd>
#include <vector>
#include <expected>
#include <rfl.hpp>
#include <rfl/json.hpp>
#include "ores.comms/messaging/message_types.hpp"
#include "ores.risk/domain/currency_version_history.hpp"

namespace ores::risk::messaging {

/**
 * @brief Request to retrieve version history for a currency.
 */
struct get_currency_history_request final {
    std::string iso_code;

    /**
     * @brief Serialize request to bytes.
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize request from bytes.
     */
    static std::expected<get_currency_history_request, comms::messaging::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_currency_history_request& v);

/**
 * @brief Response containing currency version history.
 */
struct get_currency_history_response final {
    bool success;
    std::string message;
    domain::currency_version_history history;

    /**
     * @brief Serialize response to bytes.
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize response from bytes.
     */
    static std::expected<get_currency_history_response, comms::messaging::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_currency_history_response& v);

}

#endif
