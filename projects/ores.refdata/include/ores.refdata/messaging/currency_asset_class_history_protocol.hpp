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
#ifndef ORES_REFDATA_MESSAGING_CURRENCY_ASSET_CLASS_HISTORY_PROTOCOL_HPP
#define ORES_REFDATA_MESSAGING_CURRENCY_ASSET_CLASS_HISTORY_PROTOCOL_HPP

#include <span>
#include <iosfwd>
#include <vector>
#include <expected>
#include "ores.comms/messaging/message_type.hpp"
#include "ores.comms/messaging/message_traits.hpp"
#include "ores.utility/serialization/error_code.hpp"
#include "ores.refdata/domain/currency_asset_class.hpp"

namespace ores::refdata::messaging {

/**
 * @brief Request to retrieve version history for a currency asset class.
 */
struct get_currency_asset_class_history_request final {
    std::string code;  ///< Primary key

    std::vector<std::byte> serialize() const;
    static std::expected<get_currency_asset_class_history_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_currency_asset_class_history_request& v);

/**
 * @brief Response containing currency asset class version history.
 */
struct get_currency_asset_class_history_response final {
    bool success;
    std::string message;
    std::vector<domain::currency_asset_class> versions;

    std::vector<std::byte> serialize() const;
    static std::expected<get_currency_asset_class_history_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_currency_asset_class_history_response& v);

}

namespace ores::comms::messaging {

template<>
struct message_traits<refdata::messaging::get_currency_asset_class_history_request> {
    using request_type = refdata::messaging::get_currency_asset_class_history_request;
    using response_type = refdata::messaging::get_currency_asset_class_history_response;
    static constexpr message_type request_message_type =
        message_type::get_currency_asset_class_history_request;
};

}

#endif
