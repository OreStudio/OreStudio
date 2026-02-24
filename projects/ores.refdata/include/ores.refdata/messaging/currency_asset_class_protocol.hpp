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
#ifndef ORES_REFDATA_MESSAGING_CURRENCY_ASSET_CLASS_PROTOCOL_HPP
#define ORES_REFDATA_MESSAGING_CURRENCY_ASSET_CLASS_PROTOCOL_HPP

#include <span>
#include <iosfwd>
#include <vector>
#include <expected>
#include "ores.comms/messaging/message_type.hpp"
#include "ores.comms/messaging/message_traits.hpp"
#include "ores.utility/serialization/error_code.hpp"
#include "ores.refdata/domain/currency_asset_class.hpp"

namespace ores::refdata::messaging {

// ============================================================================
// Currency Asset Class Messages
// ============================================================================

/**
 * @brief Request to retrieve all currency asset classes.
 */
struct get_currency_asset_classes_request final {
    std::vector<std::byte> serialize() const;
    static std::expected<get_currency_asset_classes_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_currency_asset_classes_request& v);

/**
 * @brief Response containing all currency asset classes.
 */
struct get_currency_asset_classes_response final {
    std::vector<domain::currency_asset_class> types;

    std::vector<std::byte> serialize() const;
    static std::expected<get_currency_asset_classes_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_currency_asset_classes_response& v);

/**
 * @brief Request to save a currency asset class (create or update).
 */
struct save_currency_asset_class_request final {
    domain::currency_asset_class type;

    std::vector<std::byte> serialize() const;
    static std::expected<save_currency_asset_class_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_currency_asset_class_request& v);

/**
 * @brief Response confirming currency asset class save operation.
 */
struct save_currency_asset_class_response final {
    bool success;
    std::string message;

    std::vector<std::byte> serialize() const;
    static std::expected<save_currency_asset_class_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_currency_asset_class_response& v);

/**
 * @brief Result for a single currency asset class deletion.
 */
struct delete_currency_asset_class_result final {
    std::string code;  ///< Primary key
    bool success;
    std::string message;
};

std::ostream& operator<<(std::ostream& s, const delete_currency_asset_class_result& v);

/**
 * @brief Request to delete one or more currency asset classes.
 */
struct delete_currency_asset_class_request final {
    std::vector<std::string> codes;  ///< Primary keys

    std::vector<std::byte> serialize() const;
    static std::expected<delete_currency_asset_class_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_currency_asset_class_request& v);

/**
 * @brief Response confirming currency asset class deletion(s).
 */
struct delete_currency_asset_class_response final {
    std::vector<delete_currency_asset_class_result> results;

    std::vector<std::byte> serialize() const;
    static std::expected<delete_currency_asset_class_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_currency_asset_class_response& v);

}

namespace ores::comms::messaging {

// Currency Asset Class traits
template<>
struct message_traits<refdata::messaging::get_currency_asset_classes_request> {
    using request_type = refdata::messaging::get_currency_asset_classes_request;
    using response_type = refdata::messaging::get_currency_asset_classes_response;
    static constexpr message_type request_message_type =
        message_type::get_currency_asset_classes_request;
};

template<>
struct message_traits<refdata::messaging::save_currency_asset_class_request> {
    using request_type = refdata::messaging::save_currency_asset_class_request;
    using response_type = refdata::messaging::save_currency_asset_class_response;
    static constexpr message_type request_message_type =
        message_type::save_currency_asset_class_request;
};

template<>
struct message_traits<refdata::messaging::delete_currency_asset_class_request> {
    using request_type = refdata::messaging::delete_currency_asset_class_request;
    using response_type = refdata::messaging::delete_currency_asset_class_response;
    static constexpr message_type request_message_type =
        message_type::delete_currency_asset_class_request;
};

}

#endif
